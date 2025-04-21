#File:    utils.R
#Purpose: Script for sourcing in utility functions to support
#         Bowler-P231464
#Authors: M. Waldman; C. Sarabia

construct_data_dictionary<-function(onedrive_wd){
  library(tidyverse)
  library(readxl)
  library(writexl)
  
  raw <- read_xlsx(path =file.path(onedrive_wd,'Data','Data Dictionary','Dictionary - Custom Data Extract - 20240406.xlsx'))
  
  # Updating the skills section
  skills <- raw |>
    subset(type == 'Skills' & corresponding_rq != 'Not Included Skills') |>
    mutate(skill = str_trim(str_extract(full_stem, ':[^:]*$')) |>
             str_remove('^:') |>
             str_squish())
  
  # combingin raw with skills data and removing/updating names
  n_prev = nrow(raw)
  new_dict <- dplyr::left_join(raw %>% dplyr::select(-skill), 
                               skills %>% dplyr::select(col_id, skill), 
                               by = join_by('col_id')) |>
    #subset(select = - skill.x) |>
    #rename(skill = skill.y) |>
    relocate(skill, .after = skill_id)
  if(!identical(n_prev,nrow(new_dict))){stop("Joined file ended up wiht different number of rows")}

  # Sanity check: All skills in skills data frame above should appeaer in new_dict
  if(!identical(skills$skill %>% unique() %>% sort(), skills$skill %>% unique() %>% sort())){stop("Skills in `skills` data frame not equivalent to skills in dictionary after joining")}
  
  
  
  # Total number of skill categories, excluding those "not include" and excluding participant generated
  # Fix misspelling of category1
  new_dict$category1[new_dict$category1=="Fluid, Electrolye, and Acid-Base Balance"] = "Fluid, Electrolyte, and Acid-Base Balance"
  K = with(new_dict, category1[type=="Skills" & corresponding_rq != "Not Included Skills"]) %>% unique() %>% sort() %>% length()
  
  # Subsetting data to just skills
  grid = expand.grid(round = 1:3, rq = c("Essential", "Competence", "Environment")) %>% arrange(round)
  final_variable_df <- lapply(1:nrow(grid), function(x){
    
    rq = grid$rq[x]
    ro = grid$round[x]
    
    subs <- new_dict |>
      subset(type == 'Skills' & round == ro & corresponding_rq == rq) %>%  # & participant_generated == 'FALSE') |>
      mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
      arrange(category1)
    
    # Seperating based on skill in the list (And arranging alphabetically)
    categories <- c(unique(subs$category1))
    skill_list <- list()
    for (x in categories) {
      skill_list[[x]] <- subset(subs, category1 == x) %>% dplyr::arrange(skill)
    }
    
    # # Renaming var variablb
    for (i in 1:K) {
      skill_list[[i]]  = skill_list[[i]] %>%
        mutate( var = tolower(paste0('skill_', sapply(str_extract_all(category1, "[A-Z]"), function(x) paste(x, collapse = "")), '_', row.names(skill_list[[i]]))))
    }
    
    out_df = skill_list %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(skill_id = 1:n())
    
    
    return(out_df)
    
  }) %>% dplyr::bind_rows()
  
  # Sanity checks
  if(with(final_variable_df, table(skill_id, corresponding_rq)) %>% nrow() != 199){warning("Total skills differed from 199 (3 duplicates) in final_variable_df")} # 
  if(with(final_variable_df,  table(skill_id, corresponding_rq) %>% unique()) != 3){warning("Expected three rounds for each skill")}
  
  # Need to append variable name with RQ and round information skill_[category]_[skill_id]_[round]_[rq]
  final_variable_df = final_variable_df %>% 
    dplyr::mutate(var = ifelse(!is.na(var),paste(var,paste0("ro",round),stringr::str_to_lower(corresponding_rq),sep="_"),var))

  n_prev = nrow(new_dict)
  new_dict = new_dict %>% 
    dplyr::select(-skill_id, -var) %>% 
    dplyr::left_join(final_variable_df %>% dplyr::select(col_id,skill_id,var), by = "col_id") %>% 
    dplyr::relocate(skill_id, .after = corresponding_rq) %>% 
    dplyr::relocate(var, .after = skill)
  if(!identical(n_prev,nrow(new_dict))){stop("Joined file ended up wiht different number of rows")}
  
  # Sanity check: Number of skill by corresponding_RQ is 178
  if(with(new_dict, table(skill_id, corresponding_rq)) %>% nrow() != 199){warning("Total skills differed from 199 (3 duplicates) in new_dict")} # Looks good 178 skills with data across three RQs
  if(with( new_dict %>% dplyr::filter(corresponding_rq %in% c("Competence","Environment","Essential")), unique(table(skill_id, corresponding_rq)) ) != 3){warning("Total skills differed from 178 in new_dict")} # Looks good 178 skills with data across three RQs
  
  #Sanity check: skill_id var all the same
  J = max(new_dict$skill_id, na.rm = T)
  for(i in 1:length(1:J)){
    tmp = new_dict %>% 
      dplyr::filter(skill_id==i) %>% 
      dplyr::mutate(var = var %>% 
                      stringr::str_remove_all(pattern = paste0("_",tolower(corresponding_rq))) %>% 
                      stringr::str_remove_all(pattern = paste0("_ro",round))
                    )
    if(length(unique(tmp$skill))>1){stop("`skill` is not unique across skills_id")}
    if(length(unique(tmp$var))>1){stop("`var` is not unique across skills_id")}
  } #Good, all are unique
  
  
  # Add variable name KEY variables for email and full name
  new_dict = new_dict %>% 
    dplyr::mutate(var = ifelse(type=="Key" & str_detect(full_stem, "Email"), "email", var), 
                  var = ifelse(type=="Key" & str_detect(full_stem, "Full Name"), "participant", var)
    )
  
  # Add progress variable
  new_dict = new_dict %>% 
    dplyr::mutate(var = ifelse(type=="Progress", paste0("progress_ro",round), var))
  
  
  # Fix Duplicate Issues
  test_dict <- new_dict |> # Change
    filter(type == 'Skills') |> # Subsetting data to just skills
    mutate(skill_origin = case_when(
      participant_generated == FALSE & corresponding_rq != 'Not Included Skills' & category1 != 'Nutrition' ~ 'Original 166', # Names the original 166 skills
      participant_generated == TRUE ~ 'Respondant Provided',  # Participant created skills from round 1
      category1 == 'Nutrition' ~ 'Omitted Nu Skills' # Nutrition skills that were not added in round 1
    )) |> mutate(skill_origin = ifelse(str_detect(skill, 'nasogastic'), 'Duplicate', skill_origin), # Looks for the nasogastic spelling to assign as dupe
                 skill_origin = ifelse(str_detect(skill, 'parenteral '), 'Duplicate', skill_origin)) # looks for the parenteral spelling to assign as dupe
  
  # Sanity check (1 perfect replication, 2 fail to replicate [off by 1])
  test_dict %>% dplyr::filter(skill_origin == "Original 166", round == 1, corresponding_rq == "Essential") %>% nrow() #Yes, replicates 166
  test_dict %>% dplyr::filter(skill_origin == "Omitted Nu Skills", round == 1, corresponding_rq == "Essential") %>% nrow() #10 instead of 9
  test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow() #22 instead 21
  
  #Above non-replicatio issues Issue likely that we have duplicate item assisting a patient with eating
  tmp = test_dict %>% dplyr::filter(skill == "Assisting a patient with eating")
  identical(test_dict %>% slice(1) %>% purrr::pluck("skill"), test_dict %>% slice(1) %>% purrr::pluck("skill")) #Exactly the same skill text so never caught 
  raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))[, tmp$col_id]
  names(raw) = tmp$col_id
  tmp2 = raw %>% apply(2,function(x)sum(!is.na(x))) # Looks like it is the even and not the odd numbers that should be trusted
  col_ids_duplicate = names(tmp2)[as.integer(names(tmp2))%%2 == 1] %>% as.integer()
  
  test_dict = test_dict %>% 
    dplyr::mutate(skill_origin = ifelse(col_id %in% col_ids_duplicate, 'Duplicate', skill_origin)) #All empty responses: Assisting a patient with eating 
  
  
  #Sanity Checks
  nn = test_dict %>% dplyr::filter(skill_origin == "Original 166", round == 1, corresponding_rq == "Essential") %>% nrow()
  if( nn != 166){warning(paste0("Not replicating 166 original skills: ", nn))} #Yes, replicates 166
  
  nn = test_dict %>% dplyr::filter(skill_origin == "Omitted Nu Skills", round == 1, corresponding_rq == "Essential") %>% nrow()
  if(nn !=  9){warning(paste0("Not replicating 9 forgotten nutrition skills: ", nn))} 
  
  nn = test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow()
  if(nn != 21){warning(paste0("Not replicating 21 respondant provided skills: ", nn))}
  

  return(new_dict %>% dplyr::arrange(col_id))
}