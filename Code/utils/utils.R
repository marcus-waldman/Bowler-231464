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
  K = with(raw, category1[type=="Skills" & corresponding_rq != "Not Included Skills" & participant_generated == F]) %>% unique() %>% length()
  
  # Subsetting data to just skills
  grid = expand.grid(round = 1:3, rq = c("Essential", "Competence", "Environment")) %>% arrange(round)
  final_variable_df <- lapply(1:nrow(grid), function(x){
    
    rq = grid$rq[x]
    ro = grid$round[x]
    
    subs <- new_dict |>
      subset(type == 'Skills' & round == ro & corresponding_rq == rq & participant_generated == 'FALSE') |>
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
  if(with(final_variable_df, table(skill_id, corresponding_rq)) %>% nrow() != 178){stop("Total skills differed from 178 in final_variable_df")} # 
  if(with(final_variable_df,  table(skill_id, corresponding_rq) %>% unique()) != 3){stop("Expected three rounds for each skill")}
  
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
  if(with(new_dict, table(skill_id, corresponding_rq)) %>% nrow() != 178){stop("Total skills differed from 178 in new_dict")} # Looks good 178 skills with data across three RQs
  if(with( new_dict %>% dplyr::filter(corresponding_rq %in% c("Competence","Environment","Essential")), unique(table(skill_id, corresponding_rq)) ) != 3){stop("Total skills differed from 178 in new_dict")} # Looks good 178 skills with data across three RQs
  
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
  

  return(new_dict %>% dplyr::arrange(col_id))
}