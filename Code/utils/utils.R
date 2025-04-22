#File:    utils.R
#Purpose: Script for sourcing in utility functions to support
#         Bowler-P231464
#Authors: M. Waldman; C. Sarabia

construct_data_dictionary<-function(onedrive_wd){
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(raster)
  
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
  new_dict = new_dict %>% 
    dplyr::mutate(category1 = recode(category1, 
                                     `Pain management` = 'Pain Management', 
                                     `Patient safety` = 'Patient Safety',  
                                     `Fluid, Electrolyte, and Acid-Base Balance` = 'Fluid, Electrolye, and Acid-Base Balance'
                                     )
                  )
  
  K = with(new_dict, category1[type=="Skills" & corresponding_rq != "Not Included Skills"]) %>% unique() %>% sort() %>% length()
  
  #Sanity check:
  # Double check that the number of categories is the same across rounds and rq
  # Finding: There are different number of skills by the Hygiene and Skin Integrity categoriy across the different research questions. 
        tmp = new_dict %>% dplyr::filter(type=="Skills" & corresponding_rq != "Not Included Skills") %>% 
              dplyr::group_by(corresponding_rq, category1, round) %>% 
              dplyr::reframe(n = n()) %>% 
              dplyr::ungroup() 
        tmp_by_rq = tmp %>% 
          dplyr::group_by(corresponding_rq, category1) %>% 
          dplyr::reframe(n_unique = length(unique(n))) %>% 
          dplyr::arrange(desc(n_unique))
        #if(sum(tmp_by_rq$n_unique>1)){message("Differing number of skills when disaggregated by category ad RQ")}
        tmp_all  = tmp %>% 
          dplyr::group_by(category1) %>% 
          dplyr::reframe(n_unique = length(unique(n))) %>% 
          dplyr::arrange(desc(n_unique))
        #if(sum(tmp_by_rq$n_unique>1)){warning("Differing number of skills by category")}
        tmp %>% tidyr::pivot_wider(names_from = "corresponding_rq", values_from = "n") %>% 
          dplyr::left_join(tmp_all, by = "category1") %>% 
          dplyr::arrange(desc(n_unique))
        # category1                     round Competence Environment Essential n_unique
        # <chr>                         <dbl>      <int>       <int>     <int>    <int>
        # 1 Hygiene                           1         15          14        15        2
        # 2 Hygiene                           2         15          14        15        2
        # 3 Hygiene                           3         15          14        15        2
        # 4 Skin Integrity and Wound Care     1         18          19        18        2
        # 5 Skin Integrity and Wound Care     2         18          19        18        2
        # 6 Skin Integrity and Wound Care     3         18          19        18        2
      # Note: Based on the counts (Hygiene decrement, and Skin increment) It seems likely that a skill got transposed in Environment
        new_dict %>% 
          dplyr::filter(category1 %in% c("Hygiene","Skin Integrity and Wound Care"))  %>% 
          dplyr::group_by(skill) %>% 
          dplyr::reframe(n_categories = length(unique(category1)), 
                         modal_category = raster::modal(category1)) %>% 
          dplyr::arrange(desc(n_categories))
        # skill                                                                 n_categories modal_category               
        # <chr>                                                                        <int> <chr>                        
        #   1 Caring for a female urinary collection device like a purewick.                   2 Hygiene                      
        #   2 NA                                                                               2 Hygiene 
      
        # So we have an NA skill assigned a category which should not occur and `Caring for a female urinary collection device like a purewick.` which should go in the Hygiene category
          # Start with assigning `Caring for a female urinary collection device like a purewick.` to hygiene
        new_dict = new_dict %>% 
            dplyr::mutate(category1 = ifelse(skill == "Caring for a female urinary collection device like a purewick.", "Hygiene", category1))
        
        #Double check that this has solved it
        tmp = new_dict %>% dplyr::filter(type=="Skills" & corresponding_rq != "Not Included Skills") %>% 
          dplyr::group_by(corresponding_rq, category1, round) %>% 
          dplyr::reframe(n = n()) %>% 
          dplyr::ungroup() 
        tmp_all  = tmp %>% 
          dplyr::group_by(category1) %>% 
          dplyr::reframe(n_unique = length(unique(n))) %>% 
          dplyr::arrange(desc(n_unique)) # Good! All unique
        tmp_by_rq = tmp %>% 
          dplyr::group_by(corresponding_rq, category1) %>% 
          dplyr::reframe(n_unique = length(unique(n))) %>% 
          dplyr::arrange(desc(n_unique))
        #Let's get the number of skills by category1, 
        skill_count_all = tmp %>% dplyr::group_by(corresponding_rq, category1) %>% dplyr::reframe(n = unique(n)) %>%  tidyr::pivot_wider(names_from = corresponding_rq, values_from = n)
    categories = skill_count_all$category1
    
        
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
  if(with(new_dict, table(skill_id, corresponding_rq)) %>% nrow() != 199){warning("Total skills differed from 199 (3 duplicates) in new_dict")} # 
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
  if(test_dict %>% dplyr::filter(skill_origin == "Original 166", round == 1, corresponding_rq == "Essential") %>% nrow() != 166){warning("`Original 166` does not identify 166 skills")} #Yes, replicates 166
  if(test_dict %>% dplyr::filter(skill_origin == "Omitted Nu Skills", round == 1, corresponding_rq == "Essential") %>% nrow() != 9){warning("Omitted Nu Skills does not identify 9 skills")}#10 instead of 9
  if(test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow() != 21){warning("Respondant provided does not identify 22 skills")} #22 instead 21
  

  
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
  
      # Given Respondent provided is not replicating, let's print out the skills
      test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% dplyr::arrange(skill_id) %>% print(n = 30)
      #Found it, we have another duplicate
      test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% dplyr::filter(skill_id %in% c(31,32)) %>% dplyr::arrange(skill_id)
      # We need to which one actually has data
      col_ids = test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", corresponding_rq == "Essential") %>% dplyr::filter(skill_id %in% c(31,32)) %>% purrr::pluck("col_id")
      raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))[, col_ids]
      names(raw) = col_ids
      tmp2 = raw %>% apply(2,function(x)sum(!is.na(x))) # Look exactly the same
      sum_val = raw %>% apply(1, function(x)sum(!is.na(x)))
      raw[sum_val>0, ] # Exactly the same, choose the correctly spelled skill
      col_ids_duplicate = test_dict %>% dplyr::filter(stringr::str_starts(skill, "Preventing and responsing")) %>% purrr::pluck("col_id")
      test_dict = test_dict %>% 
        dplyr::mutate(skill_origin = ifelse(col_id %in% col_ids_duplicate, 'Duplicate', skill_origin)) #All empty responses: Assisting a patient with eating 
      
      test_dict %>% dplyr::filter(skill_origin == "Duplicate", round == 1, corresponding_rq == "Essential") %>% dplyr::group_by(skill) %>% dplyr::summarise(n = n())
  
    nn = test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow()
    if(nn != 21){warning(paste0("Not replicating 21 respondant provided skills: ", nn))}   
    
    
    # Clean up by removing skill_id from duplicates
    test_dict = test_dict %>% dplyr::mutate(skill_id = ifelse(skill_origin == "Duplicate", NA, skill_id), var = ifelse(skill_origin == "Duplicate", NA, var))
    test_dict %>% dplyr::filter(!is.na(var)) %>%  dplyr::group_by(corresponding_rq, round) %>% dplyr::summarise(n = n()) #looks good replicating the 196 skill
    
  # Replicate the orgininall 166 skill counts in Fara's 4/15/2025 email
    tmp = test_dict %>% dplyr::filter(skill_origin=="Original 166") %>% 
      dplyr::group_by(corresponding_rq, category1, round) %>% 
      dplyr::reframe(n = n()) %>% 
      dplyr::ungroup() 
    #Let's get the number of skills by category1, 
    skill_count_166 = tmp %>% dplyr::group_by(corresponding_rq, category1) %>% dplyr::reframe(n = unique(n)) %>%  tidyr::pivot_wider(names_from = corresponding_rq, values_from = n) %>% 
      dplyr::mutate(Fara = dplyr::case_when(
        startsWith(category1,"Activity") ~ 17, 
        startsWith(category1,"Asepsis") ~ 6,
        startsWith(category1,"Bowel") ~ 6, 
        startsWith(category1,"Cardiovascular") ~ 5, 
        startsWith(category1,"Fluid") ~ 14, 
        startsWith(category1, "Laboratory") ~ 13, 
        startsWith(category1,"Medication") ~ 32, 
        startsWith(category1,"Hygiene") ~ 14,
        startsWith(category1,"Oxygenation") ~ 20, 
        startsWith(category1,"Pain") ~ 6, 
        startsWith(category1,"Patient Safety") ~ 4, 
        startsWith(category1, "Skin") ~ 15, 
        startsWith(category1,"Urinary") ~ 13
      ))
    sum(skill_count_166$Fara) + 1
    sum(skill_count_166$Essential)  
  
    # categories that do not replicate Fara
    test_dict %>% dplyr::filter(
      corresponding_rq == "Essential", 
      round == 1, 
      skill_origin == "Original 166", 
      category1 %in% (skill_count_166 %>% dplyr::filter(Essential != Fara) %>% purrr::pluck("category1"))
    ) %>% 
    dplyr::select(col_id,skill_id,category1,skill) %>% dplyr::arrange(category1,skill_id) %>% 
    dplyr::filter(startsWith(category1,"Medication")) %>% 
      print(n = 50)
    
    test_dict %>% dplyr::filter(skill_origin %in% c("Omitted Nu Skills","Original 166", "Respondant Provided"), round == 1, corresponding_rq == "Essential") %>% dplyr::group_by(skill_origin) %>% dplyr::reframe(n = n()) 
      
      
  return(test_dict %>% dplyr::arrange(col_id))
}
