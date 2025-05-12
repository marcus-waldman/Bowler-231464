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
    #filter(type == 'Skills') |> # Subsetting data to just skills
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
    
    
  # Include key with rater
    test_dict = test_dict %>% dplyr::mutate(var = ifelse(full_stem == "SYSTEM: Owner Full Name" & type == "Key", "participant", var), 
                                            var = ifelse(startsWith(full_stem,"SYSTEM: Owner Email") & type=="Key", "email", var))
    
      
  return(test_dict %>% dplyr::arrange(col_id))
}

get_longdat_skills<-function(rq, dat){
  
  ret = dat %>% dplyr::select(participant, email,dplyr::ends_with(tolower(rq)))
  names(ret) = stringr::str_remove_all(names(ret), paste0("_",rq))
  ret = ret %>% 
    tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
    dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                  varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
    dplyr::relocate(round,.before=value) %>% 
    dplyr::arrange(participant,varshort,round)
  names(ret)[names(ret)=="value"] = rq
  
  return(ret)
}

varshort_skills<-function(dict){
  
  out =dict %>%
    dplyr::filter(startsWith(var, "skill")) %>% 
    dplyr::mutate(
      varshort = var %>% 
        stringr::str_remove_all(pattern = paste0("_", tolower(corresponding_rq))) %>% 
        stringr::str_remove_all(pattern = paste0("_ro",round))
    ) %>% 
    dplyr::group_by(varshort) %>% 
    dplyr::reframe(skill = unique(skill), category = category1[1], skill_origin = skill_origin[1]) %>% 
    na.omit()
  
  print(table(out$skill_origin)) # yes, 9,166,21
  
  return(out)
  
  
}


get_essential_environment_competence_longdat<-function(raw, dict){
  #Extract only the appropriate columns (i.e., questions about skills, progress, and participant)
  extracted_cols =  with(dict, col_id[ var == "participant" | var == "email" | startsWith(var,"progress") | startsWith(var,"skill")]) %>% sort()
  dat = raw %>% dplyr::select(dplyr::any_of(extracted_cols))
  
  #Remove any rows in the dict corresponding to columns in raw that are not used
  dict_dat = dict %>% dplyr::slice(extracted_cols)
  
  # Rename the variables according to the dictionary var code
  names(dat) = dict_dat$var
  
  # Construct long dataset that with participant responses to each RQ by round and skill
  longdat = get_longdat_skills(rq="essential", dat = dat) %>% 
    dplyr::left_join(get_longdat_skills(rq="environment", dat = dat), by = c("participant", "email", "varshort", "round")) %>% 
    dplyr::left_join(get_longdat_skills(rq = "competence", dat = dat), by = c("participant", "email", "varshort", "round"))
  
  
  
  # Remove Leslie Graham's duplicate responses associated with incorrect email address
  longdat = longdat %>% 
    dplyr::filter(email != "leslie.graham@durhamcollege.cs")
  
  
  
  # Identify and mask round 2 and round 3 responses to skills that reached 80% consensus in round 1
  consensus_skill_ro1 = longdat %>% 
    dplyr::filter(round==1) %>% 
    dplyr::group_by(varshort) %>% 
    dplyr::reframe(pct_essential_ro1 = mean(essential == "Yes", na.rm = T)) %>% 
    dplyr::filter(pct_essential_ro1 >= .80 | pct_essential_ro1 <=.20) %>% 
    purrr::pluck("varshort")
  
  longdat = longdat %>% 
    dplyr::mutate(
      essential = dplyr::case_when(
        (varshort %in% consensus_skill_ro1) & (round %in% c(2,3)) ~ NA,
        .default = essential
      ), 
      environment = dplyr::case_when(
        (varshort %in% consensus_skill_ro1) & (round %in% c(2,3)) ~ NA,
        .default = environment
      ), 
      competence = dplyr::case_when(
        (varshort %in% consensus_skill_ro1) & (round %in% c(2,3)) ~ NA,
        .default = competence
      )
    )
  
  # Identify and mask responses to skills in round 3 that reached 80% consensus in round 1
  consensus_skill_ro2 = longdat %>% 
    dplyr::filter(round==2) %>% 
    dplyr::group_by(varshort) %>% 
    dplyr::reframe(pct_essential_ro1 = mean(essential == "Yes", na.rm = T)) %>% 
    dplyr::filter(pct_essential_ro1 >= .80 | pct_essential_ro1 <=.20) %>% 
    purrr::pluck("varshort")
  
  longdat = longdat %>% 
    dplyr::mutate(
      essential = dplyr::case_when(
        (varshort %in% consensus_skill_ro2) & (round==3) ~ NA,
        .default = essential
      ), 
      environment = dplyr::case_when(
        (varshort %in% consensus_skill_ro2) & (round==3) ~ NA,
        .default = environment
      ), 
      competence = dplyr::case_when(
        (varshort %in% consensus_skill_ro2) & (round==3) ~ NA,
        .default = competence
      )
    )
  
  
  #Sanity check: Do we still have 196 skills per round
  longdat %>% dplyr::group_by(round) %>% dplyr::reframe(n = length(unique(varshort))) #yes
  
  
  #Mask the erroneously carry forward responses recorded for individuals who did not participate in rounds 2 or 3
  
  #Construct a long progress dataframe that give the progress of each responder in each round
  progress_df = raw[,dict$col_id[which(dict$type == "Progress" | dict$var=="participant" | dict$var=="email")]] %>% data.frame()
  names(progress_df) = c("participant", "email","progress_ro1", "progress_ro2", "progress_ro3")
  progress_df = progress_df %>% 
    dplyr::mutate(across(progress_ro1:progress_ro3, as.numeric)) %>% 
    tidyr::pivot_longer(progress_ro1:progress_ro3, names_to = "round", values_to = "progress") %>% 
    dplyr::mutate(round = round %>% stringr::str_remove_all("progress_ro") %>% as.integer())
  
  
  # Filter out Leslie' Graham's duplicate responses from incorrect email
  progress_df = progress_df %>% dplyr::filter(email != "leslie.graham@durhamcollege.cs") 
  
  ## To be counted as sufficient progress, require at least 80% in that round
  #  progress_df = progress_df  %>% 
  #    dplyr::filter(progress>=80) 
  
  # Get the number of respondants that meet qualifications
  progress_df %>% dplyr::group_by(round) %>% dplyr::summarise(n = length(unique(email)))
  
  # Join the progress data frame
  longdat = longdat %>% 
    dplyr::left_join(progress_df, by = c("participant","email","round")) %>% 
    dplyr::relocate(progress, .before = essential)
  
  #Sanity check: Do we still have 196 skills per round
  longdat %>% dplyr::group_by(round) %>% dplyr::reframe(n = length(unique(varshort))) #yes      
  
  # Set missing responses where insufficient progress was made
  longdat = longdat %>% 
    dplyr::mutate(
      essential = dplyr::case_when(is.na(progress) ~ NA, .default = essential),
      environment = dplyr::case_when(is.na(progress) ~ NA, .default = environment),
      competence = dplyr::case_when(is.na(progress) ~ NA, .default = competence),
    )
  
  # Sanity check: is there a correlation between the progress in a round and the number of essential responses answered
  essentials_answered = longdat %>% dplyr::group_by(participant,round) %>% dplyr::reframe(progress = unique(progress), n_ans = sum(!is.na(essential)))
  ggplot(essentials_answered, aes(x = progress, y = n_ans)) + geom_point() + facet_grid(round~.) #Yes, however it appears that in round 3 people who had 100% progress asnwered differnet number of questions there's a correlation 
  # Follow up: People with 100% progress answer different number of questions?
  longdat %>% dplyr::filter(progress==100) %>% dplyr::group_by(participant,round) %>% dplyr::reframe(progress = unique(progress), n_ans = sum(!is.na(essential))) %>% dplyr::ungroup() %>% dplyr::group_by(round) %>% dplyr::reframe(min = min(n_ans), max = max(n_ans))
  # Yes, there does seem to be differing number of questrions answered based on 100% progress in round 3.
  
  # Sanity check: Missing data patterns need to make sense. In otherwords, only can answer environment and competence if answered essential
  for(ro in 1:3){
    print(paste0("round ", ro))
    print(mice::md.pattern(longdat %>% dplyr::filter(round == ro) %>% dplyr::select(essential,environment,competence), plot = F))
  }
  # Appears in round 3 there are four incidents where essential is missing but environment and or competence is not missing. 
  
  # Mask any responses to environment and competence if essential is missing
  longdat = longdat %>% 
    dplyr::mutate(
      environment = ifelse(is.na(essential), NA, environment), 
      competence = ifelse(is.na(essential), NA, competence)
    )
  
  # Sanity check: Missing data patterns need to make sense. In otherwords, only can answer environment and competence if answered essential
  for(ro in 1:3){
    print(paste0("round ", ro))
    print(mice::md.pattern(longdat %>% dplyr::filter(round == ro) %>% dplyr::select(essential,environment,competence), plot = F))
  } #Yes, they all make sense now.
  
  
  
  # Sanity check: Do any round 1 consensus items show up in round 2 or 3 in longdat?
  intersect(longdat %>% dplyr::filter(round > 1) %>% purrr::pluck("varshort") %>% unique(), consensus_skill_ro1) #No
  
  # Sanity check Do any round 2 consensus items show up in round 3 in longdat?
  intersect(longdat %>% dplyr::filter(round > 2) %>% purrr::pluck("varshort") %>% unique(), consensus_skill_ro2) #No
  
  
  
  # Finalize the dataset 
  
  #Drop rows where there is no essential response (and hence no competence or environment response)
  longdat = longdat %>% 
    dplyr::filter(!is.na(essential))
  
  
  # Sanity check See pattern of included items by round      
  longdat %>% dplyr::mutate(answered = !is.na(essential)) %>% dplyr::group_by(varshort, round) %>% dplyr::reframe(answered = sum(answered)>0) %>% dplyr::mutate(answered = ifelse(answered==F, NA, answered)) %>% 
    tidyr::pivot_wider(names_from = round, values_from = answered) %>% 
    dplyr::select(-varshort) %>% 
    md.pattern()
  #Caution! NOTE that there are three items that show up in round 3 but do not show up in rounds 1 and 2
  #Identify which three items
  longdat %>% dplyr::mutate(answered = !is.na(essential)) %>% dplyr::group_by(varshort, round) %>% dplyr::reframe(answered = sum(answered)>0) %>% dplyr::mutate(answered = ifelse(answered==F, NA, answered)) %>% 
    tidyr::pivot_wider(names_from = round, values_from = answered) %>% 
    dplyr::filter(is.na(`1`), is.na(`2`)) %>% 
    dplyr::left_join(varshort_skills(dict = dict), by = "varshort")
  # All three items are nutrition items that were omitted
  
  
  # Drop rows corresponding to participants with no recorded progress in the round
  longdat = longdat %>% 
    dplyr::filter(!is.na(progress)) 
  
  # Identify the n=36 rounds 1-3 completers and add a column identifying if a participant was a completer
  completers_36 = longdat %>% 
    dplyr::group_by(email, round) %>% 
    dplyr::reframe(progress = progress[1]) %>% 
    dplyr::group_by(email) %>% 
    dplyr::reframe(n = n()) %>% 
    dplyr::filter(n==3)
  longdat = longdat %>% dplyr::mutate(completer = (email %in% completers_36$email)) %>% 
    dplyr::relocate(completer, .after = participant)
  
  # Sanity check Expect no environment resopnse if participant asnwered "no" to essential
  longdat %>% dplyr::filter(essential == "No") %>% with(., table(environment,competence,round, useNA = "always"))
    # Most are not answered, but there are a small amount of cases where they were answered
    # Response: Mask those small amount of cases where they were answered
    longdat = longdat %>% 
      dplyr::mutate(
        competence = ifelse(essential == "No", NA, competence), 
        environment = ifelse(essential == "No", NA, environment)
      )

  # Clean-up: Make the categories for environment prettier and shorter
  longdat = longdat %>% 
    dplyr::mutate(
      environment = dplyr::case_when(
        startsWith(environment, "clinical") ~ "Clinical", 
        startsWith(environment, "didactic") ~ "Didactic", 
        startsWith(environment, "simulation") ~ "Simulation", 
        .default = NA
      )
    )
  
  # Turn to essential, environment, competence to factor
   longdat = longdat %>% 
     dplyr::mutate(
       essential = dplyr::case_when(
         essential == "No" ~ 0,
         essential == "Yes" ~ 1, 
         .default = NA
       ), 
       competence = dplyr::case_when(
         competence == "No" ~ 0,
         competence == "Yes" ~ 1, 
         .default = NA
       ), 
       environment = dplyr::case_when(
         environment == "Didactic" ~ 0,
         environment == "Simulation" ~ 1, 
         environment == "Clinical" ~ 2,
         .default = NA
       )
     ) %>% 
     dplyr::mutate(
       essential = factor(essential, levels = 0:1, labels = c("No","Yes")), 
       competence = factor(competence, levels = 0:1, labels = c("No","Yes")),
       environment = factor(environment, levels = 0:2, labels = c("Didactic","Simulation", "Clinical"))
     ) 
   
   longdat$essential = relevel(longdat$essential, ref = "No")
   longdat$competence = relevel(longdat$competence, ref = "No")
   longdat$environment = relevel(longdat$environment, ref = "Didactic")
   
    
  #### Sanity checks ###
  # Participants in each round
  identical(
    longdat %>% dplyr::group_by(round) %>% dplyr::reframe(n = length(unique(email))), 
    progress_df %>% dplyr::group_by(round) %>% dplyr::summarise(n = length(unique(email)))
  ) # Yes 
  
  # Skills in each round
  longdat %>% dplyr::group_by(round) %>% dplyr::reframe(nskills = length(unique(varshort))) 
  #Does the 108 make sense
  identical(166 - length(consensus_skill_ro1) + (9 - 3) + 21,108) 
  # Yes 108 = 166 items administered - 85 consensus items in round 1 + adding 6 of 9 omitted nutrition items + adding 21 user provided items in round 3
  #Does 89 make sense
  identical(108 - length(consensus_skill_ro2) + 3, 89) 
  #Yes 89 = 108 round 2 items - 22 items reaching consensus + the remaining 3 omitted nutrition items.
  

  
  return(longdat)
  
}

get_essential_by_round<-function(longdat, round = T){
  consensus_long = lapply(1:3, function(ro){
    
    consensus_df = longdat %>% dplyr::filter(round == ro) %>% 
      dplyr::group_by(varshort) %>% 
      dplyr::reframe(n = length(essential), 
                     `Yes (%)` = 100*mean(essential == "Yes"), 
                     `No (%)` = 100*mean(essential == "No")
                     )
    
    
    varshort_df = varshort_skills(dict=dict) %>% 
      dplyr::left_join(consensus_df, by = "varshort") %>% 
      dplyr::mutate(round = ro, rq = "Is this skill essential?") %>% 
      dplyr::relocate(rq,round)
    
    return(varshort_df)
    
  }) %>% dplyr::bind_rows()
  
  if(round){
    consensus_long = consensus_long %>% 
      dplyr::mutate(across(`Yes (%)`:`No (%)`, function(x){round(x,1)}))
  }
  return(consensus_long)
}

get_competence_by_round<-function(longdat, round = T){
  consensus_long = lapply(1:3, function(ro){
    
    consensus_df = longdat %>% dplyr::filter(round == ro, essential == "Yes") %>% 
      dplyr::group_by(varshort) %>% 
      dplyr::reframe(n = length(!is.na(competence)), 
                     `Yes (%)` = 100*mean(competence == "Yes",na.rm=T), 
                     `No (%)` = 100*mean(competence == "No",na.rm =T)
      )
    
    
    varshort_df = varshort_skills(dict=dict) %>% 
      dplyr::left_join(consensus_df, by = "varshort") %>% 
      dplyr::mutate(round = ro, rq = "Should skill be taught to competence?") %>% 
      dplyr::relocate(rq,round)
    
    return(varshort_df)
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate( Mode = ifelse(`No (%)` > `Yes (%)`, "No", "Yes"))
  
  if(round){
    consensus_long = consensus_long %>% 
      dplyr::mutate(across(`Yes (%)`:`No (%)`, function(x){round(x,1)}))
  }
  
  return(consensus_long)
}


get_environment_by_round<-function(longdat, round = T){
  consensus_long = lapply(1:3, function(ro){
    
    consensus_df = longdat %>% dplyr::filter(round == ro, essential == "Yes") %>% 
      dplyr::group_by(varshort) %>% 
      dplyr::reframe(n = length(!is.na(environment)), 
                     `Didactic (%)` = 100*mean(environment == "Didactic",na.rm =T), 
                     `Simulation (%)` = 100*mean(environment == "Simulation",na.rm =T), 
                     `Clinical (%)` = 100*mean(environment == "Clinical",na.rm=T)
      )
    
    
    varshort_df = varshort_skills(dict=dict) %>% 
      dplyr::left_join(consensus_df, by = "varshort") %>% 
      dplyr::mutate(round = ro, rq = "How should skill be taught?") %>% 
      dplyr::relocate(rq,round)
    
    return(varshort_df)
    
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(
      Mode = dplyr::case_when(
        .default = "Didactic", 
        (`Simulation (%)` > `Didactic (%)`) & (`Simulation (%)` > `Clinical (%)`) ~ "Simulation", 
        (`Clinical (%)` > `Didactic (%)`) & (`Clinical (%)` > `Simulation (%)`) ~ "Clinical"
      )
    )
  
  if(round){
    consensus_long = consensus_long %>% 
      dplyr::mutate(across(`Didactic (%)`:`Clinical (%)`, function(x){round(x,1)}))
  }
  
  return(consensus_long)
}




# 
# root_wd<-function(analyst,computer){
#   analyst = tolower(analyst); computer = tolower(computer)
#   
#   if(analyst=="marcus"){
#     root = ifelse(computer=="work", "C:/Users/waldmanm", "C:/Users/marcu")
#   } else if (analyst == "cristian"){
#     root = ifelse(computer=="work","C:/Users/sarabiac", "/Users/cristiansarabia/Library/CloudStorage")
#   } else {
#     root = NULL
#   }
#   return(root)
# }
# onedrive_wd<-function(who, where){
#   
#   who = tolower(who); where = tolower(where)
#   root = root_wd(analyst=who,computer=where)
#   if(who=="marcus"){
#      
#        onedrive <- 
#          file.path(
#            root, 
#            "The University of Colorado Denver", 
#             "Bowler, Fara - March 2023_FB BH SH"
#            )
#    
#     } else if (who == "cristian"){
#        
#       onedrive <- 
#          file.path(
#            root, 
#            ifelse(where=="home", 
#                   "OneDrive-TheUniversityofColoradoDenver", 
#                   "OneDrive - The University of Colorado Denver"
#                   ), 
#             "College of Nursing", 
#             "Skills Study", 
#             "Bowler, Fara's files - March 2023_FB BH SH"
#           )
#       
#    } else {
#      
#      onedrive <- NULL
#      
#    }
#   
#   return(onedrive)
#   
# }
# github_wd<-function(who, where){
#   
#   who = tolower(who); where = tolower(where)
#   root = root_wd(analyst=who,computer=where)
#   if(who=="marcus"){
#     
#     git <- 
#       file.path(
#         root, 
#         "git-repositories", 
#         "Bowler-231464"
#       )
#     
#   } else if (who == "cristian"){
#     
#     git <- 
#       file.path(
#         root, 
#         ifelse(tolower(where)=="home", 
#                "OneDrive-TheUniversityofColoradoDenver", 
#                "OneDrive - The University of Colorado Denver"
#         ), 
#         "College of Nursing", 
#         "Repos",
#         ifelse(tolower(where)=="home", 
#                "Mac Repo", 
#                "Windows Repo"
#         ),
#         "Bowler-231464"
#       )
#     
#   } else {
#     
#     git <- NULL
#     
#   }
#   
#   return(git)
#   
# }
# wd<-function(which.dir = "root", who, where){
#   dplyr::case_when(
#     which.dir=="root" ~  root_wd(analyst=who,computer=where), 
#     which.dir=="onedrive" ~ onedrive_wd(who=who, where=where), 
#     which.dir=="github" ~ github_wd(who=who,where=where), 
#     .default = NULL
#   )
# }