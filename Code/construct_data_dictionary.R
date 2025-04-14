# Skills Study
# Data Dictionary

library(tidyverse)
library(readxl)
library(writexl)

root_wd = "C:/Users/waldmanm"
onedrive_wd = file.path(root_wd, "The University of Colorado Denver/Bowler, Fara - March 2023_FB BH SH/")


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
  new_dict <- full_join(raw %>% dplyr::select(-skill,-participant_generated), skills, by = join_by('col_id', 'round', 'type', 'category1', 'category2', 'corresponding_rq', 'var', 'skill_id', 'full_stem')) |>
    #subset(select = - skill.x) |>
    #rename(skill = skill.y) |>
    relocate(skill, .after = skill_id)
  if(!identical(n_prev,nrow(new_dict))){stop("Joined file ended up wiht different number of rows")}
  
  
  # Total number of skill categories, excluding those "not include" and excluding participant generated
  K = with(raw, category1[type=="Skills" & corresponding_rq != "Not Included Skills" & participant_generated == F]) %>% unique() %>% length()
  
  # Subsetting data to just skills
  final_variable_df <- lapply(c("Essential", "Competence", "Environment"), function(rq){
    
    subs <- new_dict |>
      subset(type == 'Skills' & round == 1 & corresponding_rq == rq & participant_generated == 'FALSE') |>
      mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
      arrange(category1)
    
    # Seperating based on skill in the list (And arranging alphabetically)
    categories <- c(unique(subs$category1))
    skill_list <- list()
    for (x in skill) {
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
  
  # Need to append variable name with RQ information
  final_variable_df = final_variable_df %>% 
    dplyr::mutate(var = ifelse(!is.na(var),paste(var,stringr::str_to_lower(corresponding_rq),sep="_"),var))
  
  # Just select the columns that are needed to join to new_dict
  # final_variable_df = final_variable_df %>% 
  #   dplyr::select(type,category1,corresponding_rq:var) 
  
  n_prev = nrow(new_dict)
  new_dict = new_dict %>% 
    dplyr::select(-skill_id, -var) %>% 
    dplyr::left_join(final_variable_df) %>% 
    dplyr::relocate(skill_id, .after = corresponding_rq) %>% 
    dplyr::relocate(var, .after = skill)
  if(!identical(n_prev,nrow(new_dict))){stop("Joined file ended up wiht different number of rows")}
  
  # Sanity check: Number of skill by corresponding_RQ is 178
  with(new_dict, table(skill_id, corresponding_rq)) # Looks good 178 skills with data across three RQs
  
  #Sanity check: skill_id var all the same
  for(i in 1:length(1:J)){
    tmp = new_dict %>% 
      dplyr::filter(skill_id==i) %>% 
      dplyr::mutate(var = stringr::str_remove_all(var, pattern = paste0("_",tolower(corresponding_rq))))
    if(length(unique(tmp$skill))>1){stop("`skill` is not unique across skills_id")}
    if(length(unique(tmp$var))>1){stop("`var` is not unique across skills_id")}
  } #Good, all are unique
  
  return(new_dict)
}

new_dict = construct_data_dictionary(onedrive_wd=onedrive_wd)


