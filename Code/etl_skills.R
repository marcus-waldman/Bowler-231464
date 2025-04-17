# Skills Study
# Data Dictionary

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)

root_wd = "C:/Users/sarabiac"
onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))

setwd(github_wd)

# Construct data dictionary
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)

# Load in the analytic data
# raw = readxl::read_xlsx(
#   path = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.xlsx"), 
#   sheet = "Data", 
#   range = "B7:CQH149"
# )
# write_rds(raw, file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))

# Extract relevant columns and rename
extracted_cols =  with(dict, col_id[!is.na(var)]) %>% sort()
  #Sanity check 
  if ( ( length(extracted_cols)-2 )/(3*3) != 178 ){"Did not extract the expected number of columns"}
  
dat = raw %>% dplyr::select(extracted_cols)
dict_dat = dict %>% dplyr::slice(extracted_cols)
names(dat) = dict_dat$var


# Sanity check: email should uniquely identify participant
dat %>% dplyr::group_by(email) %>% dplyr::reframe(n = length(unique(participant)), participant = paste0(participant, sep = "; ")) %>% arrange(-n) #Yes


# Create a variable short and extraxct a skill description
varshort_skills = dict_dat %>% 
  dplyr::mutate(
    varshort = var %>% 
      stringr::str_remove_all(pattern = paste0("_", tolower(corresponding_rq))) %>% 
      stringr::str_remove_all(pattern = paste0("_ro",round))
    ) %>% 
  dplyr::group_by(varshort) %>% 
  dplyr::reframe(skill = unique(skill), category = category1[1]) %>% 
  na.omit()


# Counts of response types
rq = "essential"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Yes = sum(value=="Yes", na.rm = T), No = sum(value=="No", na.rm = T), Missing = sum(is.na(value))) %>% 
  dplyr::mutate(pct_Yes = round(100*Yes/(Yes + No)), 
                rq = "Essential") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


rq = "competence"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
competence_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Yes = sum(value=="Yes", na.rm = T), No = sum(value=="No", na.rm = T), Missing = sum(is.na(value))) %>% 
  dplyr::mutate(pct_Yes = round(100*Yes/(Yes + No)), rq = "Competence") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


rq = "environment"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
environment_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Clinical = sum(value=="clinical setting", na.rm = T), 
                 Simulation = sum(value=="simulation/ skills", na.rm = T), 
                 Didactic = sum(value=="didactic only", na.rm = T),
                 Missing = sum(is.na(value))) %>% 
  dplyr::mutate(rq = "Environment") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


counts_df = essential_responses %>% 
  dplyr::select(-rq) %>% 
  dplyr::left_join(competence_responses %>% dplyr::select(-(rq:skill)), by = dplyr::join_by(varshort,round)) %>% 
  dplyr::left_join(environment_responses %>% dplyr::select(-(rq:skill)), by = dplyr::join_by(varshort, round))




# Look at response by individual  (just essential)
items_agree_ro1 = counts_df %>% dplyr::filter(round == 1) %>% dplyr::select(category:varshort,pct_Yes.x) %>% 
  dplyr::arrange(-pct_Yes.x)

tmp = dat %>% dplyr::select(participant,email,dplyr::starts_with("progress"), dplyr::ends_with("_essential"))
names(tmp) = stringr::str_remove_all(names(tmp), "_essential")

items_keep = c(paste0(items_agree_ro1$varshort,"_ro1"), paste0(items_agree_ro1$varshort,"_ro2"), paste0(items_agree_ro1$varshort,"_ro3"))
items_keep = sort(items_keep)

tmp = tmp %>% dplyr::select(participant:progress_ro3, dplyr::any_of(items_keep)) %>% dplyr::arrange(desc(progress_ro1), desc(progress_ro2), desc(progress_ro3)) 


tmp_list = lapply(1:nrow(tmp), function(i){
  
  df_i = tmp %>% dplyr::select(-starts_with("progress_")) %>% dplyr::slice(i) %>% tidyr::pivot_longer(skill_aic_1_ro1:skill_ue_3_ro3, names_to = "var") %>% 
    dplyr::mutate(ro = NA, 
                  ro = ifelse(endsWith(var,"1"), 1, ro), 
                  ro = ifelse(endsWith(var,"2"), 2, ro), 
                  ro = ifelse(endsWith(var,"3"), 3, ro)) %>% 
    dplyr::mutate(varshort = var %>% 
                    stringr::str_remove_all(pattern = "_ro1") %>% 
                    stringr::str_remove_all(pattern = "_ro2") %>% 
                    stringr::str_remove_all(pattern = "_ro3")
    ) %>% 
    dplyr::select(-var) %>% 
    tidyr::pivot_wider(names_from = "ro", values_from = "value", names_prefix = "essential_ro") %>% 
    dplyr::arrange(varshort) %>% 
    dplyr::left_join(tmp %>% dplyr::select(email, starts_with("progress")), by = "email") %>% 
    dplyr::relocate(participant:varshort, progress_ro1, essential_ro1, progress_ro2, essential_ro2, progress_ro3, essential_ro3) %>% 
    dplyr::left_join(counts_df %>% dplyr::filter(round==1) %>% dplyr::select(varshort,category,skill, pct_Yes.x) %>% dplyr::rename(pct_Yes_ro1 = pct_Yes.x), b = "varshort") %>% 
    dplyr::select(varshort,participant:email,category,skill,pct_Yes_ro1,progress_ro1:essential_ro3)
  
  return(df_i)
  
})


names(tmp_list)  = tmp$participant

#write.csv(tmp_list[[4]], file =  file.path(onedrive_wd, "Data", "Data Dictionary","Case Study of Abigail Barron.csv"))

