# Skills Study
# Data Dictionary

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)

root_wd = "C:/Users/waldmanm"
onedrive_wd = file.path(root_wd, "The University of Colorado Denver/Bowler, Fara - March 2023_FB BH SH/")
github_wd = file.path(root_wd, "git-repositories/Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))


# Construct data dictionary
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)

# Load in the analytic data
raw = readxl::read_xlsx(
  path = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.xlsx"), 
  sheet = "Data", 
  range = "B7:CQH149"
)

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

writexl::write_xlsx(
  counts_df, 
  path = file.path(onedrive_wd, "Data", "Data Dictionary", "Response Counts - Custom Data Extract - 20240406.xlsx")
)

