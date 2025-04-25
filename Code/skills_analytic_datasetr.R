rm(list =ls())

library(tidyverse)
library(readxl)
library(writexl)

root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"The University of Colorado Denver", "Bowler, Fara - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))

#Load in the dictionary and raw dataset
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))


#Extract only the appropriate columns (i.e., questions about skills, progress, and participant)
extracted_cols =  with(dict, col_id[!is.na(var)]) %>% sort()
dat = raw %>% dplyr::select(dplyr::any_of(extracted_cols))

#Remove any rows in the dict corresponding to columns in raw that are not used
dict_dat = dict %>% dplyr::slice(extracted_cols)

# Rename the variables according to the dictionary var code
names(dat) = dict_dat$var

# Construct long dataset that with participant responses to each RQ by round and skill
longdat = get_longdat_skills(rq="essential", dat = dat) %>% 
  dplyr::left_join(get_longdat_skills(rq="environment", dat = dat), by = c("participant", "email", "varshort", "round")) %>% 
  dplyr::left_join(get_longdat_skills(rq = "competence", dat = dat), by = c("participant", "email", "varshort", "round"))

# Identify and mask skills that reached 80% consensus in round 1
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

# Identify and mask skills that reached 80% consensus in round 1
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



#Mask the erroneously carry forward responses recorded for individuals who did not participate in that round
  
  #Construct a long progress dataframe
    progress_df = raw[,dict$col_id[which(dict$type == "Progress" | dict$var=="participant" | dict$var=="email")]] %>% data.frame()
    names(progress_df) = c("participant", "email","progress_ro1", "progress_ro2", "progress_ro3")
    progress_df = progress_df %>% 
      dplyr::filter(!is.na(progress_ro1), !is.na(progress_ro2), !is.na(progress_ro3)) %>% 
      dplyr::mutate(across(progress_ro1:progress_ro3, as.numeric)) %>% 
      tidyr::pivot_longer(progress_ro1:progress_ro3, names_to = "round", values_to = "progress") %>% 
      dplyr::mutate(round = round %>% stringr::str_remove_all("progress_ro") %>% as.integer())
  
  # Join the progress data frame
    longdat = longdat %>% 
      dplyr::left_join(progress_df, by = c("participant","email","round")) %>% 
      dplyr::relocate(progress, .before = essential)
  
  # Mask
    longdat = longdat %>% 
      dplyr::mutate(
        essential = dplyr::case_when(is.na(progress) ~ 0, .default = essential),
        environment = dplyr::case_when(is.na(progress) ~ 0, .default = environment),
        competence = dplyr::case_when(is.na(progress) ~ 0, .default = competence),
      )
    
  
  
