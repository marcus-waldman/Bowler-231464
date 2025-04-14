# File:     skill-counts.R
# Purpose:  Generates summaries of skill counts from data dictionary

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)


onedrive_root = "C:/Users/waldmanm/The University of Colorado Denver/"

# Set working directory
setwd(file.path(onedrive_root, "Bowler, Fara - March 2023_FB BH SH"))
dict = readxl::read_excel(path = file.path(getwd(), "Data/Data Dictionary/Dictionary - Custom Data Extract - 20240406.xlsx"))

# Summary of skill count by round 
sheet1 = dict %>% dplyr::filter(type == "Skills") %>% 
  dplyr::group_by(corresponding_rq, round) %>% dplyr::reframe(n_vars = n()) %>% 
  dplyr::filter(corresponding_rq %in% c("Essential","Environment","Competence")) %>% 
  tidyr::pivot_wider(names_from = "corresponding_rq", values_from = "n_vars")

# Detailed skill count break down 
sheet2 = dict %>% dplyr::filter(type == "Skills") %>% 
  dplyr::group_by(corresponding_rq, participant_generated, category1, round) %>% dplyr::reframe(n_vars = n()) %>% 
  dplyr::filter(corresponding_rq %in% c("Essential","Environment","Competence")) %>% 
  tidyr::pivot_wider(names_from = "corresponding_rq", values_from = "n_vars") %>% 
  dplyr::arrange(category1, round) %>% 
  dplyr::relocate(round,category1,participant_generated)

# Save the skill counts into an Excel file
writexl::write_xlsx(list(Summary = sheet1, Detailed = sheet2), path = file.path(getwd(), "Data/Data Dictionary/Skill Counts - Custom Data Extract - 20240406.xlsx"))