# Skills Study
# Creating Participant Data DF

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)

# Adding location
# Windows Location
#root_wd = "C:/Users/sarabiac"
#onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")


# Mac Location
root_wd = "/Users/cristiansarabia/Library/CloudStorage"
onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")


# Bringing in data data
raw_demo <- read_excel(path = file.path(onedrive_wd, 'Data', 'Marcus and Cristian', 'calibrum_data_extract_part_demo.xlsx')) |>
  dplyr::select(c(1, 5, 6))

demo_data_pivot <- raw_demo |>
  pivot_wider(names_from = Question, values_from = `User Selection`,values_fn = list)

