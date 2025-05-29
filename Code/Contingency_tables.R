# Skills Study
# Contingency Tables

library(tidyverse)


root_wd = "C:/Users/sarabiac"
onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")


source(file.path(github_wd, "Code", "participant_demographics_data.R"))

demo = demo_graphics_data(onedrive_wd=onedrive_wd)
