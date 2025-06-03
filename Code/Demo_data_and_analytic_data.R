rm(list = ls())

# Skills Study
# Creating Analytic Data Set

library(tidyverse)



# Cristian'S Windows
#root_wd = "C:/Users/sarabiac"
#onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Cristian's Mac
#root_wd = "/Users/cristiansarabia/Library/CloudStorage"
#onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")


# Marcus W. Locations
#root_wd = "C:/Users/waldmanm/"
#onedrive_wd = file.path(root_wd,"The University of Colorado Denver", "Bowler, Fara - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

# Marcus's Home Desktop (White-Rhino)
root_wd = "C:/Users/marcu"
onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")


#source(file.path(github_wd, "Code", "participant_demographics_data.R"))
source(file.path(github_wd, "Code", "utils", "utils.R"))


# Load in analytic dataset
dat = demo_and_response_data(onedrive_wd = onedrive_wd)



