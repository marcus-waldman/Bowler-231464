# Skills Study
# Contingency Tables

library(tidyverse)


# Cristian'S Windows
#root_wd = "C:/Users/sarabiac"
#onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Cristian's Mac
root_wd = "/Users/cristiansarabia/Library/CloudStorage"
onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")



# Bring in Analytic data

source(file.path(github_wd, "Code", "utils", "utils.R"))


# Load in analytic dataset
dat = demo_and_response_data(onedrive_wd = onedrive_wd) 

# Restrict to round 1 only and just for roles and skill
skill_data <- dat |>
  dplyr::filter(round ==1) |>
  dplyr::select(c(1, 7, 10, 13))


skill_table <- skill_data |>
  group_by(skill) |>
  group_split() |>
  setNames(unique(skill_data$skill)) |>
  lapply(function(df) list(table=table(df$role_primary, df$essential), test=NULL, bonferroni.pvalue=NA))



for(i in 1:166) {
  output <- fisher.test(skill_table[[i]]$table)
  skill_table[[i]]$test <- output
  skill_table[[i]]$bonferroni.pvalue <- output$p.value*166
}





# list(
#   table =
#     p_val = test$p.value,
#   stat = test$statistic,
#   expected = test$expected
# )