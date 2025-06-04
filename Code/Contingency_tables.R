# Skills Study
# Contingency Tables

library(tidyverse)


# Cristian'S Windows
root_wd = "C:/Users/sarabiac"
onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Cristian's Mac
#root_wd = "/Users/cristiansarabia/Library/CloudStorage"
#onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")



# Bring in Analytic data
#source(file.path(, "Code", "utils", "utils.R"))

skill_data = demo_and_response_data(onedrive_wd = onedrive_wd)

skill_table <- skill_data |>
  group_by(skill) |>
  group_split() |>
  setNames(unique(skill_data$skill)) |>
  lapply(function(df) table(df$role, df$essential))


for(i in length(skil_table)) {
  output <- fisher.test(skil_table[[i]])
  print(output)
  print(output$p.value /166)
}




# list(
#   table =
#     p_val = test$p.value,
#   stat = test$statistic,
#   expected = test$expected
# )