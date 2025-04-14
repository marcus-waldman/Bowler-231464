# Naming Variables
# Skills Study
library(tidyverse)
library(readxl)
library(writexl)

# Reading in data
raw <- read_xlsx('Dictionary - Custom Data Extract - 20240406.xlsx')

# Subsetting data to just skills
subs <- raw |>
  subset(type == 'Skills' & round == 1 & corresponding_rq == 'Essential' & participant_generated == 'FALSE') |>
  mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
  arrange(category1)

# Seperating based on skill in the list
skill <- c(unique(subs$category1))
skill_list <- list()

for (x in skill) {
  skill_list[[x]] <- subset(subs, category1 == x)
}

# Renaming var variablb
for (i in 1:14) {
  skill_list[[i]] %<>%
    mutate(var = tolower(paste0('skill_', sapply(str_extract_all(category1, "[A-Z]"), function(x) paste(x, collapse = "")), '_', row.names(skill_list[[i]]))))
}

# Binding list together into a DF  
final_variable_df <- do.call(rbind, skill_list)

# If needed to be exported to Excel
# write_xlsx(final_variable_df, )
