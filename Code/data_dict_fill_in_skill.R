# Skills Study
# Data Dictionary

library(tidyverse)
library(readxl)
library(writexl)

# Reading in data
raw <- read_xlsx('Dictionary - Custom Data Extract - 20240406.xlsx')


# Updating the skills section
skills <- raw |>
  subset(type == 'Skills' & corresponding_rq != 'Not Included Skills') |>
  mutate(skill = str_trim(str_extract(full_stem, ':[^:]*$')) |>
           str_remove('^:') |>
           str_squish())

# combingin raw with skills data and removing/updating names
new_dict <- full_join(raw, skills, by = join_by('col_id', 'round', 'type', 'category1', 'category2', 'corresponding_rq', 'var', 'skill_id', 'full_stem')) |>
  subset(select = - skill.x) |>
  rename(skill = skill.y) |>
  relocate(skill, .after = skill_id)


write_xlsx(new_dict, 'updated_dictionary.xlsx')

