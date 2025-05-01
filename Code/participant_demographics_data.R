# Skills Study
# Demographics

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)
library(stringr)

# Windows Locations
root_wd = "C:/Users/sarabiac"
onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Bringin in raw data
raw_long <- read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))

# df with needed columns
df <- raw_long |>
  filter(!is.na(`Please choose your primary role:...24`)) |>
  dplyr::select(c(1, 24:48, 50:55)) |>
  rename(name = `SYSTEM: Owner Full Name...1`) |>
  distinct(name, .keep_all = TRUE)


# getting df with 1 column questions
main_df <- df |>
  dplyr::select(c(1, 2, 10:11, 23:24, 26, 25))|>
  rename(
    `Primary Role` = `Please choose your primary role:...24`,
    `Which category do you best identify` = `To which category do you best identify?...32`,
    `Age Category` = `Age category...33`,
    `What geog. region are you located` = `What geographical region are you located?...45`,
    `How many years have you worked in an educator role` = `How many years have you worked in an educator role?...48`,
    `Years experience as a RN` = `How many years experience do you have as a registered nurse?...47`) |>
  mutate(`What geog. region are you located` = ifelse(`What geog. region are you located` == 'International:', paste('Int: ', `What geographical region are you located?...46`), `What geog. region are you located`)) |>
  select(-6)


# collapsing columns together for Question 2
q1 <- df |>
  select(c(1, 3:8)) |>
  rename(`Clinical Instructor/Academic` = `What role do you currently hold? : Clinical Instructor/Scholar...25`,
         `Other` = `What role do you currently hold?...26`,
         `Simulationist` = `What role do you currently hold? : Simulationist...27`,
         `Clinical Educator/Practice` = `What role do you currently hold? : Clinical educator in the practice setting...28`,
         `New Grad Res. Coord./Educator` = `What role do you currently hold? : New graduate (residency) coordinator/educator...29`,
         `Preceptor` = `What role do you currently hold? : Preceptor...30`) |>
  pivot_longer(cols = -name, names_to = 'option', values_to = 'value') |>
  mutate(option_clean = case_when(
    option == 'other' & !is.na(value) & value != '' ~ value,
    option == 'other' ~ NA_character_,
    value == 'Yes' ~ option,
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(option_clean)) |>
  group_by(name) |>
  summarise(`Role Currently Hold` = paste(option_clean, collapse = ', '), .groups = 'drop')


# collapsing columns together for Question 5
q5 <- df |>
  dplyr::select(c(1, 12:21))|>
  rename(
    `OB` = `What is your primary area of clinical expertise?  : OB...34`,
    `Other` = `What is your primary area of clinical expertise?...35`,
    `Pediatrics` = `What is your primary area of clinical expertise?  : Peds...36`,
    `Critical Care` = `What is your primary area of clinical expertise?  : Critical Care...37`,
    `ED` = `What is your primary area of clinical expertise?  : Emergency department...38`,
    `Perioperative` = `What is your primary area of clinical expertise?  : Peri-operative...39`,
    `Ambulatory Care` = `What is your primary area of clinical expertise?  : Ambulatory care...40`,
    `Medical-Surgical` = `What is your primary area of clinical expertise?  : Medical/surgical...41`,
    `Population Health` = `What is your primary area of clinical expertise?  : Population health (community)...42`,
    `Behavioral Health`= `What is your primary area of clinical expertise?  : Behavioral health...43`) |>
  pivot_longer(cols = -name, names_to = 'option', values_to = 'value') |>
  mutate(option_clean = case_when(
    option == 'other' & !is.na(value) & value != '' ~ value,
    option == 'other' ~ NA_character_,
    value == 'Yes' ~ option,
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(option_clean)) |>
  group_by(name) |>
  summarise(`Primary area of expert.` = paste(option_clean, collapse = ', '), .groups = 'drop')


# collapsing columns together for Question 9
q9 <- df |>
  dplyr::select(c(1, 27:30))|>
  rename(`ADN` = `If you are at an academic school of nursing, what level of program does your school offer? : ADN...50`,
         `BSN` = `If you are at an academic school of nursing, what level of program does your school offer? : BSN...51`,
         `MS` = `If you are at an academic school of nursing, what level of program does your school offer? : MS direct entry       (pre-licensure)...52`,
         `NA` = `If you are at an academic school of nursing, what level of program does your school offer? : N/A...53`) |>
  pivot_longer(cols = -name, names_to = 'option', values_to = 'value') |>
  filter(value == 'Yes') |>
  group_by(name) |>
  summarise(`Level of Program(s)` = paste(option, collapse = ', '), .groups = 'drop')


# collapsing columns together for Question 10
q10 <- df |>
  dplyr::select(c(1, 31:32)) |>
  rename(`Clinical Setting` = `If you are at a clinical agency, what kind of setting?...54`) |>
  mutate(`Clinical Setting` = ifelse(`Clinical Setting` == 'Other:', paste('Other: ', `If you are at a clinical agency, what kind of setting?...55`), `Clinical Setting`)) |>
  select(-3)


# Joining multiple dfs into one
df_1 <- left_join(main_df, q1)
df_2 <- left_join(df_1, q5)
df_3 <- left_join(df_2, q9)

final_demo_df <- left_join(df_3, q10)

# reordering columsn 
# final_demo_df <- final_demo_df
#  relocate(`Role Currently Hold`, .after = `Primary Role`)
