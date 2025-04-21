# Issue one
# Skills study creating dupe vairable and where skill came from variable

test_dict <- dict |> # Change
  filter(type == 'Skills') |> # Subsetting data to just skills
  mutate(skill_origin = case_when(
    participant_generated == FALSE & corresponding_rq != 'Not Included Skills' & category1 != 'Nutrition' ~ 'Original 166', # Names the original 166 skills
    participant_generated == TRUE ~ 'Respondant Provided',  # Participant created skills from round 1
    category1 == 'Nutrition' ~ 'Omitted Nu Skills' # Nutrition skills that were not added in round 1
  )) |> mutate(skill_origin = ifelse(str_detect(skill, 'nasogastic'), 'Duplicate', skill_origin), # Looks for the nasogastic spelling to assign as dupe
              skill_origin = ifelse(str_detect(skill, 'parenteral '), 'Duplicate', skill_origin)) # looks for the parenteral spelling to assign as dupe
 
  
# Note: The Skill Origin variable DOES NOT INCLUDE the duplicate for the variable as we don't know which variable is the duplicated (both spelled the same)

# Issue 2
# To update the names of the 21 respondents.

# utils script line #44
subs <- new_dict |>
  subset(type == 'Skills' & round == ro & corresponding_rq == rq) |> # & participant_generated == 'FALSE') |> # removing participant generate portion to include participant generated skills
  mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
  arrange(category1)