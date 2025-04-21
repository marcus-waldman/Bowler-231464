# Issue one
# Skills study creating dupe vairable and where skill came from variable

test_dict <- new_dict |> # Change
  filter(type == 'Skills') |> # Subsetting data to just skills
  mutate(skill_origin = case_when(
    participant_generated == FALSE & corresponding_rq != 'Not Included Skills' & category1 != 'Nutrition' ~ 'Original 166', # Names the original 166 skills
    participant_generated == TRUE ~ 'Respondant Provided',  # Participant created skills from round 1
    category1 == 'Nutrition' ~ 'Omitted Nu Skills' # Nutrition skills that were not added in round 1
  )) |> mutate(skill_origin = ifelse(str_detect(skill, 'nasogastic'), 'Duplicate', skill_origin), # Looks for the nasogastic spelling to assign as dupe
              skill_origin = ifelse(str_detect(skill, 'parenteral '), 'Duplicate', skill_origin)) # looks for the parenteral spelling to assign as dupe
 
# Sanity check (1 perfect replication, 2 fail to replicate [off by 1])
test_dict %>% dplyr::filter(skill_origin == "Original 166", round == 1, corresponding_rq == "Essential") %>% nrow() #Yes, replicates 166
test_dict %>% dplyr::filter(skill_origin == "Omitted Nu Skills", round == 1, corresponding_rq == "Essential") %>% nrow() #10 instead of 9
test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow() #22 instead 21

#Above non-replicatio issues Issue likely that we have duplicate item assisting a patient with eating
tmp = test_dict %>% dplyr::filter(skill == "Assisting a patient with eating")
identical(test_dict %>% slice(1) %>% purrr::pluck("skill"), test_dict %>% slice(1) %>% purrr::pluck("skill")) #Exactly the same skill text so never caught 
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))[, tmp$col_id]
names(raw) = tmp$col_id
tmp2 = raw %>% apply(2,function(x)sum(!is.na(x))) # Looks like it is the even and not the odd numbers that should be trusted
col_ids_duplicate = names(tmp2)[as.integer(names(tmp2))%%2 == 1] %>% as.integer()

test_dict = test_dict %>% 
  dplyr::mutate(skill_origin = ifelse(col_id %in% col_ids_duplicate, 'Duplicate', skill_origin)) #All empty responses: Assisting a patient with eating 


test_dict %>% dplyr::filter(skill_origin == "Original 166", round == 1, corresponding_rq == "Essential") %>% nrow() #Yes, replicates 166
test_dict %>% dplyr::filter(skill_origin == "Omitted Nu Skills", round == 1, corresponding_rq == "Essential") %>% nrow() #9 
test_dict %>% dplyr::filter(skill_origin == "Respondant Provided", round == 1, corresponding_rq == "Essential") %>% nrow() #22 instead 21




# Note: The Skill Origin variable DOES NOT INCLUDE the duplicate for the variable as we don't know which variable is the duplicated (both spelled the same)

# Issue 2
# To update the names of the 21 respondents.

# utils script line #44
subs <- new_dict |>
  subset(type == 'Skills' & round == ro & corresponding_rq == rq) |> # & participant_generated == 'FALSE') |> # removing participant generate portion to include participant generated skills
  mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
  arrange(category1)


subs <- new_dict |>
  subset(type == 'Skills' & round == ro & corresponding_rq == rq & participant_generated == 'FALSE') |>
  mutate(category1 = recode(category1, `Pain management` = 'Pain Management', `Patient safety` = 'Patient Safety'))|> # Re-coding to have upper case for following word.
  arrange(category1)
