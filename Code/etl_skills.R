# Skills Study
# Data Dictionary

rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)


# Cristian
# root_wd = "/Users/cristiansarabia/Library/CloudStorage"
# onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
# github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")



# Marcus
root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"The University of Colorado Denver", "Bowler, Fara - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

source(file.path(github_wd, "Code", "utils", "utils.R"))

#setwd(github_wd)

# Construct data dictionary
dict = construct_data_dictionary(onedrive_wd=onedrive_wd)
  # Sanity check that we are replicating number of item
  dict %>% dplyr::filter(skill_origin %in% c("Omitted Nu Skills","Original 166", "Respondant Provided"), round == 1, corresponding_rq == "Essential") %>% dplyr::group_by(skill_origin) %>% dplyr::reframe(n = n()) 
  dict %>% dplyr::filter(!is.na(var), round == 1, corresponding_rq == "Essential") %>% dplyr::group_by(skill_origin) %>% dplyr::reframe(n = n()) # also good
  
# Load in the analytic data
# raw = readxl::read_xlsx(
#   path = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.xlsx"), 
#   sheet = "Data", 
#   range = "B7:CQH149"
# )
# write_rds(raw, file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))
raw = readr::read_rds(file = file.path(onedrive_wd, "Data", "Custom Data Extract - 20240406.rds"))

# Extract relevant columns and rename
extracted_cols =  with(dict, col_id[!is.na(var)]) %>% sort()
  #Sanity check 
  if ( ( length(extracted_cols) -1 )/(3*3) != 196 ){"Did not extract the expected number of columns"}
  
dat = raw %>% dplyr::select(extracted_cols)
dict_dat = dict %>% dplyr::slice(extracted_cols)
names(dat) = dict_dat$var


# Sanity check: email should uniquely identify participant
dat %>% dplyr::group_by(participant) %>% dplyr::reframe(n = length(unique(participant))) %>%  arrange(-n) #Yes

# Extract the progress
progress_df = raw[,dict$col_id[which(dict$type == "Progress" | dict$var=="participant" | dict$var=="email")]] %>% data.frame()
names(progress_df) = c("participant", "email","progress_ro1", "progress_ro2", "progress_ro3")
progress_df



# Create a variable short and extraxct a skill description
varshort_skills = dict_dat %>% 
  dplyr::mutate(
    varshort = var %>% 
      stringr::str_remove_all(pattern = paste0("_", tolower(corresponding_rq))) %>% 
      stringr::str_remove_all(pattern = paste0("_ro",round))
    ) %>% 
  dplyr::group_by(varshort) %>% 
  dplyr::reframe(skill = unique(skill), category = category1[1], skill_origin = skill_origin[1]) %>% 
  na.omit()
table(varshort_skills$skill_origin) # yes, 9,166,21

# Counts of response types in each outnd
rq = "essential"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Yes = sum(value=="Yes", na.rm = T), No = sum(value=="No", na.rm = T), Missing = sum(is.na(value))) %>% 
  dplyr::mutate(pct_Yes = round(100*Yes/(Yes + No)), 
                rq = "Essential") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


rq = "competence"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
competence_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Yes = sum(value=="Yes", na.rm = T), No = sum(value=="No", na.rm = T), Missing = sum(is.na(value))) %>% 
  dplyr::mutate(pct_Yes = round(100*Yes/(Yes + No)), rq = "Competence") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


rq = "environment"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
environment_responses = tmp %>% tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::group_by(varshort, round) %>% 
  dplyr::reframe(Clinical = sum(value=="clinical setting", na.rm = T), 
                 Simulation = sum(value=="simulation/ skills", na.rm = T), 
                 Didactic = sum(value=="didactic only", na.rm = T),
                 Missing = sum(is.na(value))) %>% 
  dplyr::mutate(rq = "Environment") %>% 
  dplyr::left_join(varshort_skills, by = "varshort") %>% 
  dplyr::relocate(rq:category) %>% 
  dplyr::relocate(rq,category)


counts_by_rq = essential_responses %>% 
  dplyr::select(-rq) %>% 
  dplyr::left_join(competence_responses %>% dplyr::select(-(rq:skill)), by = dplyr::join_by(varshort,round)) %>% 
  dplyr::left_join(environment_responses %>% dplyr::select(-(rq:skill)), by = dplyr::join_by(varshort, round))




### Let's look at number of nonmissing responses by round and skill_origin, just essential
rq = "essential"
tmp = dat %>% dplyr::select(email,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_responses = tmp %>% 
  tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::left_join(varshort_skills %>% dplyr::select(varshort, skill_origin), by = "varshort") %>% 
  dplyr::group_by(skill_origin, round) %>% 
  dplyr::reframe(n = length(unique(varshort))) %>% 
  tidyr::pivot_wider(names_from = "round", values_from = "n", names_prefix = "round_")
  
# Clear evidence that the omitted new skills  was included in round 1
  # skill_origin        round_1 round_2 round_3
  # <chr>                 <int>   <int>   <int>
  # 1 Omitted Nu Skills         6       6       9
  # 2 Original 166            166     166     166
  # 3 Respondant Provided      NA      21      21

# Look at responses by individual by round of omitted nutrition skills
tmp = dat %>% dplyr::select(participant,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_NU_responses = tmp %>% 
  tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::left_join(varshort_skills %>% dplyr::select(varshort, skill_origin), by = "varshort") %>% 
  dplyr::filter(skill_origin=="Omitted Nu Skills") %>% 
  tidyr::pivot_wider(names_from = "round", values_from = "value", names_prefix = "round_") %>% 
  dplyr::relocate(round_1, .before = "round_2") %>% 
  dplyr::group_by(participant)
print(essential_NU_responses, n = 100)
# # The issue with the Omitted Nu Skills appears to only be from Leslie Grahm
#    participant      varshort   skill_origin      round_1 round_2 round_3
# 3  Leslie Graham    skill_n_8  Omitted Nu Skills Yes     Yes     NA     
# 64 Leslie Graham    skill_n_9  Omitted Nu Skills No      No      NA     
# 65 Leslie Graham    skill_n_7  Omitted Nu Skills Yes     Yes     NA     
# 66 Leslie Graham    skill_n_10 Omitted Nu Skills Yes     Yes     NA     
# 67 Leslie Graham    skill_n_3  Omitted Nu Skills Yes     Yes     NA     
# 68 Leslie Graham    skill_n_4  Omitted Nu Skills Yes     Yes     NA  


# Case study on "Cleaning a wound" which should have been excluded in Round 1
wound_varshort = varshort_skills %>% 
  dplyr::filter(skill == "Cleaning a wound") %>% 
  purrr::pluck("varshort")

tmp = dat %>% dplyr::select(participant,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_WOUND_responses = tmp %>% 
  tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::filter(!is.na(value), varshort == wound_varshort) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = "round", values_from = "value", names_prefix = "round_") %>% 
  dplyr::left_join(varshort_skills %>% dplyr::select(varshort, skill), by = "varshort") %>% 
  dplyr::relocate(skill, .after = varshort)

vs = "skill_n_10"
# Let's make data frames for each skill
wide_responses_all <- lapply(varshort_skills$varshort, function(vs){

  print(vs)
  
  tmp = dat %>% dplyr::select(email,participant,dplyr::ends_with(rq))
  names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
  essential_VS_responses = tmp %>% 
    tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
    dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                  varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
    dplyr::filter(!is.na(value), varshort == vs) %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "round", values_from = "value", names_prefix = "round_") %>% 
    dplyr::left_join(varshort_skills %>% dplyr::select(varshort, skill), by = "varshort") %>% 
    dplyr::relocate(skill, .after = varshort) %>% 
    dplyr::left_join(progress_df %>% dplyr::select(-participant) , by = "email") %>% 
    dplyr::relocate(starts_with("progress"), .before = starts_with("round")) %>% 
    dplyr::left_join(dict %>% dplyr::filter(!is.na(var), round == 1, corresponding_rq=="Essential") %>% dplyr::select(skill,category1), by = "skill") %>% 
    dplyr::relocate(category1, .after=participant) 
  
  
  if( !("round_3" %in% names(essential_VS_responses)) ) { 
    
    essential_VS_responses = essential_VS_responses %>% 
      dplyr::mutate(round_3 = NA)
    
  }
  
  if( !("round_2" %in% names(essential_VS_responses)) ) { 
    
    essential_VS_responses = essential_VS_responses %>% 
      dplyr::mutate(round_2 = NA) %>% 
      dplyr::relocate(round_2, .before = round_3)
    
  }
  
  
  if( !("round_1" %in% names(essential_VS_responses)) ) { 
    
    essential_VS_responses = essential_VS_responses %>% 
      dplyr::mutate(round_1 = NA) %>% 
      dplyr::relocate(round_1, .before = round_2)
    
  }
  
  essential_VS_responses = essential_VS_responses %>% 
    dplyr::select(email:skill,progress_ro1,progress_ro2,progress_ro3, round_1,round_2,round_3)
  
  
  consensus_VS_response = essential_VS_responses %>% 
    dplyr::select(round_1, round_2, round_3) %>% 
    apply(2,function(x){round(100*mean(x=="Yes", na.rm=T),1)}) %>% as.numeric()
  
  consensus_VS_response[is.nan(consensus_VS_response)] <- NA
  
  names(consensus_VS_response) = paste0("consensus_ro", 1:3)
  
  essential_VS_responses = essential_VS_responses %>% 
    dplyr::mutate(consensus_ro1 = consensus_VS_response[1], 
                  consensus_ro2 = consensus_VS_response[2], 
                  consensus_ro3 = consensus_VS_response[3]
                  )

  essential_VS_responses = essential_VS_responses %>% 
    dplyr::relocate(email,participant:skill, consensus_ro1,progress_ro1, round_1, consensus_ro2,progress_ro2, round_2, consensus_ro3,progress_ro3, round_3)
  

  return(essential_VS_responses)  
  
})


writexl::write_xlsx(wide_responses_all %>% dplyr::bind_rows(), path = file.path(onedrive_wd, "Data", "Data Dictionary", "All Responses with Progress and Consensus.xlsx"))



# Check consistency of responses from round 1 to round 2 for those that answered both and only items that had 90+% consesnsus in round 1

# Below indicates that round 1 responses NOT carried forward to round 2 (both changes in responses and no data for those that made progress)
with(wide_responses_all  %>% dplyr::bind_rows() %>% dplyr::filter(consensus_ro1 > 85) %>% 
  dplyr::filter(!is.na(round_1), !is.na(round_2)), 
  table(round_1,round_2))

wide_responses_all %>% dplyr::bind_rows() %>%  dplyr::filter(!is.na(round_2), is.na(progress_ro2))



# Below would suggest round 2 responses WERE carried forward to round 3 (both not changes in responses AND existing round 3 data for those that report no round 3 progress [assume not invited])
with(wide_responses_all  %>% dplyr::bind_rows() %>% dplyr::filter(consensus_ro2 >= 80) %>% 
       dplyr::filter(!is.na(round_2), !is.na(round_3)), 
     table(round_2,round_3))


wide_responses_all %>% dplyr::bind_rows() %>%  dplyr::filter(!is.na(round_3), is.na(progress_ro3))



wound_varshort = varshort_skills %>% 
  dplyr::filter(skill == "Cleaning a wound") %>% 
  purrr::pluck("varshort")

tmp = dat %>% dplyr::select(participant,dplyr::ends_with(rq))
names(tmp) = stringr::str_remove_all(names(tmp), paste0("_",rq))
essential_WOUND_responses = tmp %>% 
  tidyr::pivot_longer(starts_with("skill"), names_to = "varshort") %>% 
  dplyr::mutate(round = stringr::str_sub(varshort, start = stringr::str_length(varshort), end = stringr::str_length(varshort)) %>% as.integer(), 
                varshort = stringr::str_remove_all(varshort, "_ro[1-9]")) %>% 
  dplyr::filter(!is.na(value), varshort == wound_varshort) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = "round", values_from = "value", names_prefix = "round_") %>% 
  dplyr::left_join(varshort_skills %>% dplyr::select(varshort, skill), by = "varshort") %>% 
  dplyr::relocate(skill, .after = varshort)



# Look at response by individual  (just essential)
items_agree_ro1 = counts_df %>% dplyr::filter(round == 1) %>% dplyr::select(category:varshort,pct_Yes.x) %>% 
  dplyr::arrange(-pct_Yes.x)

tmp = dat %>% dplyr::select(participant,email,dplyr::starts_with("progress"), dplyr::ends_with("_essential"))
names(tmp) = stringr::str_remove_all(names(tmp), "_essential")

items_keep = c(paste0(items_agree_ro1$varshort,"_ro1"), paste0(items_agree_ro1$varshort,"_ro2"), paste0(items_agree_ro1$varshort,"_ro3"))
items_keep = sort(items_keep)

tmp = tmp %>% dplyr::select(participant:progress_ro3, dplyr::any_of(items_keep)) %>% dplyr::arrange(desc(progress_ro1), desc(progress_ro2), desc(progress_ro3)) 


tmp_list = lapply(1:nrow(tmp), function(i){
  
  df_i = tmp %>% dplyr::select(-starts_with("progress_")) %>% dplyr::slice(i) %>% tidyr::pivot_longer(skill_aic_1_ro1:skill_ue_3_ro3, names_to = "var") %>% 
    dplyr::mutate(ro = NA, 
                  ro = ifelse(endsWith(var,"1"), 1, ro), 
                  ro = ifelse(endsWith(var,"2"), 2, ro), 
                  ro = ifelse(endsWith(var,"3"), 3, ro)) %>% 
    dplyr::mutate(varshort = var %>% 
                    stringr::str_remove_all(pattern = "_ro1") %>% 
                    stringr::str_remove_all(pattern = "_ro2") %>% 
                    stringr::str_remove_all(pattern = "_ro3")
    ) %>% 
    dplyr::select(-var) %>% 
    tidyr::pivot_wider(names_from = "ro", values_from = "value", names_prefix = "essential_ro") %>% 
    dplyr::arrange(varshort) %>% 
    dplyr::left_join(tmp %>% dplyr::select(email, starts_with("progress")), by = "email") %>% 
    dplyr::relocate(participant:varshort, progress_ro1, essential_ro1, progress_ro2, essential_ro2, progress_ro3, essential_ro3) %>% 
    dplyr::left_join(counts_df %>% dplyr::filter(round==1) %>% dplyr::select(varshort,category,skill, pct_Yes.x) %>% dplyr::rename(pct_Yes_ro1 = pct_Yes.x), b = "varshort") %>% 
    dplyr::select(varshort,participant:email,category,skill,pct_Yes_ro1,progress_ro1:essential_ro3)
  
  return(df_i)
  
})


names(tmp_list)  = tmp$participant

#write.csv(tmp_list[[4]], file =  file.path(onedrive_wd, "Data", "Data Dictionary","Case Study of Abigail Barron.csv"))

