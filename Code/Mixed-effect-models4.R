rm(list = ls())

# Skills Study
# Creating Analytic Data Set

library(tidyverse)
library(lme4)
library(lmtest)
library(future)
library(future.apply)

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(mice)
library(broom.mixed)
library(brms)

library(glm2)


plan(strategy="multisession", workers = 8)

# Cristian'S Windows
#root_wd = "C:/Users/sarabiac"
#onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Cristian's Mac
#root_wd = "/Users/cristiansarabia/Library/CloudStorage"
#onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")


# Marcus W. Locations
root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

# Marcus's Home Desktop (White-Rhino) 
#root_wd = "C:/Users/marcu"
#onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")


#source(file.path(github_wd, "Code", "participant_demographics_data.R"))
source(file.path(github_wd, "Code", "utils", "utils.R"))

# Load in analytic dataset
M = 10
B = 10
implist = demo_and_response_data(onedrive_wd = onedrive_wd, M = M) 

# Clean up the 
implist =pbapply::pblapply(1:M, function(m){
  
  dat_m = implist[[m+1]] %>% 
    dplyr::filter(round==1) %>% 
    dplyr::mutate(rid = 1:n()) %>% 
    dplyr::relocate(rid)
  
 
  expertise_df = dat_m %>% 
    dplyr::select(rid, expertise) %>% 
    dplyr::filter(!is.na(expertise)) %>% 
    fastDummies::dummy_cols("expertise", split = ",", remove_first_dummy = T)
  
  role_current_df = dat_m %>% 
    dplyr::select(rid, role_current) %>% 
    dplyr::filter(!is.na(role_current)) %>% 
    fastDummies::dummy_cols("role_current", split = ",", remove_first_dummy = T)
  
  dat_m = dat_m %>% 
    dplyr::left_join(expertise_df %>% dplyr::select(-expertise), by = "rid") %>% 
    dplyr::left_join(role_current_df %>% dplyr::select(-role_current), by = "rid")
  
  
  names(dat_m) = names(dat_m) %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_replace_all("-", "_") %>% 
    stringr::str_replace_all("\\/", "_") %>% 
    stringr::str_replace_all("\\.", "") %>% 
    stringr::str_remove_all("expertise_") %>% 
    stringr::str_remove_all("experience_") %>%
    stringr::str_remove_all("role_current_")
  
  dat_m = dat_m %>% 
    dplyr::select(imp, rid,name,varshort,essential,category,role_primary, gender, edu_years:Simulationist, dplyr::any_of(dplyr::starts_with("wgt"))) 
  
  
  
  #Let's get standardized estimates of the continuous variables
  demo_dat = dat_m %>% 
    dplyr::group_by(name) %>% 
    dplyr::reframe(edu_years = edu_years[1], rn_years = rn_years[1], age_years = age_years[1]) %>% 
    dplyr::mutate(z_edu_years = (edu_years - mean(edu_years, na.rm = T))/sd(edu_years, na.rm = T) ) %>% 
    dplyr::mutate(z_rn_years = (rn_years - mean(rn_years, na.rm = T))/sd(rn_years, na.rm = T) ) %>% 
    dplyr::mutate(z_age_years = (age_years - mean(age_years, na.rm = T))/sd(age_years, na.rm = T) )
   dat_m = dat_m %>% dplyr::left_join(demo_dat %>% dplyr::select(-(edu_years:age_years)), by = "name")
    
  
  # Add in the multilevel-bootstrapped weights
  N = length(unique(dat_m$name))
  W = N*brms::rdirichlet(B, rep(1,N)) %>% t() %>% data.frame()
  names(W) = paste0("wgt", 1:B)
  weights_df = data.frame(name = unique(dat_m$name)) %>% 
    dplyr::mutate(wgt = 1) %>% 
    dplyr::bind_cols(W)
  dat_m = dat_m %>% dplyr::left_join(weights_df, by = "name")
  # Ensure weights sum to number of rows
  dat_m = dat_m %>% dplyr::mutate(across(starts_with("wgt"), .fns = function(w){w/mean(w)}))
  
  
  # Turn essential into a 0/1 variable
  dat_m = dat_m %>% dplyr::mutate(essential = as.integer(essential=="Yes"))
  
  
  return(dat_m)
})


#-------------------------------------------------------------------------------
# Main effects
#-------------------------------------------------------------------------------


#sink(file = file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up", "role_primary.txt"))
# Null model

x.formula = as.formula(~ 1 + Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Care*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category)
y.var = "essential"

t1 = proc.time()
fit = glmer(
  essential ~ Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Care*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category + (1|name), 
  data = implist[[2]], 
  nAGQ = 12, 
  verbose = T, 
  family = "binomial", 
  control =  glmerControl(optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3, trace = 3)), 
  )
proc.time()-t1

library(fastglm)

t1 = proc.time()
out_list = pbapply::pblapply(1:1000, function(i){
  #fit = fastglm::fastglmPure(y = as.integer(implist[[2]]$essential=="Yes"), 
  #                           x = model.matrix(~ 1 + Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Care*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category, data = implist[[2]]), 
  #                           family = binomial())
  #return(fit$coefficients)

  fit = glm( as.integer(implist[[2]]$essential=="Yes") ~  1 + Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Care*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category, data = implist[[2]], family = "binomial")
  return(coef(fit))
})
proc.time() - t1


proc.time()-t1
           family = "binomial", 
           data = implist[[2]])

mira_0 = fit_glmer(formula = essential ~ category + (1|name), 
                   implist = implist, optimizer = c("bobyqa", "nlminbwrap"), nAGQ = 1, optCtrl = list(eval.max = 2E7, iter.max = 5E3)
                   )
# role_primary
mira_role_primary = fit_glmer(essential ~ role_primary*category + (1|name), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_primary = summary(D1(mira_role_primary, mira_0))$comparisons$p.value
# gender
mira_gender = fit_glmer(essential ~ gender*category + (1|name), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_gender = summary(D1(mira_gender, mira_0))$comparisons$p.value
# age_years
mira_age_years = fit_glmer(essential ~ age_years*category, implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_age_years = summary(D1(mira_age_years, mira_0))$comparisons$p.value
# edu_years
mira_edu_years = fit_glmer(essential ~ edu_years*category + (1|name), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_edu_years = summary(D1(mira_edu_years, mira_0))$comparisons$p.value
# rn_years
mira_rn_years = fit_glmer(essential ~ rn_years*category + (1|name), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_rn_years = summary(D1(mira_rn_years, mira_0))$comparisons$p.value
# expertise
mira_expertise = fit_glmer(essential ~ Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Car*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category + (1|name), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_expertise = summary(D1(mira_expertise, mira_0))$comparisons$p.value
# role_current
mira_role_current = fit_glmer(essential ~ New_Grad_Res_Coord_Educator*category + Clinical_Instructor_Academic*category + Preceptor*category + Simulationist*category + (1|name), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_current = summary(D1(mira_role_current, mira_0))$comparisons$p.value


#-------------------------------------------------------------------------------
# Interactions
#-------------------------------------------------------------------------------

# Interaction: role_primaryXgender
mira_role_primary_gender = fit_glmer(essential ~ role_primary + gender + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXgender = fit_glmer(essential ~ role_primary*gender + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_primaryXgender = summary(D1(mira_role_primaryXgender, mira_role_primary_gender))$comparisons$p.value
# Interaction: role_primaryXage_years
mira_role_primary_age_years = fit_glmer(essential ~ role_primary + age_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXage_years = fit_glmer(essential ~ role_primary*age_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa"))
pval_role_primaryXage_years = summary(D1(mira_role_primaryXage_years, mira_role_primary_age_years))$comparisons$p.value
# Interaction: role_primaryXedu_years
mira_role_primary_edu_years = fit_glmer(essential ~ role_primary + edu_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXedu_years = fit_glmer(essential ~ role_primary*edu_years + (1|name) + (1|varshort), implist = implist)
pval_role_primaryXedu_years = summary(D1(mira_role_primaryXedu_years, mira_role_primary_edu_years))$comparisons$p.value
# Interaction: role_primaryXrn_years
mira_role_primary_rn_years = fit_glmer(essential ~ role_primary + rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXrn_years = fit_glmer(essential ~ role_primary*rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_primaryXrn_years = summary(D1(mira_role_primaryXrn_years, mira_role_primary_rn_years))$comparisons$p.value
# Interaction: role_primaryXexpertise
mira_role_primary_expertise = fit_glmer(essential ~ role_primary + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics  + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXexpertise = fit_glmer(essential ~ role_primary*Medical_Surgical + role_primary*Population_Health + role_primary*Behavioral_Health + role_primary*Critical_Care + role_primary*ED + role_primary*Perioperative + role_primary*OB + role_primary*Pediatrics  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_primaryXexpertise = summary(D1(mira_role_primaryXexpertise, mira_role_primary_expertise))$comparisons$p.value
# Interaction: role_primaryXrole_current
mira_role_primary_current = fit_glmer(essential ~ role_primary + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_role_primaryXcurrent = fit_glmer(essential ~ role_primary*New_Grad_Res_Coord_Educator + role_primary*Clinical_Instructor_Academic + role_primary*Preceptor + role_primary*Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_role_primaryXcurrent = summary(D1(mira_role_primaryXcurrent, mira_role_primary_current))$comparisons$p.value


# Interaction: genderXage_years
mira_gender_age_years = fit_glmer(essential ~ gender + age_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_genderXage_years = fit_glmer(essential ~ gender*age_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa"))
pval_genderXage_years = summary(D1(mira_genderXage_years, mira_gender_age_years))$comparisons$p.value
# Interaction: genderXedu_years
mira_gender_edu_years = fit_glmer(essential ~ gender + edu_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_genderXedu_years = fit_glmer(essential ~ gender*edu_years + (1|name) + (1|varshort), implist = implist)
pval_genderXedu_years = summary(D1(mira_genderXedu_years, mira_gender_edu_years))$comparisons$p.value
# Interaction: genderXrn_years
mira_gender_rn_years = fit_glmer(essential ~ gender + rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_genderXrn_years = fit_glmer(essential ~ gender*rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_genderXrn_years = summary(D1(mira_genderXrn_years, mira_gender_rn_years))$comparisons$p.value
# Interaction: genderXexpertise
mira_gender_expertise = fit_glmer(essential ~ gender + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics  + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_genderXexpertise = fit_glmer(essential ~ gender*Medical_Surgical + gender*Population_Health + gender*Behavioral_Health + gender*Critical_Care + gender*ED + gender*Perioperative + gender*OB + gender*Pediatrics  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_genderXexpertise = summary(D1(mira_genderXexpertise, mira_gender_expertise))$comparisons$p.value
# Interaction: genderXrole_current
mira_gender_current = fit_glmer(essential ~ gender + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_genderXcurrent = fit_glmer(essential ~ gender*New_Grad_Res_Coord_Educator + gender*Clinical_Instructor_Academic + gender*Preceptor + gender*Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_genderXcurrent = summary(D1(mira_genderXcurrent, mira_gender_current))$comparisons$p.value


# Interaction: age_yearsXedu_years
mira_age_years_edu_years = fit_glmer(essential ~ age_years + edu_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_age_yearsXedu_years = fit_glmer(essential ~ age_years*edu_years + (1|name) + (1|varshort), implist = implist)
pval_age_yearsXedu_years = summary(D1(mira_age_yearsXedu_years, mira_age_years_edu_years))$comparisons$p.value
# Interaction: age_yearsXrn_years
mira_age_years_rn_years = fit_glmer(essential ~ age_years + rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_age_yearsXrn_years = fit_glmer(essential ~ age_years*rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_age_yearsXrn_years = summary(D1(mira_age_yearsXrn_years, mira_age_years_rn_years))$comparisons$p.value
# Interaction: age_yearsXexpertise
mira_age_years_expertise = fit_glmer(essential ~ age_years + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics  + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_age_yearsXexpertise = fit_glmer(essential ~ age_years*Medical_Surgical + age_years*Population_Health + age_years*Behavioral_Health + age_years*Critical_Care + age_years*ED + age_years*Perioperative + age_years*OB + age_years*Pediatrics  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_age_yearsXexpertise = summary(D1(mira_age_yearsXexpertise, mira_age_years_expertise))$comparisons$p.value
# Interaction: age_yearsXrole_current
mira_age_years_current = fit_glmer(essential ~ age_years + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_age_yearsXcurrent = fit_glmer(essential ~ age_years*New_Grad_Res_Coord_Educator + age_years*Clinical_Instructor_Academic + age_years*Preceptor + age_years*Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_age_yearsXcurrent = summary(D1(mira_age_yearsXcurrent, mira_age_years_current))$comparisons$p.value



# Interaction: edu_yearsXrn_years
mira_edu_years_rn_years = fit_glmer(essential ~ edu_years + rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_edu_yearsXrn_years = fit_glmer(essential ~ edu_years*rn_years + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_edu_yearsXrn_years = summary(D1(mira_edu_yearsXrn_years, mira_edu_years_rn_years))$comparisons$p.value
# Interaction: edu_yearsXexpertise
mira_edu_years_expertise = fit_glmer(essential ~ edu_years + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics  + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_edu_yearsXexpertise = fit_glmer(essential ~ edu_years*Medical_Surgical + edu_years*Population_Health + edu_years*Behavioral_Health + edu_years*Critical_Care + edu_years*ED + edu_years*Perioperative + edu_years*OB + edu_years*Pediatrics  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_edu_yearsXexpertise = summary(D1(mira_edu_yearsXexpertise, mira_edu_years_expertise))$comparisons$p.value
# Interaction: edu_yearsXrole_current
mira_edu_years_current = fit_glmer(essential ~ edu_years + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_edu_yearsXcurrent = fit_glmer(essential ~ edu_years*New_Grad_Res_Coord_Educator + edu_years*Clinical_Instructor_Academic + edu_years*Preceptor + edu_years*Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_edu_yearsXcurrent = summary(D1(mira_edu_yearsXcurrent, mira_edu_years_current))$comparisons$p.value



# Interaction: rn_yearsXexpertise
mira_rn_years_expertise = fit_glmer(essential ~ rn_years + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics  + (1|name) + (1|varshort), implist = implist,  optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_rn_yearsXexpertise = fit_glmer(essential ~ rn_years*Medical_Surgical + rn_years*Population_Health + rn_years*Behavioral_Health + rn_years*Critical_Care + rn_years*ED + rn_years*Perioperative + rn_years*OB + rn_years*Pediatrics  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_rn_yearsXexpertise = summary(D1(mira_rn_yearsXexpertise, mira_rn_years_expertise))$comparisons$p.value
# Interaction: rn_yearsXrole_current
mira_rn_years_current = fit_glmer(essential ~ rn_years + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_rn_yearsXcurrent = fit_glmer(essential ~ rn_years*New_Grad_Res_Coord_Educator + rn_years*Clinical_Instructor_Academic + rn_years*Preceptor + rn_years*Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_rn_yearsXcurrent = summary(D1(mira_rn_yearsXcurrent, mira_rn_years_current))$comparisons$p.value


# Interaction:  expertiseXrole_current
mira_expertise_current = fit_glmer(essential ~ Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist  + (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
mira_expertiseXcurrent = fit_glmer(essential ~ Medical_Surgical*New_Grad_Res_Coord_Educator + Medical_Surgical*Clinical_Instructor_Academic + Medical_Surgical*Preceptor + Medical_Surgical*Simulationist +
                                               Population_Health*New_Grad_Res_Coord_Educator + Population_Health*Clinical_Instructor_Academic + Population_Health*Preceptor + Population_Health*Simulationist +
                                               Behavioral_Health*New_Grad_Res_Coord_Educator + Behavioral_Health*Clinical_Instructor_Academic + Behavioral_Health*Preceptor + Behavioral_Health*Simulationist + 
                                               Critical_Care*New_Grad_Res_Coord_Educator + Critical_Care*Clinical_Instructor_Academic + Critical_Care*Preceptor + Critical_Care*Simulationist +
                                               ED*New_Grad_Res_Coord_Educator + ED*Clinical_Instructor_Academic + ED*Preceptor + ED*Simulationist +
                                               Perioperative*New_Grad_Res_Coord_Educator + Perioperative*Clinical_Instructor_Academic + Perioperative*Preceptor + Perioperative*Simulationist +
                                               OB*New_Grad_Res_Coord_Educator + OB*Clinical_Instructor_Academic + OB*Preceptor + OB*Simulationist +
                                               Pediatrics*New_Grad_Res_Coord_Educator + Pediatrics*Clinical_Instructor_Academic + Pediatrics*Preceptor + Pediatrics*Simulationist
                                              (1|name) + (1|varshort), implist = implist, optimizer = c("bobyqa", "nlminbwrap"), optCtrl = list(eval.max = 2E7, iter.max = 5E3))
pval_expertiseXcurrent = summary(D1(mira_expertiseXcurrent, mira_expertise_current))$comparisons$p.value


