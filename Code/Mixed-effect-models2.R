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
dat = demo_and_response_data(onedrive_wd = onedrive_wd) 

# Restrict to round 1 only
dat = dat %>% dplyr::filter(round==1) %>% 
  dplyr::mutate(rid = 1:n()) %>% 
  dplyr::relocate(rid)

# Let's recode the expertise & dummies
expertise_df = dat %>% 
  dplyr::select(rid, expertise) %>% 
  dplyr::filter(!is.na(expertise)) %>% 
  fastDummies::dummy_cols("expertise", split = ",")

# setting_df = dat %>% 
#   dplyr::select(name, setting) %>% 
#   dplyr::filter(!is.na(setting)) %>% 
#   fastDummies::dummy_cols("setting", split = ",")

# Attach expertise data
dat = dat %>% 
  dplyr::left_join(expertise_df, by = "rid")

# Cannot get region model to converge
groupings_vec = c(
  "role_primary",
  "gender",
  "age_years",
  "experience_edu_years",
  "experience_rn_years"
) 


J = length(groupings_vec)
for(j in 1:J){
  
  x = groupings_vec[j]
  print(x)
  dat_x = dat %>%
    dplyr::select("essential", "category", "name", "varshort", dplyr::any_of(groupings_vec)) %>%
    dplyr::rename(grouping = x) 
  if(is.ordered(dat_x$grouping)){
    dat_x$grouping = as.integer(dat_x$grouping)
    
    median_val = dat_x %>%
      dplyr::group_by(name) %>%
      dplyr::reframe(grouping = grouping[1]) %>%
      purrr::pluck("grouping") %>%
      median(na.rm = T)
    
    dat_x = dat_x %>%
      dplyr::mutate(grouping = (grouping>median_val))
    
  }
  if(is.factor(dat_x$grouping) | is.ordered(dat_x$grouping)){
    contrasts(dat_x$grouping) = "contr.sum"
  }
  
  
  fit0 = lme4::glmer(
    essential~category+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  filepath =  paste0(x, " Model 0.html")
  sjPlot::tab_model(fit0, title = paste0(x, " Model 0.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  
  fit1 = lme4::glmer(
    essential~category+grouping+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  filepath =  paste0(x, " Model 1.html")
  sjPlot::tab_model(fit1, title = paste0(x, " Model 1.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  
  
  fit2 = lme4::glmer(
    essential~category+grouping+category*grouping+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  
  filepath =  paste0(x, " Model 2.html")
  sjPlot::tab_model(fit2, title = paste0(x, " Model 2.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  
  if(x == "role_primary"){
    
    fit3 = lme4::glmer(
      essential~category*grouping*experience_rn_years+(1|name) + (1|varshort), 
      data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort,experience_rn_years) %>% na.omit(), 
      family = "binomial", 
      verbose = 1, 
      control = glmerControl(optimizer = "bobyqa")
    )
    filepath =  paste0(x, " Model 3.html")
    sjPlot::tab_model(fit3, title = paste0(x, " Model 3.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
    
    
    fit4 = lme4::glmer(
      essential~category*grouping*experience_rn_years*age_years+(1|name) + (1|varshort), 
      data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort,experience_rn_years,age_years) %>% na.omit(), 
      family = "binomial", 
      verbose = 1, 
      control = glmerControl(optimizer = "bobyqa")
    )
    filepath =  paste0(x, " Model 4.html")
    sjPlot::tab_model(fit4, title = paste0(x, " Model 4.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
    
  }
  
  
}

out_list<-future.apply::future_lapply(1:J, function(j){

  x = groupings_vec[j]
  print(x)
  dat_x = dat %>%
    dplyr::select("essential", "category", "name", "varshort", dplyr::any_of(groupings_vec)) %>%
    dplyr::rename(grouping = x) 
  if(is.ordered(dat_x$grouping)){
    dat_x$grouping = as.integer(dat_x$grouping)

    median_val = dat_x %>%
      dplyr::group_by(name) %>%
      dplyr::reframe(grouping = grouping[1]) %>%
      purrr::pluck("grouping") %>%
      median(na.rm = T)

    dat_x = dat_x %>%
      dplyr::mutate(grouping = (grouping>median_val))

  }
  if(is.factor(dat_x$grouping) | is.ordered(dat_x$grouping)){
    contrasts(dat_x$grouping) = "contr.sum"
  }

  
  fit0 = lme4::glmer(
    essential~category+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  
  filepath =  paste0(x, " Model 0.html")
  sjPlot::tab_model(fit0, title = paste0(x, " Model 0.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  
  fit1 = lme4::glmer(
    essential~category+grouping+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  
  filepath =  paste0(x, " Model 1.html")
  sjPlot::tab_model(fit1, title = paste0(x, " Model 0.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  

  fit2 = lme4::glmer(
    essential~category+grouping+category*grouping+(1|name) + (1|varshort), 
    data = dat_x %>% dplyr::select(essential,category,grouping,name,varshort) %>% na.omit(), 
    family = "binomial", 
    verbose = 1, 
    control = glmerControl(optimizer = "bobyqa")
  )
  
  filepath =  paste0(x, " Model 2.html")
  sjPlot::tab_model(fit1, title = paste0(x, " Model 2.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
  
  if(x == "role_primary"){
    
    fit3 = lme4::glmer(
      essential~category*grouping*experience_rn_years+(1|name) + (1|varshort), 
      data = dat_x %>% dplyr::select(essential,category,grouping,experience_rn_years) %>% na.omit(), 
      family = "binomial", 
      verbose = 1, 
      control = glmerControl(optimizer = "bobyqa")
    )
    
    
    filepath =  paste0(x, " Model 3.html")
    sjPlot::tab_model(fit1, title = paste0(x, " Model 3.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
    
    fit4 = lme4::glmer(
      essential~category*grouping*experience_rn_years*age_years+(1|name) + (1|varshort), 
      data = dat_x %>% dplyr::select(essential,category,grouping,experience_rn_years,age_years) %>% na.omit(), 
      family = "binomial", 
      verbose = 1, 
      control = glmerControl(optimizer = "bobyqa")
    )
    
    filepath =  paste0(x, " Model 4.html")
    sjPlot::tab_model(fit1, title = paste0(x, " Model 4.html"), file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, filepath))
    
  }

  lrt_01 = lmtest::lrtest(fit0,fit1) %>%
    data.frame() %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(Comparison = "M0 vs. M1", Grouping = x)

  lrt_02 = lmtest::lrtest(fit0,fit2) %>%
    data.frame() %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(Comparison = "M0 vs. M2", Grouping = x)

  lrt_12 = lmtest::lrtest(fit1,fit2) %>%
    data.frame() %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(Comparison = "M1 vs. M2", Grouping = x)


  if(x == "role_primary"){

    lrt_03 = lmtest::lrtest(fit0,fit3) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M0 vs. M3", Grouping = x)

    lrt_04 = lmtest::lrtest(fit0,fit4) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M0 vs. M4", Grouping = x)

    lrt_13 = lmtest::lrtest(fit1,fit3) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M1 vs. M3", Grouping = x)

    lrt_14 = lmtest::lrtest(fit1,fit4) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M1 vs. M4", Grouping = x)

    lrt_23 = lmtest::lrtest(fit2,fit3) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M2 vs. M3", Grouping = x)

    lrt_24 = lmtest::lrtest(fit1,fit4) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M2 vs. M4", Grouping = x)


    lrt_34 = lmtest::lrtest(fit3,fit4) %>%
      data.frame() %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(Comparison = "M3 vs. M4", Grouping = x)
  }

  lrts_x = lrt_01 %>%
    dplyr::bind_rows(lrt_02) %>%
    dplyr::bind_rows(lrt_12)

  if(x == "role_primary"){
     lrts_x = lrts_x %>%
       dplyr::bind_rows(lrt_03) %>%
       dplyr::bind_rows(lrt_04) %>%
       dplyr::bind_rows(lrt_13) %>%
       dplyr::bind_rows(lrt_14) %>%
       dplyr::bind_rows(lrt_23) %>%
       dplyr::bind_rows(lrt_24) %>%
       dplyr::bind_rows(lrt_34)
  }

  fits = list(fit0=fit0,fit1=fit1,fit2=fit2)
  if(x == "role_primary"){
    fits$fit3 = fit3;
    fits$fit4 = fit4;
  }

  # lrts_x = lrts_x %>%
  #   dplyr::mutate(pvalue.unadjusted = Pr..Chisq., N = nrow(dat_x), missing_data_strategy = "Complete Case Analysis") %>%
  #   dplyr::select(Grouping,Comparison, missing_data_strategy, N, LogLik, Df, Chisq, pvalue.unadjusted) %>%
  #   dplyr::mutate(stars = ifelse(pvalue.unadjusted<.05, "*","")) 

  #writexl::write_xlsx(lrts_x, path = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up", "coefficients", x, paste0(x, " LRTs.xlsx")))
  
  
  #return(list(lrts = lrts_x, fits = fits))
  return(NULL)
})
names(out_list) = groupings_vec
#readr::write_rds(out_list, file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up","mixed_models.rds"), compress = "gz")
#out_list = readr::read_rds(file = file.path(onedrive_wd, "Meeting Memos", "2025-06-04 Follow-up","mixed_models.rds"))





out_df = lapply(1:J, function(j){
  lrts_j = out_list[[j]]$lrts
  lrt02_j = lrts_j %>% dplyr::filter(Comparison == "M0 vs. M2")
  pval_02 = lrt02_j %>% purrr::pluck("pvalue.bonferroni")
  if(pval_02<.05){
    return(lrts_j)
  } else {
    return(lrt02_j)
  }
}) %>% dplyr::bind_rows()


writexl::write_xlsx(
  out_df, 
  path = file.path(onedrive_wd,"Meeting Memos", "2025-06-04 Follow-up","Likelihood-Ratio-Test-mixed-models.xlsx")
)
