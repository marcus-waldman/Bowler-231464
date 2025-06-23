rm(list = ls())

# Skills Study
# Creating Analytic Data Set

library(tidyverse)
library(lme4)
library(lmtest)
library(future)
library(future.apply)
library(doFuture)

library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(mice)
library(brms)

library(glm2)


library(marginaleffects)  # For predictions()
library(MASS)             # For ginv()
library(multiwayvcov)

library(glmnet)

library(caret)
library(rpart)
library(rpart.plot)


plan(strategy="multisession", workers = future::availableCores())
options(future.globals.maxSize = 128 * 1024^3)

# Cristian'S Windows
#root_wd = "C:/Users/sarabiac"
#onedrive_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive - The University of Colorado Denver/College of Nursing/Repos/Windows Repo/Bowler-231464")

# Cristian's Mac
#root_wd = "/Users/cristiansarabia/Library/CloudStorage"
#onedrive_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Skills Study/Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd, "OneDrive-TheUniversityofColoradoDenver/College of Nursing/Repos/Mac Repo/Bowler-231464")


# Marcus W. Locations
#root_wd = "C:/Users/waldmanm/"
#onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

# Marcus's Home Desktop (White-Rhino) 
#root_wd = "C:/Users/marcu"
#onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

# CSPH-Biostats Cluster
root_wd = "/biostats_share/waldmanm"
onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

#source(file.path(github_wd, "Code", "participant_demographics_data.R"))
source(file.path(github_wd, "Code", "utils", "utils.R"))
source(file.path(github_wd, "Code", "utils", "glm2_multiway.R"))

# Load in analytic dataset
M = 50
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
    
  # Turn essential into a 0/1 variable
  dat_m = dat_m %>% dplyr::mutate(essential = as.integer(essential=="Yes"))
  
  #
  
  return(dat_m)
})
skill_categories = unique(implist[[1]]$category) %>% as.character()

### CART 
longimp = implist %>% dplyr::bind_rows() %>% 
  dplyr::left_join(implist[[1]] %>% dplyr::group_by(name) %>% dplyr::summarise() %>%  dplyr::mutate(fid = 1:n()), by = "name") %>% 
  dplyr::mutate(essential = ifelse(essential==1,"Yes","No") %>% as.factor)

# Create custom folds using the foldid column
registerDoFuture()

K=length(skill_categories)
for(k in 1:K){
  longimp_k = longimp %>% dplyr::filter(category == skill_categories[k])
  folds_k <- lapply(1:max(longimp_k$fid), function(f) which(longimp_k$fid == f))
  names(folds_k) <- paste0("Fold", 1:max(longimp_k$fid))
  
  # Set up caret training using parallel CV
  train_control_k <- trainControl(
    method = "cv",
    number = length(folds_k),
    indexOut = folds_k,
    allowParallel = TRUE
  )
  
  # Train the CART model
  set.seed(456)
  cart_model_k <- caret::train(
    essential~role_primary+gender+age_years+edu_years+rn_years +
      Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics +
      New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist,
    data = longimp_k,
    method = "rpart",
    trControl = train_control_k,
    tuneLength = 10
  )
  
  print(cart_model_k)
  
  
  setwd(file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up"))
  pdf(file = paste0("CART-",stringr::str_replace_all(skill_categories[k],"\\/", "-"),".pdf"), width = 14, height = 10)
  rpart.plot(cart_model_k$finalModel)
  dev.off()
  
}








###
# Baseline Model
baseline <- list(formula = essential~category, wald.variables = "category", fit = NULL, D1 = NULL)
baseline$fit = pool_glm2_multiway(formula = essential ~ category, imputed_list = implist, cluster_vars = "name")
baseline$D1 = Wald(baseline$fit, terms = "category")


# Main Effects Models
K = length(skill_categories)
main_effects <-  list(
  role_primary = list( formula = essential~category+role_primary, wald.comparison = NULL, fit = NULL, D1 = NULL), 
  gender = list( formula = essential~category+gender, wald.comparison = NULL, fit = NULL, D1 = NULL), 
  age_years = list( formula = essential~category+age_years, wald.comparison = NULL, fit = NULL, D1 = NULL), 
  edu_years = list( formula = essential~category+edu_years, wald.comparison = NULL, fit = NULL, D1 = NULL), 
  rn_years = list( formula = essential ~ category+rn_years, wald.comparison = NULL, fit = NULL, D1 = NULL), 
  expertise = list( formula = essential ~ category + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics, 
                    wald.comparison = baseline$formula, 
                    fit = NULL, 
                    D1 = NULL), 
  role_current = list( formula = essential ~ category + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist, 
                       wald.comparison = baseline$formula, 
                       fit = NULL, 
                       D1 = NULL)
)
P = length(main_effects)
covariates = names(main_effects)
for(p in 1:P){
  fit_kj = pool_glm2_multiway(formula =  main_effects[[covariates[p]]]$formula, 
                              imputed_list = implist, 
                              cluster_vars = c("name"))
  main_effects[[covariates[p]]]$fit = fit_kj
  if( !is.null(main_effects[[covariates[p]]]$wald.comparison) ){
    wald.terms = setdiff( attr(terms( main_effects[[covariates[p]]]$formula), "term.labels"), attr(terms( baseline$formula), "term.labels"))
    main_effects[[covariates[p]]]$D1 = Wald(fit_kj, terms = wald.terms)
  }
}


#Create a one-way interactions template template
one_way_interactions <-  list(
  role_primary = list( formula = essential~category*role_primary, wald.comparison = main_effects[["role_primary"]]$formula, fit = NULL), 
  gender = list( formula = essential~category*gender, wald.comparison = main_effects[["gender"]]$formula, fit = NULL), 
  age_years = list( formula = essential~category*age_years, wald.comparison = main_effects[["age_years"]]$formula, fit = NULL), 
  edu_years = list( formula = essential~category*edu_years, wald.comparison = main_effects[["edu_years"]]$formula, fit = NULL), 
  rn_years = list( formula = essential ~ category*rn_years, wald.comparison = main_effects[["rn_years"]]$formula, fit = NULL), 
  expertise = list( formula = essential ~ category*Medical_Surgical + category*Population_Health + category*Behavioral_Health + category*Critical_Care + category*ED + category*Perioperative + category*OB + category*Pediatrics, 
                    wald.comparison =  main_effects[["expertise"]]$formula, 
                    fit = NULL), 
  role_current = list( formula = essential ~ category*New_Grad_Res_Coord_Educator + category*Clinical_Instructor_Academic + category*Preceptor + category*Simulationist, 
                       wald.comparison = main_effects[["role_current"]]$formula, 
                       fit = NULL)
)
for(p in 1:P){
  fit_kj = pool_glm2_multiway(formula =  one_way_interactions[[covariates[p]]]$formula, 
                              imputed_list = implist, 
                              cluster_vars = c("name"))
  one_way_interactions[[covariates[p]]]$fit = fit_kj
  if( !is.null(one_way_interactions[[covariates[p]]]$wald.comparison) ){
    wald.terms = setdiff(fit_kj$results$Term, main_effects[[covariates[[p]]]]$fit$results$Term)
    one_way_interactions[[covariates[p]]]$D1 = Wald(fit_kj, terms = wald.terms)
  }
}


wald_notes<-function(wald){
  if(is.null(wald)){return("p=N/A")}
  return(paste0("p=", ifelse(wald$p.value<.001, "<.001", signif(wald$p.value,3))))
}

wd_current = getwd()
setwd(file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up"))
for(p in 1:P){
  pvals = paste0("M1 vs. M0: ", wald_notes(main_effects[[p]]$D1),"; M2 vs. M1: ", wald_notes(one_way_interactions[[p]]$D1))
  tab_model(
    baseline$fit, main_effects[[p]]$fit, one_way_interactions[[p]]$fit, 
    rm.terms = paste0("category [", unique(implist[[1]]$category), "]"), 
    dv.labels = paste0("Model ", 1:3), 
    title = paste0(toupper(covariates[p]),"<br>", "Wald tests: ",pvals), 
    file = paste0(covariates[p],"-coefficients-table.html")
  )
}

####

longimp = implist %>% dplyr::bind_rows() %>% 
  dplyr::left_join(implist[[1]] %>% dplyr::group_by(name) %>% dplyr::summarise() %>%  dplyr::mutate(fid = 1:n()), by = "name")


x.formula = ~ -1 + category*role_primary*gender + category*role_primary*age_years + category*role_primary*edu_years + category*role_primary*rn_years +
  category*role_primary*Medical_Surgical + category*role_primary*Population_Health + category*role_primary*Behavioral_Health + category*role_primary*Critical_Care + category*role_primary*ED + category*role_primary*Perioperative + category*role_primary*OB + category*role_primary*Pediatrics +
  category*role_primary*New_Grad_Res_Coord_Educator + category*role_primary*Clinical_Instructor_Academic + category*role_primary*Preceptor + category*role_primary*Simulationist +
  category*gender*age_years + category*gender*edu_years + category*gender*rn_years +
  category*gender*Medical_Surgical + category*gender*Population_Health + category*gender*Behavioral_Health + category*gender*Critical_Care + category*gender*ED + category*gender*Perioperative + category*gender*OB + category*gender*Pediatrics +
  category*gender*New_Grad_Res_Coord_Educator + category*gender*Clinical_Instructor_Academic + category*gender*Preceptor + category*gender*Simulationist +
  category*age_years*edu_years + category*age_years*rn_years +
  category*age_years*Medical_Surgical + category*age_years*Population_Health + category*age_years*Behavioral_Health + category*age_years*Critical_Care + category*age_years*ED + category*age_years*Perioperative + category*age_years*OB + category*age_years*Pediatrics +
  category*age_years*New_Grad_Res_Coord_Educator + category*age_years*Clinical_Instructor_Academic + category*age_years*Preceptor + category*age_years*Simulationist +
  category*edu_years*rn_years +
  category*edu_years*Medical_Surgical + category*edu_years*Population_Health + category*edu_years*Behavioral_Health + category*edu_years*Critical_Care + category*edu_years*ED + category*edu_years*Perioperative + category*edu_years*OB + category*edu_years*Pediatrics +
  category*edu_years*New_Grad_Res_Coord_Educator + category*edu_years*Clinical_Instructor_Academic + category*edu_years*Preceptor + category*edu_years*Simulationist +
  category*Medical_Surgical*New_Grad_Res_Coord_Educator +  category*Medical_Surgical*Clinical_Instructor_Academic +  category*Medical_Surgical*Preceptor +  category*Medical_Surgical*Simulationist +
  category*Population_Health*New_Grad_Res_Coord_Educator +  category*Population_Health*Clinical_Instructor_Academic +  category*Population_Health*Preceptor +  category*Population_Health*Simulationist +
  category*Behavioral_Health*New_Grad_Res_Coord_Educator +  category*Behavioral_Health*Clinical_Instructor_Academic +  category*Behavioral_Health*Preceptor +  category*Behavioral_Health*Simulationist +
  category*Critical_Care*New_Grad_Res_Coord_Educator +  category*Critical_Care*Clinical_Instructor_Academic +  category*Critical_Care*Preceptor +  category*Critical_Care*Simulationist +
  category*ED*New_Grad_Res_Coord_Educator +  category*ED*Clinical_Instructor_Academic +  category*ED*Preceptor +  category*ED*Simulationist +
  category*Perioperative*New_Grad_Res_Coord_Educator +  category*Perioperative*Clinical_Instructor_Academic +  category*Perioperative*Preceptor +  category*Perioperative*Simulationist +
  category*OB*New_Grad_Res_Coord_Educator +  category*OB*Clinical_Instructor_Academic +  category*OB*Preceptor +  category*OB*Simulationist +
  category*Pediatrics*New_Grad_Res_Coord_Educator +  category*Pediatrics*Clinical_Instructor_Academic +  category*Pediatrics*Preceptor +  category*Pediatrics*Simulationist

registerDoFuture()
fit_cv = glmnet::cv.glmnet(
  x = model.matrix(x.formula, data = longimp), 
  y = longimp$essential, 
  family = "binomial", 
  alpha = 1,
  foldid = longimp$fid, 
  intercept = T, 
  standardize = T, 
  trace.it = T, 
  parallel = T
)

fit = pool_glm2_multiway(formula = essential ~ Medical_Surgical*category + Population_Health*category + Behavioral_Health*category + Critical_Care*category + ED*category + Perioperative*category + OB*category+ Pediatrics*category, imputed_list = implist, cluster_vars = "name")
tab_model(fit)





