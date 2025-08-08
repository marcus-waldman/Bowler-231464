# Helper functions to fit individual covariate models
fit_role_primary_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,role_primary,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + role_primary + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*role_primary + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Primary Role",
    data = dat_k,
    model_0 = m0,
    model_1 = m1, 
    model_2 = m2,
    lrt = lrt,
    notes = "General difference only"
  ))
}

fit_gender_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,gender,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + gender + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*gender + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Gender",
    data = dat_k,
    model_0 = m0,
    model_1 = m1,
    model_2 = m2,
    lrt = lrt,
    notes = "General difference only"
  ))
}

fit_age_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,age_years,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + age_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*age_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Age",
    data = dat_k,
    model_0 = m0,
    model_1 = m1,
    model_2 = m2,
    lrt = lrt,
    notes = "General difference + interaction"
  ))
}

fit_edu_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,edu_years,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + edu_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*edu_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Education",
    data = dat_k,
    model_0 = m0,
    model_1 = m1,
    model_2 = m2,
    lrt = lrt,
    notes = "None significant"
  ))
}

fit_rn_years_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,rn_years,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + rn_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*rn_years + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Nursing Experience",
    data = dat_k,
    model_0 = m0,
    model_1 = m1,
    model_2 = m2,
    lrt = lrt,
    notes = "Interactions and main effects significant"
  ))
}

fit_expertise_models <- function(data) {
  library(lme4)
  dat_k = data %>% dplyr::select(essential,category,Medical_Surgical:Pediatrics,varshort,name) %>% na.omit()
  m0 = glmer(essential~category + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m1 = glmer(essential~category + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  m2 = glmer(essential~category*Medical_Surgical + category*Population_Health + category*Behavioral_Health + category*Critical_Care + category*ED + category*Perioperative + category*OB + category*Pediatrics + (1|name), data = dat_k, family = binomial, verbose = 1, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1E6)))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Expertise",
    data = dat_k,
    model_0 = m0,
    model_1 = m1,
    model_2 = m2,
    lrt = lrt,
    notes = "None significant"
  ))
}

fit_covariate_models <- function(data) {
  
  library(future.apply)
  
  # Define the list of model fitting functions
  model_functions <- list(
    role_primary = fit_role_primary_models,
    gender = fit_gender_models,
    age_years = fit_age_models,
    edu_years = fit_edu_models,
    rn_years = fit_rn_years_models,
    expertise = fit_expertise_models
  )
  
  # Fit models in parallel
  covariate_results <- future_lapply(model_functions, function(fit_func) {
    fit_func(data)
  }, future.seed = TRUE)
  
  return(covariate_results)
}

