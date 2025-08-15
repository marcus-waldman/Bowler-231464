# Helper functions to fit individual covariate models using GLMMadaptive
fit_role_primary_models <- function(data) {
  
  library(GLMMadaptive)
  
  dat_k = data %>% dplyr::select(essential,category,role_primary,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + role_primary, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * role_primary, random = ~ 1 || name, data = dat_k, family = binomial(),  control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
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

fit_role_current_models <- function(data) {
  
  library(GLMMadaptive)
  
  dat_k = data %>% dplyr::select(essential,category,role_current,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + role_current, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * role_current, random = ~ 1 || name, data = dat_k, family = binomial(),  control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  return(list(
    label = "Current Role",
    data = dat_k,
    model_0 = m0,
    model_1 = m1, 
    model_2 = m2,
    lrt = lrt,
    notes = "General difference only"
  ))
}

fit_gender_models <- function(data) {
  library(GLMMadaptive)
  dat_k = data %>% dplyr::select(essential,category,gender,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + gender, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * gender, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
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
  library(GLMMadaptive)
  dat_k = data %>% dplyr::select(essential,category,age,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + age, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * age, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
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
  library(GLMMadaptive)
  dat_k = data %>% dplyr::select(essential,category,edu,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + edu, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * edu, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
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

fit_rn_models <- function(data) {
  library(GLMMadaptive)
  dat_k = data %>% dplyr::select(essential,category,rn,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + rn, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * rn, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
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
  library(GLMMadaptive)
  library(parallel)
  
  cl <- makeCluster(8); setDefaultCluster(cl = cl)
  
  dat_k = data %>% dplyr::select(essential,category,expertise,varshort,name) %>% na.omit()
  m0 = mixed_model(essential ~ category, random = ~ 1 || name, data = dat_k, family = binomial(), control =  list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m1 = mixed_model(essential ~ category + expertise, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  m2 = mixed_model(essential ~ category * expertise, random = ~ 1 || name, data = dat_k, family = binomial(), control = list(verbose = T, optimizer = "optim", iter_EM = 0, iter_qN = 30, max_coef_value = 100))
  lrt = lmtest::lrtest(m0,m1,m2)
  
  stopCluster(cl)
  
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



get_dat2<-function(dat, x){
  
  dat2 = dat %>% 
    dplyr::select(name,category,varshort,essential,dplyr::any_of(x))
  
  names_na = dat2$name[is.na(dat2[,x])] %>% as.character() %>% unique()
  
  if(x == "role_primary"){
    
    dat2 = dat2 %>% 
      dplyr::mutate(
        role_primary__1 = stringr::str_split_i(role_primary, ", ",1), 
        role_primary__2 = stringr::str_split_i(role_primary, ", ",2)
      ) %>% 
      dplyr::select(-role_primary) %>% 
      tidyr::pivot_longer(role_primary__1:role_primary__2, names_to = "variable", values_to = "role_primary_long") %>% 
      dplyr::select(-variable) 
    x = "role_primary_long"
  }
  
  if(x == "role_current"){
    
    dat2 = dat2 %>% 
      dplyr::mutate(
        role_current__1 = stringr::str_split_i(role_current, ", ",1), 
        role_current__2 = stringr::str_split_i(role_current, ", ",2)
      ) %>% 
      dplyr::select(-role_current) %>% 
      tidyr::pivot_longer(role_current__1:role_current__2, names_to = "variable", values_to = "role_current_long") %>% 
      dplyr::select(-variable) 
    
    x = "role_current_long"
  }
  
  if(x == "expertise"){
    
    dat2 = dat2 %>% 
      dplyr::mutate(
        expertise__1 = stringr::str_split_i(expertise, ", ",1), 
        expertise__2 = stringr::str_split_i(expertise, ", ",2), 
        expertise__3 = stringr::str_split_i(expertise, ", ",3)
      ) %>% 
      dplyr::select(-expertise) %>% 
      tidyr::pivot_longer(expertise__1:expertise__3, names_to = "variable", values_to = "expertise_long") %>% 
      dplyr::select(-variable) 
    x = "expertise_long"
  }
  
  
  
  if(length(names_na)>0){
    ids = which(dat2$name %in% names_na)
    dat2[ids,x] = "zzMissing"
  }
  
  names(dat2) = stringr::str_remove_all(names(dat2), "_long")
  
  return(dat2 %>% na.omit())
  
}


fit_covariate_models <- function(data) {
  
  library(future.apply)
  
  # Define the list of model fitting functions
  model_functions <- list(
    role_primary = fit_role_primary_models,
    role_current = fit_role_current_models,
    gender = fit_gender_models,
    age = fit_age_models,
    edu = fit_edu_models,
    rn = fit_rn_models,
    expertise = fit_expertise_models
  )
  
  # Fit models in parallel
  covariate_results <- future.apply::future_lapply(names(model_functions), function(x) {
    print(x)
    fit_func = model_functions[[x]]
    fit_func(get_dat2(data,x))
  }, future.seed = 42)
  
  names(covariate_results) = names(model_functions)
  return(covariate_results)
}


# Map display names back to covariate_fits names
covariate_name_map<-function(x){
  c(
    "Primary Role" = "role_primary",
    "Current Role" = "role_current",
    "Gender" = "gender", 
    "Age" = "age",
    "Education" = "edu",
    "Nursing Experience" = "rn",
    "Expertise" = "expertise"
  )
} 