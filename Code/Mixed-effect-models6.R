rm(list = ls())


library(tidyverse)
library(lmtest)
library(gt)
library(emmeans)
library(ggeffects)
library(future)
library(future.apply)
library(ggtext)
library(patchwork)

future::plan(strategy = "multisession", workers =  8)


# Marcus W. Locations
root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")
tables_wd = file.path(onedrive_wd, "Publications", "tables", "R")
figures_wd = file.path(onedrive_wd, "Publications", "figures", "R")


# Marcus's Home Desktop (White-Rhino) 
#root_wd = "C:/Users/marcu"
#onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

setwd(github_wd)
#source(file.path(github_wd, "Code", "participant_demographics_data.R"))
source(file.path(github_wd, "Code", "utils", "utils.R"))
source(file.path(github_wd, "Code", "utils", "glmm_long_models.R"))
source(file.path(github_wd, "Code", "prob_helpers.R"))

dat = demo_and_response_data(onedrive_wd = onedrive_wd, M = 0, clean_gender = F)


# Clean up the 
  
  dat = dat%>% 
    dplyr::filter(round==1) %>% 
    dplyr::mutate(rid = 1:n()) %>% 
    dplyr::relocate(rid)
  
 
# Expertise
  # Create dummy variables 
  expertise_df = dat %>% 
    dplyr::select(rid, expertise) %>% 
    dplyr::filter(!is.na(expertise)) %>% 
    fastDummies::dummy_cols("expertise", split = ",", remove_first_dummy = T)
  # Make sum contrast coding
    ids = which(expertise_df$expertise == "Ambulatory Care")
    expertise_df[ids,-(1:2)] = -1
  # Join and drop dummy codes where there are not at least 5 variables
  dat = dat %>% dplyr::left_join(expertise_df %>% dplyr::select(-expertise), by = "rid") %>% dplyr::select(-dplyr::any_of(dummies_to_drop(.,"expertise_")))
  
# Current role
  role_current_df = dat %>% 
    dplyr::select(rid, role_current) %>% 
    dplyr::filter(!is.na(role_current)) %>% 
    fastDummies::dummy_cols("role_current", split = ",", remove_first_dummy = T)
  # Make sum contrast coding
    ids = which(role_current_df$role_current == "Clinical Educator/Practice")
    role_current_df[ids,-(1:2)] = -1
  # Join and drop dummy codes where there are not at least 5 variables
  dat = dat %>% dplyr::left_join(role_current_df %>% dplyr::select(-role_current), by = "rid") %>% dplyr::select(-dplyr::any_of(dummies_to_drop(.,"role_current_")))
  
  
  names(dat) = names(dat) %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_replace_all("-", "_") %>% 
    stringr::str_replace_all("\\/", "_") %>% 
    stringr::str_replace_all("\\.", "") %>% 
    stringr::str_remove_all("expertise_") %>% 
    stringr::str_remove_all("experience_") %>%
    stringr::str_remove_all("role_current_")
  
  dat = dat %>% 
    dplyr::select(rid,name,varshort,essential,category,role_primary, role_current, expertise, rn, edu, age, gender) 
  
  dat = dat %>% dplyr::mutate(essential = ifelse(essential == "Yes", 1 , 0))
  

# Function to fit models and perform LRT for covariates using parallel processing

# Fit all covariate models
#covariate_fits <- fit_covariate_models(dat)
#write_rds(covariate_fits, file = file.path(onedrive_wd,"Data", "Model Fits", "model6_fits.rds"), compress = "gz")
  
covariate_fits = readr::read_rds(file = file.path(onedrive_wd,"Data", "Model Fits", "model6_fits.rds"))

# Create likelihood ratio test results table
create_lrt_table <- function(covariate_fits, correction_method = "BH") {
  
  # Check convergence of all models before proceeding
  convergence_issues <- list()
  
  for(cov_name in names(covariate_fits)) {
    cov_fit <- covariate_fits[[cov_name]]
    
    # Check convergence for each model (m0, m1, m2)
    models <- list(m0 = cov_fit$model_0, m1 = cov_fit$model_1, m2 = cov_fit$model_2)
    
    for(model_name in names(models)) {
      model <- models[[model_name]]
      
      # For glmer objects, check convergence
      if(inherits(model, "glmerMod")) {
        conv_code <- model@optinfo$conv$opt
        if(conv_code != 0) {
          convergence_issues[[paste0(cov_name, "_", model_name)]] <- paste0(
            "Model ", model_name, " for ", cov_name, " did not converge (code: ", conv_code, ")"
          )
        }
        
        # Check for additional convergence warnings
        if(length(model@optinfo$warnings) > 0) {
          convergence_issues[[paste0(cov_name, "_", model_name, "_warn")]] <- paste0(
            "Model ", model_name, " for ", cov_name, " has warnings: ", 
            paste(model@optinfo$warnings, collapse = "; ")
          )
        }
      }
      
      # For GLMMadaptive mixed_model objects, check convergence
      if(inherits(model, "MixMod")) {
        # Check convergence code from optimization
        if(!is.null(model$opt) && model$opt$convergence != 0) {
          convergence_issues[[paste0(cov_name, "_", model_name)]] <- paste0(
            "Model ", model_name, " for ", cov_name, " did not converge (code: ", model$opt$convergence, ")"
          )
        }
        
        # Check for warnings or messages
        if(!is.null(model$warnings) && length(model$warnings) > 0) {
          convergence_issues[[paste0(cov_name, "_", model_name, "_warn")]] <- paste0(
            "Model ", model_name, " for ", cov_name, " has warnings: ", 
            paste(model$warnings, collapse = "; ")
          )
        }
        
        # Check if EM algorithm converged (if used)
        if(!is.null(model$converged) && !model$converged) {
          convergence_issues[[paste0(cov_name, "_", model_name, "_em")]] <- paste0(
            "Model ", model_name, " for ", cov_name, " EM algorithm did not converge"
          )
        }
      }
    }
  }
  
  # If there are convergence issues, prompt user
  if(length(convergence_issues) > 0) {
    cat("WARNING: Some models did not converge properly:\n")
    for(issue in convergence_issues) {
      cat("- ", issue, "\n")
    }
    cat("\nLikelihood ratio tests may not be reliable with non-converged models.\n")
    response <- readline(prompt = "Do you want to continue with the LRT analysis despite these convergence issues? (y/n): ")
    
    if(tolower(trimws(response)) != "y") {
      stop("Analysis stopped due to convergence concerns.")
    }
    cat("Proceeding with analysis (user accepted convergence risks)...\n\n")
  }
  
  # Extract LRT results from fitted models
  lrt_results <- list(
    "Primary Role" = covariate_fits$role_primary$lrt,
    "Current Role" = covariate_fits$role_current$lrt,
    "Gender" = covariate_fits$gender$lrt,
    "Age" = covariate_fits$age$lrt,
    "Education" = covariate_fits$edu$lrt,
    "Nursing Experience" = covariate_fits$rn$lrt,
    "Expertise" = covariate_fits$expertise$lrt
  )
  
  # Extract raw p-values for both main effects and interactions
  raw_p_main <- sapply(lrt_results, function(x) x$`Pr(>Chisq)`[2])
  raw_p_interaction <- sapply(lrt_results, function(x) x$`Pr(>Chisq)`[3])
  
  # Apply multiple comparison correction to main effects (6 tests)
  corrected_main <- raw_p_main
  main_p_valid <- !is.na(raw_p_main)
  if(sum(main_p_valid) > 0) {
    corrected_main[main_p_valid] <- p.adjust(raw_p_main[main_p_valid], method = correction_method)
  }
  
  # Apply multiple comparison correction to interaction effects (6 tests)
  corrected_interaction <- raw_p_interaction
  interaction_p_valid <- !is.na(raw_p_interaction)
  if(sum(interaction_p_valid) > 0) {
    corrected_interaction[interaction_p_valid] <- p.adjust(raw_p_interaction[interaction_p_valid], method = correction_method)
  }
  
  # Format p-values - only show corrected values
  format_p_corrected <- function(corrected_p) {
    if (is.na(corrected_p)) return("--")
    if (corrected_p < .001) return("< .001")
    return(sprintf("%.3f", corrected_p))
  }
  
  # Extract results and create table
  table_data <- data.frame(
    Expert_Characteristic = names(lrt_results),
    M0_vs_M1_ChiSq = sapply(lrt_results, function(x) round(x$Chisq[2], 3)),
    M0_vs_M1_df = sapply(lrt_results, function(x) x$Df[2]),
    M0_vs_M1_p = sapply(corrected_main, format_p_corrected),
    M1_vs_M2_ChiSq = sapply(lrt_results, function(x) round(x$Chisq[3], 3)),
    M1_vs_M2_df = sapply(lrt_results, function(x) x$Df[3]),
    M1_vs_M2_p = sapply(corrected_interaction, format_p_corrected)
  )
  
  # Create APA-formatted table using gt
  gt_table <- table_data %>%
    gt() %>%
    tab_spanner(
      label = "Model 0 vs. Model 1 (Main Effect)",
      columns = c(M0_vs_M1_ChiSq, M0_vs_M1_df, M0_vs_M1_p)
    ) %>%
    tab_spanner(
      label = "Model 1 vs. Model 2 (Interaction Effect)",
      columns = c(M1_vs_M2_ChiSq, M1_vs_M2_df, M1_vs_M2_p)
    ) %>%
    cols_label(
      Expert_Characteristic = "Expert Characteristic",
      M0_vs_M1_ChiSq = html("&chi;<sup>2</sup>"),
      M0_vs_M1_df = html("<em>df</em>"),
      M0_vs_M1_p = html("<em>p</em><sup>a</sup>"),
      M1_vs_M2_ChiSq = html("&chi;<sup>2</sup>"),
      M1_vs_M2_df = html("<em>df</em>"),
      M1_vs_M2_p = html("<em>p</em><sup>a</sup>")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title()
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 14,
      column_labels.font.weight = "bold",
      table.font.names = "Times New Roman"
    ) %>%
    cols_align(
      align = "center",
      columns = c(M0_vs_M1_ChiSq, M0_vs_M1_df, M0_vs_M1_p, M1_vs_M2_ChiSq, M1_vs_M2_df, M1_vs_M2_p)
    ) %>%
    tab_footnote(
      footnote = paste0("P-values adjusted for multiple comparisons using ", 
                       switch(correction_method,
                              "BH" = "Benjamini-Hochberg FDR correction",
                              "bonferroni" = "Bonferroni correction",
                              "holm" = "Holm-Bonferroni correction",
                              paste0(correction_method, " correction"))),
      locations = cells_column_labels(columns = c(M0_vs_M1_p, M1_vs_M2_p))
    )
  
  return(gt_table)
}

# Create and display the table
lrt_table <- create_lrt_table(covariate_fits)
print(lrt_table)



response_probability_table<-function(x,dat,lrt_table){
  dat2_x = get_dat2(dat, x)
  
  lrt_results_x = lrt_table$`_data` %>% dplyr::filter(Expert_Characteristic == names(covariate_name_map())[covariate_name_map()==x])
  
  
  # ... overall (grand mean)
  overall_agg_x = dat2_x %>% 
    dplyr::select(name:essential, dplyr::any_of(x)) %>% 
    dplyr::group_by(.data[[x]]) %>% 
    dplyr::summarise(Overall = mean(essential, na.rm=T))
  
  lrt_M0_vs_M1 = data.frame(lrt_overall = with(lrt_results_x, paste0("Chi2", "(", M0_vs_M1_df ,") = ", round(M0_vs_M1_ChiSq,2), ", p-adj = ", M0_vs_M1_p)))
  
  overall_agg_x = lrt_M0_vs_M1 %>% dplyr::bind_rows(overall_agg_x)  %>% dplyr::relocate(lrt_overall, .after = .data[[x]]) %>% 
    dplyr::mutate(across(where(is.numeric), function(x){round(100*x,1) %>% as.character() }))
  
  # ... by skill category
  category_agg_x = dat2_x %>% 
    dplyr::select(name:essential, dplyr::any_of(x)) %>% 
    dplyr::group_by(category, .data[[x]]) %>% 
    dplyr::reframe(essential = mean(essential, na.rm = T)) %>% 
    tidyr::pivot_wider(names_from= "category",values_from = "essential") 
  
  
  lrt_M1_vs_M2 = data.frame(lrt_categories = with(lrt_results_x, paste0("Chi2", "(", M1_vs_M2_df ,") = ", round(M1_vs_M2_ChiSq,2), ", p-adj = ", M1_vs_M2_p)))
  category_agg_x = lrt_M1_vs_M2 %>% dplyr::bind_rows(category_agg_x) %>% dplyr::relocate(lrt_categories, .after = .data[[x]])
  
  # ... now combine them
  table_x = overall_agg_x %>% dplyr::left_join(category_agg_x, by = x) %>% 
    dplyr::mutate(across(where(is.numeric), function(x){round(100*x,1) %>% as.character() }))
  
  # ...now append with counts
  counts_x = dat2_x %>% 
    dplyr::group_by(.data[[x]]) %>% 
    dplyr::summarise(n = length(unique(name))) 
  
  tab_out = table_x %>% dplyr::left_join(counts_x, by =x) %>% dplyr::relocate(n, .after = .data[[x]]) 
  
  names(tab_out)[1] = "Expert Characteristic"
  
  tab_out[,1] = as.character(tab_out[,1])
  tab_out$`Expert Characteristic`[1] =  paste0("**",names(covariate_name_map())[covariate_name_map()==x], "**")
  
  return(tab_out)  
}


probability_table = lapply(covariate_name_map(), function(x) response_probability_table(x,dat,lrt_table)) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(`Expert Characteristic` = stringr::str_remove_all(`Expert Characteristic`, "zz")) %>% 
  dplyr::mutate(across(where(is.numeric), as.character))

probability_table[is.na(probability_table)] = ""

