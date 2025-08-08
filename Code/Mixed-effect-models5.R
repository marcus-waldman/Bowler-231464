rm(list = ls())


library(tidyverse)
library(lmtest)
library(gt)
library(emmeans)
library(ggeffects)
library(future)
library(future.apply)

future::plan(strategy = "multisession", workers =  8)


# Marcus W. Locations
root_wd = "C:/Users/waldmanm/"
onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

# Marcus's Home Desktop (White-Rhino) 
#root_wd = "C:/Users/marcu"
#onedrive_wd = file.path(root_wd,"OneDrive - The University of Colorado Denver", "Bowler, Fara's files - March 2023_FB BH SH")
#github_wd = file.path(root_wd,"git-repositories", "Bowler-231464")

setwd(github_wd)
#source(file.path(github_wd, "Code", "participant_demographics_data.R"))
source(file.path(github_wd, "Code", "utils", "utils.R"))
source(file.path(github_wd, "Code", "utils", "glmm_models.R"))
source(file.path(github_wd, "Code", "prob_helpers.R"))

dat = demo_and_response_data(onedrive_wd = onedrive_wd, M = 0)


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
    dplyr::select(rid,name,varshort,essential,category,role_primary, gender, edu_years:Simulationist) 
  
  #Let's get standardized estimates of the continuous variables
  demo_dat = dat %>% 
    dplyr::group_by(name) %>% 
    dplyr::reframe(edu_years = edu_years[1], rn_years = rn_years[1], age_years = age_years[1]) %>% 
    dplyr::mutate(edu_years_c = (edu_years - mean(edu_years, na.rm = T)) ) %>% 
    dplyr::mutate(rn_years_c = (rn_years - mean(rn_years, na.rm = T)) ) %>% 
    dplyr::mutate(age_years_c = (age_years - mean(age_years, na.rm = T)) )
   dat = dat %>% dplyr::left_join(demo_dat %>% dplyr::select(-(edu_years:age_years)), by = "name")
    
  # Turn essential into a 0/1 variable
  dat = dat %>% dplyr::mutate(essential = as.integer(essential=="Yes"))
  
  # Set the reference category
  dat$category = relevel(dat$category, ref = "Asepsis and Infection Control")
  dat$gender = relevel(dat$gender, ref = "Female")
  dat$role_primary = relevel(dat$role_primary, ref = "Practice Clinical Expert")
  
  
  #Lets apply sum contrasts
  contrasts(dat$category) = "contr.sum"  
  contrasts(dat$gender) = "contr.sum"
  contrasts(dat$role_primary) = "contr.sum"
  
  

# Function to fit models and perform LRT for covariates using parallel processing

# Fit all covariate models
#covariate_fits <- fit_covariate_models(dat)
#write_rds(covariate_fits, file = file.path(onedrive_wd,"Data", "Model Fits", "model5_fits.rds"), compress = "gz")
  
covariate_fits = readr::read_rds(file = file.path(onedrive_wd,"Data", "Model Fits", "model5_fits.rds"))

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
    "Gender" = covariate_fits$gender$lrt,
    "Age" = covariate_fits$age_years$lrt,
    "Education" = covariate_fits$edu_years$lrt,
    "Nursing Experience" = covariate_fits$rn_years$lrt,
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
    tab_header(
      title = "Likelihood Ratio Tests for Expert Characteristic Effects"
    ) %>%
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
      column_labels.font.weight = "bold"
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
                              paste0(correction_method, " correction")),
                       " (6 comparisons for main effects, 6 comparisons for interaction effects)."),
      locations = cells_column_labels(columns = c(M0_vs_M1_p, M1_vs_M2_p))
    )
  
  return(gt_table)
}

# Create and display the table
lrt_table <- create_lrt_table(covariate_fits)
print(lrt_table)


# Create regression coefficient tables for significant effects
create_regression_tables <- function(lrt_table, covariate_fits, alpha = 0.05) {
  
  # Debug: Let's reconstruct p-values directly from covariate_fits instead of parsing gt table
  lrt_results <- list(
    "Primary Role" = covariate_fits$role_primary$lrt,
    "Gender" = covariate_fits$gender$lrt,
    "Age" = covariate_fits$age_years$lrt,
    "Education" = covariate_fits$edu_years$lrt,
    "Nursing Experience" = covariate_fits$rn_years$lrt,
    "Expertise" = covariate_fits$expertise$lrt
  )
  
  # Extract raw p-values for both main effects and interactions
  raw_p_main <- sapply(lrt_results, function(x) x$`Pr(>Chisq)`[2])
  raw_p_interaction <- sapply(lrt_results, function(x) x$`Pr(>Chisq)`[3])
  
  # Apply multiple comparison correction (same as in create_lrt_table)
  corrected_main <- raw_p_main
  main_p_valid <- !is.na(raw_p_main)
  if(sum(main_p_valid) > 0) {
    corrected_main[main_p_valid] <- p.adjust(raw_p_main[main_p_valid], method = "BH")
  }
  
  corrected_interaction <- raw_p_interaction
  interaction_p_valid <- !is.na(raw_p_interaction)
  if(sum(interaction_p_valid) > 0) {
    corrected_interaction[interaction_p_valid] <- p.adjust(raw_p_interaction[interaction_p_valid], method = "BH")
  }
  
  # Map display names back to covariate_fits names
  covariate_name_map <- c(
    "Primary Role" = "role_primary",
    "Gender" = "gender", 
    "Age" = "age_years",
    "Education" = "edu_years",
    "Nursing Experience" = "rn_years",
    "Expertise" = "expertise"
  )
  
  # Initialize output list
  regression_tables <- list()
  
  # Function to create coefficient table - manual approach for GLMMadaptive compatibility
  create_coef_table <- function(model, title) {
    library(sjPlot)
    library(gt)
    
    cat("\n=== DEBUG: Creating table for", title, "===\n")
    
    # Debug: Check the model formula to see what we're working with
    cat("Model formula:", deparse(model$call$fixed), "\n")
    
    # Extract coefficient summary from GLMMadaptive model
    model_summary <- summary(model)
    cat("Model summary names:", names(model_summary), "\n")
    
    # Get the coefficient table - GLMMadaptive uses different structure
    if("coef_table" %in% names(model_summary)) {
      coef_table <- model_summary$coef_table
      cat("Using coef_table\n")
    } else if("tTable" %in% names(model_summary)) {
      coef_table <- model_summary$tTable
      cat("Using tTable\n")
    } else if("coefficients" %in% names(model_summary)) {
      coef_table <- model_summary$coefficients  
      cat("Using coefficients\n")
    } else {
      cat("No recognizable coefficient table found\n")
      cat("Available names:", names(model_summary), "\n")
      return(NULL)
    }
    
    cat("Coefficient table structure:\n")
    print(str(coef_table))
    cat("Row names:", rownames(coef_table), "\n")
    cat("Column names:", colnames(coef_table), "\n")
    
    # Check if we have the expected columns
    expected_cols <- c("Estimate", "Std.Err", "z-value", "p-value")
    available_cols <- colnames(coef_table)
    cat("Expected columns:", expected_cols, "\n")
    cat("Available columns:", available_cols, "\n")
    
    # Try to match columns flexibly
    estimate_col <- which(grepl("Estimate|estimate", available_cols))[1]
    se_col <- which(grepl("Std.Err|SE|se", available_cols))[1]
    z_col <- which(grepl("z-value|z.value|t-value|t.value", available_cols))[1]
    p_col <- which(grepl("p-value|p.value|Pr", available_cols))[1]
    
    cat("Column indices - Estimate:", estimate_col, "SE:", se_col, "z:", z_col, "p:", p_col, "\n")
    
    if(any(is.na(c(estimate_col, se_col, z_col, p_col)))) {
      cat("Missing required columns\n")
      return(NULL)
    }
    
    # Convert to data frame
    coef_df <- data.frame(
      Term = rownames(coef_table),
      Estimate = coef_table[, estimate_col],
      SE = coef_table[, se_col], 
      z_value = coef_table[, z_col],
      p_value = coef_table[, p_col],
      stringsAsFactors = FALSE
    )
    
    cat("Before filtering - Terms:", coef_df$Term, "\n")
    
    # For interaction models, we want to show interaction terms
    if(grepl("Interaction Model", title)) {
      # For interaction models, keep interaction terms (contains :) but remove main category effects
      # Remove main category effects (start with "category" but don't contain ":")
      # Keep everything else including interactions (contain ":")
      coef_df <- coef_df[!(grepl("^category", coef_df$Term) & !grepl(":", coef_df$Term)), ]
      cat("Interaction model - keeping interaction terms and main expertise effects\n")
    } else {
      # For main effect models, filter out all category terms
      coef_df <- coef_df[!grepl("^category", coef_df$Term), ]
      cat("Main effect model - removing all category terms\n")
    }
    
    cat("After filtering - Terms:", coef_df$Term, "\n")
    cat("Final row count:", nrow(coef_df), "\n")
    
    if(nrow(coef_df) == 0) {
      cat("No rows remaining after filtering\n")
      return(NULL)
    }
    
    # Keep original term names (we'll clean up prefixes later)
    coef_df$Term_clean <- coef_df$Term
    
    # Add significance stars to estimates
    coef_df$stars <- sapply(coef_df$p_value, function(p) {
      if(is.na(p)) return("")
      if(p < 0.001) return("***")
      if(p < 0.01) return("**")
      if(p < 0.05) return("*")
      return("")
    })
    
    # Combine estimate with stars
    coef_df$Est_with_stars <- paste0(sprintf("%.3f", coef_df$Estimate), 
                                     ifelse(coef_df$stars != "", 
                                            paste0("<sup>", coef_df$stars, "</sup>"), 
                                            ""))
    
    # Format p-values (unadjusted)
    coef_df$p_formatted <- sapply(coef_df$p_value, function(p) {
      if(is.na(p)) return("")
      if(p < 0.001) return("< 0.001")
      return(sprintf("%.3f", p))
    })
    
    # Create professional table using gt
    gt_table <- coef_df %>%
      dplyr::select(Term_clean, Est_with_stars, SE, z_value, p_formatted) %>%
      gt() %>%
      tab_header(title = title) %>%
      cols_label(
        Term_clean = "Predictor",
        Est_with_stars = html("<em>Est</em>"),
        SE = html("SE"),
        z_value = html("<em>z</em>"),
        p_formatted = html("<em>p</em>")
      ) %>%
      fmt_number(
        columns = c(SE, z_value),
        decimals = 3
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"), 
        locations = cells_title()
      ) %>%
      cols_align(
        align = "left",
        columns = Term_clean
      ) %>%
      cols_align(
        align = "center", 
        columns = c(Est_with_stars, SE, z_value, p_formatted)
      ) %>%
      tab_options(
        table.font.size = 12,
        heading.title.font.size = 14
      ) %>%
      tab_footnote(
        footnote = "* p < 0.05, ** p < 0.01, *** p < 0.001. P-values are unadjusted. Category main effects omitted from table.",
        locations = cells_column_labels(columns = Est_with_stars)
      )
    
    return(gt_table)
  }
  
  # Loop through each expert characteristic using the corrected p-values
  for(char_display in names(lrt_results)) {
    char_name <- covariate_name_map[char_display]
    
    if(is.na(char_name)) next
    
    # Get corrected p-values
    main_p <- corrected_main[char_display]
    interaction_p <- corrected_interaction[char_display]
    
    # Debug output
    cat("Checking", char_display, "- Main p:", main_p, "Interaction p:", interaction_p, "\n")
    
    # Check if main effect is significant
    if(!is.na(main_p) && main_p <= alpha) {
      cat("Creating main effect table for", char_display, "\n")
      main_title <- paste0("Main Effect Model: ", char_display)
      main_table <- create_coef_table(covariate_fits[[char_name]]$model_1, main_title)
      
      if(!is.null(main_table)) {
        regression_tables[[paste0(char_name, "_main")]] <- main_table
      }
    }
    
    # Check if interaction effect is significant
    if(!is.na(interaction_p) && interaction_p <= alpha) {
      cat("Creating interaction table for", char_display, "\n")
      interaction_title <- paste0("Interaction Model: ", char_display, " × Category")
      interaction_table <- create_coef_table(covariate_fits[[char_name]]$model_2, interaction_title)
      
      if(!is.null(interaction_table)) {
        regression_tables[[paste0(char_name, "_interaction")]] <- interaction_table
      }
    }
  }
  
  return(regression_tables)
}


# Create and display regression tables for significant effects
regression_tables <- create_regression_tables(lrt_table, covariate_fits)

# Display each table
for(i in seq_along(regression_tables)) {
  cat("\n", names(regression_tables)[i], ":\n")
  print(regression_tables[[i]])
}


source("Code/prob_helpers.R")

test_main_effect_prob_est(covariate_fits, "role_primary")
test_interactive_effects_prob_est(covariate_fits, var = "expertise")


main_effect_role_primary_df = main_effect_prob_est(covariate_fits, "role_primary")

interactive_effects_expertise_df = interactive_effects_prob_est(covariate_fits, var = "expertise")
agg_interactive_effects_expertise_df = interactive_effects_expertise_df %>% 
  dplyr::group_by(category) %>% 
  dplyr::reframe(mu = plogis(mean(qlogis(prob)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(category = as.character(category))
interactive_effects_expertise_df = interactive_effects_expertise_df %>% 
  dplyr::left_join(agg_interactive_effects_expertise_df, by = "category") %>% 
  dplyr::mutate(significant = (mu<ci_lb | mu > ci_ub)) %>% 
  dplyr::filter(significant) %>% 
  dplyr::mutate(category = as.character(category))

# Filter aggregated data to only include significant categories
agg_interactive_effects_expertise_df_filtered = agg_interactive_effects_expertise_df %>%
  dplyr::filter(category %in% unique(interactive_effects_expertise_df$category))

ggplot() + 
  geom_errorbarh(data = interactive_effects_expertise_df, aes(x = prob, y = covariate_level, xmin = ci_lb, xmax = ci_ub)) + 
  geom_vline(data = agg_interactive_effects_expertise_df_filtered, aes(xintercept = mu)) + 
  facet_grid(category~., scales = "free_y")
# test_interactive_effects_prob_est(covariate_fits, var = "role_primary")  
# test_interactive_effects_prob_est(covariate_fits, var = "age_years")
#sink()

# 
# # Function to create main effects plot
# create_main_effects_plot <- function(covariate_fits, var) {
#   
#   # 1. Create list this_cov_list = covariate_fits[[var]]
#   this_cov_list <- covariate_fits[[var]]
#   if(is.null(this_cov_list)) {
#     stop(paste("Variable", var, "not found in covariate_fits"))
#   }
#   
#   # 2. Identify whether var is numeric or factor variable
#   var_cols <- names(this_cov_list$data)
#   covariate_col <- setdiff(var_cols, c("essential", "category"))
#   
#   if(length(covariate_col) > 1) {
#     # For expertise with multiple columns, treat as categorical
#     is_numeric <- FALSE
#   } else {
#     is_numeric <- is.numeric(this_cov_list$data[[covariate_col[1]]])
#   }
#   
#   # 3. Check if likelihood ratio test comparing model 1 to model 0 is significant
#   lrt_results <- this_cov_list$lrt
#   main_effect_sig <- lrt_results$`Pr(>Chisq)`[2] < 0.05
#   if(is.na(main_effect_sig)) main_effect_sig <- FALSE
#   
#   if(!main_effect_sig) {
#     return(NULL)
#   }
#   
#   # 4. Create coefficient table excluding category control variable
#   library(emmeans)
#   marg_means <- emmeans(
#     covariate_fits[[var]]$model_1, 
#     setdiff(all.vars(covariate_fits[[var]]$model_1$formula), c("essential", "category"))
#     )
#                                                                                                                 
#   
#   
#   coef_summary <- summary(this_cov_list$model_1)$coefficients
#   # Filter out category and intercept terms
#   non_category_rows <- !grepl("^category|^\\(Intercept\\)", rownames(coef_summary))
#   coef_summary <- coef_summary[non_category_rows, , drop = FALSE]
#   coef_df = data.frame(coef_summary)
#   
#   # 5. Calculate the reference category deviation
#   reference_category_deviation(coef_df$Estimate,)
#   
#   
#   # Return the coefficient table for now
#   return(coef_summary)
# }
# 
# hi = create_effect_plots(covariate_fits, "expertise")
###
# # Baseline Model
# baseline <- list(formula = essential~category, wald.variables = "category", fit = NULL, D1 = NULL)
# baseline$fit = pool_glm2_multiway(formula = essential ~ category, imputed_list = implist, cluster_vars = "name")
# baseline$D1 = Wald(baseline$fit, terms = "category")
# 
# 
# # Main Effects Models
# K = length(skill_categories)
# main_effects <-  list(
#   role_primary = list( formula = essential~category+role_primary, wald.comparison = NULL, fit = NULL, D1 = NULL),
#   gender = list( formula = essential~category+gender, wald.comparison = NULL, fit = NULL, D1 = NULL),
#   age_years = list( formula = essential~category+age_years, wald.comparison = NULL, fit = NULL, D1 = NULL),
#   edu_years = list( formula = essential~category+edu_years, wald.comparison = NULL, fit = NULL, D1 = NULL),
#   rn_years = list( formula = essential ~ category+rn_years, wald.comparison = NULL, fit = NULL, D1 = NULL),
#   expertise = list( formula = essential ~ category + Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics,
#                     wald.comparison = baseline$formula,
#                     fit = NULL,
#                     D1 = NULL),
#   role_current = list( formula = essential ~ category + New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist,
#                        wald.comparison = baseline$formula,
#                        fit = NULL,
#                        D1 = NULL)
# )
# P = length(main_effects)
# covariates = names(main_effects)
# for(p in 1:P){
#   fit_kj = pool_glm2_multiway(formula =  main_effects[[covariates[p]]]$formula,
#                               imputed_list = implist,
#                               cluster_vars = c("name"))
#   main_effects[[covariates[p]]]$fit = fit_kj
#   if( !is.null(main_effects[[covariates[p]]]$wald.comparison) ){
#     wald.terms = setdiff( attr(terms( main_effects[[covariates[p]]]$formula), "term.labels"), attr(terms( baseline$formula), "term.labels"))
#     main_effects[[covariates[p]]]$D1 = Wald(fit_kj, terms = wald.terms)
#   }
# }
# 
# 
# #Create a one-way interactions template template
# one_way_interactions <-  list(
#   role_primary = list( formula = essential~category*role_primary, wald.comparison = main_effects[["role_primary"]]$formula, fit = NULL),
#   gender = list( formula = essential~category*gender, wald.comparison = main_effects[["gender"]]$formula, fit = NULL),
#   age_years = list( formula = essential~category*age_years, wald.comparison = main_effects[["age_years"]]$formula, fit = NULL),
#   edu_years = list( formula = essential~category*edu_years, wald.comparison = main_effects[["edu_years"]]$formula, fit = NULL),
#   rn_years = list( formula = essential ~ category*rn_years, wald.comparison = main_effects[["rn_years"]]$formula, fit = NULL),
#   expertise = list( formula = essential ~ category*Medical_Surgical + category*Population_Health + category*Behavioral_Health + category*Critical_Care + category*ED + category*Perioperative + category*OB + category*Pediatrics,
#                     wald.comparison =  main_effects[["expertise"]]$formula,
#                     fit = NULL),
#   role_current = list( formula = essential ~ category*New_Grad_Res_Coord_Educator + category*Clinical_Instructor_Academic + category*Preceptor + category*Simulationist,
#                        wald.comparison = main_effects[["role_current"]]$formula,
#                        fit = NULL)
# )
# for(p in 1:P){
#   fit_kj = pool_glm2_multiway(formula =  one_way_interactions[[covariates[p]]]$formula,
#                               imputed_list = implist,
#                               cluster_vars = c("name"))
#   one_way_interactions[[covariates[p]]]$fit = fit_kj
#   if( !is.null(one_way_interactions[[covariates[p]]]$wald.comparison) ){
#     wald.terms = setdiff(fit_kj$results$Term, main_effects[[covariates[[p]]]]$fit$results$Term)
#     one_way_interactions[[covariates[p]]]$D1 = Wald(fit_kj, terms = wald.terms)
#   }
# }
# 
# 
# wald_notes<-function(wald){
#   if(is.null(wald)){return("p=N/A")}
#   return(paste0("p=", ifelse(wald$p.value<.001, "<.001", signif(wald$p.value,3))))
# }
# 
# wd_current = getwd()
# setwd(file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up"))
# for(p in 1:P){
#   pvals = paste0("Model 2 vs. Model 1: ", wald_notes(main_effects[[p]]$D1),"; Model 3 vs. Model 2: ", wald_notes(one_way_interactions[[p]]$D1))
#   tab_model(
#     baseline$fit, main_effects[[p]]$fit, one_way_interactions[[p]]$fit,
#     #rm.terms = paste0("category [", unique(implist[[1]]$category), "]"),
#     dv.labels = paste0("Model ", 1:3),
#     title = paste0(toupper(covariates[p]),"<br>", "Wald tests: ",pvals),
#     file = paste0(covariates[p],"-coefficients-table.html")
#   )
# }


# age_years



### CART
# longimp = implist %>% dplyr::bind_rows() %>%
#   dplyr::left_join(implist[[1]] %>% dplyr::group_by(name) %>% dplyr::summarise() %>%  dplyr::mutate(fid = 1:n()), by = "name") %>%
#   dplyr::mutate(essential = ifelse(essential==1,"Yes","No") %>% as.factor)
# #
# # Create custom folds using the foldid column
# registerDoFuture()
#
# K=length(skill_categories)
# for(k in 1:K){
#   longimp_k = longimp %>% dplyr::filter(category == skill_categories[k]) %>% dplyr::mutate(id= 1:n())
#   grid_k = expand.grid(fid = unique(longimp_k$fid), varshort =  unique(longimp_k$varshort))
#   folds_k <- lapply(1:nrow(grid_k),
#                     function(x){longimp_k %>% dplyr::filter(fid==grid_k$fid[x], varshort == grid_k$varshort[x]) %>% purrr::pluck("id")}
#                   )
#   names(folds_k) <- paste0("Fold", 1:nrow(grid_k))
#
#   # Set up caret training using parallel CV
#   train_control_k <- trainControl(
#     method = "cv",
#     number = length(folds_k),
#     indexOut = folds_k,
#     allowParallel = TRUE
#   )
#
#   # Train the CART model
#   set.seed(456)
#   cart_model_k <- caret::train(
#     essential~role_primary+gender+age_years+edu_years+rn_years +
#       Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics +
#       New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist,
#     data = longimp_k,
#     method = "rpart",
#     trControl = train_control_k,
#     tuneLength = 10
#   )
#
#   setwd(file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up"))
#   pdf(file = paste0("CART-",stringr::str_replace_all(skill_categories[k],"\\/", "-"),".pdf"), width = 14, height = 10)
#   rpart.plot(cart_model_k$finalModel, type = 4, fallen.leaves = F, extra = "auto")
#   dev.off()
#
# }



### RF: Variable importance 
longimp = implist %>% dplyr::bind_rows() %>%
  dplyr::left_join(implist[[1]] %>% dplyr::group_by(name) %>% dplyr::summarise() %>%  dplyr::mutate(fid = 1:n()), by = "name") %>%
  dplyr::mutate(essential = ifelse(essential==1,"Yes","No") %>% as.factor)
#
# Create custom folds using the foldid column
#registerDoFuture()

K=length(skill_categories)
importance_df = pbapply::pblapply(1:K, function(k){
  
  set.seed(2025)
  longimp_k = longimp %>% dplyr::filter(category == skill_categories[k]) %>% dplyr::mutate(id= 1:n())
  
  
  fits = future.apply::future_lapply(1:M, function(m){
    fit_ranger = ranger::ranger(
      formula = essential~role_primary+gender+age_years+edu_years+rn_years +
        Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics +
        New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist,
      data = longimp_k %>% dplyr::filter(imp==m),
      probability = T, 
      importance = "permutation")
    
    return(fit_ranger)
  }, future.seed = 2025)

  
  imp_df = lapply(1:M, function(m){
    vi=importance(fits[[m]])
    imp_df = data.frame(Overall = vi, Variable = names(vi)) %>% dplyr::arrange(desc(Overall)) %>% dplyr::mutate(category = skill_categories[k], .imp = m)
    return(imp_df)
  }) %>% dplyr::bind_rows() %>% 
  dplyr::group_by(Variable) %>% 
  dplyr::reframe(Overall = mean(Overall)) %>% 
  dplyr::mutate(category = skill_categories[k])
  
  
  return(imp_df)
  
  # grid_k = expand.grid(fid = unique(longimp_k$fid), varshort =  unique(longimp_k$varshort))
  # folds_k <- lapply(1:nrow(grid_k),
  #                   function(x){longimp_k %>% dplyr::filter(fid==grid_k$fid[x], varshort == grid_k$varshort[x]) %>% purrr::pluck("id")}
  #                 )
  # names(folds_k) <- paste0("Fold", 1:nrow(grid_k))
  # 
  # # Set up caret training using parallel CV
  # train_control_k <- trainControl(
  #   method = "cv",
  #   number = length(folds_k),
  #   indexOut = folds_k,
  #   allowParallel = TRUE, 
  #   classProbs = T, 
  #   summaryFunction = twoClassSummary,
  #   savePredictions = "final"
  # )
  # 
  # # Train the CART model
  # set.seed(456)
  # rf_model_k <- caret::train(
  #   essential~role_primary+gender+age_years+edu_years+rn_years +
  #     Medical_Surgical + Population_Health + Behavioral_Health + Critical_Care + ED + Perioperative + OB + Pediatrics +
  #     New_Grad_Res_Coord_Educator + Clinical_Instructor_Academic + Preceptor + Simulationist,
  #   data = longimp_k,
  #   method = "ranger",
  #   metric = "ROC",
  #   trControl = train_control_k,
  #   tuneLength = 10, 
  #   importance = "impurity"
  # )

  #setwd(file.path(onedrive_wd, "Meeting Memos", "2025-06-11 Follow-up"))
  #pdf(file = paste0("CART-",stringr::str_replace_all(skill_categories[k],"\\/", "-"),".pdf"), width = 14, height = 10)
  #part.plot(cart_model_k$finalModel, type = 4, fallen.leaves = F, extra = "auto")
  #dev.off()

}) %>% dplyr::bind_rows()


pdf(file = "Relative Variable Importance.pdf", height = 11, width = 8.5)
for(k in 1:K){
  importance_k = importance_df %>% 
    dplyr::filter(category == skill_categories[k]) %>% 
    dplyr::mutate(Overall = Overall/(max(Overall)-min(Overall))) %>% 
    dplyr::mutate(Overall = Overall - min(Overall)) %>% 
    dplyr::mutate(Demographic_Variable = plyr::mapvalues(Variable, from = Variable, to = 1:n()) %>% ordered(levels = 1:n(), labels = Variable)  )
  
  
  plt_k = ggplot(importance_k, aes(x = Demographic_Variable, y = Overall, fill = Overall)) + 
    geom_bar(stat = "identity", show.legend = F) +
    theme_blank() +
    labs(y = "Variable Importance (Relative)", title = skill_categories[k]) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_fill_gradient(low = "black", high = "royalblue") 
  
  
  print(plt_k)
}
dev.off()

