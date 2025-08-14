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

lrt_table %>% gtsave(filename = file.path(tables_wd, "lrt_table.html"))


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
    
    # Keep all terms including category main effects
    cat("Keeping all terms including category main effects\n")
    
    cat("After filtering - Terms:", coef_df$Term, "\n")
    cat("Final row count:", nrow(coef_df), "\n")
    
    if(nrow(coef_df) == 0) {
      cat("No rows remaining after filtering\n")
      return(NULL)
    }
    
    # Clean up term names by removing variable prefixes
    coef_df$Term_clean <- coef_df$Term
    
    # Remove common variable prefixes
    coef_df$Term_clean <- gsub("^role_primary", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^gender", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^age_years_c", "Age (centered)", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^edu_years_c", "Education Years (centered)", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^rn_years_c", "Nursing Years (centered)", coef_df$Term_clean)
    
    # Clean up interaction terms
    coef_df$Term_clean <- gsub("role_primary", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("gender", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^category", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub(":", " × ", coef_df$Term_clean)
    
    # Replace underscores with spaces for expertise and role variables
    coef_df$Term_clean <- gsub("_", " ", coef_df$Term_clean)
    
    # Remove any leading/trailing whitespace or special characters
    coef_df$Term_clean <- trimws(coef_df$Term_clean)
    
    # Add significance stars to estimates
    coef_df$stars <- sapply(coef_df$p_value, function(p) {
      if(is.na(p)) return("")
      if(p < 0.001) return("***")
      if(p < 0.01) return("**")
      if(p < 0.05) return("*")
      return("")
    })
    
    # Combine estimate with stars
    coef_df$Est_with_stars <- paste0(sprintf("%.3f", coef_df$Estimate), coef_df$stars)
    
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
        heading.title.font.size = 14,
        table.font.names = "Times New Roman"
      ) %>%
      tab_footnote(
        footnote = "* p < 0.05, ** p < 0.01, *** p < 0.001. P-values are unadjusted.",
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
regression_tables[[1]] %>%  gtsave(filename = file.path(tables_wd, "role_primary_main_effect_coefficients.html"))
regression_tables[[2]] %>%  gtsave(filename = file.path(tables_wd, "expertise_coefficients.html"))


source("Code/table_plot_helpers.R")


# Create model comparison tables for each characteristic
characteristics <- names(covariate_fits)

# Define table references and titles for each characteristic
table_info <- list(
  role_primary = list(
    reference = "Supplementary Table 1",
    title = "Primary Role: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  ),
  gender = list(
    reference = "Supplementary Table 2", 
    title = "Gender: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  ),
  age_years = list(
    reference = "Supplementary Table 3",
    title = "Age: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  ),
  edu_years = list(
    reference = "Supplementary Table 4",
    title = "Education Years: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  ),
  rn_years = list(
    reference = "Supplementary Table 5",
    title = "Nursing Experience: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  ),
  expertise = list(
    reference = "Supplementary Table 6",
    title = "Expertise: Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models."
  )
)

# Loop through each characteristic and create tables
for(char in characteristics) {
  cat("Creating table for:", char, "\n")
  
  # Get table info or use defaults
  if(char %in% names(table_info)) {
    ref <- table_info[[char]]$reference
    title <- table_info[[char]]$title
  } else {
    ref <- paste("Supplementary Table", which(characteristics == char))
    title <- paste0(stringr::str_to_title(gsub("_", " ", char)), 
                   ": Baseline (Model 1), Main Effects (Model 2), and Interactions (Model 3) Models.")
  }
  
  # Create the table
  comparison_table <- create_model_comparison_table(
    covariate_fits, 
    characteristic = char, 
    table_reference = ref,
    table_title = title
  )
  
  # Save the table
  filename <- paste0(char, "_model_building.html")
  gtsave(comparison_table, filename = file.path(tables_wd, "model-building", filename))
  
  cat("Saved table for", char, "as", filename, "\n")
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
  dplyr::mutate(category = as.character(category)) %>%
  dplyr::mutate(category_clean = gsub("^category", "", category)) %>% 
  dplyr::filter(p_value < .05)

# Filter aggregated data to only include significant categories
agg_interactive_effects_expertise_df_filtered = agg_interactive_effects_expertise_df %>%
  dplyr::filter(category %in% unique(interactive_effects_expertise_df$category)) %>%
  dplyr::mutate(category_clean = gsub("^category", "", category))

# Prepare data for bar plot including grand mean
plot_data <- interactive_effects_expertise_df %>%
  dplyr::select(category_clean, covariate_level, prob, ci_lb, ci_ub) %>%
  dplyr::mutate(
    covariate_level = gsub("_", " ", covariate_level),  # Replace underscores with spaces
    bar_type = "Expertise Level"
  )

# Add grand mean data as separate bars
grand_mean_data <- agg_interactive_effects_expertise_df_filtered %>%
  dplyr::select(category_clean, mu) %>%
  dplyr::mutate(
    covariate_level = "Mean",
    prob = mu,
    ci_lb = mu,  # No error bars for grand mean
    ci_ub = mu,
    bar_type = "Grand Mean"
  ) %>%
  dplyr::select(category_clean, covariate_level, prob, ci_lb, ci_ub, bar_type)

# Combine the data
combined_plot_data <- dplyr::bind_rows(plot_data, grand_mean_data)

# Reorder factor levels to put Mean first
combined_plot_data$covariate_level <- factor(
  combined_plot_data$covariate_level,
  levels = c("Mean", sort(unique(plot_data$covariate_level)))
)

# Create custom labels with bold formatting for "Mean"
level_labels <- levels(combined_plot_data$covariate_level)
bold_labels <- ifelse(level_labels == "Mean", "**Overall**", level_labels)

plot_interact = 
  ggplot(combined_plot_data, aes(x = covariate_level, y = prob, fill = bar_type)) + 
  geom_col(alpha = 0.7) +
  # geom_errorbar(
  #   data = combined_plot_data[combined_plot_data$bar_type == "Expertise Level", ],
  #   aes(ymin = ci_lb, ymax = ci_ub), 
  #   width = 0.25,
  #   color = "grey50"
  # ) +
  geom_text(
    data = combined_plot_data[combined_plot_data$bar_type == "Expertise Level", ],
    aes(label = gsub("^0\\.", ".", sprintf("%.2f", prob))),
    vjust = 1.2,
    size = 3,
    family = "Times New Roman",
    fontface = "bold",
    color = "black"
  ) +
  geom_text(
    data = combined_plot_data[combined_plot_data$bar_type == "Grand Mean", ],
    aes(label = gsub("^0\\.", ".", sprintf("%.2f", prob))),
    vjust = 1.2,
    size = 3,
    family = "Times New Roman",
    fontface = "bold",
    color = "white"
  ) +
  facet_wrap(~ category_clean, scales = "free_x") +
  scale_x_discrete(drop = FALSE, labels = bold_labels) +
  scale_fill_manual(
    values = c("Expertise Level" = "steelblue", "Grand Mean" = "black"),
    guide = "none"
  ) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Expertise-by-Skill Category Interactions*",
    caption = "*Only significant interaction terms shown in chart"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    strip.text = element_text(size = 10, face = "bold", family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", hjust = 0),
    plot.subtitle = element_text(family = "Times New Roman"),
    plot.caption = element_text(family = "Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
  ) + 
  geom_hline(yintercept = 0, size = 1, col = "grey50")

plot_interact

ggsave(plot_interact, filename = file.path(figures_wd, "skill-endorsement-probabilities.png"), height = 5.5, width = 5.5, bg = "white")




# Main effects plot for role_primary
# Prepare data for main effects bar plot
main_plot_data <- main_effect_role_primary_df %>%
  dplyr::mutate(
    level = gsub("_", " ", level),  # Replace underscores with spaces
    bar_type = "Role Level"
  )

# Create custom labels with bold formatting for reference level
main_level_labels <- unique(main_plot_data$level)
main_bold_labels <- ifelse(main_level_labels == "Clinical Educator/Practice", "**Clinical Educator/Practice**", main_level_labels)

plot_main_effects = 
  ggplot(main_plot_data, aes(x = prob, y = level)) + 
  geom_col(alpha = 0.7, fill = "steelblue") +
  geom_errorbarh(
    aes(xmin = ci_lb, xmax = ci_ub), 
    height = 0.25,
    color = "grey50"
  ) +
  geom_text(
    aes(label = gsub("^0\\.", ".", sprintf("%.2f", prob))),
    hjust = 1.2,
    size = 3,
    family = "Times New Roman",
    fontface = "bold",
    color = "black"
  ) +
  scale_y_discrete(labels = main_bold_labels) +
  labs(
    x = element_blank(),
    y = element_blank(),
    subtitle = "Primary Role Main Effects"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.y = ggtext::element_markdown(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", hjust = 0),
    plot.subtitle = element_text(family = "Times New Roman"),
    plot.caption = element_text(family = "Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
  ) +
  geom_vline(xintercept = 0, size = 2, col = "grey50") +
  coord_cartesian(expand = FALSE)

plot_main_effects

ggsave(plot_main_effects, filename = file.path(figures_wd, "primary-role-main-effects.png"), height = 4, width = 6, bg = "white")

# Create combined plot with plot_main_effects on top (1/3) and plot_interact on bottom (2/3)
plot_combined <- plot_main_effects / plot_interact + 
  plot_layout(heights = c(1, 2)) &
  theme(plot.margin = margin(2, 2, 2, 2, "pt"))

plot_combined

ggsave(plot_combined, filename = file.path(figures_wd, "combined-effects-plot.png"), height = 8, width = 8, bg = "white")


