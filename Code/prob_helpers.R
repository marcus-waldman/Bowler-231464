# Probability Estimation Helper Functions
# This file contains helper functions for calculating probability estimates and confidence intervals
# for mixed-effect models with GLMMadaptive

# Load required library
library(GLMMadaptive)

# ============================================================================
# SHARED HELPER FUNCTIONS
# ============================================================================

# Helper function to create prediction data for reference categories
create_reference_pred_data <- function(category, covariate_cols, category_levels, var, is_reference_category) {
  pred_data <- data.frame(
    category = factor(category, levels = category_levels)
  )
  
  if(var %in% c("expertise", "role_current") || length(covariate_cols) > 1) {
    # Multiple dummy variables case (expertise or role_current)
    # For reference covariate (Ambulatory Care or Clinical Educator/Practice),
    # all dummy variables should be -1 (this represents the reference level)
    for(col in covariate_cols) {
      pred_data[[col]] <- -1 #Correction made here. Changed from 0 to -1
    }
  }
  
  return(pred_data)
}

# Helper function to create non-reference prediction data
create_nonreference_pred_data <- function(category, covariate_level, covariate_cols, category_levels, var, model_data) {
  if(var %in% c("expertise", "role_current") || length(covariate_cols) > 1) {
    # Multiple dummy variables case
    pred_data <- data.frame(
      category = factor(category, levels = category_levels)
    )
    
    # Set all covariate columns to 0, then set the specific one to 1
    for(col in covariate_cols) {
      if(col == covariate_level) {
        pred_data[[col]] <- 1
      } else {
        pred_data[[col]] <- 0
      }
    }
    
  } else if(length(covariate_cols) == 1) {
    # Single covariate case
    covariate_col <- covariate_cols[1]
    covariate_data <- model_data[[covariate_col]]
    
    pred_data <- data.frame(
      category = factor(category, levels = category_levels)
    )
    
    if(is.factor(covariate_data) || is.character(covariate_data)) {
      pred_data[[covariate_col]] <- factor(covariate_level, levels = levels(covariate_data))
    } else {
      # Continuous variable - convert label to actual value
      covariate_mean <- mean(covariate_data, na.rm = TRUE)
      covariate_sd <- sd(covariate_data, na.rm = TRUE)
      
      if(covariate_level == "Low (-1 SD)") {
        pred_data[[covariate_col]] <- covariate_mean - covariate_sd
      } else if(covariate_level == "Mean") {
        pred_data[[covariate_col]] <- covariate_mean
      } else if(covariate_level == "High (+1 SD)") {
        pred_data[[covariate_col]] <- covariate_mean + covariate_sd
      }
    }
  }
  
  return(pred_data)
}

# Helper function to calculate probability estimates using design matrix approach
calculate_probability_estimates <- function(pred_data, model) {
  # Extract model formula and create design matrix
  formula_obj <- eval(model$call$fixed)
  terms_obj <- terms(formula_obj)
  formula_rhs <- reformulate(attr(terms_obj, "term.labels"))
  X <- model.matrix(formula_rhs, data = pred_data)
  
  # Get coefficients and variance-covariance matrix
  beta <- fixef(model)
  vcov_full <- vcov(model)
  beta_names <- names(beta)
  vcov_beta <- vcov_full[beta_names, beta_names, drop = FALSE]
  
  # Calculate linear predictor
  linear_pred <- X %*% beta
  
  # Calculate standard error
  var_linear_pred <- X %*% vcov_beta %*% t(X)
  se_linear_pred <- sqrt(diag(var_linear_pred))
  
  # Transform to probability scale
  prob <- plogis(linear_pred)
  
  # Calculate confidence intervals
  ci_lower_linear <- linear_pred - 1.96 * se_linear_pred
  ci_upper_linear <- linear_pred + 1.96 * se_linear_pred
  
  ci_lower <- plogis(ci_lower_linear)
  ci_upper <- plogis(ci_upper_linear)
  
  return(list(
    prob = as.numeric(prob),
    ci_lb = as.numeric(ci_lower),
    ci_ub = as.numeric(ci_upper)
  ))
}

# ============================================================================
# MAIN FUNCTIONS
# ============================================================================

# Function to create main effect probability estimates with confidence intervals
main_effect_prob_est <- function(covariate_fits, var) {
  library(GLMMadaptive)
  
  # Get the covariate list for the specified variable
  this_cov_list <- covariate_fits[[var]]
  if(is.null(this_cov_list)) {
    stop(paste("Variable", var, "not found in covariate_fits"))
  }
  
  # Get the main effects model (model_1)
  model <- this_cov_list$model_1
  
  # Extract model data and identify the covariate column(s)
  model_data <- this_cov_list$data
  var_cols <- names(model_data)
  covariate_cols <- setdiff(var_cols, c("essential", "category", "varshort", "name"))
  
  # Handle different variable types
  if(length(covariate_cols) == 1) {
    # Single covariate (continuous or factor)
    covariate_col <- covariate_cols[1]
    covariate_data <- model_data[[covariate_col]]
    
    if(is.factor(covariate_data) || is.character(covariate_data)) {
      # Factor variable - get unique levels
      unique_levels <- unique(covariate_data)
      unique_levels <- unique_levels[!is.na(unique_levels)]
      
      # Create prediction data for each level
      pred_data_list <- lapply(unique_levels, function(level) {
        # Get the actual levels from the model data
        category_levels <- levels(model_data$category)
        
        # Use the first level as reference (or find "Fundamental" if it exists)
        reference_level <- if("Fundamental" %in% category_levels) {
          "Fundamental"
        } else {
          category_levels[1]
        }
        
        pred_data <- data.frame(
          category = factor(reference_level, levels = category_levels),
          stringsAsFactors = FALSE
        )
        pred_data[[covariate_col]] <- factor(level, levels = levels(covariate_data))
        return(pred_data)
      })
      names(pred_data_list) <- unique_levels
      
    } else {
      # Continuous variable - use mean and +/- 1 SD
      covariate_mean <- mean(covariate_data, na.rm = TRUE)
      covariate_sd <- sd(covariate_data, na.rm = TRUE)
      
      pred_values <- c(covariate_mean - covariate_sd, covariate_mean, covariate_mean + covariate_sd)
      pred_labels <- c("Low (-1 SD)", "Mean", "High (+1 SD)")
      
      pred_data_list <- lapply(seq_along(pred_values), function(i) {
        # Get the actual levels from the model data
        category_levels <- levels(model_data$category)
        
        # Use the first level as reference (or find "Fundamental" if it exists)
        reference_level <- if("Fundamental" %in% category_levels) {
          "Fundamental"
        } else {
          category_levels[1]
        }
        
        pred_data <- data.frame(
          category = factor(reference_level, levels = category_levels)
        )
        pred_data[[covariate_col]] <- pred_values[i]
        return(pred_data)
      })
      names(pred_data_list) <- pred_labels
    }
    
  } else {
    # Multiple covariates (e.g., expertise variables)
    # Create binary combinations for each expertise area
    pred_data_list <- list()
    
    for(covariate_col in covariate_cols) {
      # Get the actual levels from the model data
      category_levels <- levels(model_data$category)
      
      # Use the first level as reference (or find "Fundamental" if it exists)
      reference_level <- if("Fundamental" %in% category_levels) {
        "Fundamental"
      } else {
        category_levels[1]
      }
      
      # Create prediction for "has this expertise"
      pred_data <- data.frame(
        category = factor(reference_level, levels = category_levels)
      )
      
      # Set all other expertise variables to 0 and this one to 1
      for(other_col in covariate_cols) {
        if(other_col == covariate_col) {
          pred_data[[other_col]] <- 1
        } else {
          pred_data[[other_col]] <- 0
        }
      }
      
      pred_data_list[[covariate_col]] <- pred_data
    }
  }
  
  # Calculate predictions and confidence intervals for each level/condition
  results_list <- lapply(names(pred_data_list), function(level_name) {
    pred_data <- pred_data_list[[level_name]]
    
    # Use helper function for calculations
    estimates <- calculate_probability_estimates(pred_data, model)
    
    # Return results
    data.frame(
      level = level_name,
      prob = estimates$prob,
      ci_lb = estimates$ci_lb,
      ci_ub = estimates$ci_ub,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine results
  result_df <- do.call(rbind, results_list)
  rownames(result_df) <- NULL
  
  # Handle reference category for sum contrasts (multiple covariate case only)
  if(length(covariate_cols) > 1) {
    # For multiple covariates like expertise, we need to add reference category
    # Get all covariate levels used in the model
    all_covariate_levels <- covariate_cols
    
    # Add reference category ("Ambulatory Care" for expertise)
    if(var == "expertise") {
      reference_level_name <- "Ambulatory Care"
    } else if(var == "role_current") {
      reference_level_name <- "Clinical Educator/Practice"
    } else {
      # Generic case - look for a reference level in the data
      reference_level_name <- "Reference"  # fallback
    }
    
    # Calculate reference category coefficients using sum constraint
    # For sum contrasts, reference coefficient = -sum(other coefficients)
    beta <- fixef(model)
    covariate_beta_values <- beta[covariate_cols]  # Get coefficients for each expertise area
    reference_coeff <- -sum(covariate_beta_values)
    
    
    # Calculate standard error for reference coefficient
    vcov_full <- vcov(model)
    beta_names <- names(beta)
    vcov_beta <- vcov_full[beta_names, beta_names, drop = FALSE]
    
    # Create contrast vector: -1 for each covariate coefficient, 0 for others
    contrast_vector <- rep(0, length(beta))
    names(contrast_vector) <- beta_names
    contrast_vector[covariate_cols] <- -1
    
    # Calculate variance of reference coefficient
    reference_var <- t(contrast_vector) %*% vcov_beta %*% contrast_vector
    reference_se <- sqrt(as.numeric(reference_var))
    
    
    # Calculate reference category prediction at same category level as others
    category_levels <- levels(model_data$category)
    reference_category <- if("Fundamental" %in% category_levels) {
      "Fundamental"
    } else {
      category_levels[1]
    }
    
    # Get intercept and category coefficient for this reference level
    intercept_coeff <- beta["(Intercept)"]
    category_coeff_name <- paste0("category", reference_category)
    category_coeff <- if(category_coeff_name %in% names(beta)) beta[category_coeff_name] else 0
    
    # Linear predictor for reference category
    reference_linear_pred <- intercept_coeff + category_coeff + reference_coeff
    
    # Transform to probability
    reference_prob <- plogis(reference_linear_pred)
    
    # Calculate confidence intervals
    reference_ci_lower_linear <- reference_linear_pred - 1.96 * reference_se
    reference_ci_upper_linear <- reference_linear_pred + 1.96 * reference_se
    
    reference_ci_lower <- plogis(reference_ci_lower_linear)
    reference_ci_upper <- plogis(reference_ci_upper_linear)
    
    
    # Add reference category row
    reference_row <- data.frame(
      level = reference_level_name,
      prob = as.numeric(reference_prob),
      ci_lb = as.numeric(reference_ci_lower),
      ci_ub = as.numeric(reference_ci_upper),
      stringsAsFactors = FALSE
    )
    
    result_df <- rbind(result_df, reference_row)
  }
  
  return(result_df)
}

# Function to create interaction effect probability estimates with confidence intervals
interactive_effects_prob_est <- function(covariate_fits, var) {
  library(GLMMadaptive)
  
  # Get the covariate list for the specified variable
  this_cov_list <- covariate_fits[[var]]
  if(is.null(this_cov_list)) {
    stop(paste("Variable", var, "not found in covariate_fits"))
  }
  
  # Extract model data to identify levels
  model_data <- this_cov_list$data
  var_cols <- names(model_data)
  covariate_cols <- setdiff(var_cols, c("essential", "category", "varshort", "name"))
  
  # Get category levels
  category_levels <- levels(model_data$category)
  
  # Get interaction model (model_2) for calculations
  model <- this_cov_list$model_2
  
  # Handle special cases for expertise and role_current (manually dummy-coded variables)
  if(var == "expertise") {
    # For expertise, only use the dummy variables (exclude reference "Ambulatory Care")
    covariate_levels <- covariate_cols  # Don't include reference category
    
  } else if(var == "role_current") {
    # For role_current, only use the dummy variables (exclude reference "Clinical Educator/Practice")  
    covariate_levels <- covariate_cols  # Don't include reference category
    
  } else if(length(covariate_cols) == 1) {
    # Single covariate (standard case)
    covariate_col <- covariate_cols[1]
    covariate_data <- model_data[[covariate_col]]
    
    if(is.factor(covariate_data) || is.character(covariate_data)) {
      # Get levels that have coefficients (exclude reference level)
      beta <- fixef(model)
      covariate_coeffs <- names(beta)[grepl(paste0("^", covariate_col), names(beta))]
      if(length(covariate_coeffs) > 0) {
        covariate_levels <- gsub(paste0("^", covariate_col), "", covariate_coeffs)
      } else {
        covariate_levels <- levels(covariate_data)[1]  # fallback
      }
    } else {
      # For continuous variables, use meaningful labels
      covariate_mean <- mean(covariate_data, na.rm = TRUE)
      covariate_sd <- sd(covariate_data, na.rm = TRUE)
      covariate_levels <- c("Low (-1 SD)", "Mean", "High (+1 SD)")
    }
  } else {
    # Multiple covariate columns (other cases)
    covariate_levels <- covariate_cols
  }
  
  # Create all combinations of category and covariate levels
  combinations <- expand.grid(
    category = category_levels,
    covariate_level = covariate_levels,
    stringsAsFactors = FALSE
  )
  
  # Initialize result dataframe
  result_df <- data.frame(
    category = combinations$category,
    covariate_level = combinations$covariate_level,
    prob = NA,
    ci_lb = NA,
    ci_ub = NA,
    stringsAsFactors = FALSE
  )
  
  
  # Extract coefficients and vcov matrix
  beta <- fixef(model)
  vcov_full <- vcov(model)
  beta_names <- names(beta)
  vcov_beta <- vcov_full[beta_names, beta_names, drop = FALSE]
  
  # First, add reference category rows to result_df if needed
  if(var == "expertise") {
    # Add rows for "Ambulatory Care" reference category
    reference_combinations <- data.frame(
      category = category_levels,
      covariate_level = rep("Ambulatory Care", length(category_levels)),
      stringsAsFactors = FALSE
    )
    reference_df <- data.frame(
      category = reference_combinations$category,
      covariate_level = reference_combinations$covariate_level,
      prob = NA,
      ci_lb = NA,
      ci_ub = NA,
      stringsAsFactors = FALSE
    )
    result_df <- rbind(result_df, reference_df)
    
  } else if(var == "role_current") {
    # Add rows for "Clinical Educator/Practice" reference category
    reference_combinations <- data.frame(
      category = category_levels,
      covariate_level = rep("Clinical Educator/Practice", length(category_levels)),
      stringsAsFactors = FALSE
    )
    reference_df <- data.frame(
      category = reference_combinations$category,
      covariate_level = reference_combinations$covariate_level,
      prob = NA,
      ci_lb = NA,
      ci_ub = NA,
      stringsAsFactors = FALSE
    )
    result_df <- rbind(result_df, reference_df)
    
  } else if(length(covariate_cols) == 1) {
    # For single factor variables, add reference category if it exists
    covariate_col <- covariate_cols[1]
    covariate_data <- model_data[[covariate_col]]
    
    if(is.factor(covariate_data)) {
      all_levels <- levels(covariate_data)
      # Find reference level (not in covariate_levels)
      reference_level <- setdiff(all_levels, covariate_levels)
      
      if(length(reference_level) > 0) {
        reference_combinations <- data.frame(
          category = category_levels,
          covariate_level = rep(reference_level[1], length(category_levels)),
          stringsAsFactors = FALSE
        )
        reference_df <- data.frame(
          category = reference_combinations$category,
          covariate_level = reference_combinations$covariate_level,
          prob = NA,
          ci_lb = NA,
          ci_ub = NA,
          stringsAsFactors = FALSE
        )
        result_df <- rbind(result_df, reference_df)
      }
    }
  }
  
  
  # Calculate probability estimates for each combination
  for(i in 1:nrow(result_df)) {
    
    # Check if this is a reference category that needs special handling
    is_reference <- FALSE
    if(var == "expertise" && result_df$covariate_level[i] == "Ambulatory Care") {
      is_reference <- TRUE
    } else if(var == "role_current" && result_df$covariate_level[i] == "Clinical Educator/Practice") {
      is_reference <- TRUE
    } else if(length(covariate_cols) == 1) {
      covariate_col <- covariate_cols[1]
      covariate_data <- model_data[[covariate_col]]
      if(is.factor(covariate_data)) {
        if(!(result_df$covariate_level[i] %in% covariate_levels)) {
          is_reference <- TRUE
        }
      }
    }
    
    if(is_reference) {
      # Use helper function for reference categories
      category_coeff_name <- paste0("category", result_df$category[i])
      is_reference_category <- !(category_coeff_name %in% names(beta))
      
      pred_data <- create_reference_pred_data(
        category = result_df$category[i], 
        covariate_cols = covariate_cols,
        category_levels = category_levels,
        var = var,
        is_reference_category = is_reference_category
      )
      
      # Handle single covariate case for reference categories
      if(length(covariate_cols) == 1 && is_reference) {
        covariate_col <- covariate_cols[1]
        covariate_data <- model_data[[covariate_col]]
        pred_data <- data.frame(
          category = factor(result_df$category[i], levels = category_levels)
        )
        # Use reference level from factor levels
        all_levels <- levels(covariate_data)
        covariate_coeffs <- names(beta)[grepl(paste0("^", covariate_col), names(beta))]
        levels_with_coeffs <- gsub(paste0("^", covariate_col), "", covariate_coeffs)
        reference_level <- setdiff(all_levels, levels_with_coeffs)[1]
        pred_data[[covariate_col]] <- factor(reference_level, levels = all_levels)
      }
      
    } else {
      # Use helper function for non-reference categories
      pred_data <- create_nonreference_pred_data(
        category = result_df$category[i],
        covariate_level = result_df$covariate_level[i],
        covariate_cols = covariate_cols,
        category_levels = category_levels,
        var = var,
        model_data = model_data
      )
    }
    
    # Use helper function for probability calculations
    estimates <- calculate_probability_estimates(pred_data, model)
    
    # Update result dataframe
    result_df$prob[i] <- estimates$prob
    result_df$ci_lb[i] <- estimates$ci_lb
    result_df$ci_ub[i] <- estimates$ci_ub
  }
  
  return(result_df)
}

# Test function to verify our probability calculations match predict() for ALL combinations
test_interactive_effects_prob_est <- function(covariate_fits, var) {
  library(GLMMadaptive)
  
  cat("\n=== Testing interactive_effects_prob_est against predict() ===\n")
  cat("Variable:", var, "\n")
  
  # Get our calculated results
  our_results <- interactive_effects_prob_est(covariate_fits, var)
  
  # Get the model and data
  this_cov_list <- covariate_fits[[var]]
  model <- this_cov_list$model_2
  model_data <- this_cov_list$data
  var_cols <- names(model_data)
  covariate_cols <- setdiff(var_cols, c("essential", "category", "varshort", "name"))
  
  cat("Testing", nrow(our_results), "combinations...\n")
  
  all_match <- TRUE
  n_tested <- 0
  n_problems <- 0
  
  for(i in 1:nrow(our_results)) {
    test_row <- our_results[i, ]
    
    # Skip reference categories for prediction comparison since predict() handles them automatically
    is_reference <- FALSE
    if(var == "expertise" && test_row$covariate_level == "Ambulatory Care") {
      is_reference <- TRUE
    } else if(var == "role_current" && test_row$covariate_level == "Clinical Educator/Practice") {
      is_reference <- TRUE
    } else if(length(covariate_cols) == 1) {
      covariate_col <- covariate_cols[1]
      covariate_data <- model_data[[covariate_col]]
      if(is.factor(covariate_data)) {
        # Get levels that have coefficients
        beta <- fixef(model)
        covariate_coeffs <- names(beta)[grepl(paste0("^", covariate_col), names(beta))]
        if(length(covariate_coeffs) > 0) {
          levels_with_coeffs <- gsub(paste0("^", covariate_col), "", covariate_coeffs)
          if(!(test_row$covariate_level %in% levels_with_coeffs)) {
            is_reference <- TRUE
          }
        }
      }
    }
    
    if(is_reference) {
      # Use helper function for reference categories (same logic as main function)
      beta <- fixef(model)
      category_coeff_name <- paste0("category", test_row$category)
      is_reference_category <- !(category_coeff_name %in% names(beta))
      
      pred_data <- create_reference_pred_data(
        category = test_row$category,
        covariate_cols = covariate_cols,
        category_levels = levels(model_data$category),
        var = var,
        is_reference_category = is_reference_category
      )
      
      # Handle single covariate case for reference categories
      if(length(covariate_cols) == 1) {
        covariate_col <- covariate_cols[1]
        covariate_data <- model_data[[covariate_col]]
        pred_data <- data.frame(
          category = factor(test_row$category, levels = levels(model_data$category))
        )
        # Use reference level from factor levels
        all_levels <- levels(covariate_data)
        covariate_coeffs <- names(beta)[grepl(paste0("^", covariate_col), names(beta))]
        levels_with_coeffs <- gsub(paste0("^", covariate_col), "", covariate_coeffs)
        reference_level <- setdiff(all_levels, levels_with_coeffs)[1]
        pred_data[[covariate_col]] <- factor(reference_level, levels = all_levels)
      }
    } else {
      # Use helper function for non-reference categories
      pred_data <- create_nonreference_pred_data(
        category = test_row$category,
        covariate_level = test_row$covariate_level,
        covariate_cols = covariate_cols,
        category_levels = levels(model_data$category),
        var = var,
        model_data = model_data
      )
    }
    
    # Get prediction from model
    model_pred <- predict(model, newdata = pred_data)
    
    # Debug: Show predict() calculation details for reference cases
    # if(is_reference) {
    #   cat("  predict() Debug for", test_row$category, "x", test_row$covariate_level, ":\n")
    #   cat("    Model prediction:", model_pred, "\n")
    #   cat("    Prediction data:\n")
    #   print(pred_data)
    # }
    
    # Compare
    our_prob <- test_row$prob
    diff <- abs(our_prob - model_pred)
    
    n_tested <- n_tested + 1
    
    if(diff > 1e-6) {
      all_match <- FALSE
      n_problems <- n_problems + 1
      cat(sprintf("MISMATCH Row %d: %s x %s\n", i, test_row$category, test_row$covariate_level))
      cat(sprintf("  Our calculation: %.6f\n", our_prob))
      cat(sprintf("  predict():       %.6f\n", model_pred))
      cat(sprintf("  Difference:      %.6f\n", diff))
      cat("  Prediction data used:\n")
      print(pred_data)
      cat("\n")
    }
  }
  
  if(all_match) {
    cat(sprintf("SUCCESS: All %d predictions match! ✓\n", n_tested))
  } else {
    cat(sprintf("ERROR: %d out of %d predictions do not match. Check calculations.\n", n_problems, n_tested))
  }
  
  return(invisible(all_match))
}

# Test function to verify main effect probability calculations match predict()
test_main_effect_prob_est <- function(covariate_fits, var) {
  library(GLMMadaptive)
  
  cat("\n=== Testing main_effect_prob_est against predict() ===\n")
  cat("Variable:", var, "\n")
  
  # Get our calculated results
  our_results <- main_effect_prob_est(covariate_fits, var)
  
  # Get the model and data
  this_cov_list <- covariate_fits[[var]]
  model <- this_cov_list$model_1  # Main effects model
  model_data <- this_cov_list$data
  var_cols <- names(model_data)
  covariate_cols <- setdiff(var_cols, c("essential", "category", "varshort", "name"))
  
  cat("Testing", nrow(our_results), "main effect predictions...\n")
  
  all_match <- TRUE
  n_tested <- 0
  n_problems <- 0
  
  for(i in 1:nrow(our_results)) {
    test_row <- our_results[i, ]
    
    # Create prediction data based on the level
    if(var %in% c("expertise", "role_current") || length(covariate_cols) > 1) {
      # Multiple covariate case (expertise or role_current)
      category_levels <- levels(model_data$category)
      reference_category <- if("Fundamental" %in% category_levels) {
        "Fundamental"
      } else {
        category_levels[1]
      }
      
      if(test_row$level == "Ambulatory Care" && var == "expertise") {
        # Reference level for expertise
        pred_data <- data.frame(
          category = factor(reference_category, levels = category_levels)
        )
        for(col in covariate_cols) {
          pred_data[[col]] <- -1
        }
      } else if(test_row$level == "Clinical Educator/Practice" && var == "role_current") {
        # Reference level for role_current
        pred_data <- data.frame(
          category = factor(reference_category, levels = category_levels)
        )
        for(col in covariate_cols) {
          pred_data[[col]] <- -1
        }
      } else {
        # Non-reference level - set the specific covariate to 1, others to 0
        pred_data <- data.frame(
          category = factor(reference_category, levels = category_levels)
        )
        for(col in covariate_cols) {
          if(col == test_row$level) {
            pred_data[[col]] <- 1
          } else {
            pred_data[[col]] <- 0
          }
        }
      }
      
    } else if(length(covariate_cols) == 1) {
      # Single covariate case
      covariate_col <- covariate_cols[1]
      covariate_data <- model_data[[covariate_col]]
      category_levels <- levels(model_data$category)
      reference_category <- if("Fundamental" %in% category_levels) {
        "Fundamental"
      } else {
        category_levels[1]
      }
      
      pred_data <- data.frame(
        category = factor(reference_category, levels = category_levels)
      )
      
      if(is.factor(covariate_data) || is.character(covariate_data)) {
        # Factor variable
        pred_data[[covariate_col]] <- factor(test_row$level, levels = levels(covariate_data))
      } else {
        # Continuous variable - convert label to actual value
        covariate_mean <- mean(covariate_data, na.rm = TRUE)
        covariate_sd <- sd(covariate_data, na.rm = TRUE)
        
        if(test_row$level == "Low (-1 SD)") {
          pred_data[[covariate_col]] <- covariate_mean - covariate_sd
        } else if(test_row$level == "Mean") {
          pred_data[[covariate_col]] <- covariate_mean
        } else if(test_row$level == "High (+1 SD)") {
          pred_data[[covariate_col]] <- covariate_mean + covariate_sd
        }
      }
    }
    
    # Get prediction from model
    model_pred <- predict(model, newdata = pred_data)
    
    # Compare
    our_prob <- test_row$prob
    diff <- abs(our_prob - model_pred)
    
    n_tested <- n_tested + 1
    
    if(diff > 1e-6) {
      all_match <- FALSE
      n_problems <- n_problems + 1
      cat(sprintf("MISMATCH Row %d: %s\n", i, test_row$level))
      cat(sprintf("  Our calculation: %.6f\n", our_prob))
      cat(sprintf("  predict():       %.6f\n", model_pred))
      cat(sprintf("  Difference:      %.6f\n", diff))
      cat("  Prediction data used:\n")
      print(pred_data)
      cat("\n")
    }
  }
  
  if(all_match) {
    cat(sprintf("SUCCESS: All %d main effect predictions match! ✓\n", n_tested))
  } else {
    cat(sprintf("ERROR: %d out of %d predictions do not match. Check calculations.\n", n_problems, n_tested))
  }
  
  return(invisible(all_match))
}