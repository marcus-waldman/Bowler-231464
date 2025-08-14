# Table and Plot Helper Functions
# This file contains helper functions for creating tables and plots from model results

# Load required libraries
library(gt)
library(dplyr)
library(GLMMadaptive)

# =============================================================================
# MODEL COMPARISON TABLE FUNCTIONS
# =============================================================================

#' Create comprehensive model comparison table for a given characteristic
#' 
#' This function takes covariate_fits and creates a single table showing
#' coefficients, model fit statistics for all three models (model_0, model_1, model_2)
#' 
#' @param covariate_fits List containing model fits for different characteristics
#' @param characteristic Character string specifying which characteristic to analyze
#'                      (e.g., "role_primary", "expertise", "gender", etc.)
#' @param table_reference Character string for table reference (appears above title, bold)
#' @param table_title Character string for table title (appears below reference, italic)
#' @return A gt table object with comprehensive model comparison
create_model_comparison_table <- function(covariate_fits, characteristic, table_reference = NULL, table_title = NULL) {
  
  # Check if characteristic exists in covariate_fits
  if (!(characteristic %in% names(covariate_fits))) {
    stop(paste("Characteristic", characteristic, "not found in covariate_fits"))
  }
  
  # Extract the characteristic data
  char_data <- covariate_fits[[characteristic]]
  
  # Extract the three models
  model_0 <- char_data$model_0  # Baseline model
  model_1 <- char_data$model_1  # Main effect model
  model_2 <- char_data$model_2  # Interaction model
  
  # Function to extract model information
  extract_model_info <- function(model, model_name) {
    # Get coefficient summary
    model_summary <- summary(model)
    
    # Extract coefficient table
    if("coef_table" %in% names(model_summary)) {
      coef_table <- model_summary$coef_table
    } else if("tTable" %in% names(model_summary)) {
      coef_table <- model_summary$tTable
    } else if("coefficients" %in% names(model_summary)) {
      coef_table <- model_summary$coefficients
    } else {
      stop(paste("Cannot find coefficient table for", model_name))
    }
    
    # Create coefficient dataframe
    coef_df <- data.frame(
      Term = rownames(coef_table),
      Estimate = coef_table[, grep("Estimate|estimate", colnames(coef_table))[1]],
      SE = coef_table[, grep("Std.Err|SE|se", colnames(coef_table))[1]],
      z_value = coef_table[, grep("z-value|z.value|t-value|t.value", colnames(coef_table))[1]],
      p_value = coef_table[, grep("p-value|p.value|Pr", colnames(coef_table))[1]],
      Model = model_name,
      stringsAsFactors = FALSE
    )
    
    # Clean up term names
    coef_df$Term_clean <- coef_df$Term
    coef_df$Term_clean <- gsub("^role_primary", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^gender", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^category", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^age_years_c", "Age (centered)", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^edu_years_c", "Education Years (centered)", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("^rn_years_c", "Nursing Years (centered)", coef_df$Term_clean)
    
    # Clean up interaction terms - remove variable prefixes from both sides of interactions
    coef_df$Term_clean <- gsub("role_primary", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("gender", "", coef_df$Term_clean)
    coef_df$Term_clean <- gsub("category", "", coef_df$Term_clean)
    
    # Replace symbols and clean up
    coef_df$Term_clean <- gsub("_", " ", coef_df$Term_clean)
    coef_df$Term_clean <- gsub(":", " × ", coef_df$Term_clean)
    coef_df$Term_clean <- trimws(coef_df$Term_clean)
    
    # Add significance stars
    coef_df$stars <- sapply(coef_df$p_value, function(p) {
      if(is.na(p)) return("")
      if(p < 0.001) return("***")
      if(p < 0.01) return("**")
      if(p < 0.05) return("*")
      return("")
    })
    
    # Combine estimate with stars and SE in parentheses below
    coef_df$Est_with_stars <- paste0(sprintf("%.3f", coef_df$Estimate), coef_df$stars, 
                                     "\n(", sprintf("%.3f", coef_df$SE), ")")
    
    # Format p-values
    coef_df$p_formatted <- sapply(coef_df$p_value, function(p) {
      if(is.na(p)) return("")
      if(p < 0.001) return("< 0.001")
      return(sprintf("%.3f", p))
    })
    
    # Get model fit statistics
    model_deviance <- round(-2 * logLik(model), 2)
    model_df <- attr(logLik(model), "df")
    
    # Store model fit info for footnotes (don't add to table)
    attr(coef_df, "model_deviance") <- model_deviance
    attr(coef_df, "model_df") <- model_df
    
    return(coef_df)
  }
  
  # Extract information from all three models
  model_0_info <- extract_model_info(model_0, "Model 0")
  model_1_info <- extract_model_info(model_1, "Model 1") 
  model_2_info <- extract_model_info(model_2, "Model 2")
  
  # Combine all model information
  all_models_df <- rbind(model_0_info, model_1_info, model_2_info)
  
  # Create a wide format table for easier comparison
  # Get unique terms across all models (no longer need to exclude fit statistics)
  all_terms <- unique(all_models_df$Term_clean)
  
  # Create comparison table
  comparison_df <- data.frame(
    Term = all_terms,
    stringsAsFactors = FALSE
  )
  
  # Extract model fit statistics for footnotes
  model_0_deviance <- attr(model_0_info, "model_deviance")
  model_0_df <- attr(model_0_info, "model_df")
  model_1_deviance <- attr(model_1_info, "model_deviance")
  model_1_df <- attr(model_1_info, "model_df")
  model_2_deviance <- attr(model_2_info, "model_deviance")
  model_2_df <- attr(model_2_info, "model_df")
  
  # Add columns for each model
  for(model_name in c("Model 0", "Model 1", "Model 2")) {
    model_data <- all_models_df[all_models_df$Model == model_name, ]
    
    # Add estimates column (now includes SE in parentheses)
    est_col <- paste0(gsub(" ", "_", model_name), "_Est")
    comparison_df[[est_col]] <- sapply(comparison_df$Term, function(term) {
      match_row <- model_data[model_data$Term_clean == term, ]
      return(if(nrow(match_row) > 0) match_row$Est_with_stars[1] else "")
    })
  }
  
  # Create gt table
  characteristic_display <- switch(characteristic,
                                 "role_primary" = "Primary Role",
                                 "gender" = "Gender",
                                 "age_years" = "Age",
                                 "edu_years" = "Education Years",
                                 "rn_years" = "Nursing Experience",
                                 "expertise" = "Expertise",
                                 characteristic)
  
  # Create the base gt table
  gt_table <- comparison_df %>%
    gt()
  
  # Add title if provided
  if (!is.null(table_reference) || !is.null(table_title)) {
    title_html <- ""
    
    if (!is.null(table_reference)) {
      title_html <- paste0("<div style='text-align: left;'><span style='font-weight: bold; font-family: Times New Roman; font-size: 12pt;'>", 
                          table_reference, "</span>")
    }
    
    if (!is.null(table_title)) {
      if (!is.null(table_reference)) {
        title_html <- paste0(title_html, "<br>")
      } else {
        title_html <- "<div style='text-align: left;'>"
      }
      title_html <- paste0(title_html, 
                          "<span style='font-style: italic; font-family: Times New Roman; font-size: 12pt;'>", 
                          table_title, "</span>")
    }
    
    if (!is.null(table_reference) || !is.null(table_title)) {
      title_html <- paste0(title_html, "</div>")
    }
    
    gt_table <- gt_table %>%
      tab_header(title = html(title_html))
  }
  
  gt_table <- gt_table %>%
    cols_label(
      Term = "",
      Model_0_Est = "Model 1",
      Model_1_Est = "Model 2", 
      Model_2_Est = "Model 3"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_align(
      align = "left",
      columns = Term
    ) %>%
    cols_align(
      align = "center",
      columns = c(Model_0_Est, Model_1_Est, Model_2_Est)
    ) %>%
    fmt(
      columns = c(Model_0_Est, Model_1_Est, Model_2_Est),
      fns = function(x) {
        # Convert newlines to HTML breaks for proper rendering
        gsub("\n", "<br>", x)
      }
    ) %>%
    tab_options(
      table.font.size = 11.5,
      heading.title.font.size = 14,
      table.font.names = "Times New Roman"
    ) %>%
    tab_footnote(
      footnote = "* p < 0.05, ** p < 0.01, *** p < 0.001. P-values are unadjusted.",
      locations = cells_column_labels(columns = c(Model_0_Est, Model_1_Est, Model_2_Est))
    ) %>%
    tab_footnote(
      footnote = paste0("Model 1: -2LL = ", model_0_deviance, ", df = ", model_0_df),
      locations = cells_column_labels(columns = Model_0_Est)
    ) %>%
    tab_footnote(
      footnote = paste0("Model 2: -2LL = ", model_1_deviance, ", df = ", model_1_df),
      locations = cells_column_labels(columns = Model_1_Est)
    ) %>%
    tab_footnote(
      footnote = paste0("Model 3: -2LL = ", model_2_deviance, ", df = ", model_2_df),
      locations = cells_column_labels(columns = Model_2_Est)
    )
  
  return(gt_table)
}

# =============================================================================
# ADDITIONAL HELPER FUNCTIONS (placeholder for future expansion)
# =============================================================================

# Additional table and plot helper functions can be added here as needed