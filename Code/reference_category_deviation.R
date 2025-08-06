reference_category_deviation <- function(model, levels) {
  # Calculate the deviation of the reference category from the unweighted mean
  # of all factor levels, with standard error, t-statistic, and p-value
  #
  # Args:
  #   model: A fitted model object from lm(), glm(), etc.
  #   levels: A character vector of coefficient names for the non-reference 
  #           categories (e.g., c("factor_varB", "factor_varC", "factor_varD"))
  #
  # Returns:
  #   A data frame with columns: est, se, t, pval
  
  # Input validation
  if (!inherits(model, c("lm", "glm"))) {
    stop("model must be a fitted lm or glm object")
  }
  
  if (!is.character(levels) || length(levels) == 0) {
    stop("levels must be a non-empty character vector")
  }
  
  # Extract coefficients and variance-covariance matrix
  all_coeffs <- coef(model)
  all_vcov <- vcov(model)
  
  # Check that all specified levels exist in the model
  missing_levels <- levels[!levels %in% names(all_coeffs)]
  if (length(missing_levels) > 0) {
    stop(paste("The following levels are not found in the model:", 
               paste(missing_levels, collapse = ", ")))
  }
  
  # Extract coefficients for specified levels
  coefficients <- all_coeffs[levels]
  
  # Extract corresponding submatrix from variance-covariance matrix
  vcov_matrix <- all_vcov[levels, levels, drop = FALSE]
  
  # Get degrees of freedom
  if (inherits(model, "lm")) {
    df_residual <- model$df.residual
  } else if (inherits(model, "glm")) {
    df_residual <- model$df.residual
  }
  
  # Number of categories (including reference)
  k <- length(coefficients) + 1
  
  # Calculate point estimate
  est <- -sum(coefficients) / k
  
  # Create contrast vector for linear combination
  # We want (-1/k, -1/k, ..., -1/k) for the non-reference categories
  contrast_vector <- rep(-1/k, length(coefficients))
  
  # Calculate variance using quadratic form: c' * V * c
  variance <- as.numeric(t(contrast_vector) %*% vcov_matrix %*% contrast_vector)
  
  # Standard error
  se <- sqrt(variance)
  
  # t-statistic
  t_stat <- est / se
  
  # Two-tailed p-value
  pval <- 2 * pt(abs(t_stat), df = df_residual, lower.tail = FALSE)
  
  # Return as data frame
  result <- data.frame(
    est = est,
    se = se,
    t = t_stat,
    pval = pval
  )
  
  return(result)
}

# Example usage:
# Fit a model with a factor variable
# model <- lm(mpg ~ factor(cyl) + wt + hp, data = mtcars)
# 
# # Specify the non-reference factor levels
# factor_levels <- c("factor(cyl)6", "factor(cyl)8")
# 
# # Calculate reference category deviation statistics
# result <- reference_category_deviation(model, factor_levels)
# print(result)
#
# # The result will show how much the reference category (cyl=4) 
# # deviates from the unweighted mean of all cylinder categories