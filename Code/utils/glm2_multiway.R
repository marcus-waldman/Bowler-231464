# -----------------------------
# Generic Wald function definition
# -----------------------------
Wald <- function(object, ...) {
  UseMethod("Wald")
}

# -----------------------------
# Main pooling function: pool_glm2_multiway
# -----------------------------
pool_glm2_multiway <- function(formula, imputed_list, cluster_vars, family = binomial()) {
  
  m <- length(imputed_list)
  
  coef_list     <- vector("list", m)
  vcov_list     <- vector("list", m)
  imputed_models <- vector("list", m)
  
  data_used <- imputed_list[[1]]
  fit_1 = glm2::glm2(formula, data = imputed_list[[1]], family = family)
  
  for (i in seq_along(imputed_list)) {
    data_i <- imputed_list[[i]]
    if(i == 1){ 
      fit_i = fit_1
    } else {
      fit_i  <- glm2::glm2(formula, data = data_i, family = family, start = coef(fit_1))
    }
    imputed_models[[i]] <- fit_i
    coef_list[[i]]      <- stats::coef(fit_i)
    vcov_list[[i]]      <- multiwayvcov::cluster.vcov(fit_i,
                                                      data_i[, cluster_vars, drop = FALSE])
  }
  
  coef_mat    <- do.call(rbind, coef_list)
  pooled_coef <- colMeans(coef_mat)
  
  U_sum    <- vcov_list[[1]]
  for(i in 2:m){U_sum = U_sum + vcov_list[[m]]}
  U_bar    <- U_sum/m #Eqn 2.18 van Buuren (2018)
  B        <- cov(coef_mat) #Eqn 2.19 van Buuren (2018) #apply(coef_mat, 2, stats::var)
  total_var <- U_bar + (1 + 1/m) * B #Eqn 10.14 Little & Rubin (2002) also eqn 2.20 van Buuren (2018)
  pooled_se <- sqrt(diag(total_var))
  
  t_stat   <- pooled_coef / pooled_se
  df       <- sapply(seq_along(pooled_coef), function(j) {
    if (B[j] == 0) Inf
    else (m-1) * ( 1 + U_bar[j,j]/((1 + 1/m)*B[j,j]) )^2  #Equivalent to Eqn 10.15 Little & Rubin (2002)
  })
  p_values <- 2 * (1 - stats::pt(abs(t_stat), df))
  
  results <- data.frame(
    Term            = names(pooled_coef),
    Pooled_Estimate = pooled_coef,
    Pooled_SE       = pooled_se,
    p_value         = p_values,
    stringsAsFactors = FALSE
  )
  
  pooled_vcov <- diag(diag(total_var))
  colnames(pooled_vcov) <- names(pooled_coef)
  rownames(pooled_vcov) <- names(pooled_coef)
  
  pooled_object <- list(
    results        = results,
    pooled_coef    = pooled_coef,
    pooled_vcov    = pooled_vcov,
    formula        = formula,
    call           = match.call(),
    imputed_models = imputed_models,
    data           = data_used
  )
  
  pooled_object$family    <- imputed_models[[1]]$family
  pooled_object$terms     <- stats::terms(formula, data = data_used)
  
  term_labels     <- attr(pooled_object$terms, "term.labels")
  full_xlevels    <- imputed_models[[1]]$xlevels
  full_contrasts  <- imputed_models[[1]]$contrasts
  
  pooled_object$xlevels   <- full_xlevels[names(full_xlevels)   %in% term_labels]
  pooled_object$contrasts <- full_contrasts[names(full_contrasts) %in% term_labels]
  
  pooled_object$ngrps <- setNames(
    sapply(cluster_vars, function(v) length(unique(data_used[[v]]))),
    cluster_vars
  )
  
  pooled_object$m     <- m
  pooled_object$B     <- B
  pooled_object$U_bar <- U_bar
  
  class(pooled_object) <- c("pooled_cluster_robust_glm2_model","glm")
  return(pooled_object)
}

# -----------------------------
# S3 Methods for pooled_cluster_robust_glm2_model
# -----------------------------
predict.pooled_cluster_robust_glm2_model <- function(object,
                                                     newdata = NULL, type = c("link","response"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) newdata <- object$data
  mm <- stats::model.matrix(object$formula, newdata)
  lp <- as.vector(mm %*% object$pooled_coef)
  if (type == "response") return(object$family$linkinv(lp))
  lp
}

vcov.pooled_cluster_robust_glm2_model <- function(object, ...) {
  object$pooled_vcov
}

coef.pooled_cluster_robust_glm2_model <- function(object, ...) {
  object$pooled_coef
}

nobs.pooled_cluster_robust_glm2_model <- function(object, ...) {
  nrow(object$data)
}

confint.pooled_cluster_robust_glm2_model <- function(object, level = 0.95, ...) {
  z     <- stats::qnorm((1 + level)/2)
  lower <- object$pooled_coef - z*sqrt(diag(object$pooled_vcov))
  upper <- object$pooled_coef + z*sqrt(diag(object$pooled_vcov))
  ci    <- cbind(lower, upper)
  rownames(ci) <- names(object$pooled_coef)
  colnames(ci) <- c("2.5 %","97.5 %")
  ci
}

print.pooled_cluster_robust_glm2_model <- function(x, ...) {
  cat("Pooled Generalized Linear Model (multiway clustering)\nCall:\n")
  print(x$call)
  cat("\nPooled Coefficients:\n")
  print(x$results)
  invisible(x)
}

summary.pooled_cluster_robust_glm2_model <- function(object, ...) {
  coef_mat <- cbind(
    Estimate    = object$pooled_coef,
    `Std. Error`= sqrt(diag(object$pooled_vcov)),
    `z value`   = object$pooled_coef/sqrt(diag(object$pooled_vcov)),
    `Pr(>|z|)`  = object$results$p_value
  )
  s <- list(
    call        = object$call,
    formula     = object$formula,
    coefficients= coef_mat,
    vcov        = object$pooled_vcov,
    nobs        = nobs(object),
    family      = object$family,
    terms       = object$terms
  )
  class(s) <- "summary.pooled_cluster_robust_glm2_model"
  s
}

get_data.pooled_cluster_robust_glm2_model <- function(x, ...) x$data

set_coef.pooled_cluster_robust_glm2_model <- function(object, new_coef, ...) {
  object$pooled_coef <- new_coef; object
}

get_ngroups.pooled_cluster_robust_glm2_model <- function(x, ...) x$ngrps

# -----------------------------
# Modified Wald: D1 statistic with pseudoinverse & term expansion
# -----------------------------
Wald.pooled_cluster_robust_glm2_model <- function(object, terms, ...) {
  if (missing(terms) || length(terms)==0)
    stop("Please specify one or more terms for the Wald test.")
  
  found <- character(0)
  for (t in terms) {
    if (t %in% names(object$pooled_coef)) {
      found <- c(found, t)
    } else {
      mchs <- grep(paste0("^",t), names(object$pooled_coef), value=TRUE)
      if (length(mchs)>0) found <- c(found, mchs)
      else stop("Term not in model: ", t)
    }
  }
  
  b_test       <- object$pooled_coef[found]
  
  ids = which(names(object$pooled_coef) %in% found)
  k = length(ids)
  
  U_bar_sub    <- object$U_bar[ids,ids]
  B_sub        <- object$B[ids,ids]
  m_val        <- object$m
  #r_vec        <- (1+1/m_val)*B_sub / (U_bar_sub + (1+1/m_val)*B_sub)
  
  r1           <- (1 + 1/m_val)*sum(diag(B_sub*MASS::ginv(U_bar_sub)))/k #Average fraction of missing information
                                                                    # Eqn 2.2 van Buuren (2018)
  
  T_tilde      <- (1+r1)*U_bar_sub #pg. 147 van Buuren (2018), Li et al. (1991)
  D1_stat      <- as.numeric(t(b_test)%*%MASS::ginv(T_tilde)%*%b_test)/k #Eqn 5.3 van Buuren (2018)
  t_val        <- k*(m_val-1) #eqn 5.4
  if (t_val>4){
    nu1 = 4 + (t_val-4)*(1+((1-2/t_val)/r1))^2 #Eqn 5.4
  } else {
    nu1 = .5*t_val*(1+1/k)*(1+1/r1)^2 #Eqn 5.4
  }

  p_value      <- 1 - stats::pf(D1_stat, df1=k, df2=nu1)
  
  res <- list(
    statistic  = D1_stat,
    parameter1 = k,
    parameter2 = nu1,
    p.value    = p_value,
    terms      = found,
    method     = "D1 Wald test (Li, Raghunathan & Rubin 1991)",
    data.name  = deparse(substitute(object))
  )
  class(res) <- "htest"
  res
}