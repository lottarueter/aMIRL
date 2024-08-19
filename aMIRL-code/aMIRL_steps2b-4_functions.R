## returns time-demeaned panel data
time_demean <- function(df, cross_section_index = "MFI.ID", time_index = "Fiscal.Year") {
  cross_section_time_indices <- c(cross_section_index, time_index)
  
  df_time_means <- df %>%
    group_by(.data[[cross_section_index]]) %>%
    transmute(across(tidyselect::peek_vars(), ~ mean(.x), .names = "{col}")) %>%
    ungroup() %>%
    select(-all_of(cross_section_time_indices))
  
  df_time_demeaned <- (df %>% select(-all_of(cross_section_time_indices))) - df_time_means
  
  df_time_demeaned <- as.data.frame(df_time_demeaned)
  
  df_time_demeaned[[cross_section_index]] <- df[[cross_section_index]]
  df_time_demeaned[[time_index]] <- df[[time_index]]
  
  return(df_time_demeaned)
}


## returns time-means of panel data
time_means <- function(df, cross_section_index = "MFI.ID", time_index = "Fiscal.Year") {
  cross_section_time_indices <- c(cross_section_index, time_index)
  
  df_time_means <- df %>%
    group_by(.data[[cross_section_index]]) %>%
    transmute(across(tidyselect::peek_vars(), ~ mean(.x), .names = "{col}")) %>%
    ungroup() %>%
    select(-all_of(cross_section_time_indices))
  
  df_time_means <- as.data.frame(df_time_means)
  
  df_time_means[[cross_section_index]] <- df[[cross_section_index]]
  df_time_means[[time_index]] <- df[[time_index]]
  
  return(df_time_means)
}


## computes values of information criteria for given glmnet-relaxed-fit
ic_ols_fit <- function(y, x, ols_fit, ols_fit_full, n_FE) {
  ## For fixed effects model: x = x_standardized_time_demeaned, since
  ## y_hat_it = alpha_hat + beta_hat x_it = y_time_means_i + beta_hat x_time_demeaned_it
  ## For pooled OLS: x = x_standardized
  
  n <- length(y)
  k <- ols_fit$df
  npar <- k + n_FE # + n_FE for fixed effects (intercept)
  
  b_ols <- coef(ols_fit)
  x <- cbind(1, x)

  y_hat <- x %*% b_ols # intercept is set to zero, so no problem with time-demeaned data w/o intercept
  
  # intercept is set to zero, so no problem with time-demeaned data w/o intercept
  residuals <- (y - y_hat)
  sse <- colSums(residuals^2)
  mse <- colMeans(residuals^2)

  bic <- n*log(mse) + npar*log(n)
  aic <- n*log(mse) + 2*npar
  aicc <- aic + (2*npar*(npar+1)) / (n-npar-1)
  hqc <- n*log(mse) + 2*npar*log(log(n))
  
  # full_model serves as benchmark for computing the error variance estimator sigsq for mcp
  k_full <- ols_fit_full$df
  npar_full <- k_full + n_FE # + n_FE for fixed effects (intercept)
  b_ols_full <- coef(ols_fit_full)
 
  y_hat_full <- x %*% b_ols_full # intercept is set to zero, so no problem with time-demeaned data w/o intercept
  residuals_full <- (y - y_hat_full)
  
  sigsq <- 1/(n-npar_full) * sum(residuals_full^2) # corresponds to OLS estimator with n_FE dummies
  mcp <- sse / sigsq - n + 2*npar # the expecation of mcp = sse / sigsq - n + 2*npar equals npar in the unbiased case
  # mcp <- mse + 2/n *sigsq * npar # the expecation of mcp = sse / sigsq - n + 2*npar equals npar in the unbiased case
  
  sst <- (n-1)*var(y)
  r2 <- 1 - (sse/sst)
  adjr2 <- (1 - (1 - r2) * (n - 1)/(n - npar - 1))
  
  crits <- rbind("bic" = bic, "aic" = aic, "aicc" = aic, "hqc" = hqc, "mcp" = mcp, "r2" = r2, "adjr2" = adjr2)
  
  return(crits)
}


## computes values of information criteria for given b_vectors
ic_b_ols <- function(y, x, b_ols, b_ols_full, n_FE) {
  ## For fixed effects model: x = x_standardized_time_demeaned, since
  ## y_hat_it = alpha_hat + beta_hat x_it = y_time_means_i + beta_hat x_time_demeaned_it
  ## For pooled OLS: x = x_standardized
  
  n <- length(y)
  k <- length(b_ols) - 1
  k_full <- length(b_ols_full) - 1 # remove intercept which is not fitted anyway...
  
  sel_vars <- names(b_ols)
  
  if("(Intercept)" %in% sel_vars) {
    sel_vars <- sel_vars[-which(sel_vars=="(Intercept)")] # remove intercept, is zero anyways
    b_ols <- b_ols[which(names(b_ols) %in% sel_vars)]
  }
  
  if(is.null(sel_vars)) {
    y_hat <- 0
  } else if(length(sel_vars) == 1) {
    y_hat <- x[,sel_vars] * b_ols
  } else {
    y_hat <- x[,sel_vars] %*% b_ols # intercept is set to zero, so no problem with time-demeaned data w/o intercept
  }
  
  residuals <- (y - y_hat)
  sse <- sum(residuals^2)
  mse <- 1/n*sse
  
  npar <- k + n_FE # + n_FE for fixed effects (intercept)
  
  bic <- n*log(mse) + npar*log(n)
  aic <- n*log(mse) + 2*npar
  aicc <- aic + (2*npar*(npar+1)) / (n-npar-1)
  hqc <- n*log(mse) + 2*npar*log(log(n))
  
  # full_model serves as benchmark for computing the error variance estimator sigsq for mcp
  npar_full <- k_full + n_FE # + n_FE for fixed effects (intercept)
  
  y_hat_full <- cbind(1, x[,names(b_ols_full)[-1]]) %*% b_ols_full # intercept is set to zero, so no problem with time-demeaned data w/o intercept
  residuals_full <- (y - y_hat_full)
  
  sigsq <- 1/(n-npar_full) * sum(residuals_full^2) # corresponds to OLS estimator with n_FE dummies
  mcp <- sse / sigsq - n + 2*npar # the expecation of mcp = sse / sigsq - n + 2*npar equals npar in the unbiased case
  # mcp <- mse + 2/n *sigsq * npar # the expecation of mcp = sse / sigsq - n + 2*npar equals npar in the unbiased case
  
  sst <- (n-1)*var(y)
  r2 <- 1 - (sse/sst)
  adjr2 <- (1 - (1 - r2) * (n - 1)/(n - npar - 1))
  
  crits <- c("bic" = bic, "aic" = aic, "aicc" = aic, "hqc" = hqc, "mcp" = mcp, "r2" = r2, "adjr2" = adjr2)
  
  return(crits)
}



## selects candidate variables
select_candidate_variables <- function(importance_measure, n_candidate_var, MB){
  names_variables <- names(importance_measure)
  selection_probabilities <- importance_measure / sum(importance_measure)
  
  if(sum(selection_probabilities != 0) < n_candidate_var) {
    n_candidate_var <- sum(selection_probabilities != 0)
  }
  
  candidate_variables <- list()
  
  for (mb in 1:MB) {
    candidate_variables[[mb]] <- sort(sample(names_variables, size = n_candidate_var, prob = selection_probabilities, replace = FALSE))
  }
  return(candidate_variables)
}


compute_r_sq <- function(y, x, y_time_means = 0, stable_set, coef_vec, n_FE) {
  ## x, y: standardized, imputed and standardized data sets (not bootstrapped! lists of length m)
  ## stable set: final variables whose empirical selection probabilities exceed threshold chosen via Mallows' Cp
  ## coef_vec: estimates for coefficients of final variables (vector)
  
  indices_coef_vec_stable_set <- which(names(coef_vec) %in% stable_set)
  indices_coef_vec_stable_set <- indices_coef_vec_stable_set[order(match(names(coef_vec)[indices_coef_vec_stable_set], stable_set))] # same ordering as stable_set
  coef_vec_stable_set <- coef_vec[indices_coef_vec_stable_set]
  
  # x_stable_set <- as.matrix(x)
  indices_x_stable_set <- which(colnames(x) %in% stable_set)
  indices_x_stable_set <- indices_x_stable_set[order(match(colnames(x)[indices_x_stable_set], stable_set))] # same ordering as coef_vec_thresh
  
  x_stable_set <- x[, indices_x_stable_set]
  
  if (length(stable_set) == 1 && is.na(stable_set)) {
    y_hat <- y_time_means
  } else if (length(stable_set) == 1) {
    y_hat <- x_stable_set * coef_vec_stable_set + y_time_means # y_time_means!=0 only, for computation of overall r square of within-transformed data
  } else {
    y_hat <- x_stable_set %*% coef_vec_stable_set + y_time_means # y_time_means!=0 only, for computation of overall r square of within-transformed data
  }

  n <- length(y)
  
  resids <- y - y_hat
  
  # compute R^2
  RSS <- sum(resids^2)
  TSS <- sum( (y -  mean(y))^2 ) # standardized data -> mean(y)=0
  
  r_sq <- 1 - RSS / TSS
  
  df_model <- n - n_FE - length(stable_set)
  df_total <- n - 1
  
  r_sq_adj <- 1 - (RSS / df_model) / (TSS / df_total)
  
  return(data.frame("r_sq" = r_sq, "r_sq_adj" = r_sq_adj))
}
