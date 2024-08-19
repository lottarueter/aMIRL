## Steps 2b-4 of the Adapted Multiple Imputation Random Lasso (aMIRL)
# Author: Lotta Rüter
# lotta.rueter@kit.edu

## IMPORTANT:
# Performs aMIRL with MFI-specific fixed effects (already accounted for by standardization and time-demeaning in step 2a) for panel_version == "balanced" and use_fixed_effects == TRUE
# Performs MIRL with overall intercept (accounted for by standardization in step 2a) for panel_version == "balanced" or "unbalanced" and use_fixed_effects == FALSE
# overall intercept (MIRL) or fixed effects (aMIRL) are already accounted for by standardization (time-demeaning) -> only use models without intercept in the following, degrees of freedom include the number of intercept / fixed effects!

## Preparations
## -----
## input imputed, bootstrapped, standardized (and time-demeaned) data!

## install/load packages
library(dplyr)
library(glmnet)
library(doParallel) # required for makeForkCluster
library(rstudioapi)
library(rlist)

## clear environment
rm(list = ls())

## set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

## load aMIRL_functions
source("aMIRL_steps2b-4_functions.R")

## Set input variables 'panel_version' and 'use_fixed_effects' (rest of code can be run without further modifications)
## -----
## balanced or unbalanced panel?
panel_version <- "unbalanced"
# panel_version <- "balanced"

## fixed effects or pooled OLS estimation?
# use_fixed_effects <- TRUE # for fixed effects estimation
use_fixed_effects <- FALSE # for pooled OLS estimation

max_it <- 1e+06

## -----

# the different target variables considered in the analysis
target_variables <- c("Financial.Performance.>.Operational.self.sufficiency",
                      "Clients.>.log.Number.of.active.borrowers",
                      "Clients.>.Number.of.active.borrowers",
                      "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                      "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita")

for(target_variable in target_variables[1:5]) {
if(target_variable == "Financial.Performance.>.Operational.self.sufficiency") {
  variables_to_remove <- c("MFI.ID",
                           "Fiscal.Year",
                           "Clients.>.log.Number.of.active.borrowers",
                           "Financial.Performance.>.Operational.self.sufficiency",
                           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
  target_variable_short <- "OSS"
} else if(target_variable == "Clients.>.log.Number.of.active.borrowers") {
  variables_to_remove <- c("MFI.ID",
                           "Fiscal.Year",
                           "Clients.>.log.Number.of.active.borrowers",
                           "Clients.>.Number.of.active.borrowers",
                           "Clients.>.Number.of.loans.outstanding",
                           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
  target_variable_short <- "logNAB"
} else if(target_variable == "Clients.>.Number.of.active.borrowers") {
  variables_to_remove <- c("MFI.ID",
                           "Fiscal.Year",
                           "Clients.>.log.Number.of.active.borrowers",
                           "Clients.>.Number.of.active.borrowers",
                           "Clients.>.Number.of.loans.outstanding",
                           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
  target_variable_short <- "NAB"
} else if(target_variable == "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita") {
  variables_to_remove <- c("MFI.ID",
                           "Fiscal.Year",
                           "Clients.>.log.Number.of.active.borrowers",
                           "Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.Average.outstanding.balance./.GNI.per.capita",
                           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
  target_variable_short <- "neglogALBG"
} else if(target_variable == "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita") {
  variables_to_remove <- c("MFI.ID",
                           "Fiscal.Year",
                           "Clients.>.log.Number.of.active.borrowers",
                           "Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.Average.outstanding.balance./.GNI.per.capita",
                           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
  target_variable_short <- "negALBG"
}

## load imputed, standardized, bootstrapped and time-demeaned data of potential regressors and target variable
if(use_fixed_effects && panel_version == "balanced") { # fixed effects model
  model_variant <- paste0(panel_version, "_aMIRL_", target_variable_short)
  
  # load data files
  load(file = paste0("../aMIRL-output/data_imputed_ls_", panel_version, "_aMIRL.RData"))
  
  # compute time-demeaned data and respective time means
  data_imputed_standardized_time_demeaned_ls <- lapply(data_imputed_standardized_ls, time_demean)
  data_imputed_bootstrapped_standardized_time_demeaned_ls <- lapply(data_imputed_bootstrapped_standardized_ls, time_demean)
  
  time_means_standardized_ls <- lapply(data_imputed_standardized_ls, time_means)
  time_means_bootstrapped_standardized_ls <- lapply(data_imputed_bootstrapped_standardized_ls, time_means)
  
  y_data_ls <- lapply(data_imputed_bootstrapped_standardized_time_demeaned_ls,
                      function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  x_data_ls <- lapply(data_imputed_bootstrapped_standardized_time_demeaned_ls,
                      function(dataframe) return(dataframe %>% select(-all_of(variables_to_remove))))
  
  y_data_not_bootstr_ls <- lapply(data_imputed_standardized_time_demeaned_ls,
                                  function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  x_data_not_bootstr_ls <- lapply(data_imputed_standardized_time_demeaned_ls,
                                  function(dataframe) return(dataframe %>% select(-all_of(variables_to_remove))))
  
  y_data_not_time_dem_not_bootstr_ls <- lapply(data_imputed_standardized_ls,
                                               function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  
  y_time_means_ls <- lapply(time_means_standardized_ls,
                            function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  
  n_FE <- length(unique(data_imputed_standardized_time_demeaned_ls[[1]]$MFI.ID)) # number of fixed effects
  
  rm(data_imputed_standardized_ls, data_imputed_bootstrapped_standardized_ls,
     data_imputed_standardized_time_demeaned_ls, data_imputed_bootstrapped_standardized_time_demeaned_ls,
     time_means_standardized_ls, time_means_bootstrapped_standardized_ls)
  
} else if(!use_fixed_effects){
  model_variant <- paste0(panel_version, "_MIRL_", target_variable_short)
  
  # load data files
  load(paste0("../aMIRL-output/data_imputed_ls_", panel_version, "_MIRL.RData"))

  y_data_ls <- lapply(data_imputed_bootstrapped_standardized_ls,
                      function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  x_data_ls <- lapply(data_imputed_bootstrapped_standardized_ls,
                      function(dataframe) return(dataframe %>% select(-all_of(variables_to_remove))))
  
  y_data_not_bootstr_ls <- lapply(data_imputed_standardized_ls,
                                  function(dataframe) return(dataframe %>% select(all_of(target_variable))))
  x_data_not_bootstr_ls <- lapply(data_imputed_standardized_ls,
                                  function(dataframe) return(dataframe %>% select(-all_of(variables_to_remove))))
  
  n_FE <- 1 # number of fixed effects (i.e. one overall intercept)
  
} else {
  stop("Fixed effects estimation is implemented for balanced panel only. Either set 'use_fixed_effects' to FALSE or panel_version to 'balanced'.")
}

M <- length(x_data_not_bootstr_ls) # number of imputed samples
MB <- length(x_data_ls) # number of imputed, bootstrapped data sets


## -----
## STEP 2b
## -----
## STEP 2b.1 : compute lambdapath
# start.date <- Sys.time()
print(paste0("Beginning of computations for ", model_variant, ": ", Sys.time()))

lambda_max_vec <- unlist(lapply(1:MB, function(mb) # max(glmnet(.)$lambda) is the minimum lambda for which no variable is selected
  max(glmnet(x = as.matrix(x_data_ls[[mb]]),
             y = as.matrix(y_data_ls[[mb]]),
             family = "gaussian", # optimizes 1/2RSS/nobs+λ∗penalty,
             alpha = 1, # lasso penalty: 1/2||beta_j||
             nlambda = 100, # default value
             standardize = FALSE, # data is already standardized
             intercept = FALSE)$lambda))) # we use time-demeaned data$lambda

# formula for lambdapath taken from "Regularization Paths for Generalized Linear Models via Coordinate Descent" by Friedman, Hastie and Tibshirani (2010)
# delta: decay parameter, .001 is suggested by the above authors
lambdapath <- round(exp(seq(log(max(lambda_max_vec)), log(max(lambda_max_vec) * .001),
                            length.out = 100)), digits = 10)
# write.csv(lambdapath, file = paste0("../aMIRL-output/lambdapath_", model_variant, ".csv"))

## STEP 2b.2 : compute panel LASSO-OLS regression estimates for each bootstrapped sample
## optimize lambda indirectly - actually optimize the set of variables that results from the specific lambda
## lasso estimates for each lambda and data set
lasso_ols_step2 <- lapply(1:MB, function(mb) glmnet(x = as.matrix(x_data_ls[[mb]]),
                                                    y = as.matrix(y_data_ls[[mb]]),
                                                    alpha = 1, # lasso penalty
                                                    lambda = lambdapath,
                                                    intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                                                    grouped = FALSE,
                                                    parallel = TRUE,
                                                    family = "gaussian", # ensures correct loss function, does not require normally distributed data
                                                    standardize = FALSE,
                                                    relax = TRUE)) # causes ols refit on active sets per lambda

## OLS-estimation of full model with all potential regressors (baseline for Mallows Cp)
ols_full_model <- lapply(1:MB, function(mb) glmnet(x = as.matrix(x_data_ls[[mb]]),
                                                   y = as.matrix(y_data_ls[[mb]]),
                                                   lambda = 0,
                                                   intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                                                   grouped = FALSE,
                                                   parallel = TRUE,
                                                   family = "gaussian", # ensures correct loss function, does not require normally distributed data
                                                   standardize = FALSE)) # don't need relax here since we do not regularize (lambda = 0!)

ic_step2 <- lapply(1:MB, function(mb) {
  print(paste0(model_variant, ", ic_step2, mb = ", mb))
  return(ic_ols_fit(y = as.matrix(y_data_ls[[mb]]),
                    x = as.matrix(x_data_ls[[mb]]),
                    ols_fit = lasso_ols_step2[[mb]]$relaxed,
                    ols_fit_full = ols_full_model[[mb]],
                    n_FE = n_FE))
})

## index of lambda with minimum average optimality criterion
ind_bic_step2 <- unlist(lapply(1:MB, function(mb) min(which(ic_step2[[mb]]["bic", ] == min(ic_step2[[mb]]["bic", ])))))
ind_aic_step2 <- unlist(lapply(1:MB, function(mb) min(which(ic_step2[[mb]]["aic", ] == min(ic_step2[[mb]]["aic", ])))))
ind_mcp_step2 <- unlist(lapply(1:MB, function(mb) min(which(ic_step2[[mb]]["mcp", ] == min(ic_step2[[mb]]["mcp", ])))))

## respective ols estimates
b_ols_bic_step2 <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_step2[[mb]]$relaxed)[,ind_bic_step2[mb]]))
b_ols_aic_step2 <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_step2[[mb]]$relaxed)[,ind_aic_step2[mb]]))
b_ols_mcp_step2 <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_step2[[mb]]$relaxed)[,ind_mcp_step2[mb]]))

b_bic_step2 <- rowMeans(b_ols_bic_step2)
b_aic_step2 <- rowMeans(b_ols_aic_step2)
b_mcp_step2 <- rowMeans(b_ols_mcp_step2)

round(cbind(b_bic_step2, b_aic_step2, b_mcp_step2),4)

## STEP 2b.3 : compute importance measure for each variable
## use average of b_estimates per variable and normalize for importance measure
I_bic <- abs(b_bic_step2)
I_aic <- abs(b_aic_step2)
I_mcp <- abs(b_mcp_step2)

round(cbind(I_bic, I_aic, I_mcp), 4)

## -----
## STEP 3
## -----
## STEP 3.1 : compute lasso-OLS for panel data for grid of lambdas (lambdapath)
# choose p/3 candidate variables (instead of p/2 - according to paper by Liu et al. (2016) similar results)
print(paste0("Step 3 for ", model_variant, ": ", Sys.time()))

set.seed(2106)
n_candidate_var <- floor(ncol(x_data_ls[[1]])/3)

candidate_variables_bic <- select_candidate_variables(importance_measure = I_bic, n_candidate_var = n_candidate_var, MB = MB)
candidate_variables_aic <- select_candidate_variables(importance_measure = I_aic, n_candidate_var = n_candidate_var, MB = MB)
candidate_variables_mcp <- select_candidate_variables(importance_measure = I_mcp, n_candidate_var = n_candidate_var, MB = MB)

lasso_ols_bic_step3 <- lapply(1:MB, function(mb) glmnet(x = as.matrix(x_data_ls[[mb]]),
                                                        y = as.matrix(y_data_ls[[mb]]),
                                                        alpha = 1, # lasso penalty
                                                        lambda = lambdapath,
                                                        intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                                                        grouped = FALSE,
                                                        parallel = TRUE,
                                                        exclude = which(!colnames(x_data_ls[[mb]]) %in% candidate_variables_bic[[mb]]),
                                                        family = "gaussian", # ensures correct loss function, does not require normally distributed data
                                                        standardize = FALSE, # data is already standardized
                                                        relax = TRUE, # fit lasso-ols
                                                        maxit = max_it))

lasso_ols_aic_step3 <- lapply(1:MB, function(mb) glmnet(x = as.matrix(x_data_ls[[mb]]),
                                                        y = as.matrix(y_data_ls[[mb]]),
                                                        alpha = 1, # lasso penalty
                                                        lambda = lambdapath,
                                                        intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                                                        grouped = FALSE,
                                                        parallel = TRUE,
                                                        exclude = which(!colnames(x_data_ls[[mb]]) %in% candidate_variables_aic[[mb]]),
                                                        family = "gaussian", # ensures correct loss function, does not require normally distributed data
                                                        standardize = FALSE, # data is already standardized
                                                        relax = TRUE, # fit lasso-ols
                                                        maxit = max_it))

lasso_ols_mcp_step3 <- lapply(1:MB, function(mb) glmnet(x = as.matrix(x_data_ls[[mb]]),
                                                        y = as.matrix(y_data_ls[[mb]]),
                                                        alpha = 1, # lasso penalty
                                                        lambda = lambdapath,
                                                        intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                                                        grouped = FALSE,
                                                        parallel = TRUE,
                                                        exclude = which(!colnames(x_data_ls[[mb]]) %in% candidate_variables_mcp[[mb]]),
                                                        family = "gaussian", # ensures correct loss function, does not require normally distributed data
                                                        standardize = FALSE, # data is already standardized
                                                        relax = TRUE, # fit lasso-ols
                                                        maxit = max_it))

## compute information criteria for (FE)OLS-fit
ic_bic_step3 <- lapply(1:MB, function(mb) {
  print(paste0(model_variant, ", ic_bic_step3, mb = ", mb))
  return(ic_ols_fit(y = as.matrix(y_data_ls[[mb]]),
                    x = as.matrix(x_data_ls[[mb]]),
                    ols_fit = lasso_ols_bic_step3[[mb]]$relaxed,
                    ols_fit_full = ols_full_model[[mb]],
                    n_FE = n_FE)) })
ic_aic_step3 <- lapply(1:MB, function(mb) {
  print(paste0(model_variant, ", ic_aic_step3, mb = ", mb))
  return(ic_ols_fit(y = as.matrix(y_data_ls[[mb]]),
                    x = as.matrix(x_data_ls[[mb]]),
                    ols_fit = lasso_ols_aic_step3[[mb]]$relaxed,
                    ols_fit_full = ols_full_model[[mb]],
                    n_FE = n_FE)) })
ic_mcp_step3 <- lapply(1:MB, function(mb) {
  print(paste0(model_variant, ", ic_mcp_step3, mb = ", mb))
  return(ic_ols_fit(y = as.matrix(y_data_ls[[mb]]),
                    x = as.matrix(x_data_ls[[mb]]),
                    ols_fit = lasso_ols_mcp_step3[[mb]]$relaxed,
                    ols_fit_full = ols_full_model[[mb]],
                    n_FE = n_FE)) })

## index of lambda with minimum average optimality criterion
ind_bic_step3 <- unlist(lapply(1:MB, function(mb) min(which(ic_bic_step3[[mb]]["bic", ] == min(ic_bic_step3[[mb]]["bic", ])))))
ind_aic_step3 <- unlist(lapply(1:MB, function(mb) min(which(ic_aic_step3[[mb]]["aic", ] == min(ic_aic_step3[[mb]]["aic", ])))))
ind_mcp_step3 <- unlist(lapply(1:MB, function(mb) min(which(ic_mcp_step3[[mb]]["mcp", ] == min(ic_mcp_step3[[mb]]["mcp", ])))))

## respective ols estimates
b_aMIRL_matrix_bic <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_bic_step3[[mb]]$relaxed)[,ind_bic_step3[mb]]))
b_aMIRL_matrix_aic <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_aic_step3[[mb]]$relaxed)[,ind_aic_step3[mb]]))
b_aMIRL_matrix_mcp <- list.cbind(lapply(1:MB, function(mb) coef(lasso_ols_mcp_step3[[mb]]$relaxed)[,ind_mcp_step3[mb]]))

b_aMIRL_bic <- rowMeans(b_aMIRL_matrix_bic)
b_aMIRL_aic <- rowMeans(b_aMIRL_matrix_aic)
b_aMIRL_mcp <- rowMeans(b_aMIRL_matrix_mcp)

round(cbind(b_aMIRL_bic, b_aMIRL_aic, b_aMIRL_mcp), 4)

## -----
# STEP 4
## -----
## STEP 4.1 : compute selection probabilities
print(paste0("Step 4 for ", model_variant, ": ", Sys.time()))

Pi_lambda_bic <- Reduce("+", lapply(1:MB, function(mb) coef(lasso_ols_bic_step3[[mb]]) != 0)) / MB
Pi_lambda_aic <- Reduce("+", lapply(1:MB, function(mb) coef(lasso_ols_aic_step3[[mb]]) != 0)) / MB
Pi_lambda_mcp <- Reduce("+", lapply(1:MB, function(mb) coef(lasso_ols_mcp_step3[[mb]]) != 0)) / MB

selection_probability_bic <- apply(Pi_lambda_bic, 1, max)
selection_probability_aic <- apply(Pi_lambda_aic, 1, max)
selection_probability_mcp <- apply(Pi_lambda_mcp, 1, max)

sel_prob_all <- cbind(selection_probability_bic, selection_probability_aic, selection_probability_mcp)

b_ols_full_model_average <- rowMeans(list.cbind(lapply(ols_full_model, function(obj) coef(obj))))

ic_bic_step4 <- lapply(1:M, function(m) {
  print(paste0("Model variant ", model_variant, ", ic_bic_step4, m = ", m))
  return(list.rbind(lapply(sort(unique(selection_probability_bic)), function(pi_hat) ic_b_ols(y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                                              x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                                              b_ols = b_aMIRL_bic[names(selection_probability_bic)[which(selection_probability_bic >= pi_hat)]],
                                                                                              b_ols_full = b_ols_full_model_average,
                                                                                              n_FE = n_FE)))) })
ic_aic_step4 <- lapply(1:M, function(m) {
  print(paste0("Model variant ", model_variant, ", ic_aic_step4, m = ", m))
  return(list.rbind(lapply(sort(unique(selection_probability_aic)), function(pi_hat) ic_b_ols(y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                                              x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                                              b_ols = b_aMIRL_aic[names(selection_probability_aic)[which(selection_probability_aic >= pi_hat)]],
                                                                                              b_ols_full = b_ols_full_model_average,
                                                                                              n_FE = n_FE)))) }) 
ic_mcp_step4 <- lapply(1:M, function(m) {
  print(paste0("Model variant ", model_variant, ", ic_mcp_step4, m = ", m))
  return(list.rbind(lapply(sort(unique(selection_probability_mcp)), function(pi_hat) ic_b_ols(y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                                              x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                                              b_ols = b_aMIRL_mcp[names(selection_probability_mcp)[which(selection_probability_mcp >= pi_hat)]],
                                                                                              b_ols_full = b_ols_full_model_average,
                                                                                              n_FE = n_FE)))) })

pi_hat_bic <- rowMeans(list.cbind(lapply(ic_bic_step4, function(df) df[,"bic"])))
pi_hat_opt_bic <- sort(unique(selection_probability_bic))[which(pi_hat_bic == min(pi_hat_bic))]; pi_hat_opt_bic

pi_hat_aic <- rowMeans(list.cbind(lapply(ic_aic_step4, function(df) df[,"bic"])))
pi_hat_opt_aic <- sort(unique(selection_probability_aic))[which(pi_hat_aic == min(pi_hat_aic))]; pi_hat_opt_aic

pi_hat_mcp <- rowMeans(list.cbind(lapply(ic_mcp_step4, function(df) df[,"bic"])))
pi_hat_opt_mcp <- sort(unique(selection_probability_mcp))[which(pi_hat_mcp == min(pi_hat_mcp))]; pi_hat_opt_mcp

## STEP 4.3 : determine stable set
variables_optimal_bic <- names(selection_probability_bic)[selection_probability_bic >= pi_hat_opt_bic]
variables_optimal_aic <- names(selection_probability_aic)[selection_probability_aic >= pi_hat_opt_aic]
variables_optimal_mcp <- names(selection_probability_mcp)[selection_probability_mcp >= pi_hat_opt_mcp]

vars_all <- unlist(list(variables_optimal_bic, variables_optimal_aic, variables_optimal_mcp))
vars_sel_twice <- unique(vars_all[duplicated(vars_all)]); vars_sel_twice
vars_sel_once <- unique(vars_all); vars_sel_once

round(cbind(b_aMIRL_bic[sort(vars_sel_twice)], b_aMIRL_aic[sort(vars_sel_twice)], b_aMIRL_mcp[sort(vars_sel_twice)]), 3)
# round(cbind(b_aMIRL_bic[vars_sel_once], b_aMIRL_aic[vars_sel_once], b_aMIRL_mcp[vars_sel_once]), 3)

## -----


## -----
# STEP 5: EVALUATION
## -----
## compute performance measures
if(model_variant == paste0("balanced_aMIRL_", target_variable_short)) {
  r_sq_within_ls_bic <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                        y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                        stable_set = variables_optimal_bic,
                                                                        coef_vec = b_aMIRL_bic,
                                                                        n_FE = n_FE)))
  r_sq_within_ls_aic <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                        y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                        stable_set = variables_optimal_aic,
                                                                        coef_vec = b_aMIRL_aic,
                                                                        n_FE = n_FE)))
  r_sq_within_ls_mcp <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                        y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                        stable_set = variables_optimal_mcp,
                                                                        coef_vec = b_aMIRL_mcp,
                                                                        n_FE = n_FE)))
  r_sq_within_average_bic <- colMeans(r_sq_within_ls_bic)
  r_sq_within_average_aic <- colMeans(r_sq_within_ls_aic)
  r_sq_within_average_mcp <- colMeans(r_sq_within_ls_mcp)
  
  r_sq_overall_ls_bic <- list.rbind(lapply(1:M, function(m) compute_r_sq(y = as.matrix(y_data_not_time_dem_not_bootstr_ls[[m]]),
                                                                         x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y_time_means = as.matrix(y_time_means_ls[[m]]),
                                                                         stable_set = variables_optimal_bic,
                                                                         coef_vec = b_aMIRL_bic,
                                                                         n_FE = n_FE)))
  r_sq_overall_ls_aic <- list.rbind(lapply(1:M, function(m) compute_r_sq(y = as.matrix(y_data_not_time_dem_not_bootstr_ls[[m]]),
                                                                         x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y_time_means = as.matrix(y_time_means_ls[[m]]),
                                                                         stable_set = variables_optimal_aic,
                                                                         coef_vec = b_aMIRL_aic,
                                                                         n_FE = n_FE)))
  r_sq_overall_ls_mcp <- list.rbind(lapply(1:M, function(m) compute_r_sq(y = as.matrix(y_data_not_time_dem_not_bootstr_ls[[m]]),
                                                                         x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y_time_means = as.matrix(y_time_means_ls[[m]]),
                                                                         stable_set = variables_optimal_mcp,
                                                                         coef_vec = b_aMIRL_mcp,
                                                                         n_FE = n_FE)))
  
  r_sq_overall_average_bic <- colMeans(r_sq_overall_ls_bic)
  r_sq_overall_average_aic <- colMeans(r_sq_overall_ls_aic)
  r_sq_overall_average_mcp <- colMeans(r_sq_overall_ls_mcp)
  
} else {
  r_sq_overall_ls_bic <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                         stable_set = variables_optimal_bic,
                                                                         coef_vec = b_aMIRL_bic,
                                                                         n_FE = n_FE)))
  r_sq_overall_ls_aic <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                         stable_set = variables_optimal_aic,
                                                                         coef_vec = b_aMIRL_aic,
                                                                         n_FE = n_FE)))
  r_sq_overall_ls_mcp <- list.rbind(lapply(1:M, function(m) compute_r_sq(x = as.matrix(x_data_not_bootstr_ls[[m]]),
                                                                         y = as.matrix(y_data_not_bootstr_ls[[m]]),
                                                                         stable_set = variables_optimal_mcp,
                                                                         coef_vec = b_aMIRL_mcp,
                                                                         n_FE = n_FE)))
  
  r_sq_overall_average_bic <- colMeans(r_sq_overall_ls_bic)
  r_sq_overall_average_aic <- colMeans(r_sq_overall_ls_aic)
  r_sq_overall_average_mcp <- colMeans(r_sq_overall_ls_mcp)
}

objects_to_save <- ls(all.names = TRUE)
objects_to_save <- objects_to_save[-which(startsWith(objects_to_save, "x_"))]
objects_to_save <- objects_to_save[-which(startsWith(objects_to_save, "y_"))]
objects_to_save <- objects_to_save[-which(objects_to_save %in% c("current_path", "objects_to_save", "target_variables"))]

# load(paste0("../aMIRL-output/results_balanced_aMIRL_OSS.RData"))
save(list = objects_to_save, file = paste0("../aMIRL-output/results_", model_variant, ".RData"))
# load(paste0("../aMIRL-output/results_", model_variant, ".RData"))

print(paste0("End of computations for ", model_variant, ": ", Sys.time()))
}
