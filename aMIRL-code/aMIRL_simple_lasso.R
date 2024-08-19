## Adapted Multiple Imputation Random Lasso (aMIRL)
## Steps 1-2a: Imputation, generation of bootstrap samples, standardization and time-demeaning

# 1: Compute M standardised imputed data sets via MICE
# 2: Draw B bootstrap samples from each m=1,...,M imputed data set, standardize (and time-demean)

## -----
# load packages
library(rstudioapi)
library(dplyr)
library(openxlsx)
library(glmnet)
library(stringr)

# clear environment
rm(list = ls())

# set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

# load aMIRL_functions
source("aMIRL_steps2b-4_functions.R")

# balanced or unbalanced panel?
panel_version <- "balanced"
# panel_version <- "unbalanced"

# use_fixed_effects <- TRUE
use_fixed_effects <- FALSE

# read in cleaned data
clean_data <- read.xlsx(paste0("../data-files/data_finsoc_", panel_version, "_clean_social_financial_plausible.xlsx"))
complete_cases_data <- clean_data[complete.cases(clean_data), ]
# table(complete_cases_data$MFI.ID) # relevant for balanced panel -> no MFI has complete cases for all years in 2009-2014! (count would have to be 6, max is 3!)
nrow(complete_cases_data) # 58 for balanced (non-MFI-specific), 177 for unbalanced

# remove character variables and those of no importance
clean_data <- clean_data %>%
  select(-all_of(c("As.of.Date", "Currency", "MFI.Name", "Period.Type")))

## -----
# 1: Impute missing observations naively via column means
## -----
data_imputed_w_colmeans <- data.frame(apply(X = clean_data, MARGIN = 2, FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))) # initial imputation: column means
colnames(data_imputed_w_colmeans) <- colnames(clean_data)

# add logged and negative versions of relevant variables to the data set
data_imputed_w_colmeans <- data_imputed_w_colmeans %>%
  mutate("Clients.>.log.Number.of.active.borrowers" = log(`Clients.>.Number.of.active.borrowers`),
         "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita" = log(`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`),
         "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita" = -log(`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`),
         "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita" = -`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`) %>%
  
  select(sort(names(.)))

data_standardized <- data_imputed_w_colmeans %>% select(c(MFI.ID, Fiscal.Year)) %>%
  bind_cols(scale(data_imputed_w_colmeans %>% select(-c(MFI.ID, Fiscal.Year))))

# the different target variables considered in the analysis
target_variables <- c("Financial.Performance.>.Operational.self.sufficiency",
                      "Clients.>.log.Number.of.active.borrowers",
                      "Clients.>.Number.of.active.borrowers",
                      "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                      "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita")

for(target_variable in target_variables) {
  if(target_variable == "Financial.Performance.>.Operational.self.sufficiency") {
    variables_to_remove <- c("MFI.ID",
                             "Fiscal.Year",
                             "Clients.>.log.Number.of.active.borrowers",
                             "Financial.Performance.>.Operational.self.sufficiency",
                             "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
    target_variable_short <- paste0("OSS")
  } else if(target_variable == "Clients.>.log.Number.of.active.borrowers") {
    variables_to_remove <- c("MFI.ID",
                             "Fiscal.Year",
                             "Clients.>.log.Number.of.active.borrowers",
                             "Clients.>.Number.of.active.borrowers",
                             "Clients.>.Number.of.loans.outstanding",
                             "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
    target_variable_short <- paste0("logNAB")
  } else if(target_variable == "Clients.>.Number.of.active.borrowers") {
    variables_to_remove <- c("MFI.ID",
                             "Fiscal.Year",
                             "Clients.>.log.Number.of.active.borrowers",
                             "Clients.>.Number.of.active.borrowers",
                             "Clients.>.Number.of.loans.outstanding",
                             "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
    target_variable_short <- paste0("NAB")
  } else if(target_variable == "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita") {
    variables_to_remove <- c("MFI.ID",
                             "Fiscal.Year",
                             "Clients.>.log.Number.of.active.borrowers",
                             "Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.Average.outstanding.balance./.GNI.per.capita",
                             "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
    target_variable_short <- paste0("neglogALBG")
  } else if(target_variable == "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita") {
    variables_to_remove <- c("MFI.ID",
                             "Fiscal.Year",
                             "Clients.>.log.Number.of.active.borrowers",
                             "Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.Average.outstanding.balance./.GNI.per.capita",
                             "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita",
                             "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita")
    target_variable_short <- paste0("negALBG")
  }
  
  model <- ifelse(use_fixed_effects, "FE", "pooled_OLS")
  model_variant <- paste0(panel_version, "_lasso_", model, "_", target_variable_short)
  
  if(use_fixed_effects == TRUE) { # fixed effects regression
    data_standardized_time_demeaned <- time_demean(data_standardized)
    
    # for computation of overall R^2
    y_not_time_demeaned <- data_standardized %>% select(all_of(target_variable))
    y_time_means <- time_means(data_standardized) %>% select(all_of(target_variable))
    
    x_data <- data_standardized_time_demeaned %>% select(-all_of(variables_to_remove))
    y_data <- data_standardized_time_demeaned %>% select(all_of(target_variable))
    
    n_FE <- length(unique(data_standardized$MFI.ID))
  } else { # pooled regression
    x_data <- data_standardized %>% select(-all_of(variables_to_remove))
    y_data <- data_standardized %>% select(all_of(target_variable))
    
    n_FE <- 1
  }
  
  lambda_max <- max(glmnet(x = as.matrix(x_data),
                           y = as.matrix(y_data),
                           family = "gaussian", # optimizes 1/2RSS/nobs+λ∗penalty,
                           alpha = 1, # lasso penalty: 1/2||beta_j||
                           nlambda = 100, # default value
                           standardize = FALSE, # data is already standardized
                           intercept = FALSE)$lambda) # we use time-demeaned data$lambda
  
  # formula for lambdapath taken from "Regularization Paths for Generalized Linear Models via Coordinate Descent" by Friedman, Hastie and Tibshirani (2010)
  # delta: decay parameter, .001 is suggested by the above authors
  lambdapath <- round(exp(seq(log(max(lambda_max)), log(max(lambda_max) * .001),
                              length.out = 100)), digits = 10)
  
  lasso_ols <- glmnet(x = as.matrix(x_data),
                      y = as.matrix(y_data),
                      alpha = 1, # lasso penalty
                      lambda = lambdapath,
                      intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                      grouped = FALSE,
                      parallel = TRUE,
                      family = "gaussian", # ensures correct loss function, does not require normally distributed data
                      standardize = FALSE,
                      relax = TRUE) # causes ols refit on active sets per lambda
  
  ## OLS-estimation of full model with all potential regressors (baseline for Mallows Cp)
  ols_full_model <- glmnet(x = as.matrix(x_data),
                           y = as.matrix(y_data),
                           lambda = 0,
                           intercept = FALSE, # already accounted for by previous standardization (and time-demeaning) of the data
                           grouped = FALSE,
                           parallel = TRUE,
                           family = "gaussian", # ensures correct loss function, does not require normally distributed data
                           standardize = FALSE) # don't need relax here since we do not regularize (lambda = 0!)
  
  ic <- ic_ols_fit(y = as.matrix(y_data),
                   x = as.matrix(x_data),
                   ols_fit = lasso_ols$relaxed,
                   ols_fit_full = ols_full_model,
                   n_FE = n_FE)
  
  ## index of lambda with minimum average optimality criterion
  ind_bic <- min(which(ic["bic",] == min(ic["bic",])))
  ind_aic <- min(which(ic["aic",] == min(ic["aic",])))
  ind_mcp <- min(which(ic["mcp",] == min(ic["mcp",])))
  
  ## respective ols estimates
  b_ols_bic <- coef(lasso_ols$relaxed)[,ind_bic]
  b_ols_aic <- coef(lasso_ols$relaxed)[,ind_aic]
  b_ols_mcp <- coef(lasso_ols$relaxed)[,ind_mcp]
  
  # selected variables
  sel_vars_bic <- names(b_ols_bic[which(b_ols_bic != 0)])
  sel_vars_aic <- names(b_ols_aic[which(b_ols_aic != 0)])
  sel_vars_mcp <- names(b_ols_mcp[which(b_ols_mcp != 0)])
  
  sel_x_bic <- as.matrix(x_data[,sel_vars_bic])
  sel_x_aic <- as.matrix(x_data[,sel_vars_aic])
  sel_x_mcp <- as.matrix(x_data[,sel_vars_mcp])
  
  # fit lm to obtain p-values
  lm_bic <- lm(unlist(y_data) ~ sel_x_bic -1)
  names(lm_bic$coefficients) <- sel_vars_bic
  
  lm_aic <- lm(unlist(y_data) ~ sel_x_aic -1)
  names(lm_aic$coefficients) <- sel_vars_aic
  
  lm_mcp <- lm(unlist(y_data) ~ sel_x_mcp -1)
  names(lm_mcp$coefficients) <- sel_vars_mcp
  
  # r squared und r squared adjusted berechnen
  if(use_fixed_effects == TRUE) {
    r_sq_overall_bic <- compute_r_sq(y = as.matrix(y_not_time_demeaned),
                                     x = as.matrix(x_data),
                                     y_time_means = y_time_means,
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_bic[which(b_ols_bic != 0)]),
                                     coef_vec = b_ols_bic)
    
    r_sq_overall_aic <- compute_r_sq(y = as.matrix(y_not_time_demeaned),
                                     x = as.matrix(x_data),
                                     y_time_means = y_time_means,
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_aic[which(b_ols_aic != 0)]),
                                     coef_vec = b_ols_aic)
    
    r_sq_overall_mcp <- compute_r_sq(y = as.matrix(y_not_time_demeaned),
                                     x = as.matrix(x_data),
                                     y_time_means = y_time_means,
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_mcp[which(b_ols_mcp != 0)]),
                                     coef_vec = b_ols_mcp)
    
    r_sq_within_bic <- compute_r_sq(y = as.matrix(y_data),
                                    x = as.matrix(x_data),
                                    n_FE = n_FE,
                                    stable_set = names(b_ols_bic[which(b_ols_bic != 0)]),
                                    coef_vec = b_ols_bic)
    
    r_sq_within_aic <- compute_r_sq(y = as.matrix(y_data),
                                    x = as.matrix(x_data),
                                    n_FE = n_FE,
                                    stable_set = names(b_ols_aic[which(b_ols_aic != 0)]),
                                    coef_vec = b_ols_aic)
    
    r_sq_within_mcp <- compute_r_sq(y = as.matrix(y_data),
                                    x = as.matrix(x_data),
                                    n_FE = n_FE,
                                    stable_set = names(b_ols_mcp[which(b_ols_mcp != 0)]),
                                    coef_vec = b_ols_mcp)
  } else {
    r_sq_overall_bic <- compute_r_sq(x = as.matrix(x_data),
                                     y = as.matrix(y_data),
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_bic[which(b_ols_bic != 0)]),
                                     coef_vec = b_ols_bic)
    
    r_sq_overall_aic <- compute_r_sq(x = as.matrix(x_data),
                                     y = as.matrix(y_data),
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_aic[which(b_ols_aic != 0)]),
                                     coef_vec = b_ols_aic)
    
    r_sq_overall_mcp <- compute_r_sq(x = as.matrix(x_data),
                                     y = as.matrix(y_data),
                                     n_FE = n_FE,
                                     stable_set = names(b_ols_mcp[which(b_ols_mcp != 0)]),
                                     coef_vec = b_ols_mcp)
  }

  save.image(file = paste0("../aMIRL-output/simple_lasso/results_", model_variant, ".RData"))
  
  }
