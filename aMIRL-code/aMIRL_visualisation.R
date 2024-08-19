## EVALUATION OF aMIRL RESULTS

## clear environment
rm(list = ls())

## install packages
# install.packages("quantreg") # required to install rqpd
# install.packages("rqpd", repos="http://r-Forge.r-project.org")

## load packages
library(dplyr)
library(readxl)
library(rqpd) # regression quantiles
library(bcaboot) # bootstrap confidence intervals
library(rlist)
library(stringr)
library(kableExtra) # latex tables

## set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

## reload these before creating any of the below tables
target_variables <- c("OSS", "logNAB", "NAB", "neglogALBG", "negALBG")
panel_version <- c("balanced", "balanced", "unbalanced")
MIRL_version <- c("aMIRL", "MIRL", "MIRL")

model_variants <- paste(rep(panel_version, each=5), rep(MIRL_version, each=5), rep(target_variables, 3), sep = "_")

## compute 25%, 50% and 75% quantile estimates for aMIRL estimates
## -------------------------------
for(model in model_variants[1:5]){
  print(paste0("Compute quantile estimates for model variant: ", model))
  
  panel_version <- ifelse(grepl("unbalanced", model), "unbalanced", "balanced")
  aMIRL_variant <- ifelse(grepl("aMIRL", model), "aMIRL", "MIRL")
  
  n_years <- 6
  
  ## load data and results files
  load(file = paste0("../aMIRL-output/data_imputed_not_standardized_ls_", panel_version, "_", aMIRL_variant, ".RData"))
  load(paste0("../aMIRL-output/results_", model, ".RData"))
  
  ## variables selected once or twice
  variables_optimal_all <- c(variables_optimal_bic, variables_optimal_aic, variables_optimal_mcp)
  variables_optimal_unique <- sort(unique(variables_optimal_all))
  print(paste0("Number of variables selected at least once: ", length(variables_optimal_unique), ", n_bic: ", length(variables_optimal_bic),  ", n_aic: ", length(variables_optimal_aic),  ", n_mcp: ", length(variables_optimal_mcp)))
  
  variables_optimal_sel_twice <- sort(unique(variables_optimal_all[duplicated(variables_optimal_all)]))
  print(paste0("Number of variables selected at least by two different final models: ", length(variables_optimal_sel_twice)))
  
  ## compute quantile estimates for model with fixed effects
  fixed_effects <- as.factor(rep(1:n_FE, each = n_years))
  formulae <- lapply(list(variables_optimal_bic, variables_optimal_aic, variables_optimal_mcp),
                     function(regressor_variables) as.formula(paste(make.names(target_variable), " ~ ", paste(make.names(regressor_variables), collapse = " + "), "-1", " | fixed_effects")))
  
  ## replace ">" in variable names for rqpd
  data_imputed_bootstrapped_ls_renamed <- lapply(data_imputed_bootstrapped_ls, function(data_ls) {
    names(data_ls) <- make.names(names(data_ls))
    return(data_ls)})
  
  ## we use lambda = 0 (fixed effects not penalized), default options: taus = 1:3/4 (0.25, 0.5, 0.75), tauw = c(0.25, 0.5, 0.25)
  coef_rq_bic <- lapply(data_imputed_bootstrapped_ls_renamed, function(data_ls) coef.rqpd(rqpd(formulae[[1]], panel = panel(method = "pfe", lambda = 0), data = data_ls, control = list(tmpmax = 100000))))
  coef_rq_aic <- lapply(data_imputed_bootstrapped_ls_renamed, function(data_ls) coef.rqpd(rqpd(formulae[[2]], panel = panel(method = "pfe", lambda = 0), data = data_ls, control = list(tmpmax = 100000))))
  coef_rq_mcp <- lapply(data_imputed_bootstrapped_ls_renamed, function(data_ls) coef.rqpd(rqpd(formulae[[3]], panel = panel(method = "pfe", lambda = 0), data = data_ls, control = list(tmpmax = 100000))))
  
  ## average coefficient estimates
  b_rq_bic <- rowMeans(list.cbind(coef_rq_bic))
  b_rq_aic <- rowMeans(list.cbind(coef_rq_aic))
  b_rq_mcp <- rowMeans(list.cbind(coef_rq_mcp))
  
  b_rq_bic <- data.frame("Q1" = b_rq_bic[1:length(variables_optimal_bic)],
                         "Q2" = b_rq_bic[(length(variables_optimal_bic)+1):(2*length(variables_optimal_bic))],
                         "Q3" = b_rq_bic[(2*length(variables_optimal_bic)+1):(3*length(variables_optimal_bic))])
  b_rq_aic <- data.frame("Q1" = b_rq_aic[1:length(variables_optimal_aic)],
                         "Q2" = b_rq_aic[(length(variables_optimal_aic)+1):(2*length(variables_optimal_aic))],
                         "Q3" = b_rq_aic[(2*length(variables_optimal_aic)+1):(3*length(variables_optimal_aic))])
  b_rq_mcp <- data.frame("Q1" = b_rq_mcp[1:length(variables_optimal_mcp)],
                         "Q2" = b_rq_mcp[(length(variables_optimal_mcp)+1):(2*length(variables_optimal_mcp))],
                         "Q3" = b_rq_mcp[(2*length(variables_optimal_mcp)+1):(3*length(variables_optimal_mcp))])
  
  rownames(b_rq_bic) <- variables_optimal_bic
  rownames(b_rq_aic) <- variables_optimal_aic
  rownames(b_rq_mcp) <- variables_optimal_mcp
  
  save(coef_rq_bic, coef_rq_aic, coef_rq_mcp, b_rq_bic, b_rq_aic, b_rq_mcp, file = paste0("../aMIRL-output/rq_estimates_", model, ".RData"))
  cat("\n\n")
}

## compute 1%, 5% and 10% confidence intervals
## -------------------------------
for(model in model_variants){
  print(paste0("Estimate significance levels for model variant: ", model))
  
  panel_version <- ifelse(grepl("unbalanced", model), "unbalanced", "balanced")
  aMIRL_variant <- ifelse(grepl("aMIRL", model), "aMIRL", "MIRL")
  
  ## load data and results files
  # load(file = paste0("../aMIRL-output/data_imputed_not_standardized_ls_", panel_version, "_", aMIRL_variant, ".RData"))
  load(paste0("../aMIRL-output/results_", model, ".RData"))
  nvars_vec <- 1:length(b_aMIRL_aic) # remove intercept
  
  set.seed(0) # for reproducibility of bcajack results
  
  bootstr_lim_bic <- lapply(nvars_vec, function(variable) bcajack(b_aMIRL_matrix_bic[variable,], 1000, func = mean, m = 1000, alpha = c(0.005, 0.025, 0.05)))
  bootstr_lim_aic <- lapply(nvars_vec, function(variable) bcajack(b_aMIRL_matrix_aic[variable,], 1000, func = mean, m = 1000, alpha = c(0.005, 0.025, 0.05)))
  bootstr_lim_mcp <- lapply(nvars_vec, function(variable) bcajack(b_aMIRL_matrix_mcp[variable,], 1000, func = mean, m = 1000, alpha = c(0.005, 0.025, 0.05)))

  ## add significance levels
  signif.01_bic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_bic[[variable]]$lims["0.005", "bca"]) == sign(bootstr_lim_bic[[variable]]$lims["0.995", "bca"])))
  signif.05_bic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_bic[[variable]]$lims["0.025", "bca"]) == sign(bootstr_lim_bic[[variable]]$lims["0.975", "bca"])))
  signif.10_bic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_bic[[variable]]$lims["0.05", "bca"]) == sign(bootstr_lim_bic[[variable]]$lims["0.95", "bca"])))
  
  signif.01_aic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_aic[[variable]]$lims["0.005", "bca"]) == sign(bootstr_lim_aic[[variable]]$lims["0.995", "bca"])))
  signif.05_aic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_aic[[variable]]$lims["0.025", "bca"]) == sign(bootstr_lim_aic[[variable]]$lims["0.975", "bca"])))
  signif.10_aic <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_aic[[variable]]$lims["0.05", "bca"]) == sign(bootstr_lim_aic[[variable]]$lims["0.95", "bca"])))
  
  signif.01_mcp <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_mcp[[variable]]$lims["0.005", "bca"]) == sign(bootstr_lim_mcp[[variable]]$lims["0.995", "bca"])))
  signif.05_mcp <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_mcp[[variable]]$lims["0.025", "bca"]) == sign(bootstr_lim_mcp[[variable]]$lims["0.975", "bca"])))
  signif.10_mcp <- unlist(lapply(nvars_vec, function(variable) sign(bootstr_lim_mcp[[variable]]$lims["0.05", "bca"]) == sign(bootstr_lim_mcp[[variable]]$lims["0.95", "bca"])))
  
  signif_bic <- cbind(signif.01_bic, signif.05_bic, signif.10_bic)
  signif_aic <- cbind(signif.01_aic, signif.05_aic, signif.10_aic)
  signif_mcp <- cbind(signif.01_mcp, signif.05_mcp, signif.10_mcp)
  
  sum_signif_bic <- rowSums(signif_bic)
  sum_signif_aic <- rowSums(signif_aic)
  sum_signif_mcp <- rowSums(signif_mcp)
  
  stars_bic <- ifelse(sum_signif_bic == 3, "", ifelse(sum_signif_bic == 2, "^{**}", ifelse(sum_signif_bic == 1, "^{*}", "^\\circ")))
  stars_aic <- ifelse(sum_signif_aic == 3, "", ifelse(sum_signif_aic == 2, "^{**}", ifelse(sum_signif_aic == 1, "^{*}", "^\\circ")))
  stars_mcp <- ifelse(sum_signif_mcp == 3, "", ifelse(sum_signif_mcp == 2, "^{**}", ifelse(sum_signif_mcp == 1, "^{*}", "^\\circ")))
  
  names(stars_bic) <- names(stars_aic) <- names(stars_mcp) <- names(b_aMIRL_bic)
  
  save(bootstr_lim_bic, bootstr_lim_aic, bootstr_lim_mcp, signif_bic, signif_aic, signif_mcp, stars_bic, stars_aic, stars_mcp,
       file = paste0("../aMIRL-output/signif_estimates_", model, ".RData"))
  cat("\n\n")
}

## -------------------------------
## tables for main body of section 4
## -------------------------------
for(model in model_variants[c(1,2,4,5)]){
  print(paste0("Build in-text result table for model variant: ", model))

  panel_version <- ifelse(grepl("unbalanced", model), "unbalanced", "balanced")
  aMIRL_variant <- ifelse(grepl("aMIRL", model), "aMIRL", "MIRL")

  ## load quantile regression estimates and results files
  load(paste0("../aMIRL-output/rq_estimates_", model, ".RData"))
  load(paste0("../aMIRL-output/results_", model, ".RData"))
  load(paste0("../aMIRL-output/signif_estimates_", model, ".RData"))

  ## variables selected once or twice
  variables_optimal_all <- c(variables_optimal_bic, variables_optimal_aic, variables_optimal_mcp)
  variables_optimal_unique <- sort(unique(variables_optimal_all))
  print(paste0("Number of variables selected at least once: ", length(variables_optimal_unique), ", n_bic: ", length(variables_optimal_bic),  ", n_aic: ", length(variables_optimal_aic),  ", n_mcp: ", length(variables_optimal_mcp)))

  variables_optimal_sel_twice <- sort(unique(variables_optimal_all[duplicated(variables_optimal_all)]))
  print(paste0("Number of variables selected at least by two different final models: ", length(variables_optimal_sel_twice)))

  ## create tables in text of section 4
  categories <- read_excel(paste0("../data-files/variable_names_categories.xls"))
  text_table <- merge(categories[, c("Category", "Variable_name_short", "Variable")],
                      data.frame(Variable = variables_optimal_sel_twice,
                                 pi_hat = format(round(selection_probability_aic[variables_optimal_sel_twice], digits = 3), nsmall = 3),
                                 b_aMIRL = paste0(format(round(b_aMIRL_aic[variables_optimal_sel_twice], digits = 3), nsmall = 3),
                                                  ifelse(stars_aic[variables_optimal_sel_twice] == "^{***}", "", stars_aic[variables_optimal_sel_twice])),
                                 b_aMIRL_exact = b_aMIRL_aic[variables_optimal_sel_twice],
                                 negative_effect = (b_aMIRL_aic[variables_optimal_sel_twice] < 0),
                                 b_rq = b_rq_aic[variables_optimal_sel_twice,],
                                 signs = sign(b_rq_aic[variables_optimal_sel_twice,]),
                                 equal_signs = sign(b_aMIRL_aic[variables_optimal_sel_twice]) == sign(b_rq_aic[variables_optimal_sel_twice,])),
                      by = "Variable") %>%
    arrange(desc(abs(b_aMIRL_exact)), Category) %>%
    arrange(Category)

  ## highlight 3 largest effects by bold
  largest_effects <- which(abs(text_table$b_aMIRL_exact) %in% tail(sort(abs(text_table$b_aMIRL_exact)),3))
  text_table$Variable_name_short[largest_effects] <- paste0("\\textbf{", text_table$Variable_name_short[largest_effects], "}")
  text_table$pi_hat[largest_effects] <- paste0("\\mathbf{", text_table$pi_hat[largest_effects], "}")
  text_table$b_aMIRL[largest_effects] <- paste0("\\mathbf{", text_table$b_aMIRL[largest_effects], "}")

  ## show numbers in math (equation) style
  text_table <- text_table %>%
    mutate(pi_hat = paste0("$", pi_hat, "$"),
           b_aMIRL = paste0("$", b_aMIRL, "$"))

  ## add signs of quantile estimates where at least one of them differs from the aMIRL estimate
  sign_differs <- rowSums(cbind(text_table$equal_signs.Q1, text_table$equal_signs.Q2, text_table$equal_signs.Q3))!=3
  not_in_final_model <- which(is.na(sign_differs))
  text_table$pi_hat[not_in_final_model] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat[not_in_final_model], "}")
  text_table$b_aMIRL[not_in_final_model] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL[not_in_final_model], "}")

  sign_indicator.Q1 <- ifelse(text_table$signs.Q1 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator.Q2 <- ifelse(text_table$signs.Q2 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator.Q3 <- ifelse(text_table$signs.Q3 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicators <- paste0(sign_indicator.Q1, sign_indicator.Q2, sign_indicator.Q3)

  text_table$b_aMIRL[which(sign_differs)] <- paste0(text_table$b_aMIRL[which(sign_differs)], sign_indicators[which(sign_differs)])

  ## color negative values red
  text_table$b_aMIRL[text_table$negative_effect] <- paste0("\\textcolor{red}{", text_table$b_aMIRL[text_table$negative_effect], "}")

  ## color variable names of different categories
  category_colors <- c("babyblue!70", "babyblue!45", "babyblue!20", "gray!40", "orange!20", "orange!45", "orange!70")
  for (cat in 1:7) {
    text_table$Variable_name_short[which(text_table$Category == cat)] <- paste0("\\colorbox{", category_colors[cat], "}{", text_table$Variable_name_short[which(text_table$Category == cat)], "}")
  }

  ## select variables
  text_table <- text_table[, c("Variable_name_short", "pi_hat", "b_aMIRL")]

  ## make two columns
  n_vars <- length(variables_optimal_sel_twice)
  if (ceiling(n_vars/2) == n_vars/2) { # even number of variables
    text_table <- cbind(text_table[1:ceiling(n_vars/2),], text_table[(ceiling(n_vars/2)+1):n_vars,])
  } else { # uneven number of variables
    text_table <- cbind(text_table[1:ceiling(n_vars/2),], rbind(text_table[(ceiling(n_vars/2)+1):n_vars,], rep(" ", 3)))
  }

  ## make latex table
  latex_text_table <- text_table %>%
    kable(caption = paste0("Empirical selection probabilities and aMIRL estimates of variables that are included in at least two of the final models with target variable ", target_variable_short),
          format = "latex",
          position = "htb",
          col.names = rep(c("\\textbf{Variable}", "$\\hat{\\pi}_\\text{aMIRL}^{AIC}$", "$b_\\text{aMIRL}^{AIC}$"), 2),
          align = rep(c("l", "c", "c"), 2),
          booktabs = TRUE,
          escape = FALSE,
          linesep = "")
  print(latex_text_table)
  cat("\n\n")
}

## -------------------------------
## aMIRL tables
## -------------------------------
for(model in model_variants[c(1,2,4,5)]){
  print(paste0("Build in-text result table for model variant: ", model))
  
  panel_version <- ifelse(grepl("unbalanced", model), "unbalanced", "balanced")
  aMIRL_variant <- ifelse(grepl("aMIRL", model), "aMIRL", "MIRL")
  
  ## load quantile regression estimates, significance estimates and results files
  load(paste0("../aMIRL-output/rq_estimates_", model, ".RData"))
  load(paste0("../aMIRL-output/results_", model, ".RData"))
  load(paste0("../aMIRL-output/signif_estimates_", model, ".RData"))
  
  ## variables selected once or twice
  variables_optimal_all <- c(variables_optimal_bic, variables_optimal_aic, variables_optimal_mcp)
  variables_optimal_unique <- sort(unique(variables_optimal_all))
  print(paste0("Number of variables selected at least once: ", length(variables_optimal_unique), ", n_bic: ", length(variables_optimal_bic),  ", n_aic: ", length(variables_optimal_aic),  ", n_mcp: ", length(variables_optimal_mcp)))
  
  ## create tables in text of section 4
  categories <- read_excel(paste0("../data-files/variable_names_categories.xls"))
  text_table <- merge(categories[, c("Category", "Variable_name_short", "Variable")], 
                      data.frame(Variable = variables_optimal_unique,
                            
                                 pi_hat_bic = format(round(selection_probability_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_bic = paste0(format(round(b_aMIRL_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                      ifelse(stars_bic[variables_optimal_unique] == "^{***}", "", stars_bic[variables_optimal_unique])),
                                 b_aMIRL_exact_bic = b_aMIRL_bic[variables_optimal_unique],
                                 sd_b_aMIRL_bic = format(round(apply(b_aMIRL_matrix_bic, 1, sd)[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_bic = (b_aMIRL_bic[variables_optimal_unique] < 0),
                                 b_rq_bic = b_rq_bic[variables_optimal_unique,],
                                 signs_bic = sign(b_rq_bic[variables_optimal_unique,]),
                                 equal_signs_bic = sign(b_aMIRL_bic[variables_optimal_unique]) == sign(b_rq_bic[variables_optimal_unique,]),
                                 
                                 pi_hat_aic = format(round(selection_probability_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_aic = paste0(format(round(b_aMIRL_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                             ifelse(stars_aic[variables_optimal_unique] == "^{***}", "", stars_aic[variables_optimal_unique])),
                                 b_aMIRL_exact_aic = b_aMIRL_aic[variables_optimal_unique],
                                 sd_b_aMIRL_aic = format(round(apply(b_aMIRL_matrix_aic, 1, sd)[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_aic = (b_aMIRL_aic[variables_optimal_unique] < 0),
                                 b_rq_aic = b_rq_aic[variables_optimal_unique,],
                                 signs_aic = sign(b_rq_aic[variables_optimal_unique,]),
                                 equal_signs_aic = sign(b_aMIRL_aic[variables_optimal_unique]) == sign(b_rq_aic[variables_optimal_unique,]),
                                 
                                 pi_hat_mcp = format(round(selection_probability_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_mcp = paste0(format(round(b_aMIRL_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                                      ifelse(stars_mcp[variables_optimal_unique] == "^{***}", "", stars_mcp[variables_optimal_unique])),
                                 b_aMIRL_exact_mcp = b_aMIRL_mcp[variables_optimal_unique],
                                 sd_b_aMIRL_mcp = format(round(apply(b_aMIRL_matrix_mcp, 1, sd)[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_mcp = (b_aMIRL_mcp[variables_optimal_unique] < 0),
                                 b_rq_mcp = b_rq_mcp[variables_optimal_unique,],
                                 signs_mcp = sign(b_rq_mcp[variables_optimal_unique,]),
                                 equal_signs_mcp = sign(b_aMIRL_mcp[variables_optimal_unique]) == sign(b_rq_mcp[variables_optimal_unique,])),
                      by = "Variable") %>%
    arrange(Variable_name_short, Category) %>% # sort according to category and alphabetically
    arrange(Category)
  
  ## show numbers in math (equation) style
  text_table <- text_table %>%
    mutate(pi_hat_bic = paste0("$", pi_hat_bic, "$"),
           b_aMIRL_bic = paste0("$", b_aMIRL_bic, "$"),
           sd_b_aMIRL_bic = paste0("$", sd_b_aMIRL_bic, "$"),
           pi_hat_aic = paste0("$", pi_hat_aic, "$"),
           b_aMIRL_aic = paste0("$", b_aMIRL_aic, "$"),
           sd_b_aMIRL_aic = paste0("$", sd_b_aMIRL_aic, "$"),
           pi_hat_mcp = paste0("$", pi_hat_mcp, "$"),
           b_aMIRL_mcp = paste0("$", b_aMIRL_mcp, "$"),
           sd_b_aMIRL_mcp = paste0("$", sd_b_aMIRL_mcp, "$"))
  
  ## colour not selected variables gray
  not_selected_bic <- which(!(text_table$Variable %in% variables_optimal_bic))
  text_table$pi_hat_bic[not_selected_bic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_bic[not_selected_bic], "}")
  text_table$b_aMIRL_bic[not_selected_bic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_bic[not_selected_bic], "}")
  text_table$sd_b_aMIRL_bic[not_selected_bic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_bic[not_selected_bic], "}")
  
  not_selected_aic <- which(!(text_table$Variable %in% variables_optimal_aic))
  text_table$pi_hat_aic[not_selected_aic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_aic[not_selected_aic], "}")
  text_table$b_aMIRL_aic[not_selected_aic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_aic[not_selected_aic], "}")
  text_table$sd_b_aMIRL_aic[not_selected_aic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_aic[not_selected_aic], "}")
  
  not_selected_mcp <- which(!(text_table$Variable %in% variables_optimal_mcp))
  text_table$pi_hat_mcp[not_selected_mcp] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_mcp[not_selected_mcp], "}")
  text_table$b_aMIRL_mcp[not_selected_mcp] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_mcp[not_selected_mcp], "}")
  text_table$sd_b_aMIRL_mcp[not_selected_mcp] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_mcp[not_selected_mcp], "}")
  
  ## add signs of quantile estimates where at least one of them differs from the aMIRL estimate
  sign_differs_bic <- rowSums(cbind(text_table$equal_signs_bic.Q1, text_table$equal_signs_bic.Q2, text_table$equal_signs_bic.Q3))!=3
  not_in_final_model_bic <- which(is.na(sign_differs_bic))
  
  sign_indicator_bic.Q1 <- ifelse(text_table$signs_bic.Q1 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_bic.Q2 <- ifelse(text_table$signs_bic.Q2 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_bic.Q3 <- ifelse(text_table$signs_bic.Q3 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicators_bic <- paste0(sign_indicator_bic.Q1, sign_indicator_bic.Q2, sign_indicator_bic.Q3)
  
  text_table$b_aMIRL_bic[which(sign_differs_bic)] <- paste0(text_table$b_aMIRL_bic[which(sign_differs_bic)], sign_indicators_bic[which(sign_differs_bic)])
  
  sign_differs_aic <- rowSums(cbind(text_table$equal_signs_aic.Q1, text_table$equal_signs_aic.Q2, text_table$equal_signs_aic.Q3))!=3
  not_in_final_model_aic <- which(is.na(sign_differs_aic))
  
  sign_indicator_aic.Q1 <- ifelse(text_table$signs_aic.Q1 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_aic.Q2 <- ifelse(text_table$signs_aic.Q2 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_aic.Q3 <- ifelse(text_table$signs_aic.Q3 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicators_aic <- paste0(sign_indicator_aic.Q1, sign_indicator_aic.Q2, sign_indicator_aic.Q3)
  
  text_table$b_aMIRL_aic[which(sign_differs_aic)] <- paste0(text_table$b_aMIRL_aic[which(sign_differs_aic)], sign_indicators_aic[which(sign_differs_aic)])
  
  
  sign_differs_mcp <- rowSums(cbind(text_table$equal_signs_mcp.Q1, text_table$equal_signs_mcp.Q2, text_table$equal_signs_mcp.Q3))!=3
  not_in_final_model_mcp <- which(is.na(sign_differs_mcp))
  
  sign_indicator_mcp.Q1 <- ifelse(text_table$signs_mcp.Q1 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_mcp.Q2 <- ifelse(text_table$signs_mcp.Q2 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicator_mcp.Q3 <- ifelse(text_table$signs_mcp.Q3 == -1, "\\textcolor{red}{$^{\\bullet}$}", "\\textcolor{junglegreen}{$^{\\bullet}$}")
  sign_indicators_mcp <- paste0(sign_indicator_mcp.Q1, sign_indicator_mcp.Q2, sign_indicator_mcp.Q3)
  
  text_table$b_aMIRL_mcp[which(sign_differs_mcp)] <- paste0(text_table$b_aMIRL_mcp[which(sign_differs_mcp)], sign_indicators_mcp[which(sign_differs_mcp)])
  
  ## color negative values red
  text_table$b_aMIRL_bic[text_table$negative_effect_bic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_bic[text_table$negative_effect_bic], "}")
  text_table$b_aMIRL_aic[text_table$negative_effect_aic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_aic[text_table$negative_effect_aic], "}")
  text_table$b_aMIRL_mcp[text_table$negative_effect_mcp] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_mcp[text_table$negative_effect_mcp], "}")
  
  ## color variable names of different categories
  category_colors <- c("babyblue!70", "babyblue!45", "babyblue!20", "gray!40", "orange!20", "orange!45", "orange!70")
  for (cat in 1:7) {
    text_table$Variable_name_short[which(text_table$Category == cat)] <- paste0("\\colorbox{", category_colors[cat], "}{", text_table$Variable_name_short[which(text_table$Category == cat)], "}")
  }
  
  ## select variables
  text_table <- text_table[, c("Variable_name_short", "pi_hat_bic", "b_aMIRL_bic", "sd_b_aMIRL_bic", "pi_hat_aic", "b_aMIRL_aic", "sd_b_aMIRL_aic", "pi_hat_mcp", "b_aMIRL_mcp", "sd_b_aMIRL_mcp")]
  
  ## make latex table
  latex_text_table <- text_table %>%
    kable(caption = paste0("Summary of results of aMIRL estimation on balanced panel for target variable ", target_variable_short),
          format = "latex",
          position = "htb",
          col.names = c("\\textbf{Variable}",
                        "$\\hat{\\pi}_\\text{aMIRL}^{BIC}$", "$b_\\text{aMIRL}^{BIC}$", "sd$_{b_\\text{aMIRL}}^{BIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL}^{AIC}$", "$b_\\text{aMIRL}^{AIC}$", "sd$_{b_\\text{aMIRL}}^{AIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL}^{MCP}$", "$b_\\text{aMIRL}^{MCP}$", "sd$_{b_\\text{aMIRL}}^{MCP}$"),
          align = c("l", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}"),
          booktabs = TRUE,
          longtable = TRUE,
          escape = FALSE,
          linesep = "") %>%
    kable_styling(latex_options = c("repeat_header"), font_size = 7)
  print(latex_text_table)
  cat("\n\n")
  
  # add optimal threshold pi_hat*, (adjusted) overall and within R squared manually
  print(paste0("Optimal threshold $\\hat{\\pi}^*$ & \\multicolumn{3}{c}{$", format(round(pi_hat_opt_bic, digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(pi_hat_opt_aic, digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(pi_hat_opt_mcp, digits = 3), nsmall = 3), "$} \\"))
  print(paste0("Overall $R_\text{av.}^2$ (adjusted $\bar{R}_\text{av.}^2$) & \\multicolumn{3}{c}{$", format(round(r_sq_overall_average_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_mcp[2], digits = 3), nsmall = 3), ")$}\\"))
  print(paste0("Within $R_\text{av.}^2$ (adjusted $\bar{R}_\text{av.}^2$) & \\multicolumn{3}{c}{$", format(round(r_sq_within_average_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_average_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_within_average_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_average_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_within_average_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_average_mcp[2], digits = 3), nsmall = 3), ")$}\\"))
  
  # print("Optimal threshold"); print(format(round(c(pi_hat_opt_bic, pi_hat_opt_aic, pi_hat_opt_mcp), digits = 3), nsmall = 3))
  # print("Overall R^2"); print(format(round(c(r_sq_overall_average_bic, r_sq_overall_average_aic, r_sq_overall_average_mcp), digits = 3), nsmall = 3))
  # print("Within R^2"); print(format(round(c(r_sq_within_average_bic, r_sq_within_average_aic, r_sq_within_average_mcp), digits = 3), nsmall = 3))
  cat("\n\n\n\n")
}


## -------------------------------
## combined MIRL tables (balanced and unbalanced data) for appendix - horizontal format!
## -------------------------------
## load relevant data
for(tar_var in c("OSS", "logNAB", "neglogALBG")){
  print(paste0("Build MIRL results table for target variable: ", tar_var))
  
  for(pan_vers in c("balanced", "unbalanced")) {
    model <- paste(pan_vers, "MIRL", tar_var, sep = "_")
    
    ## load results files
    load(paste0("../aMIRL-output/results_", model, ".RData"))
    load(paste0("../aMIRL-output/signif_estimates_", model, ".RData"))
    
    if(pan_vers == "balanced") {
      variables_optimal_balanced_bic <- variables_optimal_bic
      variables_optimal_balanced_aic <- variables_optimal_aic
      variables_optimal_balanced_mcp <- variables_optimal_mcp
      
      thresholds_balanced <- c(pi_hat_opt_bic, pi_hat_opt_aic, pi_hat_opt_mcp)
      
      r_sq_overall_average_balanced_bic <- r_sq_overall_average_bic
      r_sq_overall_average_balanced_aic <- r_sq_overall_average_aic
      r_sq_overall_average_balanced_mcp <- r_sq_overall_average_mcp
      
      pi_hat_balanced_bic <- selection_probability_bic
      b_aMIRL_balanced_bic <- b_aMIRL_bic
      sd_b_aMIRL_balanced_bic <- apply(b_aMIRL_matrix_bic, 1, sd)
      
      pi_hat_balanced_aic <- selection_probability_aic
      b_aMIRL_balanced_aic <- b_aMIRL_aic
      sd_b_aMIRL_balanced_aic <- apply(b_aMIRL_matrix_aic, 1, sd)
      
      pi_hat_balanced_mcp <- selection_probability_mcp
      b_aMIRL_balanced_mcp <- b_aMIRL_mcp
      sd_b_aMIRL_balanced_mcp <- apply(b_aMIRL_matrix_mcp, 1, sd)
      
      stars_balanced_bic <- stars_bic
      stars_balanced_aic <- stars_aic
      stars_balanced_mcp <- stars_mcp
      
      opt_thresh_balanced <- c(pi_hat_opt_bic, pi_hat_opt_aic, pi_hat_opt_mcp)
      overall_r_squared_balanced <- c(r_sq_overall_average_bic, r_sq_overall_average_aic, r_sq_overall_average_mcp)
    } else {
      variables_optimal_unbalanced_bic <- variables_optimal_bic
      variables_optimal_unbalanced_aic <- variables_optimal_aic
      variables_optimal_unbalanced_mcp <- variables_optimal_mcp
      
      thresholds_unbalanced <- c(pi_hat_opt_bic, pi_hat_opt_aic, pi_hat_opt_mcp)
      
      r_sq_overall_average_unbalanced_bic <- r_sq_overall_average_bic
      r_sq_overall_average_unbalanced_aic <- r_sq_overall_average_aic
      r_sq_overall_average_unbalanced_mcp <- r_sq_overall_average_mcp
      
      pi_hat_unbalanced_bic <- selection_probability_bic
      b_aMIRL_unbalanced_bic <- b_aMIRL_bic
      sd_b_aMIRL_unbalanced_bic <- apply(b_aMIRL_matrix_bic, 1, sd)
      
      pi_hat_unbalanced_aic <- selection_probability_aic
      b_aMIRL_unbalanced_aic <- b_aMIRL_aic
      sd_b_aMIRL_unbalanced_aic <- apply(b_aMIRL_matrix_aic, 1, sd)
      
      pi_hat_unbalanced_mcp <- selection_probability_mcp
      b_aMIRL_unbalanced_mcp <- b_aMIRL_mcp
      sd_b_aMIRL_unbalanced_mcp <- apply(b_aMIRL_matrix_mcp, 1, sd)
      
      stars_unbalanced_bic <- stars_bic
      stars_unbalanced_aic <- stars_aic
      stars_unbalanced_mcp <- stars_mcp
      
      opt_thresh_unbalanced <- c(pi_hat_opt_bic, pi_hat_opt_aic, pi_hat_opt_mcp)
      overall_r_squared_unbalanced <- c(r_sq_overall_average_bic, r_sq_overall_average_aic, r_sq_overall_average_mcp)
    }
  }
  
  variables_optimal_all <- c(variables_optimal_balanced_bic, variables_optimal_balanced_aic, variables_optimal_balanced_mcp,
                             variables_optimal_unbalanced_bic, variables_optimal_unbalanced_aic, variables_optimal_unbalanced_mcp)
  variables_optimal_unique <- unique(variables_optimal_all)
  
  print(paste0("Number of variables selected at least once: ", length(variables_optimal_unique),
               ", n_balanced_bic: ", length(variables_optimal_balanced_bic), ", n_balanced_aic: ", length(variables_optimal_balanced_aic),  ", n_balanced_mcp: ", length(variables_optimal_balanced_mcp),
               ", n_unbalanced_bic: ", length(variables_optimal_unbalanced_bic),  ", n_unbalanced_aic: ", length(variables_optimal_unbalanced_aic),  ", n_unbalanced_mcp: ", length(variables_optimal_unbalanced_mcp)))
  
  variables_optimal_sel_twice <- sort(unique(variables_optimal_all[duplicated(variables_optimal_all)]))
  print(paste0("Number of variables selected at least by two different final models: ", length(variables_optimal_sel_twice)))
  
  ## create table
  categories <- read_excel(paste0("../data-files/variable_names_categories.xls"))
  text_table <- merge(categories[, c("Category", "Variable_name_short", "Variable")], 
                      data.frame(Variable = variables_optimal_unique,
                                 
                                 pi_hat_balanced_bic = format(round(pi_hat_balanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_balanced_bic = paste0(format(round(b_aMIRL_balanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                               ifelse(stars_balanced_bic[variables_optimal_unique] == "^{***}", "", stars_balanced_bic[variables_optimal_unique])),
                                 b_aMIRL_exact_balanced_bic = b_aMIRL_bic[variables_optimal_unique],
                                 sd_b_aMIRL_balanced_bic = format(round(sd_b_aMIRL_balanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_balanced_bic = (b_aMIRL_balanced_bic[variables_optimal_unique] < 0),
                                 
                                 pi_hat_balanced_aic = format(round(pi_hat_balanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_balanced_aic = paste0(format(round(b_aMIRL_balanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                               ifelse(stars_balanced_aic[variables_optimal_unique] == "^{***}", "", stars_balanced_aic[variables_optimal_unique])),
                                 b_aMIRL_exact_balanced_aic = b_aMIRL_aic[variables_optimal_unique],
                                 sd_b_aMIRL_balanced_aic = format(round(sd_b_aMIRL_balanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_balanced_aic = (b_aMIRL_balanced_aic[variables_optimal_unique] < 0),
                                 
                                 pi_hat_balanced_mcp = format(round(pi_hat_balanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_balanced_mcp = paste0(format(round(b_aMIRL_balanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                                               ifelse(stars_balanced_mcp[variables_optimal_unique] == "^{***}", "", stars_balanced_mcp[variables_optimal_unique])),
                                 b_aMIRL_exact_balanced_mcp = b_aMIRL_mcp[variables_optimal_unique],
                                 sd_b_aMIRL_balanced_mcp = format(round(sd_b_aMIRL_balanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_balanced_mcp = (b_aMIRL_balanced_mcp[variables_optimal_unique] < 0),
                                 
                                 pi_hat_unbalanced_bic = format(round(pi_hat_unbalanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_unbalanced_bic = paste0(format(round(b_aMIRL_unbalanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                                 ifelse(stars_unbalanced_bic[variables_optimal_unique] == "^{***}", "", stars_unbalanced_bic[variables_optimal_unique])),
                                 b_aMIRL_exact_unbalanced_bic = b_aMIRL_bic[variables_optimal_unique],
                                 sd_b_aMIRL_unbalanced_bic = format(round(sd_b_aMIRL_unbalanced_bic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_unbalanced_bic = (b_aMIRL_unbalanced_bic[variables_optimal_unique] < 0),
                                 
                                 pi_hat_unbalanced_aic = format(round(pi_hat_unbalanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_unbalanced_aic = paste0(format(round(b_aMIRL_unbalanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                                               ifelse(stars_unbalanced_aic[variables_optimal_unique] == "^{***}", "", stars_unbalanced_aic[variables_optimal_unique])),
                                 b_aMIRL_exact_unbalanced_aic = b_aMIRL_aic[variables_optimal_unique],
                                 sd_b_aMIRL_unbalanced_aic = format(round(sd_b_aMIRL_unbalanced_aic[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_unbalanced_aic = (b_aMIRL_unbalanced_aic[variables_optimal_unique] < 0),
                                 
                                 pi_hat_unbalanced_mcp = format(round(pi_hat_unbalanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                 b_aMIRL_unbalanced_mcp = paste0(format(round(b_aMIRL_unbalanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                                                 ifelse(stars_unbalanced_mcp[variables_optimal_unique] == "^{***}", "", stars_unbalanced_mcp[variables_optimal_unique])),
                                 
                                 b_aMIRL_exact_unbalanced_mcp = b_aMIRL_mcp[variables_optimal_unique],
                                 sd_b_aMIRL_unbalanced_mcp = format(round(sd_b_aMIRL_unbalanced_mcp[variables_optimal_unique], digits = 3), nsmall = 3),
                                 negative_effect_unbalanced_mcp = (b_aMIRL_unbalanced_mcp[variables_optimal_unique] < 0)),
                      by = "Variable") %>%
    arrange(Variable_name_short, Category) %>% # sort according to category and alphabetically
    arrange(Category)
  
  ## show numbers in math (equation) style
  text_table <- text_table %>%
    mutate(pi_hat_balanced_bic = paste0("$", pi_hat_balanced_bic, "$"),
           b_aMIRL_balanced_bic = paste0("$", b_aMIRL_balanced_bic, "$"),
           sd_b_aMIRL_balanced_bic = paste0("$", sd_b_aMIRL_balanced_bic, "$"),
           pi_hat_balanced_aic = paste0("$", pi_hat_balanced_aic, "$"),
           b_aMIRL_balanced_aic = paste0("$", b_aMIRL_balanced_aic, "$"),
           sd_b_aMIRL_balanced_aic = paste0("$", sd_b_aMIRL_balanced_aic, "$"),
           pi_hat_balanced_mcp = paste0("$", pi_hat_balanced_mcp, "$"),
           b_aMIRL_balanced_mcp = paste0("$", b_aMIRL_balanced_mcp, "$"),
           sd_b_aMIRL_balanced_mcp = paste0("$", sd_b_aMIRL_balanced_mcp, "$"),
           
           pi_hat_unbalanced_bic = paste0("$", pi_hat_unbalanced_bic, "$"),
           b_aMIRL_unbalanced_bic = paste0("$", b_aMIRL_unbalanced_bic, "$"),
           sd_b_aMIRL_unbalanced_bic = paste0("$", sd_b_aMIRL_unbalanced_bic, "$"),
           pi_hat_unbalanced_aic = paste0("$", pi_hat_unbalanced_aic, "$"),
           b_aMIRL_unbalanced_aic = paste0("$", b_aMIRL_unbalanced_aic, "$"),
           sd_b_aMIRL_unbalanced_aic = paste0("$", sd_b_aMIRL_unbalanced_aic, "$"),
           pi_hat_unbalanced_mcp = paste0("$", pi_hat_unbalanced_mcp, "$"),
           b_aMIRL_unbalanced_mcp = paste0("$", b_aMIRL_unbalanced_mcp, "$"),
           sd_b_aMIRL_unbalanced_mcp = paste0("$", sd_b_aMIRL_unbalanced_mcp, "$"))
  
  ## colour not selected variables gray
  not_selected_balanced_bic <- which(!(text_table$Variable %in% variables_optimal_balanced_bic))
  text_table$pi_hat_balanced_bic[not_selected_balanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_balanced_bic[not_selected_balanced_bic], "}")
  text_table$b_aMIRL_balanced_bic[not_selected_balanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_balanced_bic[not_selected_balanced_bic], "}")
  text_table$sd_b_aMIRL_balanced_bic[not_selected_balanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_balanced_bic[not_selected_balanced_bic], "}")
  
  not_selected_balanced_aic <- which(!(text_table$Variable %in% variables_optimal_balanced_aic))
  text_table$pi_hat_balanced_aic[not_selected_balanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_balanced_aic[not_selected_balanced_aic], "}")
  text_table$b_aMIRL_balanced_aic[not_selected_balanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_balanced_aic[not_selected_balanced_aic], "}")
  text_table$sd_b_aMIRL_balanced_aic[not_selected_balanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_balanced_aic[not_selected_balanced_aic], "}")
  
  not_selected_balanced_mcp <- which(!(text_table$Variable %in% variables_optimal_balanced_mcp))
  text_table$pi_hat_balanced_mcp[not_selected_balanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_balanced_mcp[not_selected_balanced_mcp], "}")
  text_table$b_aMIRL_balanced_mcp[not_selected_balanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_balanced_mcp[not_selected_balanced_mcp], "}")
  text_table$sd_b_aMIRL_balanced_mcp[not_selected_balanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_balanced_mcp[not_selected_balanced_mcp], "}")
  
  not_selected_unbalanced_bic <- which(!(text_table$Variable %in% variables_optimal_unbalanced_bic))
  text_table$pi_hat_unbalanced_bic[not_selected_unbalanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_unbalanced_bic[not_selected_unbalanced_bic], "}")
  text_table$b_aMIRL_unbalanced_bic[not_selected_unbalanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_unbalanced_bic[not_selected_unbalanced_bic], "}")
  text_table$sd_b_aMIRL_unbalanced_bic[not_selected_unbalanced_bic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_unbalanced_bic[not_selected_unbalanced_bic], "}")
  
  not_selected_unbalanced_aic <- which(!(text_table$Variable %in% variables_optimal_unbalanced_aic))
  text_table$pi_hat_unbalanced_aic[not_selected_unbalanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_unbalanced_aic[not_selected_unbalanced_aic], "}")
  text_table$b_aMIRL_unbalanced_aic[not_selected_unbalanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_unbalanced_aic[not_selected_unbalanced_aic], "}")
  text_table$sd_b_aMIRL_unbalanced_aic[not_selected_unbalanced_aic] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_unbalanced_aic[not_selected_unbalanced_aic], "}")
  
  not_selected_unbalanced_mcp <- which(!(text_table$Variable %in% variables_optimal_unbalanced_mcp))
  text_table$pi_hat_unbalanced_mcp[not_selected_unbalanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$pi_hat_unbalanced_mcp[not_selected_unbalanced_mcp], "}")
  text_table$b_aMIRL_unbalanced_mcp[not_selected_unbalanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$b_aMIRL_unbalanced_mcp[not_selected_unbalanced_mcp], "}")
  text_table$sd_b_aMIRL_unbalanced_mcp[not_selected_unbalanced_mcp] <- paste0("\\textcolor{gray!70}{", text_table$sd_b_aMIRL_unbalanced_mcp[not_selected_unbalanced_mcp], "}")
  
  ## color negative values red
  text_table$b_aMIRL_balanced_bic[text_table$negative_effect_balanced_bic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_balanced_bic[text_table$negative_effect_balanced_bic], "}")
  text_table$b_aMIRL_balanced_aic[text_table$negative_effect_balanced_aic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_balanced_aic[text_table$negative_effect_balanced_aic], "}")
  text_table$b_aMIRL_balanced_mcp[text_table$negative_effect_balanced_mcp] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_balanced_mcp[text_table$negative_effect_balanced_mcp], "}")
  
  text_table$b_aMIRL_unbalanced_bic[text_table$negative_effect_unbalanced_bic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_unbalanced_bic[text_table$negative_effect_unbalanced_bic], "}")
  text_table$b_aMIRL_unbalanced_aic[text_table$negative_effect_unbalanced_aic] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_unbalanced_aic[text_table$negative_effect_unbalanced_aic], "}")
  text_table$b_aMIRL_unbalanced_mcp[text_table$negative_effect_unbalanced_mcp] <- paste0("\\textcolor{red}{", text_table$b_aMIRL_unbalanced_mcp[text_table$negative_effect_unbalanced_mcp], "}")
  
  ## color variable names of different categories
  category_colors <- c("babyblue!70", "babyblue!45", "babyblue!20", "gray!40", "orange!20", "orange!45", "orange!70")
  for (cat in 1:7) {
    text_table$Variable_name_short[which(text_table$Category == cat)] <- paste0("\\colorbox{", category_colors[cat], "}{", text_table$Variable_name_short[which(text_table$Category == cat)], "}")
  }
  
  ## select variables
  text_table <- text_table[, c("Variable_name_short",
                               "pi_hat_balanced_bic", "b_aMIRL_balanced_bic", "sd_b_aMIRL_balanced_bic",
                               "pi_hat_balanced_aic", "b_aMIRL_balanced_aic", "sd_b_aMIRL_balanced_aic",
                               "pi_hat_balanced_mcp", "b_aMIRL_balanced_mcp", "sd_b_aMIRL_balanced_mcp",
                               
                               "pi_hat_unbalanced_bic", "b_aMIRL_unbalanced_bic", "sd_b_aMIRL_unbalanced_bic",
                               "pi_hat_unbalanced_aic", "b_aMIRL_unbalanced_aic", "sd_b_aMIRL_unbalanced_aic",
                               "pi_hat_unbalanced_mcp", "b_aMIRL_unbalanced_mcp", "sd_b_aMIRL_unbalanced_mcp")]
  
  ## make latex table
  latex_text_table <- text_table %>%
    kable(caption = paste0("Summary of results of aMIRL estimation on balanced panel for target variable ", target_variable_short),
          format = "latex",
          position = "htb",
          col.names = c("\\textbf{Variable}",
                        "$\\hat{\\pi}_\\text{aMIRL, unb.}^{BIC}$", "$b_\\text{aMIRL, unb.}^{BIC}$", "sd$_{b_\\text{aMIRL, unb.}}^{BIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL, bal.}^{AIC}$", "$b_\\text{aMIRL, bal.}^{AIC}$", "sd$_{b_\\text{aMIRL, bal.}}^{AIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL, bal.}^{MCP}$", "$b_\\text{aMIRL, bal.}^{MCP}$", "sd$_{b_\\text{aMIRL, bal.}}^{MCP}$",
                        "$\\hat{\\pi}_\\text{aMIRL, unb.}^{BIC}$", "$b_\\text{aMIRL, unb.}^{BIC}$", "sd$_{b_\\text{aMIRL, unb.}}^{BIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL, unb.}^{AIC}$", "$b_\\text{aMIRL, unb.}^{AIC}$", "sd$_{b_\\text{aMIRL, unb.}}^{AIC}$",
                        "$\\hat{\\pi}_\\text{aMIRL, unb.}^{MCP}$", "$b_\\text{aMIRL, unb.}^{MCP}$", "sd$_{b_\\text{aMIRL, unb.}}^{MCP}$"),
          align = c("l", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}"),
          booktabs = TRUE,
          longtable = TRUE,
          escape = FALSE,
          linesep = "") %>%
    kable_styling(latex_options = c("repeat_header"), font_size = 7)
  print(latex_text_table)
  cat("\n\n")
  
  # add optimal threshold pi_hat*, (adjusted) overall and within R squared manually
  
  # add optimal threshold pi_hat*, (adjusted) overall and within R squared manually
  print(paste0("Optimal threshold $\\hat{\\pi}^*$ & \\multicolumn{3}{c}{$", format(round(thresholds_balanced[1], digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(thresholds_balanced[2], digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(thresholds_balanced[3], digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(thresholds_unbalanced[1], digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(thresholds_unbalanced[2], digits = 3), nsmall = 3),
               "$} & \\multicolumn{3}{c}{$", format(round(thresholds_unbalanced[3], digits = 3), nsmall = 3), "$}\\"))
  print(paste0("Overall $R_\text{av.}^2$ (adjusted $\bar{R}_\text{av.}^2$) & \\multicolumn{3}{c}{$", format(round(r_sq_overall_average_balanced_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_balanced_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_balanced_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_balanced_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_balanced_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_balanced_mcp[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_unbalanced_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_unbalanced_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_unbalanced_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_unbalanced_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{3}{c}{$",
               format(round(r_sq_overall_average_unbalanced_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_average_unbalanced_mcp[2], digits = 3), nsmall = 3), ")$}\\"))
  cat("\n\n\n\n")
}

## -------------------------------
## combined lasso tables (balanced, FE; balanced, pooled; unbalanced, pooled) for appendix - horizontal format!
## -------------------------------
## load relevant data
for(tar_var in c("OSS", "logNAB", "neglogALBG")){
  # use normal standard errors
  print(paste0("Build lasso results table for target variable: ", tar_var))
  
  for(pan_vers in c("balanced", "unbalanced")) {
    if(pan_vers == "balanced") {
      for(model_var in c("FE", "pooled_OLS")) {
        load(paste0("../aMIRL-output/simple_lasso/results_balanced_lasso_", model_var, "_", tar_var, ".RData"))
        if(model_var == "FE"){
          lm_balanced_FE_bic <- summary(lm_bic)$coefficients
          lm_balanced_FE_aic <- summary(lm_aic)$coefficients
          lm_balanced_FE_mcp <- summary(lm_mcp)$coefficients
          
          variables_balanced_FE_bic <- rownames(lm_balanced_FE_bic)
          variables_balanced_FE_aic <- rownames(lm_balanced_FE_aic)
          variables_balanced_FE_mcp <- rownames(lm_balanced_FE_mcp)
          
          r_sq_overall_balanced_FE_bic <- r_sq_overall_bic
          r_sq_overall_balanced_FE_aic <- r_sq_overall_aic
          r_sq_overall_balanced_FE_mcp <- r_sq_overall_mcp
          
          r_sq_within_balanced_FE_bic <- r_sq_within_bic
          r_sq_within_balanced_FE_aic <- r_sq_within_aic
          r_sq_within_balanced_FE_mcp <- r_sq_within_mcp
        } else{
          lm_balanced_pooled_bic <- summary(lm_bic)$coefficients
          lm_balanced_pooled_aic <- summary(lm_aic)$coefficients
          lm_balanced_pooled_mcp <- summary(lm_mcp)$coefficients
          
          variables_balanced_pooled_bic <- str_remove_all(rownames(lm_balanced_pooled_bic), "`")
          variables_balanced_pooled_aic <- str_remove_all(rownames(lm_balanced_pooled_aic), "`")
          variables_balanced_pooled_mcp <- str_remove_all(rownames(lm_balanced_pooled_mcp), "`")
          
          r_sq_overall_balanced_pooled_bic <- r_sq_overall_bic
          r_sq_overall_balanced_pooled_aic <- r_sq_overall_aic
          r_sq_overall_balanced_pooled_mcp <- r_sq_overall_mcp
        }
      }
    } else {
      load(paste0("../aMIRL-output/simple_lasso/results_unbalanced_lasso_pooled_OLS_", tar_var, ".RData"))
      
      lm_unbalanced_pooled_bic <- summary(lm_bic)$coefficients
      lm_unbalanced_pooled_aic <- summary(lm_aic)$coefficients
      lm_unbalanced_pooled_mcp <- summary(lm_mcp)$coefficients
      
      variables_unbalanced_pooled_bic <- str_remove_all(rownames(lm_unbalanced_pooled_bic), "`")
      variables_unbalanced_pooled_aic <- str_remove_all(rownames(lm_unbalanced_pooled_aic), "`")
      variables_unbalanced_pooled_mcp <- str_remove_all(rownames(lm_unbalanced_pooled_mcp), "`")
      
      r_sq_overall_unbalanced_pooled_bic <- r_sq_overall_bic
      r_sq_overall_unbalanced_pooled_aic <- r_sq_overall_aic
      r_sq_overall_unbalanced_pooled_mcp <- r_sq_overall_mcp
    }
  }
  variables_optimal_all <- c(variables_balanced_FE_bic, variables_balanced_FE_aic, variables_balanced_FE_mcp,
                             variables_balanced_pooled_bic, variables_balanced_pooled_aic, variables_balanced_pooled_mcp, 
                             variables_unbalanced_pooled_bic, variables_unbalanced_pooled_aic, variables_unbalanced_pooled_mcp)
  variables_optimal_unique <- unique(variables_optimal_all)
  
  print(paste0("Number of variables selected at least once: ", length(variables_optimal_unique),
               ", n_balanced_FE_bic: ", length(variables_balanced_FE_bic), ", n_balanced_FE_aic: ", length(variables_balanced_FE_aic),  ", n_balanced_FE_mcp: ", length(variables_balanced_FE_mcp),
               ", n_balanced_pooled_bic: ", length(variables_balanced_pooled_bic), ", n_balanced_pooled_aic: ", length(variables_balanced_pooled_aic),  ", n_balanced_pooled_mcp: ", length(variables_balanced_pooled_mcp),
               ", n_unbalanced_pooled_bic: ", length(variables_unbalanced_pooled_bic),  ", n_unbalanced_pooled_aic: ", length(variables_unbalanced_pooled_aic),  ", n_unbalanced_pooled_mcp: ", length(variables_unbalanced_pooled_mcp)))
  
  
  stars_balanced_FE_bic <- ifelse(lm_balanced_FE_bic[,4] <= 0.01, "", ifelse(lm_balanced_FE_bic[,4] <= 0.05, "^{**}", ifelse(lm_balanced_FE_bic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_balanced_FE_aic <- ifelse(lm_balanced_FE_aic[,4] <= 0.01, "", ifelse(lm_balanced_FE_aic[,4] <= 0.05, "^{**}", ifelse(lm_balanced_FE_aic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_balanced_FE_mcp <- ifelse(lm_balanced_FE_mcp[,4] <= 0.01, "", ifelse(lm_balanced_FE_mcp[,4] <= 0.05, "^{**}", ifelse(lm_balanced_FE_mcp[,4] <= 0.1, "^{*}", "^\\circ")))
  
  stars_balanced_pooled_bic <- ifelse(lm_balanced_pooled_bic[,4] <= 0.01, "", ifelse(lm_balanced_pooled_bic[,4] <= 0.05, "^{**}", ifelse(lm_balanced_pooled_bic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_balanced_pooled_aic <- ifelse(lm_balanced_pooled_aic[,4] <= 0.01, "", ifelse(lm_balanced_pooled_aic[,4] <= 0.05, "^{**}", ifelse(lm_balanced_pooled_aic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_balanced_pooled_mcp <- ifelse(lm_balanced_pooled_mcp[,4] <= 0.01, "", ifelse(lm_balanced_pooled_mcp[,4] <= 0.05, "^{**}", ifelse(lm_balanced_pooled_mcp[,4] <= 0.1, "^{*}", "^\\circ")))
  
  stars_unbalanced_pooled_bic <- ifelse(lm_unbalanced_pooled_bic[,4] <= 0.01, "", ifelse(lm_unbalanced_pooled_bic[,4] <= 0.05, "^{**}", ifelse(lm_unbalanced_pooled_bic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_unbalanced_pooled_aic <- ifelse(lm_unbalanced_pooled_aic[,4] <= 0.01, "", ifelse(lm_unbalanced_pooled_aic[,4] <= 0.05, "^{**}", ifelse(lm_unbalanced_pooled_aic[,4] <= 0.1, "^{*}", "^\\circ")))
  stars_unbalanced_pooled_mcp <- ifelse(lm_unbalanced_pooled_mcp[,4] <= 0.01, "", ifelse(lm_unbalanced_pooled_mcp[,4] <= 0.05, "^{**}", ifelse(lm_unbalanced_pooled_mcp[,4] <= 0.1, "^{*}", "^\\circ")))
  
  ## add zeros where variable was not selected but is contained in variables_optimal_all
  b_balanced_FE_bic <- paste0(format(round(lm_balanced_FE_bic[,1], digits = 3), nsmall = 3), stars_balanced_FE_bic)
  b_balanced_FE_aic <- paste0(format(round(lm_balanced_FE_aic[,1], digits = 3), nsmall = 3), stars_balanced_FE_aic)
  b_balanced_FE_mcp <- paste0(format(round(lm_balanced_FE_mcp[,1], digits = 3), nsmall = 3), stars_balanced_FE_mcp)
  
  b_balanced_pooled_bic <- paste0(format(round(lm_balanced_pooled_bic[,1], digits = 3), nsmall = 3), stars_balanced_pooled_bic)
  b_balanced_pooled_aic <- paste0(format(round(lm_balanced_pooled_aic[,1], digits = 3), nsmall = 3), stars_balanced_pooled_aic)
  b_balanced_pooled_mcp <- paste0(format(round(lm_balanced_pooled_mcp[,1], digits = 3), nsmall = 3), stars_balanced_pooled_mcp)
  
  b_unbalanced_pooled_bic <- paste0(format(round(lm_unbalanced_pooled_bic[,1], digits = 3), nsmall = 3), stars_unbalanced_pooled_bic)
  b_unbalanced_pooled_aic <- paste0(format(round(lm_unbalanced_pooled_aic[,1], digits = 3), nsmall = 3), stars_unbalanced_pooled_aic)
  b_unbalanced_pooled_mcp <- paste0(format(round(lm_unbalanced_pooled_mcp[,1], digits = 3), nsmall = 3), stars_unbalanced_pooled_mcp)
  
  sd_balanced_FE_bic <- format(round(lm_balanced_FE_bic[,2], digits = 3), nsmall = 3)
  sd_balanced_FE_aic <- format(round(lm_balanced_FE_aic[,2], digits = 3), nsmall = 3)
  sd_balanced_FE_mcp <- format(round(lm_balanced_FE_mcp[,2], digits = 3), nsmall = 3)
  
  sd_balanced_pooled_bic <- format(round(lm_balanced_pooled_bic[,2], digits = 3), nsmall = 3)
  sd_balanced_pooled_aic <- format(round(lm_balanced_pooled_aic[,2], digits = 3), nsmall = 3)
  sd_balanced_pooled_mcp <- format(round(lm_balanced_pooled_mcp[,2], digits = 3), nsmall = 3)
  
  sd_unbalanced_pooled_bic <- format(round(lm_unbalanced_pooled_bic[,2], digits = 3), nsmall = 3)
  sd_unbalanced_pooled_aic <- format(round(lm_unbalanced_pooled_aic[,2], digits = 3), nsmall = 3)
  sd_unbalanced_pooled_mcp <- format(round(lm_unbalanced_pooled_mcp[,2], digits = 3), nsmall = 3)
  
  names(b_balanced_FE_bic) <- names(sd_balanced_FE_bic) <- str_replace_all(rownames(lm_balanced_FE_bic), "`", "")
  names(b_balanced_FE_aic) <- names(sd_balanced_FE_aic) <- str_replace_all(rownames(lm_balanced_FE_aic), "`", "")
  names(b_balanced_FE_mcp) <- names(sd_balanced_FE_mcp) <- str_replace_all(rownames(lm_balanced_FE_mcp), "`", "")
  
  names(b_balanced_pooled_bic) <- names(sd_balanced_pooled_bic) <- str_replace_all(rownames(lm_balanced_pooled_bic), "`", "")
  names(b_balanced_pooled_aic) <- names(sd_balanced_pooled_aic) <- str_replace_all(rownames(lm_balanced_pooled_aic), "`", "")
  names(b_balanced_pooled_mcp) <- names(sd_balanced_pooled_mcp) <- str_replace_all(rownames(lm_balanced_pooled_mcp), "`", "")
  
  names(b_unbalanced_pooled_bic) <- names(sd_unbalanced_pooled_bic) <- str_replace_all(rownames(lm_unbalanced_pooled_bic), "`", "")
  names(b_unbalanced_pooled_aic) <- names(sd_unbalanced_pooled_aic) <- str_replace_all(rownames(lm_unbalanced_pooled_aic), "`", "")
  names(b_unbalanced_pooled_mcp) <- names(sd_unbalanced_pooled_mcp) <- str_replace_all(rownames(lm_unbalanced_pooled_mcp), "`", "")
  
  b_sd_table <- t(bind_rows(b_balanced_FE_bic, sd_balanced_FE_bic, b_balanced_FE_aic, sd_balanced_FE_aic, b_balanced_FE_mcp, sd_balanced_FE_mcp,
                            b_balanced_pooled_bic, sd_balanced_pooled_bic, b_balanced_pooled_aic, sd_balanced_pooled_aic, b_balanced_pooled_mcp, sd_balanced_pooled_mcp,
                            b_unbalanced_pooled_bic, sd_unbalanced_pooled_bic, b_unbalanced_pooled_aic, sd_unbalanced_pooled_aic, b_unbalanced_pooled_mcp, sd_unbalanced_pooled_mcp))

  colnames(b_sd_table) <- c("b_balanced_FE_bic", "sd_balanced_FE_bic", "b_balanced_FE_aic", "sd_balanced_FE_aic", "b_balanced_FE_mcp", "sd_balanced_FE_mcp",
                            "b_balanced_pooled_bic", "sd_balanced_pooled_bic", "b_balanced_pooled_aic", "sd_balanced_pooled_aic", "b_balanced_pooled_mcp", "sd_balanced_pooled_mcp",
                            "b_unbalanced_pooled_bic", "sd_unbalanced_pooled_bic", "b_unbalanced_pooled_aic", "sd_unbalanced_pooled_aic", "b_unbalanced_pooled_mcp", "sd_unbalanced_pooled_mcp")
  
  b_sd_table[is.na(b_sd_table)] <- 0
  rownames_b_sd_table <- rownames(b_sd_table)
  
  ## colour negative values red and values of not selected variables gray
  b_sd_table <- apply(b_sd_table, 2, function(value) ifelse(startsWith(value, '-'), paste0("\\textcolor{red}{$", value, "$}"), ifelse(value == "0", "\\textcolor{gray!70}{$0$}", paste0("$", value, "$"))))
  b_sd_table <- data.frame("Variable" = rownames_b_sd_table, b_sd_table)
  
  ## create table
  categories <- read_excel(paste0("../data-files/variable_names_categories.xls"))
  text_table <- merge(categories[, c("Category", "Variable_name_short", "Variable")], 
                      b_sd_table,
                      by = "Variable")%>%
    arrange(Variable_name_short, Category) %>% # sort according to category and alphabetically
    arrange(Category)
  
  ## color variable names of different categories
  category_colors <- c("babyblue!70", "babyblue!45", "babyblue!20", "gray!40", "orange!20", "orange!45", "orange!70")
  for (cat in 1:7) {
    text_table$Variable_name_short[which(text_table$Category == cat)] <- paste0("\\colorbox{", category_colors[cat], "}{", text_table$Variable_name_short[which(text_table$Category == cat)], "}")
  }
  
  ## select variables
  text_table <- text_table[, c("Variable_name_short",
                               "b_balanced_FE_bic", "sd_balanced_FE_bic", "b_balanced_FE_aic", "sd_balanced_FE_aic", "b_balanced_FE_mcp", "sd_balanced_FE_mcp",
                               "b_balanced_pooled_bic", "sd_balanced_pooled_bic", "b_balanced_pooled_aic", "sd_balanced_pooled_aic", "b_balanced_pooled_mcp", "sd_balanced_pooled_mcp",
                               "b_unbalanced_pooled_bic", "sd_unbalanced_pooled_bic", "b_unbalanced_pooled_aic", "sd_unbalanced_pooled_aic", "b_unbalanced_pooled_mcp", "sd_unbalanced_pooled_mcp")]
  
  ## make latex table
  latex_text_table <- text_table %>%
    kable(caption = paste0("Summary of results of two-stage lasso estimation on balanced and unbalanced panel for target variable ", target_variable_short),
          format = "latex",
          position = "htb",
          col.names = c("\\textbf{Variable}",
                        "$b_\\text{bal., FE}^{BIC}$", "sd$_{b_\\text{bal., FE}^{BIC}}$", "$b_\\text{bal., FE}^{AIC}$", "sd$_{b_\\text{bal., FE}^{AIC}}$", "$b_\\text{bal., FE}^{MCP}$", "sd$_{b_\\text{bal., FE}^{MCP}}$", 
                        "$b_\\text{bal., pooled}^{BIC}$", "sd$_{b_\\text{bal., pooled}^{BIC}}$", "$b_\\text{bal., pooled}^{AIC}$", "sd$_{b_\\text{bal., pooled}^{AIC}}$", "$b_\\text{bal., pooled}^{MCP}$", "sd$_{b_\\text{bal., pooled}^{MCP}}$", 
                        "$b_\\text{unbal., pooled}^{BIC}$", "sd$_{b_\\text{unbal., pooled}^{BIC}}$", "$b_\\text{unbal., pooled}^{AIC}$", "sd$_{b_\\text{unbal., pooled}^{AIC}}$", "$b_\\text{unbal., pooled}^{MCP}$", "sd$_{b_\\text{unbal., pooled}^{MCP}}$"),
          align = c("l", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}", "P{0.7cm}", "P{1.2cm}", "P{0.7cm}"),
          booktabs = TRUE,
          longtable = TRUE,
          escape = FALSE,
          linesep = "") %>%
    kable_styling(latex_options = c("repeat_header"), font_size = 7)
  print(latex_text_table)
  cat("\n\n")
  
  # add (adjusted) overall and within R squared manually
  print(paste0("Overall $R_\text{av.}^2$ (adjusted $\bar{R}_\text{av.}^2$) & \\multicolumn{2}{c}{$", format(round(r_sq_overall_balanced_FE_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_FE_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_balanced_FE_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_FE_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_balanced_FE_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_FE_mcp[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_balanced_pooled_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_pooled_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_balanced_pooled_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_pooled_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_balanced_pooled_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_balanced_pooled_mcp[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_unbalanced_pooled_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_unbalanced_pooled_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_unbalanced_pooled_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_unbalanced_pooled_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_overall_unbalanced_pooled_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_overall_unbalanced_pooled_mcp[2], digits = 3), nsmall = 3), ")$}\\"))
  
  print(paste0("Within $R_\text{av.}^2$ (adjusted $\bar{R}_\text{av.}^2$) & \\multicolumn{2}{c}{$", format(round(r_sq_within_balanced_FE_bic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_balanced_FE_bic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_within_balanced_FE_aic[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_balanced_FE_aic[2], digits = 3), nsmall = 3), ")$} & \\multicolumn{2}{c}{$",
               format(round(r_sq_within_balanced_FE_mcp[1], digits = 3), nsmall = 3),
               "\\;(", format(round(r_sq_within_balanced_FE_mcp[2], digits = 3), nsmall = 3),
               ")$} & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{---} & \\multicolumn{2}{c}{---}\\"))

  cat("\n\n\n\n")
}