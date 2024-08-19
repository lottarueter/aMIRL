## Steps 1-2a of the Adapted Multiple Imputation Random Lasso (aMIRL)
# Author: Lotta Rüter
# lotta.rueter@kit.edu

# Comprises respective MIRL steps for unbalanced panel or cross-sectional data naturally
# Code additionally contains checks on (the number of) complete cases

## Steps 1-2a:
# 1: Compute M standardized imputed data sets via mice
# 2a: Draw B bootstrap samples from each m=1,...,M imputed data set, standardize

## aMIRL: for balanced panel data if aMIRL_variant == "aMIRL"
# - use REEMtree for imputation of continuous target variables and cart for binary targets
# IMPORTANT! Identifier "id" and random effects part "random" have to be specified in mice.impute.REEMtree from aMIRL_steps1-2a_functions
# in our case: id = "MFI.ID", random = formula(~1|MFI.ID)

## MIRL: for aMIRL_variant == "MIRL"
# - use classification and regression trees (cart) in imputation step

## -----
## load packages
library(rstudioapi)
library(mice)
library(dplyr)
library(openxlsx)
library(readxl)
library(REEMtree)

## clear environment
rm(list = ls())

## set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

## balanced or unbalanced panel?
panel_version <- "balanced"
# panel_version <- "unbalanced" # use cart for imputations

aMIRL_variant <- "aMIRL" # use REEMtrees and cart for imputations
# aMIRL_variant <- "MIRL" # use cart for imputations

panel_aMIRL_combination <- paste0(panel_version, "_", aMIRL_variant)

# load functions for steps 1-2a of aMIRL algorithm
source("aMIRL_steps1-2a_functions.R")

# read in cleaned data
clean_data <- read.xlsx(paste0("../data-files/data_finsoc_", panel_version, "_clean_social_financial_plausible.xlsx"))

## -----
## 1: Compute M imputed data sets via MICE
## -----
M <- 10 # number of imputed data sets
C <- 20 # number of cycles
start_seed <- 1

## 1.0: Preliminary checks & preparation steps
## check complete cases data
complete_cases_data <- clean_data[complete.cases(clean_data), ]
# table(complete_cases_data$MFI.ID) # relevant for balanced panel -> no MFI has complete cases for all years in 2009-2014! (count would have to be 6, max is 3!)
# nrow(complete_cases_data) # 58 for balanced (non-MFI-specific), 177 for unbalanced (bevor Profit Margin = 2000 entfernt wurden)

## remove character variables and irrelevant predictors
clean_data <- clean_data %>%
  select(-all_of(c("As.of.Date", "Currency", "MFI.Name", "Period.Type")))

# modify names for mice step
original_variable_names <- names(clean_data)
modified_variable_names <- gsub("[[:punct:]]+", ".", original_variable_names) # for formulae in mice procedure
names(clean_data) <- modified_variable_names

## 1.1 Compute initial imputation
## use mode for binary variables and column mean for continuous variables
binary <- sapply(clean_data, is_binary)

## amount of missingness of binary variables
# sum(is.na(clean_data[, binary]))/(ncol(clean_data[, binary]) * nrow(clean_data[, binary]))

data_imputed_median_mean <- clean_data
data_imputed_median_mean[,binary] <- sapply(data_imputed_median_mean[,binary], function(x) replace(x, is.na(x), get_mode(x))) # initial imputation for binary variables: column modes
data_imputed_median_mean <- sapply(data_imputed_median_mean, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))) # initial imputation for remaining continuous variables: column means
data_imputed_median_mean <- data.frame(data_imputed_median_mean)

## 1.2: Determine imputation methods
if(panel_version == "balanced" && aMIRL_variant == "aMIRL") {
  imputation_methods <- ifelse(binary, "cart", "REEMtree")
} else {
  imputation_methods <- "cart"
}

## 1.3: Run imputation procedure with mice
data_imputed <- mice(data = clean_data, m = M, maxit = C,
                     method = imputation_methods,
                     data.init = data_imputed_median_mean,
                     remove.collinear = FALSE,
                     seed = start_seed)

# save(data_imputed, file = paste0("../aMIRL-output/data_imputed_", panel_aMIRL_combination, ".RData"))
load(paste0("../aMIRL-output/data_imputed_", panel_aMIRL_combination, ".RData"))

## 1.4: Replace implausible values <0 and >1 with 0 and 1 and store imputed data sets in list
data_imputed_ls <- limit_0_1(data_imputed, clean_data)

for (m in 1:M){
  names(data_imputed_ls[[m]]) <- original_variable_names
  data_imputed_ls[[m]] <- data_imputed_ls[[m]] %>%
    ## add logged and negative versions of relevant variables to the data set
    mutate("Clients.>.log.Number.of.active.borrowers" = log(`Clients.>.Number.of.active.borrowers`),
           "Outreach.>.log.Average.loan.balance.per.borrower./.GNI.per.capita" = log(`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`),
           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita" = -log(`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`),
           "Outreach.>.neg.Average.loan.balance.per.borrower./.GNI.per.capita" = -`Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`) %>%
    select(sort(names(.)))
  
  ## store index variables in the first two columns of the data frame
  index_data <- data_imputed_ls[[m]] %>% select(c(MFI.ID, Fiscal.Year))
  data_imputed_ls[[m]] <- data_imputed_ls[[m]] %>% select(-c(MFI.ID, Fiscal.Year))
  data_imputed_ls[[m]] <- index_data %>% bind_cols(data_imputed_ls[[m]])
}


## -----
## 2: Draw B bootstrap samples from each m=1,...,M imputed data set, standardize (and time-demean)
## -----
## 2.1: For each generated, imputed data set generate B bootstrap samples of size of original data frame
## boostrap MFI.IDs -> "one observation" = all six observations of one MFI.ID
B <- 100 # number of bootstrap samples per imputed data set
data_imputed_bootstrapped_ls <- list()

if (panel_aMIRL_combination == "balanced_aMIRL") { # sample MFIs with replacement and include all their observations
  MFIs <- unique(data_imputed_ls[[1]]$MFI.ID)
  n_MFIs <- length(MFIs)
  
  for (m in 1:M) {
    print(paste("m = ", m))
    
    for (b in 1:B) {
      entry <- (m-1)*B+b
      set.seed(entry)
      sample_MFI <- sort(sample(MFIs, size=n_MFIs, replace=TRUE))
      data_imputed_bootstrapped_ls[[entry]] <- data.frame()
      
      for (i in 1:n_MFIs){
        data_imputed_bootstrapped_ls[[entry]] <- rbind(data_imputed_bootstrapped_ls[[entry]],
                                                       data_imputed_ls[[m]][which(data_imputed_ls[[m]]$MFI.ID == sample_MFI[i]),])
      }
    }
  }
} else { # sample with replacement from all observations
  n <- nrow(data_imputed_ls[[1]])
  
  for (m in 1:M) {
    print(paste("m = ", m))
    
    for (b in 1:B) {
      entry <- (m-1)*B+b
      set.seed(entry)
      sample_observations <- sort(sample(1:n, size=n, replace=TRUE))
      data_imputed_bootstrapped_ls[[entry]] <- data_imputed_ls[[m]][sample_observations, ]
    }
  }
}

# for computation of quantile estimates in evaluation step (tables in main body of section 4)
if (panel_aMIRL_combination == "balanced_aMIRL") {
  save(data_imputed_ls, data_imputed_bootstrapped_ls,
       file = paste0("../aMIRL-output/data_imputed_not_standardized_ls_", panel_aMIRL_combination, ".RData"))
}

## 2.2: Standardize imputed (bootstrapped) data sets
## do not scale MFI.ID and Fiscal.Year
data_imputed_standardized_ls <- lapply(data_imputed_ls, function(ls) ls %>% select(c(MFI.ID, Fiscal.Year)) %>%
                                         bind_cols(scale(ls %>% select(-c(MFI.ID, Fiscal.Year)))))
data_imputed_bootstrapped_standardized_ls <- lapply(data_imputed_bootstrapped_ls, function(ls) ls %>% select(c(MFI.ID, Fiscal.Year)) %>%
                                                      bind_cols(scale(ls %>% select(-c(MFI.ID, Fiscal.Year)))))

save(data_imputed_standardized_ls, data_imputed_bootstrapped_standardized_ls,
     file = paste0("../aMIRL-output/data_imputed_ls_", panel_aMIRL_combination, ".RData"))