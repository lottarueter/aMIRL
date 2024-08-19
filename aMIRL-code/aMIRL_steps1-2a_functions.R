## Functions for aMIRL imputation and bootstrapping steps 1-2a
## Author: Lotta Rüter
## lotta.rueter@kit.edu

## install.packages("REEMtree")
library(REEMtree)
library(dplyr)

## determines if x is binary or not
is_binary <- function(x) {
  x0 <- na.omit(x)
  is.numeric(x) && length(unique(x0)) %in% 1:2 && all(x0 %in% 0:1)
}

## computes mode of x
get_mode <- function(x, na.rm = TRUE) {
  if(na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## imputation function for mice using REEMtrees
mice.impute.REEMtree <- function(y, ry, x, wy = NULL, cv = TRUE, id = "MFI.ID", random = formula(~1|MFI.ID), ...) {
  if(!is.numeric(y)) {
    stop("target variable y must be numeric")
  } else if(is.null(wy)) {
    wy <- !ry
  }
  
  data_all <- data.frame(y, x)
  regressors <- colnames(x)[!(colnames(x) %in% id)]
  
  fit <- REEMtree(as.formula(paste0("y ~ ", paste(regressors, collapse=" + "))),
                  data = data_all,
                  random = random,
                  subset = ry, # observations that are used for fitting
                  cv = cv)
  
  return(predict(fit, data_all, id = data_all[, id], EstimateRandomEffects = TRUE)[wy])
}


## replaces imputed values <0 with 0 (>1 with 1) of variables that only have observed values >=0 (<=1)
limit_0_1 <- function(mice_object, original_data) {
  variables_greater_zero <- sapply(original_data, function(x) sum(na.omit(x)<0)==0)
  variables_smaller_one <- sapply(original_data, function(x) sum(na.omit(x)>1)==0)
  
  m_vec <- 1:mice_object$m
  data_imputed_ls <- list()
  zeros <- ones <- c()
  number_nas <- sum(is.na(original_data))
  number_obs <- ncol(original_data) * nrow(original_data)
  
  for(m in m_vec) {
    data_imputed_ls[[m]] <- complete(mice_object, m)
    
    zeros[m] <- sum(sapply(data_imputed_ls[[m]][,variables_greater_zero], function(x) sum(x<0)))
    ones[m] <- sum(sapply(data_imputed_ls[[m]][,variables_smaller_one], function(x) sum(x>1)))
    
    data_imputed_ls[[m]][,variables_greater_zero] <- sapply(data_imputed_ls[[m]][,variables_greater_zero], function(x) replace(x, x<0, 0))
    data_imputed_ls[[m]][,variables_smaller_one] <- sapply(data_imputed_ls[[m]][,variables_smaller_one], function(x) replace(x, x>1, 1))
  }
  
  print(paste0("# of values <0 replaced with 0 in imputed data set m=", m_vec, ": ", zeros, ", i.e. ", round(zeros/number_nas *100, 2), "% of missing observations and ", round(zeros/number_obs *100, 2), "% of total number of observations"))
  print(paste0("# of values >1 replaced with 1 in imputed data set m=", m_vec, ": ", ones, ", i.e. ", round(ones/number_nas *100, 2), "% of missing observations and ", round(zeros/number_obs *100, 2), "% of total number of observations"))
  
  data_imputed_ls
}
