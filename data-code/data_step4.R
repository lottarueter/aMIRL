## Remove implausible values

# 1: Inspect and potentially remove values <0 and >1
# 2: Remove variables with now >50% missing
# 3: Save data set

# Lotta Rüter
# lotta.rueter@kit.edu

## -----
library(rstudioapi)
library(dplyr)
library(openxlsx)

# clear environment
rm(list = ls())

# set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

# balanced or unbalanced panel?
panel_version <- "balanced"
# panel_version <- "unbalanced"

# load cleaned data, output from step 3
data_finsoc_selected <- read.xlsx(paste0("../data-files/data_finsoc_", panel_version, "_clean_social_financial.xlsx"))
data_finsoc_to_inspect <- data_finsoc_selected %>% select(-all_of(c("As.of.Date", "Currency", "Fiscal.Year", "MFI.ID", "MFI.Name", "Period.Type")))

## -----
# 1: Inspect and potentially remove values <0 and >1
## -----
# inspect (i) min and (ii) max values wrt plausibility
if(panel_version == "balanced") {
  # (i) min values
  min_values <- sapply(data_finsoc_to_inspect, function(col) min(na.omit(col)))
  min_inspect <- data.frame("variable" = names(which(min_values < 0)),
                            "implausible" = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
                            "reason" = c("Liabilities and borrowings can be negative",
                                         "Equity can be negative",
                                         "Equity can be negative",
                                         "",
                                         "Liabilities can be negative",
                                         "Liabilities and assets can be negative",
                                         "Equity can be negative",
                                         "ROA can be negative",
                                         "ROE can be negative",
                                         "Capital can be negative",
                                         "Debt to equity can be negative",
                                         "Financial expense can be negative",
                                         "Potential tax repayments",
                                         "Can be negative",
                                         "Profit margin negative in case of overall loss",
                                         "Impairment losses can be negative",
                                         "Less money is repaid than lended and/or inflation is greater than nominal yield",
                                         "More loans recovered than written off",
                                         "",
                                         "Decision not to pay back individuals / organisations with overpaid accounts"))
  
  # remove implausible values (replace them with NA)
  for(i in 1:sum(min_inspect$implausible)) {
    variable <- min_inspect$variable[which(min_inspect$implausible==1)[i]]
    data_finsoc_selected[which(data_finsoc_selected[,variable]<0), variable] <- NA
  }
  
  # (ii) max values
  max_values <- sapply(data_finsoc_to_inspect, function(col) max(na.omit(col)))
  max_inspect <- data.frame("variable" = names(which(max_values > 1)),
                            "implausible" = NA,
                            "reason" = NA)
                            
  max_inspect <- data.frame("variable" = names(which(max_values > 1)),
                            "implausible" = c(rep(0,21), rep(1,2), rep(0,11), rep(1,10), rep(0,9), 1, rep(0,2), 1, rep(0,11), 1, 1, 0, 0, 1),
                            "reason" = c(rep("Not limited by 1", times = 21),
                                         rep("", times = 2),
                                         rep("Not limited by 1", times = 11),
                                         rep("", times = 10),
                                         rep("Not limited by 1", times = 9),
                                         "",
                                         rep("Not limited by 1", times = 2),
                                         "",
                                         rep("Not limited by 1", times = 11),
                                         rep("",2),
                                         rep("Not limited by 1", times = 2),
                                         ""))

  # remove implausible values (replace them with NA)
  for(i in 1:sum(max_inspect$implausible)) {
    variable <- max_inspect$variable[which(max_inspect$implausible==1)[i]]
    data_finsoc_selected[which(data_finsoc_selected[,variable]>1), variable] <- NA
  }
} else if(panel_version == "unbalanced") {
  # (i) min values
  min_values <- sapply(data_finsoc_to_inspect, function(col) min(na.omit(col)))
  min_inspect <- data.frame("variable" = names(which(min_values < 0)),
                            "implausible" = c(0,0,0,1,0,0,1,rep(0,25),1,0),
                            "reason" = c("Liabilities and borrowings can be negative",
                                         "Equity can be negative",
                                         "Equity can be negative",
                                         "",
                                         "Liabilities can be negative",
                                         "Liabilities and assets can be negative",
                                         "",
                                         "Equity can be negative",
                                         "Equity can be negative",
                                         "Liabilities can be negative",
                                         "OSS can be negative",
                                         "ROA can be negative",
                                         "ROE can be negative",
                                         "Capital can be negative",
                                         "Debt to equity can be negative",
                                         "Financial expense can be negative",
                                         "Repayment of penalty fees",
                                         "Interest expense can be negative",
                                         "Potential tax repayments",
                                         rep("Expenses an be negative, e.g. via repayments or refund", 9),
                                         "Profit margin negative in case of overall loss",
                                         "Impairment losses can be negative",
                                         "Less money is repaid than lended and/or inflation is greater than nominal yield",
                                         "More loans recovered than written off",
                                         "",
                                         "Decision not to pay back individuals / organisations with overpaid accounts"))
  
  # remove implausible values (replace them with NA)
  for(i in 1:sum(min_inspect$implausible)) {
    variable <- min_inspect$variable[which(min_inspect$implausible==1)[i]]
    data_finsoc_selected[which(data_finsoc_selected[,variable]<0), variable] <- NA
  }
  
  # (ii) max values
  max_values <- sapply(data_finsoc_to_inspect, function(col) max(na.omit(col)))
  max_inspect <- data.frame("variable" = names(which(max_values > 1)),
                            "implausible" = NA,
                            "reason" = NA)
  
  max_inspect <- data.frame("variable" = names(which(max_values > 1)),
                            "implausible" = c(rep(0,21), rep(1,4), rep(0,11), rep(1,12), rep(0,10), 1, rep(0,2), rep(1,2), rep(0,19), rep(1,3), 0, 1, 0, 1),
                            "reason" = c(rep("Not limited by 1", times = 21),
                                         rep("", times = 4),
                                         rep("Not limited by 1", times = 11),
                                         rep("", times = 12),
                                         rep("Not limited by 1", times = 10),
                                         "",
                                         rep("Not limited by 1", times = 2),
                                         rep("",2),
                                         rep("Not limited by 1", times = 19),
                                         rep("",3),
                                         "Not limited by 1",
                                         "",
                                         "Not limited by 1",
                                         ""))
  
  # remove implausible values (replace them with NA)
  for(i in 1:sum(max_inspect$implausible)) {
    variable <- max_inspect$variable[which(max_inspect$implausible==1)[i]]
    data_finsoc_selected[which(data_finsoc_selected[,variable]>1), variable] <- NA
  }
}


## -----
# 2: Remove variables with now >50% missing
## -----
# remove variables that through the process of transformation now have more than 50% missing observations
if(panel_version == "balanced") {
  ind_variables_too_many_NAs <- which(colSums(is.na(data_finsoc_selected)) > (nrow(data_finsoc_selected)/2))
  data_finsoc_selected <- data_finsoc_selected[, -ind_variables_too_many_NAs] # remove "Governance.&.HR.>.Human.resource.policies.in.place" and "Governance.&.HR.>.SPM.champion.and/or.SPM.committee.on.board"
} else if(panel_version == "unbalanced") {
  # for unbalanced panel, remove the same variables as for balanced panel
  data_finsoc_selected <- data_finsoc_selected[,-which(colnames(data_finsoc_selected) %in% c("Governance.&.HR.>.Human.resource.policies.in.place", "Governance.&.HR.>.SPM.champion.and/or.SPM.committee.on.board"))]
  # check which variables with >50% missing remain and how much of the observations are missing
  ind_variables_too_many_NAs <- which(colSums(is.na(data_finsoc_selected)) > (nrow(data_finsoc_selected)/2))
  colSums(is.na(data_finsoc_selected[,ind_variables_too_many_NAs]))/nrow(data_finsoc_selected) # only slightly > 50% -> keep those variables to use same variables as in balanced panel
  # remove Profit Margin outlier (72000)
  max_profit_margin <- max(na.omit(data_finsoc_selected$`Revenue.&.Expenses.>.Profit.margin`))
  data_finsoc_selected$`Revenue.&.Expenses.>.Profit.margin`[which(data_finsoc_selected$`Revenue.&.Expenses.>.Profit.margin` == max_profit_margin)] <- NA
}

## -----
# 3: Save data set
## -----
write.xlsx(data_finsoc_selected, file = paste0("../data-files/data_finsoc_", panel_version, "_clean_social_financial_plausible.xlsx"))