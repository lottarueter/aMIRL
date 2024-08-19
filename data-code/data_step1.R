### Initial data preparations

# 1: Import and join financial and social data on MFIs
# 2: Determine optimal balanced panel (largest panel with as many years as possible)
# 3: Delete variables with >50% missing
# 4: Construct unbalanced panel with same variables as balanced panel
# 5: Save data set

# Lotta Rüter
# lotta.rueter@kit.edu

## -----
# load packages
library(rstudioapi)
library(dplyr)
library(openxlsx)

# set working directory
file_path = rstudioapi::getActiveDocumentContext()$path # get file path
setwd(dirname(file_path))

# define function to determine MFIs which have consecutive observations available in time period start_year:end_year
MFIs_available_per_window <- function(w, col_sy = col_start_year, col_ey = col_end_year) {
  col_vec <- col_sy[w]:col_ey[w]
  
  if(length(col_vec) == 1) {
    return(data_finsoc_available$MFI.ID[which(data_finsoc_available[,col_vec])])
  }
  else{
    return(data_finsoc_available$MFI.ID[which(rowSums(data_finsoc_available[,col_vec]) == length(col_vec))])
  }
}

## -----
## 1: Import and join social and financial data on MFIs
## -----
# data of financial and social performance measures
data_fin <- read.xlsx("../data-source-files/mix-market-financial-performance-dataset-in-usd.xlsx", na.strings = c("", "NA"), detectDates = TRUE)
data_soc <- read.xlsx("../data-source-files/mix-market-social-performance-dataset.xlsx", na.strings = c("", "NA"), detectDates = TRUE)

# join financial and social data
data_finsoc <- inner_join(y = data_soc, x = data_fin, by = c("MFI.ID", "Fiscal.Year", "MFI.Name", "As.of.Date", "Period.Type")) %>% 
  select(sort(tidyselect::peek_vars()))
# nrow(data_finsoc)

years_unique <- sort(unique(data_finsoc$Fiscal.Year))
n_years <- length(years_unique)
MFI.ID_unique <- sort(unique(data_finsoc$MFI.ID)); length(MFI.ID_unique)
n_MFI <- length(MFI.ID_unique)


## -----
## 2: Determine optimal balanced panel (largest panel with as many years as possible)
## -----
# create dataframe with information on whether any observations are available in certain year for selected MFI
data_finsoc_available <- setNames(data.frame("MFI.ID"= MFI.ID_unique,
                                             t(sapply(MFI.ID_unique, function(id) is.element(years_unique, subset(data_finsoc, MFI.ID == id)[,"Fiscal.Year"])))),
                                  c("MFI.ID", years_unique))
round(colSums(data_finsoc_available)/length(MFI.ID_unique), 2) # proportion of observed MFIs per year

start_year <- rep(years_unique, n_years:1)
window_size <- unlist(sapply(n_years:1, function(longest_window_length) seq(1, longest_window_length, 1)))
end_year <- (start_year + window_size - 1)
windows <- data.frame(start_year, end_year, window_size)

col_start_year <- rep(2:13, n_years:1) # 2007 = col 2, ..., 2018 = col 13
col_end_year <- col_start_year + window_size - 1

MFI.IDs_windows <- lapply(1:length(col_start_year), MFIs_available_per_window) # list of MFIs with (incomplete) observations available for all years in chosen window
no_MFIs_windows <- unlist(lapply(1:length(MFI.IDs_windows), function(w) length(MFI.IDs_windows[[w]])))

windows <- data.frame(windows, no_MFIs_windows, window_no_obs = (windows$window_size * no_MFIs_windows)) # contains information for table 1
windows_decr <- windows[order(windows$window_no_obs, decreasing = TRUE),]
# 2011-2014 contains 6 more observations than 2009-2014, but two less years, so we choose 2009-2014, i.e. row 2 of windows_decr

years_window <- windows_decr$start_year[2]:windows_decr$end_year[2] # 2011-2014 has 6 more observations, but two less years, so we choose 2009-2014 instead
MFI.IDs_window <- MFI.IDs_windows[[as.numeric(rownames(windows_decr)[2])]] # row of window 2009-2014 in dataframe "window", see rowname of windows_decr

## -----
# 3: Delete variables with >50% missing
## -----
# 3.1 join data fin and soc for selected balanced panel and previous plus following year (i.e. 2011-2014 for resp. MFIs plus 2010 and 2015)
data_finsoc_balanced <- data_finsoc %>%
  filter(MFI.ID %in% MFI.IDs_window & Fiscal.Year %in% years_window)

# 3.2 check structure of missingness
missing_balanced_absolute <- data_finsoc_balanced %>%
  group_by(Fiscal.Year) %>%
  summarise_all(~sum(is.na(.)))

missing_balanced_relative <- round(rbind(missing_balanced_absolute / length(MFI.IDs_window), colMeans(missing_balanced_absolute / length(MFI.IDs_window))), 2)
missing_balanced_relative$Fiscal.Year <- c(years_window, NA)
rownames(missing_balanced_relative) <- c(years_window, "Mean")

# 3.3 look at stats of variables in data_finsoc_balanced that are observed >50% (# obs >= threshold)
treshold <- nrow(data_finsoc_balanced) * 0.5

data_finsoc_balanced_stats_missing <-
  data.frame("n_obs" = colSums(!is.na(data_finsoc_balanced)), # number of observations per variable in balanced
             "n_obs_miss" = colSums(is.na(data_finsoc_balanced)), # number of missing observations
             "index_column" = 1:ncol(data_finsoc_balanced)) %>%  # vector containing indices (column numbers) of respective variable
  arrange(desc(n_obs))

data_finsoc_balanced_stats_keep <- data_finsoc_balanced_stats_missing %>% filter(n_obs>=treshold)
# data_finsoc_balanced_stats_rmv <- data_finsoc_balanced_stats_missing %>% filter(n_obs<treshold)
# length(rownames(data_finsoc_balanced_stats_rmv)) # number of deleted variables

data_finsoc_balanced_keep <- data_finsoc_balanced %>%
  select(rownames(data_finsoc_balanced_stats_keep))

## -----
## 4: Construct unbalanced panel with same variables as balanced panel
## -----
data_finsoc_unbalanced_keep <- data_finsoc %>%
  select(rownames(data_finsoc_balanced_stats_keep))

missing_unbalanced_absolute <- data_finsoc_unbalanced_keep %>%
  mutate(Number.MFIs.per.Year = NA) %>% # combined with sum(is.na(.)) results in the number of MFIs observed per year in the unbalanced panel
  group_by(Fiscal.Year) %>%
  summarise_all(~sum(is.na(.)))

missing_unbalanced_relative <- round(rbind(missing_unbalanced_absolute / missing_unbalanced_absolute$Number.MFIs.per.Year, colMeans(missing_unbalanced_absolute / missing_unbalanced_absolute$Number.MFIs.per.Year)), 2)
missing_unbalanced_relative <- missing_unbalanced_relative[,-which(colnames(missing_unbalanced_relative) =="Number.MFIs.per.Year")]
missing_unbalanced_relative$Fiscal.Year <- c(missing_unbalanced_absolute$Fiscal.Year, NA)
rownames(missing_unbalanced_relative) <- c(missing_unbalanced_absolute$Fiscal.Year, "Mean")

# check if some variables are missing >50%
missing_unbalanced_relative_greater50 <- missing_unbalanced_relative[, missing_unbalanced_relative[13,] > 0.5 & !is.na(missing_unbalanced_relative[13,])]
# missingness only slightly >50% -> keep these 10 variables, most will be transformed in the following steps anyway

## -----
# 5: Save data set
## -----
# xlsx preserves ">" and "(" etc. in variable names 
write.xlsx(data_finsoc_balanced_keep, file = "../data-files/data_finsoc_balanced_keep.xlsx")
write.xlsx(data_finsoc_unbalanced_keep, file = "../data-files/data_finsoc_unbalanced_keep.xlsx")