## -----
## Data Plots and Tables
## -----
# install packages
library(dplyr)
library(RColorBrewer)
library(rworldmap)
library(countrycode)
library(openxlsx)
library(readxl)
library(corrplot)
library(rlist)
library(stringr)
library(kableExtra)

# clear environment
rm(list = ls())

# set working directory
current_path = rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

# load functions from "Data_Prep_Functions"
source(paste(dirname(current_path), "/data_functions.R", sep = ""))

# company data incl. geographical information
data_comp <- read.csv("../data-source-files/MFICompanyMetaData.csv", sep=";", na.strings = c("", "NA"))

clean_data_final_balanced <- openxlsx::read.xlsx(paste0("../data-files/data_finsoc_balanced_clean_social_financial_plausible.xlsx"))
clean_data_final_unbalanced <- openxlsx::read.xlsx(paste0("../data-files/data_finsoc_unbalanced_clean_social_financial_plausible.xlsx"))

## -----
## Figure 1: World Map
## -----
## prepare balanced data
MFI.final <- data.frame("MFI.ID" = unique(clean_data_final_balanced$MFI.ID))
data_comp_final <- left_join(MFI.final, data_comp, by = "MFI.ID")
data_count_final <- data.frame(table(data_comp_final$Country))
names(data_count_final) <- c("Country", "Count")
levels(data_count_final$Country)[which(data_count_final$Country == "China, People's Republic of")] <- "China"
levels(data_count_final$Country)[which(data_count_final$Country == "Cote d'Ivoire (Ivory Coast)")] <- "Cote d'Ivoire"

## create a map-shaped window
oldPar <- par(mar=c(0,0,0,0), oma = c(0,0,0,0)) # (bottom, left, top, right)
par(oldPar)

# join to a coarse resolution map
spdf <- joinCountryData2Map(data_count_final, joinCode="NAME", nameJoinColumn="Country") # verbose = TRUE, if details on failed codes are desired
mapDevice(mai = c(0, 0, 0, 0)) # titleSpace = NULL
map <- mapCountryData(spdf, nameColumnToPlot="Count", catMethod=0:max(data_count_final$Count), mapTitle = "", colourPalette = "heat", oceanCol = "lightblue", missingCountryCol = "white", addLegend=FALSE); map
do.call(addMapLegend, c(map, legendLabel = "all", legendWidth = 1, labelFontSize = 0.8))

## -----
## Table 2: Country Table
## -----
continents_final <- countrycode(sourcevar = data_count_final[, "Country"],
                                origin = "country.name", destination = "continent")
continents_final[which(is.na(continents_final))] <- "Europe" # Kosovo
continents_final[which(continents_final == "Americas")] <- "America"

regions_final <- countrycode(sourcevar = data_count_final[, "Country"],
                             origin = "country.name", destination = "region")
data_count_final_region <- data.frame(table(data_comp_final$Region))

data_count_country_region <- data.frame(data_count_final, "Continent" = tidyr::replace_na(continents_final, "Europe"), "Region" = regions_final) %>%
  arrange(Region) %>%
  arrange(Continent)
table(data_count_country_region$Region, data_count_country_region$Continent) # countries per region and continent
table(data_count_country_region$Continent) # MFIs per continent

data_count_countreg <- data.frame(data_count_country_region) %>%
  mutate("ContReg" = paste(Continent, Region)) %>%
  select(-c("Continent", "Region")) %>%
  group_by(ContReg) %>%
  mutate(csum = cumsum(Count))
aggregate(data_count_countreg$csum, by = list(Category = data_count_countreg$ContReg), FUN = max) # MFIs per continent, region combination

## -----
## Figure 3: Correlation Plot
## -----
# standardisation does not affect correlation
load(paste0("../aMIRL-output/data_imputed_ls_balanced_aMIRL.RData"))
source("../aMIRL-code/aMIRL_steps2b-4_functions.R")

categories <- data.frame(read_excel(paste0("../data-files/variable_names_categories.xls")))

## determine 10 most highly correlated variables with OSS
cor_OSS_raw <- cor(clean_data_final_balanced %>%
                     select(-c("MFI.ID", "Currency", "MFI.Name", "As.of.Date", "Fiscal.Year", "Period.Type")),
                   use = "pairwise.complete.obs")["Financial.Performance.>.Operational.self.sufficiency", ]
names_OSS_top10_correlated <- head(names(sort(abs(cor_OSS_raw), decreasing = TRUE)), 10)
categories$Variable_name_short <- str_replace_all(categories$Variable_name_short, "([$])", "")
rownames(categories) <- categories$Variable
names_OSS_top10_correlated_short <- categories[names_OSS_top10_correlated, ]$Variable_name_short
names_OSS_top10_correlated_short[1] <- "OSS"
par(mai = c(0,0,0,0), mfrow=c(1,1)) # oma = c(0,0,0,0), fig = c(0,0.33,0.5,1) # c(bottom, left, top, right)

# a) original data
dat <- clean_data_final_balanced[, names_OSS_top10_correlated]
names(dat) <- names_OSS_top10_correlated_short
cor_dat <- cor(dat, use = "pairwise.complete.obs")

pdf("../data-figures/OSS_a.cor_normal.pdf", width = 10, height = 10, paper = "special")

corrplot(cor_dat, method="color",
         col=colorRampPalette(c("deepskyblue3","white","darkorange3"))(200),
         tl.col = "black", addCoef.col = "black",
         tl.srt = 45, type="upper", diag=FALSE)

dev.off()

## (b) original data, standardised, time-demeaned data
dat_std <- scale(clean_data_final_balanced[, names_OSS_top10_correlated])
dat_std_td <- time_demean(cbind(clean_data_final_balanced[, c("MFI.ID", "Fiscal.Year")], dat_std))[,1:10]
names(dat_std_td) <- names_OSS_top10_correlated_short

cor_dat_std_td <- cor(dat_std_td, use = "pairwise.complete.obs")

pdf("../data-figures/OSS_b.cor_normal_std_td.pdf", width = 10, height = 10, paper = "special")

corrplot(cor_dat_std_td, method="color",
         col=colorRampPalette(c("deepskyblue3","white","darkorange3"))(200),
         tl.col = "black", addCoef.col = "black",
         tl.srt = 45, type="upper", diag=FALSE)

dev.off()

## (c) imputed data
cor_dat_imp_std <- lapply(data_imputed_standardized_ls, function(x) cor(x[, names_OSS_top10_correlated]))
av_cor_dat_imp_std <- Reduce("+", cor_dat_imp_std) / length(cor_dat_imp_std)
rownames(av_cor_dat_imp_std) <- colnames(av_cor_dat_imp_std) <- names_OSS_top10_correlated_short

pdf("../data-figures/OSS_c.cor_imp_mean.pdf", width = 10, height = 10, paper = "special")

corrplot(av_cor_dat_imp_std, method="color",
         col=colorRampPalette(c("deepskyblue3","white","darkorange3"))(200),
         tl.col = "black", addCoef.col = "black",
         tl.srt = 45, type="upper", diag=FALSE)

dev.off()
dem_cor_OSS_imp <- lapply(cor_dat_imp_std, function(x) (x-av_cor_dat_imp_std)^2)
sd_cor_OSS_imp <- sqrt(Reduce("+", dem_cor_OSS_imp) / (length(dem_cor_OSS_imp) - 1))

## (d) imputed standardised, time-demeaned data
imp_data_std_td <- lapply(data_imputed_standardized_ls, time_demean)
cor_dat_imp_std_td <- lapply(imp_data_std_td, function(x) cor(x[, names_OSS_top10_correlated]))
av_cor_dat_imp_std_td <- Reduce("+", cor_dat_imp_std_td) / length(cor_dat_imp_std_td)
rownames(av_cor_dat_imp_std_td) <- colnames(av_cor_dat_imp_std_td) <- names_OSS_top10_correlated_short

pdf("../data-figures/OSS_d.cor_imp_std_td_mean.pdf", width = 10, height = 10, paper = "special")

corrplot(av_cor_dat_imp_std_td, method="color",
         col=colorRampPalette(c("deepskyblue3","white","darkorange3"))(200),
         tl.col = "black", addCoef.col = "black",
         tl.srt = 45, type="upper", diag=FALSE)

dev.off()
dem_cor_OSS_imp_std_td <- lapply(cor_dat_imp_std_td, function(x) (x-av_cor_dat_imp_std_td)^2)
sd_cor_OSS_imp <- max(sqrt(Reduce("+", dem_cor_OSS_imp_std_td) / (length(dem_cor_OSS_imp_std_td) - 1)))


## -----
## Table 9: Variable Pre-Selection and Transformation Process
## -----
data_keep <- read.xlsx("../data-files/data_finsoc_balanced_keep.xlsx", sep=";", na.strings = c("", "NA"))
names_dk <- names(data_keep)[! names(data_keep) %in% c("MFI.ID", "MFI.Name", "Fiscal.Year", "Currency", "Period.Type", "As.of.Date")]
names_dk[order(nchar(names_dk), names_dk)]
# write.csv2(names_dk, "../data-files/variables_keep.csv")
# all further adjustments were made manually which lead to the table in the appendix of the paper

## -----
## Table 10: Descriptive Statistics of Balanced Data
## -----
categories <- read_excel(paste0("../data-files/variable_names_categories.xls"))

# modified summary function which returns number of NA's even if there aren't any
my_summary <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

pan_vers <- "balanced"
for(pan_vers in c("balanced", "unbalanced")) {
  clean_data <- openxlsx::read.xlsx(paste0("../data-files/data_finsoc_", pan_vers, "_clean_social_financial_plausible.xlsx"))
  overall_missingess <- sum(is.na(clean_data)) / (nrow(clean_data) * ncol(clean_data)); print(paste0("Amount of overall missingness: ", overall_missingess))
  
  clean_data <- clean_data %>%
    mutate("Clients.>.log.Number.of.active.borrowers" = log(`Clients.>.Number.of.active.borrowers`),
           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita" = `Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`)
  
  # compute maximum potential correlation
  ## determine maximum correlation with target variables
  cor_total <- cor(clean_data %>%
                     select(-c("As.of.Date", "Currency", "Fiscal.Year", "MFI.Name", "MFI.ID", "Period.Type")),
                   use = "pairwise.complete.obs")
  
  # print(head(sort(abs(cor_total["Financial.Performance.>.Operational.self.sufficiency", ]), decreasing = TRUE), 10))
  # print(head(sort(abs(cor_total["Clients.>.log.Number.of.active.borrowers", ]), decreasing = TRUE), 10))
  # print(head(sort(abs(cor_total["Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita", ]), decreasing = TRUE), 10))
  
  clean_data_with_impl <- read.xlsx(paste0("../data-files/data_finsoc_", pan_vers, "_clean_social_financial.xlsx")) %>%
    select(-c("Governance.&.HR.>.Human.resource.policies.in.place", "Governance.&.HR.>.SPM.champion.and/or.SPM.committee.on.board")) %>%
    mutate("Clients.>.log.Number.of.active.borrowers" = log(`Clients.>.Number.of.active.borrowers`),
           "Outreach.>.neg.log.Average.loan.balance.per.borrower./.GNI.per.capita" = `Outreach.>.Average.loan.balance.per.borrower./.GNI.per.capita`)
  
  n_impl <- sapply(clean_data, function(col) sum(is.na(col))) - sapply(clean_data_with_impl, function(col) sum(is.na(col)))
  n_impl <- data.frame("Variable" = names(n_impl), "n_impl" = n_impl)
  
  summary_table <- as.data.frame(do.call(rbind, lapply(clean_data %>%
                                                         select(-c("As.of.Date", "Currency", "Fiscal.Year", "MFI.Name", "MFI.ID", "Period.Type")), my_summary)))
  colnames(summary_table) <- c("Min.", "Q1", "Median", "Mean", "Q3", "Max.", "NA's")
  summary_table <- data.frame("Variable" = rownames(summary_table), summary_table)
  summary_table[,2:8] <- sapply(summary_table[,2:8], as.numeric)
  summary_table <- merge(summary_table, n_impl, by = "Variable")
  SD <- sapply(clean_data %>%
                 select(-c("As.of.Date", "Currency", "Fiscal.Year", "MFI.Name", "MFI.ID", "Period.Type")),
               function(col) sd(col, na.rm=TRUE))

  SD_df <- data.frame("Variable" = names(SD), "SD" = format(round(SD, 3), scientific = FALSE))
  summary_table$NA.s <- as.numeric(summary_table$NA.s)/nrow(clean_data)
  summary_table <- data.frame("Variable" = summary_table[,1], summary_table[,2:ncol(summary_table)])
  summary_table <- merge(summary_table, SD_df)
  
  # round to third decimal, cut off decimals from values > 99,999
  for(j in c(2:(ncol(summary_table)-2),ncol(summary_table))) {
    for(i in 1:nrow(summary_table)) {
      if(abs(as.numeric(summary_table[i,j])) <= 99999) {
        summary_table[i,j] <- format(round(as.numeric(summary_table[i,j]), 3), nsmall = 3, scientific = FALSE)
      } else {
        summary_table[i,j] <- format(round(as.numeric(summary_table[i,j]), 0), scientific = FALSE)
      }
    }
  }
  
  summary_table <- merge(categories[, c("Category", "Variable_name_short", "Variable")], summary_table, by = "Variable")
  summary_table <- summary_table %>%
    arrange(Category, Variable_name_short)
  
  category_colors <- c("babyblue!70", "babyblue!45", "babyblue!20", "gray!40", "orange!20", "orange!45", "orange!70")
  for (cat in 1:7) {
    summary_table$Variable_name_short[which(summary_table$Category == cat)] <- paste0("\\colorbox{", category_colors[cat], "}{", summary_table$Variable_name_short[which(summary_table$Category == cat)], "}")
  }

  # order columns
  summary_table <- summary_table[, c(3,7,12,4:6,8,9,11,10)]

  ## make latex table
  latex_summary_table <- summary_table %>%
    kable(caption = paste0("Descriptive Statistics for Balanced Panel"),
          format = "latex",
          position = "htb",
          col.names = c("\\textbf{Variable}", "Mean", "SD", "Min.", "1Q", "Median", "3Q", "Max.", "$N_\\text{impl}$", "\\% NA"),
          booktabs = TRUE,
          longtable = TRUE,
          escape = FALSE,
          linesep = "") %>%
    kable_styling(latex_options = c("repeat_header"), font_size = 7)
  print(latex_summary_table)
}
