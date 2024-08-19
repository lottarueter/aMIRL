# Code for the paper _Model Determination for High-Dimensional Longitudinal Data With Missing Observations: An Application to Microfinance Data_

by Lotta Rüter and Melanie Schienle.

## Notes
+ All computations were performed with R version 4.4.0 (2024-04-24)
+ Required packages: `bcaboot`, `corrplot`, `countrycode`, `doParallel`, `dplyr`, `glmnet`, `kableExtra`, `mice`, `openxlsx`, `quantreg`, `RColorBrewer`, `readxl`, `REEMtree`, `rlist`, `rqpd`, `rstudioapi`, `rworldmap`, `stringr`

## Contents
#### `/data-source-files`
+ The data files were retrieved from https://datacatalog.worldbank.org/dataset/mix-market, retrieved on September 4, 2023 (published under Creative Commons Attribution 4.0 License, CC-BY 4.0).

#### `/data-code`
+ `data_step1.R` merges the Financial Performance Data Set in USD and the Social Performance Data Set, illustrates the choice of the window for the balanced panel, deletes variables with more than 50% missing observations in the chosen window, constructs respective largest unbalanced panel containing the same variables, saves `data_finsoc_balanced_keep.xlsx` and `data_finsoc_unbalanced_keep.xlsx` in `/data-files` folder.
+ `data_step2.R` prepares social data variables: removes redundant variables, transforms existing variables by creating dummy and factor variables, saves result in `data_finsoc_balanced_clean_social.xlsx` or `data_finsoc_unbalanced_clean_social.xlsx` depending on which `panel_version` was chosen in the beginning (`"balanced"` or `"unbalanced"`)
+ `data_step3.R` prepares financial data variables: keeps, removes and transforms according to categories from original data set in alphabetical order, replaces infs and NaNs, saves result in `data_finsoc_balanced_clean_social_financial.xlsx` or `data_finsoc_unbalanced_clean_social_financial.xlsx` depending on which `panel_version` was chosen in the beginning (`"balanced"` or `"unbalanced"`)
+ `data_step4.R` inspects values *<0* and *>1* regarding plausibility, removes implausible values, removes variables with now more than 50% missing values, saves result in `data_finsoc_balanced_clean_social_financial_plausible.xlsx` or `data_finsoc_unbalanced_clean_social_financial_plausible.xlsx` depending on which `panel_version` was chosen in the beginning ("balanced" or "unbalanced")
+ `data_visualisation.R` contains code for Figure 2 (world map), Table 2 (MFIs per country and region), Figure 3 (correlation plots), the basis for Table 9 (construction of the balanced panel), Table 10 and Table 11 (descriptive statistics of the balanced and unbalanced panel data)

#### `/data-files`
+ contains the files `data_finsoc_balanced_keep.xlsx`, `data_finsoc_balanced_clean_social.xlsx`, `data_finsoc_balanced_clean_social_financial.xlsx`, `data_finsoc_balanced_clean_social_financial_plausible.xlsx` produced by `/data-code/data_step1.R` to `/data-code/data_step4.R` as well as their respective versions for the unbalanced panel
+ `variables_names_categories.xlsx` contains the manual assignment of the variables in the final data set to the factors introduced in the paper and depicted in Figure 1.

#### `/data-figures`
+ Figure 2: `map.pdf` produced by `/data-code/data_visualisation.R`
+ Figure 3: `OSS_a.cor_normal.pdf`, `OSS_b.cor_normal_std_td`, `OSS_c.cor_imp_mean.pdf`, `OSS_d.cor_imp_std_td_mean.pdf`, `OSS_e.cor_normal_mean_imp.pdf`, `OSS_f.cor_mean_imp_std_td.pdf` produced by `/data-code/data_visualisation.R`

#### `/aMIRL-code`
+ `aMIRL_steps1-2a.R` loads `aMIRL_steps1-2a_functions.R`, imputes the missing values *M=10* times for the given `data-files/data_finsoc_balanced_clean_social_financial_plausible.xlsx` (or unbalanced) data set either a) via REEMtrees and classification trees (`panel_version = "balanced"` and `aMIRL_variant = "aMIRL"`) and thereby accounting for the panel structure in the data or b) via classification and regression trees (`panel_version = "balanced"` and `aMIRL_variant = "MIRL"` or `panel_version = "unbalanced"` and `aMIRL_variant = "MIRL"`) and consequently disregarding the panel structure, computes *B=100* bootstrap samples of each of the *M=10* completed data sets, standardises the data, saves resulting data
+ `aMIRL_steps1-2a_functions.R` contains additional functions required for the computations performed in `aMIRL_steps1-2a.R`
+ `aMIRL_steps2b-4.R` loads `aMIRL_steps2b-4_functions.R`, performs steps 2b-4 of the (a)MIRL algorithm. Requires the files `aMIRL-output/data_imputed_ls_balanced_aMIRL.RData` (or alternatively `aMIRL-output/data_imputed_ls_balanced_MIRL.RData` or `aMIRL-output/data_imputed_ls_unbalanced_MIRL.RData`) produced by `aMIRL_steps1-2a.R`. Since these files are large, they are not uploaded to GitHub, but can be made available upon request. The same holds true for the output files of this code chunk (`aMIRL-output/results_balanced_aMIRL_OSS.RData` and related variants)
+ `aMIRL_simple_lasso.R` contains code for simple lasso analysis on mean-imputed (time-demeaned) data from Section 4.3. Results are stored in `aMIRL-output/simple_lasso/`
+ `aMIRL_visualisation.R` computes post-selection quantile estimates and significances of the (a)MIRL estimates via bootstrap confidence intervals, contains code for tables in Section 4 (Tables 4-7) and the appendix (Tables 12-20), requires output-files from `aMIRL_steps2b-4.R` which are not contained in the GitHub repo due to their large size. They can be made available upon request.

#### `/aMIRL-output`
+ target folder where the output files of the imputation step (`data_imputed_ls_balanced_aMIRL.RData`, `data_imputed_ls_balanced_MIRL.RData`, `data_imputed_ls_unbalanced_MIRL.RData`) and the (a)MIRL algorithm (`results_balanced_aMIRL_OSS.RData` and related versions for other target variables, MIRL variant and unbalanced panel) are stored. Since these files are all very large, they are not uploaded to GitHub but can be made available upon request
+ `rq_estimates[...]` files contain post-selection regression quantiles for aMIRL models computed in `aMIRL_visualisation.R`
+ `signif_estimates[...]` files contain significances of (a)MIRL estimates computed in `aMIRL_visualisation.R` via bootstrap confidence intervals
+ `simple_lasso/` contains results from analysis with simple lasso on mean-imputed (time-demeaned) data from Section 4.3
