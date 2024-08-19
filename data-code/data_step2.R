## Preparation of social variables

# 1: Remove redundant variables
# 2: Transform existing variables: create dummy and factor variables
# 3: Save data set

# Lotta Rüter
# lotta.rueter@kit.edu

## -----
# load packages
library(openxlsx)
library(dplyr)
library(stringr)

# set working directory
current_path = rstudioapi::getActiveDocumentContext()$path # get the path of your current open file
setwd(dirname(current_path))

# clear environment
rm(list = ls())

# balanced or unbalanced panel?
panel_version <- "balanced"
# panel_version <- "unbalanced"

# load output from step 1
data_finsoc_selected <- read.xlsx(paste("../data-files/data_finsoc_", panel_version, "_keep.xlsx", sep = "")) # (un)balanced panel


## -----
# 1: Remove redundant variables
## -----
# 1.1: Remove utterly redundant variables
# None of the Audit variable contains information for balanced panel (always either 0 or NA, only for 10 values 1)
# proof: which(data_finsoc_selected[,5:21]==1) or rowSums(clean_data[,5:21]) & tail(rowSums(clean_data[,5:21]),224)
data_finsoc_selected <- data_finsoc_selected %>% 
  select(-all_of(c(names(data_finsoc_selected)[which(str_detect(names(data_finsoc_selected),"Audit.and.Rating.>."))],
                   names(data_finsoc_selected)[which(str_detect(names(data_finsoc_selected),"Client.protection.>.Client.protection.assessment.>."))],
                   names(data_finsoc_selected)[which(str_detect(names(data_finsoc_selected),"Client.protection.>.Written.policy.on.client.collection.practices.>."))],
                   names(data_finsoc_selected)[which(str_detect(names(data_finsoc_selected),"Governance.&.HR.>.Board.member.with.SP.education.and.or.work.experience.>."))])))

# 1.2: Remove variables that yield too detailed information (qualitatively): Environmental variables
data_finsoc_selected <- data_finsoc_selected %>% 
  select(-all_of(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Environment"))]))


## -----
# 2: Transform existing variables: create dummy and factor variables
## -----
# Create dummy variables
# Make list of meaning of the variables
# ending with "Partially" -> label as NA (very little observations anyway)
# ending with "Unknown" -> label as NA

# 2.1 Yes, No, Partially, Unknown dummies
yes_dummies <- names(data_finsoc_selected)[which(endsWith(names(data_finsoc_selected),"Yes"))]
no_dummies <- names(data_finsoc_selected)[which(endsWith(names(data_finsoc_selected),"No"))]
# cbind(yes_dummies, no_dummies) # shows that corresponding dummies have the same index
temp_df <- data.frame(sapply(1:length(yes_dummies), function(i) {ifelse(data_finsoc_selected[,yes_dummies[i]]==1, 1, ifelse(data_finsoc_selected[,no_dummies[i]]==1,0,NA))}))
names(temp_df) <- str_sub(yes_dummies, end=-7)

# names of variables to delete
variables_ex_dummies <- unlist(sapply(1:length(names(temp_df)), function(i) names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),paste(names(temp_df), ".>.", sep="")[i]))]))

# update data_finsoc_selected
data_finsoc_selected <- data_finsoc_selected %>%
  select(-all_of(variables_ex_dummies)) %>% 
  bind_cols(temp_df)

# 2.2 Interest method                                                                                                                         
# ifelse(data_finsoc_selected$`Client.protection.>.Interest.rate.calculation.method(s).>.Flat.interest.method`==1,0,NA)))
# for some entries both = 1 -> 167 168 204 319 334 335 336 530 531 532 587 588 710 711 712 801 802 803 804 851 852 906, ignored
variables_ex_dummies <- c("Client.protection.>.Interest.rate.calculation.method(s).>.Declining.balance.interest.method",
                          "Client.protection.>.Interest.rate.calculation.method(s).>.Flat.interest.method") # remove
# update data_finsoc_selected
data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols(`Client.protection.>.Interest.rate.calculation.method.>.Declining.balance.interest.method`= ifelse(data_finsoc_selected$`Client.protection.>.Interest.rate.calculation.method(s).>.Declining.balance.interest.method`==1, 1,
                                                                                                               ifelse(data_finsoc_selected$`Client.protection.>.Interest.rate.calculation.method(s).>.Flat.interest.method`==1,0,NA))) %>%
  select(-all_of(variables_ex_dummies))

# 2.3 Staff incentives to remove
data_finsoc_selected <- data_finsoc_selected %>%
  select(-all_of(c(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Governance.&.HR.>.Bases.for.staff.incentives.>.Number.of.clients.>.Bases.for.staff.incentives.on.number.of.clients.>"))],
                   names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Governance.&.HR.>.Bases.for.staff.incentives.>.None.of.the.above"))])))

# 2.4 Human resources policy in place (Anti-harassment, grievance resolution, non-discrimination, safety, social protection)
data_finsoc_selected <- data_finsoc_selected %>%
  bind_cols("Governance.&.HR.>.Human.resource.policies.in.place" = ifelse(rowSums(cbind(data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.Anti.harassment.policy`,
                                                                                        data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.Grievance.resolution.policy`,
                                                                                        data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.Non.discrimination.policy`,
                                                                                        data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.Safety.policy`,
                                                                                        data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.Social.protection.(medical.insurance.and.or.pension.contribution)`), na.rm=TRUE) >= 1, 1,
                                                                          ifelse(data_finsoc_selected$`Governance.&.HR.>.Human.resource.policies.in.place.>.None.of.the.above` == 1,0,NA))) %>%
  select(-all_of(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Governance.&.HR.>.Human.resource.policies.in.place.>."))]))

# 2.5 Credit product offering
# No NAs for Non.income.generating.loans and Non.income.generating.loans for balanced panel
# idea: (a) income and non-income -> as separate variable (b) only non-income -> as separate variable (c) only income -> as baseline variable (not modelled) (d) no loans -> non-existent in data set
# however, we find that 'only income-generating loans' is almost perfectly correlated with 'Income and non-income generating loans' -> only include (a) in our analysis
# help: 1 if only income-generating loans are offered, 0 if income-generating and non-income-generating loans are offered, -1 if only non-income-generating loans are offered
help <- data_finsoc_selected$`Products.&.services.>.Credit.product.offering.>.Income.generating.loans` - data_finsoc_selected$`Products.&.services.>.Credit.product.offering.>.Non-income.generating.loans`
# sum(na.omit(data_finsoc_selected$`Products.&.services.>.Credit.product.offering.>.Does.not.offer.credit.products`)) == 0 
# -> all MFIs offer credit products
# sum(na.omit(help == 1)) + sum(na.omit(help == 0)) + sum(na.omit(help == -1)) + sum(is.na(help))

data_finsoc_selected <- data_finsoc_selected %>%
  select(-all_of(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Products.&.services.>.Credit.product.offering.>."))])) %>%
  bind_cols("Products.&.services.>.Credit.product.offering.>.Income.and.non-income.generating.loans" = as.numeric(help==0))
            # "Products.&.services.>.Credit.product.offering.>.Only.income.generating.loans" = as.numeric(help==1) # do not include since almost perfectly correlated with 'Income and non-income generating loans'

# 2.6 Savings product offering
data_finsoc_selected <- data_finsoc_selected %>%
  bind_cols("Products.&.services.>.Savings.product.offering" = ifelse(rowSums(cbind(data_finsoc_selected$`Products.&.services.>.Savings.product.offering.>.Compulsory.savings.accounts`,
                                                                                    data_finsoc_selected$`Products.&.services.>.Savings.product.offering.>.Voluntary.savings.accounts`), na.rm=TRUE)>=1, 1,
                                                                      ifelse(data_finsoc_selected$`Products.&.services.>.Savings.product.offering.>.Does.not.offer.savings.products` == 1,0,NA))) %>%
  select(-all_of(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Products.&.services.>.Savings.product.offering.>."))]))

# poverty_targets <- names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Social.goals.>.Poverty.targets.>."))]
# rowSums(data_finsoc_selected[,poverty_targets[c(1,3,4)]]) + data_finsoc_selected[,poverty_targets[2]]

# 2.7 Development goals
variables_ex_dummies <- c(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Social.goals.>.Development.goals.>."))])

data_finsoc_selected <- data_finsoc_selected %>%
  bind_cols("Social.goals.>.Development.goals.>.Economic.improvement" = ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Development.of.start.up.enterprises`,
                                                                                             data_finsoc_selected$`Social.goals.>.Development.goals.>.Employment.generation`,
                                                                                             data_finsoc_selected$`Social.goals.>.Development.goals.>.Increased.access.to.financial.services`,
                                                                                             data_finsoc_selected$`Social.goals.>.Development.goals.>.Growth.of.existing.businesses`,
                                                                                             data_finsoc_selected$`Social.goals.>.Development.goals.>.Poverty.reduction`), na.rm=TRUE) >= 1, 1,
                                                                               ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Development.of.start.up.enterprises`,
                                                                                                    data_finsoc_selected$`Social.goals.>.Development.goals.>.Employment.generation`,
                                                                                                    data_finsoc_selected$`Social.goals.>.Development.goals.>.Increased.access.to.financial.services`,
                                                                                                    data_finsoc_selected$`Social.goals.>.Development.goals.>.Growth.of.existing.businesses`,
                                                                                                    data_finsoc_selected$`Social.goals.>.Development.goals.>.Poverty.reduction`), na.rm=FALSE) == 0, 0, NA)),
            "Social.goals.>.Development.goals.>.Education.opportunities" = ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Children's.schooling`,
                                                                                                data_finsoc_selected$`Social.goals.>.Development.goals.>.Improvement.of.adult.education`,
                                                                                                data_finsoc_selected$`Social.goals.>.Development.goals.>.Youth.opportunities`), na.rm=TRUE) >= 1, 1,
                                                                                  ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Children's.schooling`,
                                                                                                       data_finsoc_selected$`Social.goals.>.Development.goals.>.Improvement.of.adult.education`,
                                                                                                       data_finsoc_selected$`Social.goals.>.Development.goals.>.Youth.opportunities`), na.rm=FALSE) == 0, 0, NA)),
            "Social.goals.>.Development.goals.>.Health.infrastructure" = ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Access.to.water.and.sanitation`,
                                                                                              data_finsoc_selected$`Social.goals.>.Development.goals.>.Health.improvement`,
                                                                                              data_finsoc_selected$`Social.goals.>.Development.goals.>.Housing`), na.rm=TRUE) >= 1, 1,
                                                                                ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Development.goals.>.Access.to.water.and.sanitation`,
                                                                                                     data_finsoc_selected$`Social.goals.>.Development.goals.>.Health.improvement`,
                                                                                                     data_finsoc_selected$`Social.goals.>.Development.goals.>.Housing`), na.rm=FALSE) == 0, 0, NA)),
            "Social.goals.>.Development.goals.>.Women's.empowerment" = data_finsoc_selected$`Social.goals.>.Development.goals.>.Gender.equality.and.women's.empowerment`)  %>% 
  select(-all_of(variables_ex_dummies))

# 2.8 Poverty targets
data_finsoc_selected <- data_finsoc_selected %>%
  bind_cols("Social.goals.>.Poverty.target" = ifelse(rowSums(cbind(data_finsoc_selected$`Social.goals.>.Poverty.targets.>.Low.income.clients`,
                                                                   data_finsoc_selected$`Social.goals.>.Poverty.targets.>.Poor.clients`,
                                                                   data_finsoc_selected$`Social.goals.>.Poverty.targets.>.Very.poor.clients`), na.rm=TRUE) >= 1, 1,
                                                     ifelse(data_finsoc_selected$`Social.goals.>.Poverty.targets.>.No.specific.poverty.target` == 1,0,NA))) %>%
  select(-all_of(names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Social.goals.>.Poverty.targets.>."))]))

# if use of more detailed information: check if correlation is an issue
# cor(data_finsoc_selected[,poverty_targets], use = "pairwise.complete.obs")
# summary(lm(data_finsoc_selected[,poverty_targets[2]] ~ data_finsoc_selected[,poverty_targets[1]]+ data_finsoc_selected[,poverty_targets[3]]+ data_finsoc_selected[,poverty_targets[4]]))

# 2.9 Target markets
# cor(data_finsoc_selected[which(startsWith(names(data_finsoc_selected),"Social.goals.>.Target.market.>."))], use = "pairwise.complete.obs")
data_finsoc_selected <- data_finsoc_selected %>%
  select(-names(data_finsoc_selected)[which(startsWith(names(data_finsoc_selected),"Social.goals.>.Target.market.>.None.of.the.above"))]) %>%
  select(sort(names(.)))

names(data_finsoc_selected)

# correct variable names where special characters got lost
names_identification <- c("Fiscal.Year",
                          "MFI.ID")
names_social_variables <- c("Client.protection.>.Clear.debt.collection.practices" ,
                            "Client.protection.>.Full.disclosure.of.prices,.terms,.and.conditions",
                            "Client.protection.>.Functioning.client.complaint.mechanism",
                            "Client.protection.>.Interest.rate.calculation.method.>.Declining.balance.interest.method",
                            "Client.protection.>.Internal.audits.verify.over-indebtedness.prevention",
                            "Client.protection.>.Privacy.data.clause.in.loan.contracts",
                            "Client.protection.>.Robust.repayment.evaluation",
                            "Governance.&.HR.>.Bases.for.staff.incentives.>.Number.of.clients",
                            "Governance.&.HR.>.Bases.for.staff.incentives.>.Portfolio.quality",
                            "Governance.&.HR.>.Bases.for.staff.incentives.>.Quality.of.interaction.with.clients.based.on.client.feedback.mechanism",
                            "Governance.&.HR.>.Bases.for.staff.incentives.>.Quality.of.social.data.collection",
                            "Governance.&.HR.>.Board.orientation.on.social.mission.and.goals",
                            "Governance.&.HR.>.Human.resource.policies.in.place",
                            "Governance.&.HR.>.SPM.champion.and/or.SPM.committee.on.board",
                            "Products.&.services.>.Compulsory.insurance",
                            "Products.&.services.>.Credit.product.offering.>.Income.and.non-income.generating.loans",
                            # "Products.&.services.>.Credit.product.offering.>.Only.income.generating.loans",
                            "Products.&.services.>.Enterprise.services.(nonfinancial)",
                            "Products.&.services.>.Health.services.(nonfinancial)",
                            "Products.&.services.>.Offers.other.financial.services",
                            "Products.&.services.>.Other.education.services.(nonfinancial)",
                            "Products.&.services.>.Savings.product.offering",
                            "Products.&.services.>.Voluntary.insurance",
                            "Products.&.services.>.Women's.empowerment.services.(nonfinancial)",
                            "Social.goals.>.Development.goals.>.Economic.improvement",
                            "Social.goals.>.Development.goals.>.Education.opportunities",
                            "Social.goals.>.Development.goals.>.Health.infrastructure",
                            "Social.goals.>.Development.goals.>.Women's.empowerment",
                            "Social.goals.>.Measures.client.poverty",
                            "Social.goals.>.Poverty.target",
                            "Social.goals.>.Target.market.>.Adolescents.and.youth.(below.18)",
                            "Social.goals.>.Target.market.>.Clients.living.in.rural.areas",
                            "Social.goals.>.Target.market.>.Clients.living.in.urban.areas",
                            "Social.goals.>.Target.market.>.Women")

# names_social_variables %in% names(data_finsoc_selected)

## -----
# 3: Save data set
## -----
# write.csv(names_social_variables, file = "../data-files/names_social_variables_finsoc_clean.csv")
write.xlsx(data_finsoc_selected, file = paste0("../data-files/data_finsoc_", panel_version, "_clean_social.xlsx")) # use xlsx to keep ">" in variable names 