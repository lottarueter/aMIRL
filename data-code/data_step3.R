## Preparation of financial variables

# 1: Keep, transform or remove variables according to categories from original data set in alphabetical order
# 2: Replace infs and NaNs
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

# load output from step 2
data_finsoc_selected <- read.xlsx(paste("../data-files/data_finsoc_", panel_version, "_clean_social.xlsx", sep = ""))


## -----
# 1: Keep, transform or remove variables according to categories in alphabetical order
## -----
# 1.1 Balance Sheet
# keep all, add "Balance.Sheet.>." in front
variables_to_rmv <- c("Average.assets",
                      "Borrowings",
                      "Cash.and.cash.equivalents",
                      "Donated.equity",
                      "Impairment.loss.allowance",
                      "Liabilities.and.equity",
                      "Net.fixed.assets",
                      "Net.loan.portfolio",
                      "Other.assets",
                      "Other.equity",
                      "Other.liabilities",
                      "Other.short.term.financial.liabilities",
                      "Paid.in.capital",
                      "Retained.earnings",
                      "Subordinated.debt")

variables_temp <- c("Assets",
                    "Equity",
                    "Liabilities")

data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols("Balance.Sheet.>.Borrowings.per.Liabilities" = data_finsoc_selected$Borrowings / data_finsoc_selected$Liabilities,
            "Balance.Sheet.>.Donated.equity.per.Equity" = data_finsoc_selected$Donated.equity / data_finsoc_selected$Equity,
            "Balance.Sheet.>.Impairment.loss.allowance.per.Gross.loan.portfolio" = data_finsoc_selected$Impairment.loss.allowance / data_finsoc_selected$Gross.Loan.Portfolio,
            "Balance.Sheet.>.Liabilities.per.Assets" = data_finsoc_selected$Liabilities / data_finsoc_selected$Assets,
            # "Balance.Sheet.>.log.Assets" = log(data_finsoc_selected$Assets),
            # "Balance.Sheet.>.log.Equity" = log(data_finsoc_selected$Equity),
            # "Balance.Sheet.>.log.Liabilities" = log(data_finsoc_selected$Liabilities),
            "Balance.Sheet.>.Net.loan.portfolio.per.Gross.loan.portfolio" = data_finsoc_selected$Net.loan.portfolio / data_finsoc_selected$Gross.Loan.Portfolio,
            "Balance.Sheet.>.Paid.in.capital.per.Equity" = data_finsoc_selected$Paid.in.capital / data_finsoc_selected$Equity,
            "Balance.Sheet.>.Retained.earnings.per.Equity" = data_finsoc_selected$Retained.earnings / data_finsoc_selected$Equity,
            "Balance.Sheet.>.Subordinated.debt.per.Liabilities" = data_finsoc_selected$Subordinated.debt / data_finsoc_selected$Equity) %>%
            # "Macroeconomic.Variables.>.Inflation" =  (data_finsoc_selected$`Financial.revenue` / data_finsoc_selected$`Average.gross.loan.portfolio` - data_finsoc_selected$`Yield.on.gross.portfolio.(real)`) / (1 + data_finsoc_selected$`Yield.on.gross.portfolio.(real)`),
            # "Macroeconomic.Variables.>.GNI.per.capita" = data_finsoc_selected$`Average.outstanding.balance` / data_finsoc_selected$`Average.loan.balance.per.borrower./.GNI.per.capita`) %>%
  rename_at(vars(all_of(variables_temp)), ~ paste("Balance.Sheet.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>%
  select(-all_of(variables_to_rmv))


# 1.2 Clients
# Transform to percentage, remove respective variables and add "Clients.>." in front
variables_to_rmv <- c("Gross.Loan.Portfolio.>.Gender.>.Female",
                      "Gross.Loan.Portfolio.>.Gender.>.Legal.Entity",
                      "Gross.Loan.Portfolio.>.Gender.>.Male",
                      "Gross.Loan.Portfolio.>.Location.>.Rural",
                      "Gross.Loan.Portfolio.>.Location.>.Urban",
                      "Gross.Loan.Portfolio.>.Relationship.>.External.Customers",
                      "Gross.Loan.Portfolio.>.Relationship.>.Management.And.Staff",
                      "Number.of.active.borrowers.>.Gender.>.Female",
                      "Number.of.active.borrowers.>.Gender.>.Legal.Entity",
                      "Number.of.active.borrowers.>.Gender.>.Male",
                      "Number.of.active.borrowers.>.Location.>.Rural",
                      "Number.of.active.borrowers.>.Location.>.Urban",
                      "Number.of.active.borrowers.>.Relationship.>.External.Customers",
                      "Number.of.active.borrowers.>.Relationship.>.Management.And.Staff",
                      "Number.of.loans.outstanding.>.Gender.>.Female",
                      "Number.of.loans.outstanding.>.Gender.>.Legal.Entity",
                      "Number.of.loans.outstanding.>.Gender.>.Male",
                      "Number.of.loans.outstanding.>.Location.>.Rural",
                      "Number.of.loans.outstanding.>.Location.>.Urban",
                      "Number.of.loans.outstanding.>.Relationship.>.External.Customers",
                      "Number.of.loans.outstanding.>.Relationship.>.Management.And.Staff",
                      "Number.of.new.borrowers")

variables_temp <- c("Gross.Loan.Portfolio",
                    "Number.of.active.borrowers",
                    "Number.of.loans.outstanding")

data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols("Clients.>.Average.Loan.Size.>.Gender.>.Female" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Gender.>.Female` / data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Female`,
            "Clients.>.Average.Loan.Size.>.Gender.>.Legal.Entity" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Gender.>.Legal.Entity` / data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Legal.Entity`,
            "Clients.>.Average.Loan.Size.>.Gender.>.Male" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Gender.>.Male` / data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Male`,
            "Clients.>.Average.Loan.Size.>.Location.>.Rural" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Location.>.Rural` / data_finsoc_selected$`Number.of.active.borrowers.>.Location.>.Rural`,
            "Clients.>.Average.Loan.Size.>.Location.>.Urban" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Location.>.Urban` / data_finsoc_selected$`Number.of.active.borrowers.>.Location.>.Urban`,
            "Clients.>.Average.Loan.Size.>.Relationship.>.External.Customers" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Relationship.>.External.Customers` / data_finsoc_selected$`Number.of.active.borrowers.>.Relationship.>.External.Customers`,
            "Clients.>.Average.Loan.Size.>.Relationship.>.Management.&.Staff" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Relationship.>.Management.And.Staff` / data_finsoc_selected$`Number.of.active.borrowers.>.Relationship.>.Management.And.Staff`,
            # "Clients.>.log.Gross.Loan.Portfolio" = log(data_finsoc_selected$Gross.Loan.Portfolio),
            "Clients.>.Percent.of.active.borrowers.>.Gender.>.Female" = data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Female` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Gender.>.Legal.Entity" = data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Legal.Entity` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Gender.>.Male" = data_finsoc_selected$`Number.of.active.borrowers.>.Gender.>.Male` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Location.>.Rural" = data_finsoc_selected$`Number.of.active.borrowers.>.Location.>.Rural` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Location.>.Urban" = data_finsoc_selected$`Number.of.active.borrowers.>.Location.>.Urban` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Relationship.>.External.Customers" = data_finsoc_selected$`Number.of.active.borrowers.>.Relationship.>.External.Customers` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Percent.of.active.borrowers.>.Relationship.>.Management.And.Staff" = data_finsoc_selected$`Number.of.active.borrowers.>.Relationship.>.Management.And.Staff` / data_finsoc_selected$`Number.of.active.borrowers`,
            "Clients.>.Number.of.new.borrowers.per.Number.of.active.borrowers" = data_finsoc_selected$Number.of.new.borrowers / data_finsoc_selected$Number.of.active.borrowers) %>%
  rename_at(vars(variables_temp), ~ paste("Clients.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>%
  select(-all_of(variables_to_rmv))

# 1.3 Credit Products
variables_to_rmv <- c("Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Consumption",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Mortgage/housing",
                      "Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Other.household.finance",
                      "Gross.Loan.Portfolio.>.Methodology.>.Individual",
                      "Gross.Loan.Portfolio.>.Methodology.>.Solidarity.Group",
                      "Gross.Loan.Portfolio.>.Methodology.>.Village.Banking.SHG",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Consumption",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Mortgage/housing",
                      "Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Other.household.finance",
                      "Number.of.loans.outstanding.>.Methodology.>.Individual",
                      "Number.of.loans.outstanding.>.Methodology.>.Solidarity.Group",
                      "Number.of.loans.outstanding.>.Methodology.>.Village.Banking.SHG")

data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols("Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Enterprise.Finance" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Enterprise.Finance.>.Large.Corporations" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Enterprise.Finance.>.Microenterprise" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Household.Financing" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Household.Financing.>.Consumption" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Consumption` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Household.Financing.>.Mortgage/housing" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Mortgage/housing` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Credit.Products.>.Percent.Household.Financing.>.Other.household.finance" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Other.household.finance` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Methodology.>.Percent.Individual" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Individual` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Methodology.>.Percent.Solidarity.Group" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Solidarity.Group` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Gross.Loan.Portfolio.>.Methodology.>.Percent.Village.Banking.SHG" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Village.Banking.SHG` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Enterprise.Finance" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Large.Corporations`, # NaN -> 0
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Loans.To.Small.And.Medium.Enterprises`, # NaN -> 0
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Enterprise.Finance.>.Microenterprise`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Household.Financing" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Household.Financing.>.Consumption" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Consumption` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Consumption`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Household.Financing.>.Mortgage/housing" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Mortgage/housing` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Mortgage/housing`,
            "Credit.Products.>.Average.Loan.Size.>.Credit.Products.>.Household.Financing.>.Other.household.finance" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Credit.Products.>.Household.Financing.>.Other.household.finance` / data_finsoc_selected$`Number.of.loans.outstanding.>.Credit.Products.>.Household.Financing.>.Other.household.finance`,
            "Credit.Products.>.Average.Loan.Size.>.Methodology.>.Individual" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Individual` / data_finsoc_selected$`Number.of.loans.outstanding.>.Methodology.>.Individual`,
            "Credit.Products.>.Average.Loan.Size.>.Methodology.>.Solidarity.Group" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Solidarity.Group` / data_finsoc_selected$`Number.of.loans.outstanding.>.Methodology.>.Solidarity.Group`,
            "Credit.Products.>.Average.Loan.Size.>.Methodology.>.Village.Banking.SHG" = data_finsoc_selected$`Gross.Loan.Portfolio.>.Methodology.>.Village.Banking.SHG` / data_finsoc_selected$`Number.of.loans.outstanding.>.Methodology.>.Village.Banking.SHG`) %>%
  select(sort(names(.))) %>%
  select(-all_of(variables_to_rmv))


# 1.4 Delinquency
variables_to_rmv <- c("Gross.Loan.Portfolio.>.Delinquency.>.One.month.or.more.>.More.than.three.months.>.From.three.to.six.months",
                      "Gross.Loan.Portfolio.>.Delinquency.>.One.month.or.more.>.More.than.three.months.>.More.than.six.months",
                      "Gross.Loan.Portfolio.>.Delinquency.>.One.month.or.more",
                      "Write.offs") # redundant since Write off ratio is included
variables_temp <- c(# "Gross.Loan.Portfolio.>.Delinquency.>.From.One.To.30.Days", # already excluded from data set: too few observations!
                    # "Gross.Loan.Portfolio.>.Delinquency.>.Not.Overdue", # already excluded from data set: too few observations!
                    "Gross.Loan.Portfolio.>.Delinquency.>.One.month.or.more.>.From.one.to.three.months",
                    "Gross.Loan.Portfolio.>.Delinquency.>.One.month.or.more.>.More.than.three.months",
                    "Gross.Loan.Portfolio.>.Delinquency.>.Renegotiated.loans")
temp_df <- data_finsoc_selected[,variables_temp] / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`

names(temp_df) <- paste("Delinquency.Percent.>.", variables_temp, sep="")
# sum(is.nan(unlist(temp_df))) = 0 -> no NaNs produced

data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols(temp_df) %>%
  select(sort(names(.))) %>% 
  select(-c(variables_to_rmv, variables_temp))

# 1.5 Deposit Products and Digital Delivery Channels NA -> no further action

# 1.6 Financial Performance
variables_temp <- c("Operational.self.sufficiency",
                    "Return.on.assets",
                    "Return.on.equity")

data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Financial.Performance.>.", variables_temp, sep="")) %>% 
  select(sort(names(.)))

# 1.7 Financing Structure
names(data_finsoc_selected)[names(data_finsoc_selected) == 'Capital./asset.ratio'] <- 'Capital.to.asset.ratio'
variables_temp <- c("Capital.to.asset.ratio", # = Balance.Sheet.>.Equity / Balance.Sheet.>.Assets
                    "Debt.to.equity.ratio", # = Balance.Sheet.>.Liabilities / Balance.Sheet.>.Equity
                    "Deposits.to.loans",
                    "Deposits.to.total.assets",
                    "Gross.loan.portfolio.to.total.assets")
data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Financing.Structure.>.", variables_temp, sep="")) %>% 
  select(sort(names(.)))
# colSums(is.na(data_finsoc_selected))

# 1.8 Income Statement
variables_to_rmv <- c("Administrative.expense",
                      "Depreciation.and.amortisation.expense",
                      "Donations",
                      "Fee.and.commission.income.on.loan.portfolio",
                      "Financial.expense.on.funding.liabilities",
                      "Financial.revenue",
                      "Financial.revenue.from.loans",
                      "Impairment.loss.(reversal.of.impairment.loss),.gross.loan.portfolio",
                      "Income.from.penalty.fees.on.loan.portfolio",
                      "Interest.expense.on.borrowings",
                      "Interest.expense.on.deposits",
                      "Interest.income.on.loan.portfolio",
                      "Net.impairment.loss,.gross.loan.portfolio",
                      "Net.non-operating.income",
                      "Non.operating.expense",
                      "Non.operating.income",
                      "Operating.expense",
                      "Other.financial.expense",
                      "Other.financial.revenue",
                      "Other.income",
                      "Other.income.from.operations",
                      "Personnel.expense",
                      "Profit.(loss)",
                      "Recoveries.on.loans.written.off",
                      "Tax.expense")

data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols("Income.Statement.>.Financial.expense.on.funding.liabilities.per.assets" = data_finsoc_selected$`Financial.expense.on.funding.liabilities` / data_finsoc_selected$`Balance.Sheet.>.Assets`,
            "Income.Statement.>.Income.from.penalty.fees.on.loan.portfolio.per.Gross.loan.portfolio" = data_finsoc_selected$`Income.from.penalty.fees.on.loan.portfolio` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Income.Statement.>.Interest.expense.on.borrowings.per.Borrowings" = data_finsoc_selected$`Interest.expense.on.borrowings` / (data_finsoc_selected$`Balance.Sheet.>.Borrowings.per.Liabilities` * data_finsoc_selected$`Balance.Sheet.>.Liabilities`),
            "Income.Statement.>.Interest.income.on.loan.portfolio.per.Gross.loan.portfolio" = data_finsoc_selected$`Interest.income.on.loan.portfolio` / data_finsoc_selected$`Clients.>.Gross.Loan.Portfolio`,
            "Income.Statement.>.Tax.Expense.per.Assets" = data_finsoc_selected$`Tax.expense` / data_finsoc_selected$`Balance.Sheet.>.Assets`) %>%
  select(sort(names(.))) %>% 
  select(-all_of(variables_to_rmv))

# 1.9 Income Statement Subtotals
variables_to_rmv <- c("Net.Income.after.taxes.and.before.donations",
                      "Net.Income.before.taxes.and.donations",
                      "Net.operating.income")
data_finsoc_selected <- data_finsoc_selected %>% 
  select(-variables_to_rmv) %>%
  select(sort(names(.)))

# 1.10 Infrastructure
variables_to_rmv <- c("Loan.officers",
                      "Loan.officers.>.Gender.>.Female",
                      "Number.of.board.members",
                      "Number.of.managers",
                      "Number.of.board.members.>.Gender.>.Female",
                      "Number.of.managers.>.Gender.>.Female",
                      "Number.of.staff.employed.for.one.year.or.more",
                      "Number.of.staff.exiting.the.organization.during.period",
                      "Offices",
                      "Other.Points.of.Service",
                      "Personnel.>.Gender.>.Female")

temp_df <- data_finsoc_selected[,c("Number.of.staff.employed.for.one.year.or.more",
                                   "Number.of.staff.exiting.the.organization.during.period")] / data_finsoc_selected$`Personnel`
# sum(is.nan(unlist(temp_df))) = 0 -> no NaNs produced
names(temp_df) <- c("Infrastructure.>.Percent.of.staff.employed.for.one.year.or.more", "Infrastructure.>.Percent.of.staff.exiting.the.organization.during.period")


data_finsoc_selected <- data_finsoc_selected %>% 
  bind_cols(temp_df) %>%
  bind_cols("Infrastructure.>.Personnel.per.office" = data_finsoc_selected$`Personnel` / data_finsoc_selected$`Offices`,
            "Infrastructure.>.Borrowers.per.office" = data_finsoc_selected$`Clients.>.Number.of.active.borrowers` / data_finsoc_selected$`Offices`,
            "Infrastructure.>.Personnel.>.Percent.Loan.officers" = data_finsoc_selected$`Loan.officers` / data_finsoc_selected$`Personnel`,
            "Infrastructure.>.Personnel.>.Percent.Board.members" = data_finsoc_selected$`Number.of.board.members` / data_finsoc_selected$`Personnel`,
            "Infrastructure.>.Personnel.>.Percent.Managers" = data_finsoc_selected$`Number.of.managers` / data_finsoc_selected$`Personnel`) %>%
  rename_at(vars("Personnel"), ~ "Infrastructure.>.Personnel") %>% 
  select(-all_of(variables_to_rmv)) %>%
  select(sort(names(.)))

# 1.11 Outreach
variables_to_rmv <- c("Average.equity",
                      "Average.gross.loan.portfolio",
                      "Average.loan.balance.per.borrower",
                      "Average.number.of.active.borrowers",
                      "Average.number.of.loans.outstanding",
                      "Average.outstanding.balance", # Gross Loan Portfolio / Number of active borrowers
                      "Percent.of.female.borrowers")

variables_temp <- c("Average.loan.balance.per.borrower./.GNI.per.capita",
                    "Average.outstanding.balance./.GNI.per.capita") # Gross Loan Portfolio / (Number of active borrowers * GNI per capita))
# sum(is.nan(unlist(temp_df))) = 22, NaNs produced by division by zero

data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Outreach.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>% 
  select(-variables_to_rmv)

# nab <- cbind(data_finsoc_selected$Balance.Sheet.>.Equity, data_finsoc_selected$Outreach.>.Average.equity)
# colSums(is.na(nab))

# 1.12 Productivity and Efficiency
variables_to_rmv <- c("Loans.per.loan.officer",
                      "Loans.per.staff.member",
                      "Personnel.allocation.ratio")
variables_temp <- c("Average.salary./.GNI.per.capita",
                    "Borrowers.per.loan.officer",
                    "Borrowers.per.staff.member",
                    "Cost.per.borrower",
                    "Cost.per.loan",
                    "Deposit.accounts.per.staff.member",
                    "Depositors.per.staff.member",
                    "Operating.expense./.loan.portfolio",
                    "Personnel.expense./.loan.portfolio")

data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Productivity.&.Efficiency.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>% 
  select(-variables_to_rmv)

# 1.13 Revenue and Expenses
variables_to_rmv <- c("Yield.on.gross.portfolio.(nominal)")
variables_temp <- c("Administrative.expense./.assets",
                    "Financial.expense./.assets",
                    "Financial.revenue./.assets",
                    "Operating.expense./.assets",
                    "Personnel.expense./.assets",
                    "Profit.margin",
                    "Provision.for.loan.impairment./.assets",
                    "Yield.on.gross.portfolio.(real)")

data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Revenue.&.Expenses.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>% 
  select(-variables_to_rmv)

# 1.14 Risk and Liquidity
variables_temp <- c("Loan.loss.rate",
                    "Non-earning.liquid.assets.as.a.%.of.total.assets",
                    "Portfolio.at.risk.>.30.days",
                    "Portfolio.at.risk.>.90.days",
                    "Risk.coverage",
                    "Write-off.ratio")
data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Risk.&.Liquidity.>.", variables_temp, sep="")) %>% 
  select(sort(names(.)))

# 1.15 Social Performance
variables_to_rmv <- c("Staff.turnover.rate")
variables_temp <- c("Borrower.retention.rate",
                    "Percent.of.female.board.members",
                    "Percent.of.female.loan.officers",
                    "Percent.of.female.managers",
                    "Percent.of.female.staff")

data_finsoc_selected <- data_finsoc_selected %>% 
  rename_at(vars(variables_temp), ~ paste("Social.Performance.>.", variables_temp, sep="")) %>% 
  select(sort(names(.))) %>% 
  select(-variables_to_rmv)
names(data_finsoc_selected)


## -----
# 2 : Replace infs and NaNs
## -----
# infs are caused by division by zero -> replace with NA
data_finsoc_selected[sapply(data_finsoc_selected, is.infinite)] <- NA
# NaNs are caused by division of zero by zero -> replace with 0
data_finsoc_selected[sapply(data_finsoc_selected, is.nan)] <- 0


## -----
# 3: Save data set
## -----
write.xlsx(data_finsoc_selected, file = paste0("../data-files/data_finsoc_", panel_version, "_clean_social_financial.xlsx"))