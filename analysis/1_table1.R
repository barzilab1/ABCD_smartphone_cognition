library(naniar)
library(dplyr)
library(scales)
library(tableone)

data_wide_test <- readRDS("data/data_wide_test.rds")
data_wide_subcohort1_test <- readRDS("data/data_wide_subcohort1_test.rds")
data_wide_subcohort2_test <- readRDS("data/data_wide_subcohort2_test.rds")
data_wide_subcohort1_test_analysis3 <- readRDS("data/data_wide_subcohort1_test_analysis3.rds")
data_wide_subcohort2_test_analysis3 <- readRDS("data/data_wide_subcohort2_test_analysis3.rds")


vars <- c("age_br____02A", "sex_br", "parents_high_edu_text_br_master_above____02A", "household_income_75kless____02A")
factor_vars <- c("sex_br", "parents_high_edu_text_br_master_above____02A", "household_income_75kless____02A")

# Sub-cohort 1----
## Table 1.1/ Analysis 1 ----
### Exposure= smartphone ownership at age 12, Outcome= cognitive tests at age 12
write.csv(print(CreateTableOne(
  data = data_wide_subcohort1_test %>% filter(!is.na(nt_p_yst_001___1____02A)),
  strata = "nt_p_yst_001___1____02A",
  vars = vars,
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.1.csv")

## Table 1.2/ Analysis 2 ----
### Exposure= age of smartphone acquisition, Outcome= cognitive tests at age 12
write.csv(print(CreateTableOne(
  data = data_wide_subcohort1_test %>% filter(!is.na(nt_p_yst_001__03____02A)),
  vars = vars,
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.2.csv")

## Table 1.3/ Analysis 3 ----
# Analysis 3- Excludes all kids with smartohones at  2-year
# Exposure= acquisition of smartphone between 2-4 year, Outcome= cognitive change between 2-4 year
write.csv(print(CreateTableOne(
  data = data_wide_subcohort1_test_analysis3 %>% filter(!is.na(exposure_group)),
  strata = "exposure_group",
  vars = c(vars, "age_br____04A"),
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.3.csv")

# Sub-cohort 2----
## Table 1.1/ Analysis 1 ----
### Exposure= smartphone ownership at age 12, Outcome= cognitive tests at age 12
write.csv(print(CreateTableOne(
  data = data_wide_subcohort2_test %>% filter(!is.na(nt_p_yst_001___1____02A)),
  strata = "nt_p_yst_001___1____02A",
  vars = vars,
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.4.csv")
## Table 1.2/ Analysis 2 ----
### Exposure= age of smartphone acquisition, Outcome= cognitive tests at age 12
write.csv(print(CreateTableOne(
  data = data_wide_subcohort2_test %>% filter(!is.na(nt_p_yst_001__03____02A)),
  vars = vars,
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.5.csv")

## Table 1.3/ Analysis 3 ----
# Analysis 3- Excludes all kids with smartohones at  2-year
# Exposure= acquisition of smartphone between 2-4 year, Outcome= cognitive change between 2-4 year
write.csv(print(CreateTableOne(
  data = data_wide_subcohort2_test_analysis3 %>% filter(!is.na(exposure_group)),
  strata = "exposure_group",
  vars = c(vars, "age_br____04A"),
  factorVars = factor_vars,
  addOverall = T,
  includeNA = T
), contDigits = 2, catDigits = 2, pDigits = 5, printToggle = FALSE), "results/table1.6.csv")

# Missingness of DVs
var_order <- c(
  "nc_y_nihtb__flnkr__uncor_score____02A",
  "nc_y_nihtb__picsq__uncor_score____02A",
  "nc_y_nihtb__picvcb__uncor_score____02A",
  "nc_y_nihtb__pttcp__uncor_score____02A",
  "nc_y_nihtb__readr__uncor_score____02A"
)
var_order_2 <- c(
  "nc_y_nihtb__flnkr__uncor_score_change2to4",
  "nc_y_nihtb__picsq__uncor_score_change2to4",
  "nc_y_nihtb__picvcb__uncor_score_change2to4",
  "nc_y_nihtb__pttcp__uncor_score_change2to4",
  "nc_y_nihtb__readr__uncor_score_change2to4"
)


get_missing_summary <- function(data, vars, filter_var = NULL) {
  if (!is.null(filter_var)) data <- data %>% filter(!is.na(.data[[filter_var]]))
  data %>%
    select(all_of(vars)) %>%
    miss_var_summary() %>%
    mutate(
      missing_N_pct = sprintf("%d (%.1f%%)", n_miss, pct_miss),
      variable = factor(variable, levels = vars)
    ) %>%
    arrange(variable) %>%
    select(variable, missing_N_pct)
}

get_missing_summary(data_wide_subcohort1_test, var_order)
get_missing_summary(data_wide_subcohort1_test, var_order, "nt_p_yst_001__03____02A")
get_missing_summary(data_wide_subcohort1_test_analysis3, var_order_2)
get_missing_summary(data_wide_subcohort2_test, var_order)
get_missing_summary(data_wide_subcohort2_test, var_order, "nt_p_yst_001__03____02A")
get_missing_summary(data_wide_subcohort2_test_analysis3, var_order_2)
