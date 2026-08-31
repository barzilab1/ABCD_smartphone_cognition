library(rvest)
library(dplyr)
library(stringr)
library(purrr)
source("analysis/analysis_utility_fun.R")

# Models - Subcohort 1----
## Table 2.1.1/ Analysis 1 - Sub-cohort 1 ----
subcohort1_analysis1_IV2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort1_analysis1_IV2y_DV2y.xls",
    var_filter = "nt_p_yst_001___1____02A",
    num_mod    = 4,
    iv_tag     = "__subcohort1"
  )
## Table 2.1.2/ Analysis 2 - Sub-cohort 1 ----
subcohort1_analysis2_IVage2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort1_analysis2_IVage2y_DV2y.xls",
    var_filter = "nt_p_yst_001__03____02A",
    num_mod    = 4,
    iv_tag     = "__subcohort1"
  )

## Table 2.1.3/ Analysis 3 - Sub-cohort 1 ----
subcohort1_analysis3_IV2to4y_DV2to4y_EXC2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort1_analysis3_IV2to4y_DV2to4y_EXC2y.xls",
    var_filter = c("exposure_groupsm_2y_4y",
                   "exposure_groupsm_3y",
                   "exposure_groupsm_4y"),
    num_mod    = 2,
    iv_tag     = "__subcohort1"
  )


combined_data_subcohort1 <- subcohort1_analysis1_IV2y_DV2y %>%
  bind_rows(subcohort1_analysis2_IVage2y_DV2y) %>%
  bind_rows(subcohort1_analysis3_IV2to4y_DV2to4y_EXC2y)


write_formatted_results_s(
  data = combined_data_subcohort1,
  output_file = "results_paper/combined_data_subcohort1.xlsx"
)



# Models - Subcohort 2----
## Table 2.1.1/ Analysis 1 - Sub-cohort 2 ----
subcohort2_analysis1_IV2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort2_analysis1_IV2y_DV2y.xls",
    var_filter = "nt_p_yst_001___1____02A",
    num_mod    = 4,
    iv_tag     = "__subcohort2"
  )
## Table 2.1.2/ Analysis 2 - Sub-cohort 2 ----
subcohort2_analysis2_IVage2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort2_analysis2_IVage2y_DV2y.xls",
    var_filter = "nt_p_yst_001__03____02A",
    num_mod    = 4,
    iv_tag     = "__subcohort2"
  )

## Table 2.1.3/ Analysis 3 - Sub-cohort 2 ----
subcohort2_analysis3_IV2to4y_DV2to4y_EXC2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort2_analysis3_IV2to4y_DV2to4y_EXC2y.xls",
    var_filter = "smartphone_owner_bin_2to4y",
    num_mod    = 4,
    iv_tag     = "__subcohort2"
  )


combined_data_subcohort2 <- subcohort2_analysis1_IV2y_DV2y %>%
  bind_rows(subcohort2_analysis2_IVage2y_DV2y) %>%
  bind_rows(subcohort2_analysis3_IV2to4y_DV2to4y_EXC2y)


write_formatted_results_s(
  data = combined_data_subcohort2,
  output_file = "results_paper/combined_data_subcohort2.xlsx"
)

# Income Sub-cohort 1 ---
subcohort1_analysis1_IVincome75kless2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort1_analysis1_IVincome75kless2y_DV2y.xls",
    var_filter = "household_income_75kless____02A",
    num_mod    = 1,
    iv_tag     = "__subcohort1"
  )


# Income Sub-cohort 2 ---
subcohort2_analysis1_IVincome75kless2y_DV2y <- 
  read_and_filter_files(
    directory  = "results/",
    pattern    = "__subcohort2_analysis1_IVincome75kless2y_DV2y.xls",
    var_filter = "household_income_75kless____02A",
    num_mod    = 1,
    iv_tag     = "__subcohort2"
  )













