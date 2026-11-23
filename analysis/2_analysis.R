library(dplyr)
source("analysis/analysis_utility_fun.R", echo=TRUE)

# Read data----

data_wide_test <- readRDS("data/data_wide_test.rds")
data_wide_test_analysis3 <- readRDS("data/data_wide_test_analysis3.rds")
data_wide_subcohort1_test <- readRDS("data/data_wide_subcohort1_test.rds")
data_wide_subcohort1_test_analysis3 <- readRDS("data/data_wide_subcohort1_test_analysis3.rds") %>% mutate(exposure_group = factor(exposure_group, c("control", "sm_2y_4y", "sm_3y", "sm_4y")))
data_wide_subcohort2_test <- readRDS("data/data_wide_subcohort2_test.rds")
data_wide_subcohort2_test_analysis3 <- readRDS("data/data_wide_subcohort2_test_analysis3.rds") %>% mutate(exposure_group = factor(exposure_group, c("control", "sm_2y_4y", "sm_3y", "sm_4y")))

random_effects_s <- "(1 | site_br____02A/family_id_br)"

## Cognition DVs - standardized uncorrected scores
list_DVs_cont_nerocog <- c("nc_y_nihtb__picvcb__uncor_score", "nc_y_nihtb__flnkr__uncor_score", "nc_y_nihtb__pttcp__uncor_score", "nc_y_nihtb__readr__uncor_score", "nc_y_nihtb__picsq__uncor_score")
list_DVs_cont_nerocog_scaled <- paste0("scale(", paste0(list_DVs_cont_nerocog, "_change2to4"), ")")
list_DVs_cont_nerocog_scaled_2Y <- paste0("scale(", paste0(list_DVs_cont_nerocog, "____02A"), ")")

## IVs
list_IVs_1.1_cs <- c("smartphone_owner_bin_2to4y", "smartphone_owner_bin_2to4y*sex_br")
list_IVs_1.2_cs <- c("nt_p_yst_001__03____02A", "nt_p_yst_001__03____02A*sex_br")
list_IVs_1.3_cs <- c("nt_p_yst_001___1____02A", "nt_p_yst_001___1____02A*sex_br") # no age data because already excluded kids who had smartphone at 2 year FU
list_IVs_2_cs <- c("nt_p_yst_001__03_firstage", "nt_p_yst_001__03_firstage*sex_br")


## Covariates----
covars_cs <- c(
  "age_br____02A + sex_br + nt_p_yst_001___2____02A + nt_p_yst_001___3____02A + nt_p_yst_001___4____02A + nt_p_yst_001___6____02A",
  "age_br____02A + sex_br + nt_p_yst_001___2____02A + nt_p_yst_001___3____02A + nt_p_yst_001___4____02A + nt_p_yst_001___6____02A + household_income____02A + parents_high_edu_br____02A",
  "age_br____02A + sex_br + nt_p_yst_001___2____02A + nt_p_yst_001___3____02A + nt_p_yst_001___4____02A + nt_p_yst_001___6____02A + household_income____02A + parents_high_edu_br____02A + lack_sleep____02A"
)

covars_cs_inc <- "age_br____02A + sex_br + nt_p_yst_001___2____02A + nt_p_yst_001___3____02A + nt_p_yst_001___4____02A + nt_p_yst_001___6____02A + parents_high_edu_br____02A"

# Models - Subcohort 1----
main_models_list_subcohort1 <- list()
## Table 2.1.1/ Analysis 1 - Sub-cohort 1 ----
### Exposure= smartphone ownership at age 12, Outcome= cognitive tests at age 12, Covariates= age, sex, other devices, income, education
main_models_list_subcohort1[["subcohort1_analysis1_IV2y_DV2y"]] <-
  run_write_models_addDVbaseline(
    data         = data_wide_subcohort1_test,
    list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
    list_covars  = covars_cs,
    list_IVs     = list_IVs_1.3_cs,
    random_eff   = random_effects_s,
    include_dv2y = F,
    ext          = "subcohort1_analysis1_IV2y_DV2y"
  )

run_write_models_addDVbaseline(
  data         = data_wide_subcohort1_test,
  list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
  list_covars  = covars_cs_inc,
  list_IVs     = "household_income_75kless____02A",
  random_eff   = random_effects_s,
  include_dv2y = F,
  ext          = "subcohort1_analysis1_IVincome75kless2y_DV2y"
)


## Table 2.1.2/ Analysis 2 - Sub-cohort 1 ----
### Exposure= age of smartphone acquisition, Outcome= cognitive tests at age 12, Covariates= age, sex, other devices, income, education
main_models_list_subcohort1[["subcohort1_analysis2_IVage2y_DV2y"]] <-
  run_write_models_addDVbaseline(
    data         = data_wide_subcohort1_test,
    list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
    list_covars  = covars_cs,
    list_IVs     = list_IVs_1.2_cs,
    random_eff   = random_effects_s,
    include_dv2y = F,
    ext          = "subcohort1_analysis2_IVage2y_DV2y"
  )

## Table 2.1.3/ Analysis 3 - Sub-cohort 1 ----
### Analysis 3- Excludes all kids with smartohones at  2-year
### Exposure= acquisition of smartphone between 2-4 year, Outcome= cognitive change between 2-4 year, Covariates= age, sex, other devices, income, education, DV at 2 year
main_models_list_subcohort1[["subcohort1_analysis3_IV2to4y_DV2to4y_EXC2y"]] <-
  run_write_models_addDVbaseline(
    data = data_wide_subcohort1_test_analysis3,
    list_DVs = list_DVs_cont_nerocog_scaled,
    list_covars = covars_cs,
    random_eff = random_effects_s,
    list_IVs = c("exposure_group"),
    include_dv2y = T,
    ext = "subcohort1_analysis3_IV2to4y_DV2to4y_EXC2y"
  )


# Models - Subcohort 2----
main_models_list_subcohort2 <- list()
## Table 2.1.1/ Analysis 1 - Sub-cohort 2 ----
### Exposure= smartphone ownership at age 12, Outcome= cognitive tests at age 12, Covariates= age, sex, other devices, income, education
main_models_list_subcohort2[["subcohort2_analysis1_IV2y_DV2y"]] <-
  run_write_models_addDVbaseline(
    data         = data_wide_subcohort2_test,
    list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
    list_covars  = covars_cs,
    list_IVs     = list_IVs_1.3_cs,
    random_eff   = random_effects_s,
    include_dv2y = F,
    ext          = "subcohort2_analysis1_IV2y_DV2y"
  )

run_write_models_addDVbaseline(
  data         = data_wide_subcohort2_test,
  list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
  list_covars  = covars_cs_inc,
  list_IVs     = "household_income_75kless____02A",
  random_eff   = random_effects_s,
  include_dv2y = F,
  ext          = "subcohort2_analysis1_IVincome75kless2y_DV2y"
)


## Table 2.1.2/ Analysis 2 - Sub-cohort 2 ----
### Exposure= age of smartphone acquisition, Outcome= cognitive tests at age 12, Covariates= age, sex, other devices, income, education
main_models_list_subcohort2[["subcohort2_analysis2_IVage2y_DV2y"]] <-
  run_write_models_addDVbaseline(
    data         = data_wide_subcohort2_test,
    list_DVs     = list_DVs_cont_nerocog_scaled_2Y,
    list_covars  = covars_cs,
    list_IVs     = list_IVs_1.2_cs,
    random_eff   = random_effects_s,
    include_dv2y = F,
    ext          = "subcohort2_analysis2_IVage2y_DV2y"
  )

## Table 2.1.3/ Analysis 3 - Sub-cohort 2 ----
### Analysis 3- Excludes all kids with smartohones at  2-year
### Exposure= acquisition of smartphone between 2-4 year, Outcome= cognitive change between 2-4 year, Covariates= age, sex, other devices, income, education, DV at 2 year
main_models_list_subcohort2[["subcohort2_analysis3_IV2to4y_DV2to4y_EXC2y"]] <-
  run_write_models_addDVbaseline(
    data = data_wide_subcohort2_test_analysis3,
    list_DVs = list_DVs_cont_nerocog_scaled,
    list_covars = covars_cs,
    random_eff = random_effects_s,
    list_IVs = c("exposure_group"),
    include_dv2y = T,
    ext = "subcohort2_analysis3_IV2to4y_DV2to4y_EXC2y"
  )


# Stratified results for figure 1----
## Analysis 1: Stratified by household income (<75K vs. >=75K)

# Analysis 1: Stratified by household income (<75K vs. >=75K)

income_var <- "household_income_75kless____02A"

for (inc in c(0, 1)) {
    inc_label <- ifelse(inc == 1, "less75k", "75kplus")

    main_models_list_subcohort1[[paste0("subcohort1_analysis1_", inc_label)]] <-
        run_write_models_addDVbaseline(
            data = data_wide_subcohort1_test %>%
                dplyr::filter(.data[[income_var]] == inc),
            list_DVs = list_DVs_cont_nerocog_scaled_2Y,
            list_covars = covars_cs_inc,
            list_IVs = list_IVs_1.3_cs,
            random_eff = random_effects_s,
            include_dv2y = FALSE,
            ext = paste0("subcohort1_analysis1_IV2y_DV2y_", inc_label)
        )

    main_models_list_subcohort2[[paste0("subcohort2_analysis1_", inc_label)]] <-
        run_write_models_addDVbaseline(
            data = data_wide_subcohort2_test %>%
                dplyr::filter(.data[[income_var]] == inc),
            list_DVs = list_DVs_cont_nerocog_scaled_2Y,
            list_covars = covars_cs_inc,
            list_IVs = list_IVs_1.3_cs,
            random_eff = random_effects_s,
            include_dv2y = FALSE,
            ext = paste0("subcohort2_analysis1_IV2y_DV2y_", inc_label)
        )
}

# Bootstrap models----
## Analysis 1----
analysis1 <- run_split_half_bootstrap_allDV(
  data = data_wide_test,
  list_DVs = list_DVs_cont_nerocog_scaled_2Y,
  list_covars = covars_cs[2],
  random_eff = random_effects_s,
  list_IVs = "nt_p_yst_001___1____02A",
  key_predictor = c("nt_p_yst_001___1____02A"),
  B = 100
)

analysis1_boot   <- analysis1$boot_df_all
analysis1_summary <- analysis1$summary

write.csv(analysis1_boot, "results/analysis1_boot.csv")
write.csv(analysis1_summary, "results/analysis1_summary.csv")

## Analysis 2----
analysis2 <- run_split_half_bootstrap_allDV(
  data = data_wide_test,
  list_DVs = list_DVs_cont_nerocog_scaled_2Y,
  list_covars = covars_cs[2],
  random_eff = random_effects_s,
  list_IVs = "nt_p_yst_001__03____02A",
  key_predictor = c("nt_p_yst_001__03____02A"),
  B = 100
)

analysis2_boot   <- analysis2$boot_df_all
analysis2_summary <- analysis2$summary

write.csv(analysis2_boot, "results/analysis2_boot.csv")
write.csv(analysis2_summary, "results/analysis2_summary.csv")

## Analysis 3----
analysis3 <- run_split_half_bootstrap_allDV(
  data = data_wide_subcohort1_test_analysis3,
  list_DVs = list_DVs_cont_nerocog_scaled,
  list_covars = covars_cs[2],
  random_eff = random_effects_s,
  list_IVs = "exposure_group",
  key_predictor = c(
    "exposure_groupsm_2y_4y",
    "exposure_groupsm_3y",
    "exposure_groupsm_4y"
  ),
  include_dv2y = TRUE,
  B = 100
)

analysis3_boot   <- analysis3$boot_df_all
analysis3_summary <- analysis3$summary

write.csv(analysis3_boot, "results/analysis3_boot.csv")
write.csv(analysis3_summary, "results/analysis3_summary.csv")
