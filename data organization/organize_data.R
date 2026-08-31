# LIBRARY----
# library(readr)
# library(purrr)
# library(tidyr)
# library(stringr)
# library(lme4)
# library(modelr)
# library(dplyr)

# source("analysis/analysis_utility_fun.R")
# source("data organization/data_utility_fun.R")
# source("configurations/path_config.R")
# path_in = "data/data_long.rds"
# out_dir = "data/"

get_mod_data <- function(path_in = "data/data_long.rds", out_dir = "data/") {
  
  merged_dat <- readRDS(path_in) %>%
    dplyr::filter(!is.na(nt_p_yst_001___1)) %>% #missing nt_p_yst_001___1 then don't have screentime_device_cell_age_p # only have data from 2-year follow-up
    
    # Age at getting a smart phone: >= 4 # Age higher than the interview age with get interview_age
    mutate(age_round = round(age_br)) %>%
    # Rounding the age before comparing # 16 screentime_device_cell_age_p & 15.75 age is fine
    mutate(nt_p_yst_001__03 = if_else(nt_p_yst_001__03 > age_round, age_round, nt_p_yst_001__03)) %>%
    mutate(nt_p_yst_001__03 = if_else(nt_p_yst_001__03 < 4, NA, nt_p_yst_001__03))
  
  # Outcomes
  outcomes <- c(
    "nc_y_nihtb__flnkr__uncor_score",
    "nc_y_nihtb__picsq__uncor_score",
    "nc_y_nihtb__picvcb__uncor_score",
    "nc_y_nihtb__pttcp__uncor_score",
    "nc_y_nihtb__readr__uncor_score"
  )
  
  
  # Main analysis data
  ## Excluded participants whose caregivers said they owned a cellphone that was not an iPhone or Android (only who said Yes in IV1)
  merged_dat_smartp <- merged_dat %>% filter((nt_p_yst_001___1 == 1 & nt_p_yst_001__02 %in% c(1, 2)) | nt_p_yst_001___1 == 0)
  saveRDS(merged_dat_smartp, file.path(out_dir, "merged_dat_smartp.rds"))
  # Wide data
  get_wide_data(merged_dat_smartp)
  
  # Generate subcohorts
  data_wide <- readRDS(file.path(out_dir, "data_wide.rds")) # N = 7,807 # wide data of merged_dat_smartp (only kids who owned smartphones)
  
  # Split data into 2 halves based on ABCD subsets
  ABCD_participants_2grs <- read.table(file = "~/Library/CloudStorage/Box-Box/2. Barzi Lab - Restricted Access/2-ABCD/ABCD data/partition/participants.tsv", header = TRUE)
  ABCD_participants_2grs <- ABCD_participants_2grs %>%
    mutate(participant_id = stringr::str_replace_all(participant_id, "NDARINV", ""))
  
  id_gr1 <- ABCD_participants_2grs %>% filter(matched_group == 1) %>% pull(participant_id)
  id_gr2 <- ABCD_participants_2grs %>% filter(matched_group == 2) %>% pull(participant_id)
  
  data_wide_subcohort1 <- data_wide %>% filter(participant_id %in% id_gr1)
  data_wide_subcohort2 <- data_wide %>% filter(participant_id %in% id_gr2)
  
  # Analysis 1 and 2
  data_wide_test <- data_wide %>%
    add_change_scores(outcomes)
  
  data_wide_subcohort1_test <- data_wide_subcohort1 %>%
    add_change_scores(outcomes)
  
  data_wide_subcohort2_test <- data_wide_subcohort2 %>%
    add_change_scores(outcomes)
  
  # Analysis 3
  # Control: no sm at 2y 3y 4y
  # Exposure: have sm 2y and 4y
  # Exposure 2: no sm 2y yes 3y
  # Exposure 3: No 3y, yes 4y
  sm_wide <- merged_dat_smartp %>%
    select(participant_id, session_id, nt_p_yst_001___1) %>%
    filter(session_id %in% c("ses-02A","ses-03A","ses-04A")) %>%
    pivot_wider(
      names_from = session_id,
      values_from = nt_p_yst_001___1
    )
  
  sm_groups <- sm_wide %>%
    mutate(
      exposure_group = case_when(
        `ses-02A` == 0 & `ses-03A` == 0 & `ses-04A` == 0 ~ "control",
        `ses-02A` == 1 & `ses-04A` == 1 ~ "sm_2y_4y",
        `ses-02A` == 0 & `ses-03A` == 1 ~ "sm_3y",
        `ses-03A` == 0 & `ses-04A` == 1 ~ "sm_4y",
        TRUE ~ NA_character_
      )
    ) %>% 
    filter(!is.na(exposure_group)) %>% 
    select(participant_id, exposure_group)
  
  data_wide_test_analysis3 <- data_wide_test %>% left_join(sm_groups) %>% add_smartphone_ownership_2to4y()
  saveRDS(data_wide_test, file.path(out_dir, "data_wide_test.rds"))
  saveRDS(data_wide_test_analysis3, file.path(out_dir, "data_wide_test_analysis3.rds"))
  saveRDS(sm_groups, file.path(out_dir, "sm_groups.rds"))
  
  data_wide_subcohort1_test_analysis3 <- data_wide_subcohort1_test %>% left_join(sm_groups) %>% add_smartphone_ownership_2to4y()
  saveRDS(data_wide_subcohort1_test, file.path(out_dir, "data_wide_subcohort1_test.rds"))
  saveRDS(data_wide_subcohort1_test_analysis3, file.path(out_dir, "data_wide_subcohort1_test_analysis3.rds"))
  
  data_wide_subcohort2_test_analysis3 <- data_wide_subcohort2_test %>% left_join(sm_groups) %>% add_smartphone_ownership_2to4y()
  saveRDS(data_wide_subcohort2_test, file.path(out_dir, "data_wide_subcohort2_test.rds"))
  saveRDS(data_wide_subcohort2_test_analysis3, file.path(out_dir, "data_wide_subcohort2_test_analysis3.rds"))
  
  path_smartp     <- file.path(out_dir, "merged_dat_smartp.rds")
  
  c(merged_smartp      = path_smartp)
  
}










