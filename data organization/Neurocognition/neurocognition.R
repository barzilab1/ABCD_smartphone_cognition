# library(dplyr)
# library(arrow)
# source("configurations/path_config.R")


get_neurocognition <- function(files_list) {
  
  datasets_list = list()
  
  ############################################################
  # Emotional Stroop Task [Youth]
  ############################################################
  # if("nc_y_est" %in% files_list){
    # stroop_task <- read_parquet(file.path(abcd_main_data_path, "nc_y_est.parquet")) # 1-3-5y
    # stroop_task <- stroop_task %>% select(matches("participant|session|(_all|congr|_eq|_equal|_acc|_rt)$"))
    # stroop_task <- stroop_task %>%
    #   # remove kids with wrong events
    #   filter(is.na(strp_scr_script_wrong_event)) %>% 
    #   ## remove outliers: stroop test rt < 200 or > 2000 are 0s
    #   mutate(across(contains("strp_") & contains("rt"),
    #                 ~case_when(.x < 200 | .x > 2000 ~ NA_real_,
    #                            TRUE ~ as.numeric(.x))))
    
    # Emotion regulation was quantified using 
    # the ratio of average reaction times for correct incongruent over correct congruent stimuli on the emotional Stroop task
    
    # stroop_task <- stroop_task %>%
    #   rowwise() %>%
    #   mutate(
    #     # interference effects in the mostly congruent block and in equal blocks
    #     interference_effect_morecongr = (strp_scr_acc_morecongrincongr - strp_scr_acc_morecongrcongr)/strp_scr_acc_morecongrcongr,
    #     interference_effect_equal = (strp_scr_acc_equalincongr - strp_scr_acc_equalcongr)/strp_scr_acc_equalcongr,
    #     
    #     # emotional information processing
    #     ## performance on happy faces in the equal block minus performance on angry faces in the equal block averaging across congruent and incongruent trials ## Kate adds the same thing for more congruent block
    #     emotion_info_process_morecongr_congr = strp_scr_conhapf75con_acc - strp_scr_conangf75con_acc,
    #     emotion_info_process_morecongr_incongr = strp_scr_inchapf75con_acc - strp_scr_incangf75con_acc,
    #     emotion_info_process_morecongr = mean(c(emotion_info_process_morecongr_congr, emotion_info_process_morecongr_incongr), na.rm = T),
    #     
    #     emotion_info_process_equal_congr = strp_scr_conhapfequal_acc - strp_scr_conangfequal_acc,
    #     emotion_info_process_equal_incongr = strp_scr_inchapfequal_acc - strp_scr_incangfequal_acc,
    #     emotion_info_process_equal = mean(c(emotion_info_process_equal_congr, emotion_info_process_equal_incongr), na.rm = T)
    #   )
    # datasets_list[["nc_y_est"]] = stroop_task
  # }
  
  
  ############################################################
  # NIH Toolbox [Youth]
  ############################################################
  # Picture Vocabulary, Flanker, Pattern Comparison, Reading Recognition, and Picture Sequence Memory
  # Be sure to use the "uncorrected" summary scores for each task. Here are the variable names for the 5 tasks: 
  # nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_pattern_uncorrected, nihtbx_reading_uncorrected, nihtbx_picture_uncorrected
  if("nc_y_nihtb" %in% files_list){
    nih_toolbox <- read_parquet(file.path(abcd_main_data_path, "nc_y_nihtb.parquet")) %>%  # bl, 2 and 4y, 6y
      select(matches("participant|session|uncor_score"))
    datasets_list[["nc_y_nihtb"]] = nih_toolbox
  }
  
  ############################################################
  ############################################################
  
  # check that all files were loaded
  if(length(files_list) != length(datasets_list)){
    missing_files = setdiff(files_list, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }
  
  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)
  
  return(combined_dataframe)
}



