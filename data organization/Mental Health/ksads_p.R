# library(dplyr)
# library(readr)
# library(readxl)
# library(stringi)
# library(purrr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_ksads_p <- function(ksad_p_tables){


  datasets_list = list()
  
  #########################################################
  # ADHD
  #########################################################
  if("ADHD" %in% ksad_p_tables){
    adhd_df <- read_parquet(file.path(abcd_main_data_path, "mh_p_ksads__adhd.parquet"))
    
    adhd_df = replace_888_with_0(adhd_df)
    
    adhd_df$adhd_dx_p = create_diagnosis_summary_var(adhd_df, T)
    
    adhd_df <- adhd_df %>% 
      select(participant_id, session_id, adhd_dx_p) %>% 
      mutate(adhd_dx_p = as.numeric(as.character(adhd_dx_p)))
    
    adhd_df = adhd_df[,c("participant_id", "session_id", "adhd_dx_p")]
    
    datasets_list[["ADHD"]] = adhd_df
  }

  ############################################################
  ############################################################

  # check that all files were loaded
  # if(length(ksad_tables) != length(datasets_list)){
  #   missing_files = setdiff(ksad_tables, names(datasets_list) )
  #   print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  # }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)
}


