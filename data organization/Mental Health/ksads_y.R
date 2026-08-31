# library(dplyr)
# library(arrow)
# library(readxl)
# library(stringi)
# library(purrr)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_ksads_y <- function(ksad_y_tables){

  datasets_list = list()
  
  #########################################################
  # Suicide
  #########################################################
  if("Suicide" %in% ksad_y_tables){
    sui_df<- read_parquet(file.path(abcd_main_data_path, "mh_y_ksads__suic.parquet"))
    
    sui_df = replace_888_with_0(sui_df)
    
    # ABCD 6 fix
    # if session id is ses-05A or ses-06A, set all past_dx to NA
    sui_df <- sui_df %>%
      mutate(across(matches("__past_dx"), ~ replace(.x, session_id %in% c("ses-05A", "ses-06A"), NA)))
    
    # Create ever (past or present) variable
    ## SA
    sui_df$SA_current_y <- apply(sui_df[,grepl("__atmpt.*pres_dx$", colnames(sui_df))], 1, \(x) {any(x == 1)*1}) 
    sui_df$SA_past_y <- apply(sui_df[,grepl("__atmpt.*past_dx$", colnames(sui_df))], 1, \(x) {any(x == 1)*1}) 
    sui_df$SA_y <-(sui_df$SA_current_y == 1 | sui_df$SA_past_y == 1)*1 
    
    ## SI: current or past = 1 & SA_y != 1 --> 1
    sui_df$SI_current_y <- apply(sui_df[,grepl("__(pass|actv|imnt).*pres_dx$", colnames(sui_df))], 1, \(x) {any(x == 1)*1}) 
    sui_df$SI_past_y <- apply(sui_df[,grepl("__(pass|actv|imnt).*past_dx$", colnames(sui_df))], 1, \(x) {any(x == 1)*1}) 
    sui_df$SI_y <- (sui_df$SI_current_y == 1 | sui_df$SI_past_y == 1)*1
    ## Those with SA_y == 1 will get SI_y == 0/NA depends on the project
    sui_df$SI_no_SA_y <- ifelse((sui_df$SI_y == 1 & !is.na(sui_df$SA_y) & sui_df$SA_y == 1 ), 0 , sui_df$SI_y) 
    
    ## nssi
    # suicide$nssi_current_y <- suicide$ksads_23_945_905_t
    # suicide$nssi_past_y <- suicide$ksads_23_956_916_t
    # suicide$nssi_y <- (suicide$nssi_current_y == 1 | suicide$nssi_past_y == 1)*1
    
    ## no suicide - no range, all 0
    # suicide$nosui_current_y <- suicide$ksads_23_955_915_t
    # suicide$nosui_past_y <- suicide$ksads_23_966_926_t
    # suicide$nosui_df <- (suicide$nosui_current_y == 1 | suicide$nosui_past_y == 1)*1
    
    sui_df <- sui_df[,grepl("^(parti|session|SA|SI)", colnames(sui_df))]
    
    # convert all columns to factor
    # cols_to_factor <- grep("^(SA|SI)", names(sui_df), value = TRUE)
    # sui_df[cols_to_factor] <- lapply(sui_df[cols_to_factor], factor)
    
    datasets_list[["Suicide"]] = sui_df
  }
  
  # library("table1")
  # library("htmltools")
  # htmltools::save_html(table1::table1(~ . | session_id, data = sui_df[-1]), "sui_by_session_id.html")
  
  #########################################################
  # Depression
  #########################################################
  if("Depression" %in% ksad_y_tables){
    depression_df <- read_parquet(file.path(abcd_main_data_path, "mh_y_ksads__dep.parquet"))
    
    depression_df <- replace_888_with_0(depression_df)
    
    depression_df$depression_dx_y <- create_diagnosis_summary_var(depression_df, T)
    
    depression_df <- depression_df %>% select(participant_id, session_id, depression_dx_y) %>% 
      mutate(depression_dx_y = as.numeric(as.character(depression_dx_y)))
    
    datasets_list[["Depression"]] = depression_df
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


