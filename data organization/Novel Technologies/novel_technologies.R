# library(dplyr)
# library(arrow)
# source("configurations/path_config.R")


get_novel_technologies <- function(files_list){

    datasets_list = list()

    ############################################################
    # Screen Time Questionnaire [Parent]
    ###########################################################
    if("nt_p_yst" %in% files_list){
        stq_p <- read_parquet(file.path(abcd_main_data_path, "nt_p_yst.parquet"))

        stq_p <- stq_p %>%
          # select(matches("participant|session|screentime_device_p__1|screentime_device_cell(_age)?_p")) %>%
          select(matches("participant|session|nt_p_yst_001___[1-6]|nt_p_yst_001__0[23]")) %>%
          filter(!is.na(nt_p_yst_001___1))
        
        stq_p[stq_p == 777 | stq_p == 999] <- NA
        
        stq_p <- droplevels(stq_p)
        
        stq_p <- stq_p %>%
          mutate(across(matches("nt_p"), ~ as.numeric(as.character(.x))))

        stq_p$nt_p_yst_001__03[stq_p$nt_p_yst_001__03 > 16 | stq_p$nt_p_yst_001__03 == 0] <- NA # outliers 111 etc # Check min cutoff??

        datasets_list[["nt_p_yst"]] = stq_p
    }


    ############################################################
    # Screen Time Questionnaire [Youth]
    ###########################################################
    # Mobile phone involvement questionnaire score
    if("nt_y_stq" %in% files_list){
        st_y <- read_parquet(file.path(abcd_main_data_path, "nt_y_stq.parquet"))

        # st_y_mpiv <- st_y %>% select(matches("src|event|screentime_phone"))
        st_y_mpiv <- st_y %>% select(matches("participant|session|nt_y_stq__phone"))
        st_y_mpiv[st_y_mpiv == 777] <- NA
        st_y_mpiv <- droplevels(st_y_mpiv)
        # mean, total of mpiv screentime_phone1 to screentime_phone8
        # Select columns that match the pattern "screentime_phone" using grepl
        selected_columns <- grep("nt_y_stq__phone_00[1-8]", names(st_y_mpiv), value = TRUE)
        
        st_y_mpiv <- st_y_mpiv %>% mutate(across(all_of(selected_columns), ~ as.numeric(as.character(.x))))

        # Apply the custom function to each row, selecting only the relevant columns
        st_y_mpiv$screentime_phone_mean <- apply(st_y_mpiv[, selected_columns], 1, \(row){
          # Check if all selected columns are NA for the row
          if (all(is.na(row))) {
            return(NA)  # If all are NA, return NA
          }
          return(mean(row, na.rm = TRUE))  # Otherwise, compute the mean (ignoring NA)
        })

        datasets_list[["nt_y_stq"]] = st_y_mpiv

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

    # write_csv(combined_dataframe, "data/physical_health.csv", na = "")
    # return("data/physical_health.csv")
    return(combined_dataframe)

}





