# library(dplyr)
# library(arrow)
# library(data.table)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")


get_physical_health <- function(files_list){
  
  datasets_list = list()
  
  ############################################################
  # Pubertal Development Scale & Menstrual Cycle Survey History [Youth]
  ############################################################
  
  if("ph_y_pds" %in% files_list){
    puberty_y <- read_parquet(file.path(abcd_main_data_path, "ph_y_pds.parquet"))
    
    puberty_y <- puberty_y[ , grep("part|session|(age|categ|mean)$", names(puberty_y))]
    
    puberty_y$male_y_late_or_post_puberty <- ifelse(puberty_y$ph_y_pds__m_categ >= 4, 1,0) |> as.factor()
    puberty_y$female_y_late_or_post_puberty <- ifelse(puberty_y$ph_y_pds__f_categ >=4, 1,0) |> as.factor()
    puberty_y$late_or_post_puberty_both_sexes <- coalesce(puberty_y$male_y_late_or_post_puberty, puberty_y$female_y_late_or_post_puberty)
    puberty_y$puberty_both_sexes <- coalesce(puberty_y$ph_y_pds__f_categ, puberty_y$ph_y_pds__m_categ)
    
    
    # notes to be consider:
    # 1. if participants reported a maximum score on the PDS for two years in a row, then the PDS was discontinued
    # -- should we copy the puberty stage to all subsequent timepoints? 
    # 2. Years 2-5 PDS collection was impacted by the pandemic, with the largest proportion of missingness in Year 3.
    
    datasets_list[["ph_y_pds"]] = puberty_y
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
