# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("dplyr", "readr", "stringr", "stringi",
               "arrow", "purrr", "tidyr",
               "lubridate", "janitor", "modelr", "lme4",
               "cdcanthro", "data.table", "readxl"),
  format = "rds" # default format, unless specifically specified in targets
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  # , controller = crew::crew_controller_local(workers = 6)
  #
  
)


# Run the R scripts in the R/ folder with your custom functions:
tar_source("configurations")
tar_source("data organization")


# Replace the target list below with your own:
list(
  # load data
  tar_target(abcd_general_data, get_abcd_general()),
  
  tar_target(friends_family_community_data, get_friends_family_community(friends_family_community_tables)),
  tar_target(novel_technologies_data, get_novel_technologies(novel_technologies_tables)),
  tar_target(physical_health_data, get_physical_health(physical_health_tables)),
  tar_target(neurocognition_data, get_neurocognition(neurocognition_tables)),
  
  # merge
  tar_target(merged_data,
             merge_all_data(tp_include = tp_include
                            , abcd_general_data
                            , friends_family_community_data
                            , novel_technologies_data
                            , physical_health_data
                            , neurocognition_data
             )),
  
  # 2. Run get_mod_data() to write all RDSs and return their paths
  tar_target(
    mod_paths,
    get_mod_data(path_in = merged_data, out_dir = "data/")
  ),

  # 3. Declare each written CSV as its own file target
  tar_target(
    merged_smartp_csv,
    mod_paths["merged_smartp"],
    format = "file"
  )
)
