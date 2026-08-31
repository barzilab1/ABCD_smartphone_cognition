# Smartphone ownership & neurocognition and imaging outcomes


The current project utilizes tables from ABCD version 6.1, as defined in [ABCD_tables_config.R](configurations/ABCD_tables_config.R).

1. Data Creation:
This part uses the [targets](https://books.ropensci.org/targets/) package to create the dataset.   
Ensure the [path_config.R](configurations/path_config.R) is set up with the location of the ABCD 5.1 files.   
To create the datasets, run the function targets::tar_make() which can be found in the [run.R](run.R) file.   
All data cleaning processes are in the [data organization](data organization) folder.

2. Table 1: Run [1_table1.R](analysis/1_table1.R) to generate descriptive tables.

3. Analyses: Run [2_analysis.R](analysis/2_analysis.R) and [3_extract_results.R](analysis/3_extract_results.R) to run the models and extract the results.

4. Run [4_plots.R](analysis/4_plots.R) to generate figure 1 and 2.
