############# file paths #############

#path to abcd Restricted Access folder
root = "~"
box_path = "Library/CloudStorage/Box-Box"
abcd_box_path = "2. Barzi Lab - Restricted Access/2-ABCD"
abcd_box_data_path = "ABCD data"
abcd_version = "6.1"
abcd_bids_main_path = "tabulated"
# abcd_bids_abcc_path = "abcc/rawdata"

prs_box_path = "Ran_Barzilay"


# ~/Library/CloudStorage/Box-Box/2. Barzi Lab - Restricted Access/2-ABCD/ABCD data/6.1
abcd_data = file.path(root, box_path, abcd_box_path, abcd_box_data_path)
# additional_files_path = file.path(root, box_path, abcd_box_path,"Additional files")
abcd_main_data_path = file.path(abcd_data, abcd_version, abcd_bids_main_path)
# abcd_abcc_data_path = file.path(abcd_data, abcd_version, abcd_bids_abcc_path)


abcd_genetics_file = file.path(abcd_data, "genetics/genetic.csv")


exposome_tyler = "Projects/exposome_Tyler/3.0/results"
p_factor_files_path = file.path(root, box_path, abcd_box_path, exposome_tyler, "p factor scores")
e_factor_files_path = file.path(root, box_path, abcd_box_path, exposome_tyler, "e factor scores")


allostatic_load_files_path = file.path(root, box_path, abcd_box_path, "Projects/Allostatic load tyler/ABCD_allostatic_load_factor_scores.csv")

