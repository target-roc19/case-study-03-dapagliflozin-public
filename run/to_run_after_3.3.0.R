#-------------------------------
# TARGET-EU Case Study 3 script

# authors: Rosa Gini, Davide Messina, Olga Paoletti, Claudia Bartolini, Sofia Miniati, Azzurra Bongianni

# v3.4.0

# added individual non-administrative censoring in CPRD

# v3.3.0
# 09 Sep 2026
# MICE of HbA1c

# v 3.2.0

# 4 Sep 2026

# addressing most comments after Draft Deliverable


# v 3.1.0 

# 05 Aug 2026

# fixes

# v 3.0.0 

# 30 Jul 2026

# complete analysis

# v 2.0.0 

# 9 Jul 2026

# v 1.1.0 - 22 Jun 2026

# fixed relevant bug in selection criteria
# fixed minor bugs

# v 1.0.4 - 17 Jun 2026

# expanded support for parquet instances

# v 1.0.3 - 17 Jun 2026

# handled in 02_parameters_CDM the case when both csv and parquet files are included

# v 1.0.2 - 12 Jun 2026

# added correct itemsets for CPRD
# added masked diagnostics for clean lab and for prescription management to g_export

# v 1.0.1 - 16 Jun 2026

# fix for BIFAP issue https://github.com/target-roc19/case-study-03-dapagliflozin/issues/69

# v 1.0.0 - 12 Jun 2026

# first release

# v 0.2.4 - 08 Apr 2026
# Fix for #30, filtering step now converts the date column to filter to an integer

# v 0.2.3 - 07 Apr 2026
# Fix for #29, CCD now imports all columns as strings

# v 0.2.2 - 02 Apr 2026
# Fix for #28, CCD now use much less memory

# v 0.2.1 - 26 Mar 2026
# Fix for #27, METADATA are now stored internally in the repo
# Updated polars installation

# v 0.2 - 25 Mar 2026
# Release to reduce CDM instance for BIFAP

# v 0.1 - 18 Mar 2026
# clean

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##%######################################################%##
#                                                          #
####                     PARAMETERS                     ####
#                                                          #
##%######################################################%##

TEST <- F
REDUCTION <- F
localprocessing <- T

# DEAP: please fill out here the address of your instance to be used in this run (@BIFAP: use the reduced instance)
dirinput <- ""

source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))
source(paste0(thisdir,"/p_parameters/05_subpopulations.R"))
source(paste0(thisdir,"/p_parameters/08_folders_submission.R"))
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R"))

##%######################################################%##
#                                                          #
####                    MAIN SCRIPT                     ####
#                                                          #
##%######################################################%##


launch_step("../transformations/00_store_settings.R")

# if (REDUCTION) {
#   launch_step("../transformations/00_T2_10_CreateConceptSetDatasets_for_reduction.R")
#   launch_step("../transformations/00_T2_20_Create_reduced_CDM.R")
# }
# 
# launch_step("../transformations/01_T2_10_CreateConceptSetDatasets.R")
# 
# launch_step("../transformations/01_T2_20_CreateItemSetDatasets.R")

if (thisdatasource == "CPRD") {
  launch_step("../transformations/01_T2_21_CreateItemSetDatasets_missing_itemset.R")  
}


# launch_step("../transformations/02_T2_10_Create_D3_PERSONS.R")
# 
# launch_step("../transformations/02_T2_20_Create_D3_OBSPERIODS.R")
#  
# launch_step("../transformations/02_T2_30_Create_D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART.R")
# 
# launch_step("../transformations/02_T2_40_Create_D3_CLEAN_LAB.R")
# 
# launch_step("../transformations/02_T2_42_Create_D3_EGFR.R")
# 
# launch_step("../transformations/02_T2_44_Create_D3_covariates_simple_TD.R")
# 
# launch_step("../transformations/02_T2_46_Create_D3_covariates_complex_TD.R")
# 
# launch_step("../transformations/02_T2_50_Create_D3_EPISODES_STUDY_DRUGS.R")
# 
# launch_step("../transformations/02_T2_60_Create_D3_ELIGIBILITY.R")
#  
# launch_step("../transformations/03_T3_10_Create_D3_STUDY_POPULATION.R")
# 
# launch_step("../transformations/04_T2_10_create_D3_covariate_imputed_TD.R")
# 
# launch_step("../transformations/04_T2_20_Create_D3_EXP_ACT_COV.R") 
# 
# launch_step("../transformations/04_T2_22_Create_D3_EXP_ACT_COV_IMPUTATION.R") 
# 
# launch_step("../transformations/04_T2_30_Create_D3_OUTCOMES.R")
# 
# launch_step("../transformations/05_T3_10_Create_D4_EXP_ACT_COV_WEIGHT.R") 
# 
# launch_step("../transformations/05_T3_12_Create_D4_EXP_ACT_COV_WEIGHT_MICE.R") 

launch_step("../transformations/05_T3_20_Create_D4_FUP.R")

# launch_step("../transformations/05_T3_30_Create_D4_EXP_ACT_COV_WEIGHT_TRUNCATED.R")
# 
# launch_step("../transformations/05_T3_32_Create_D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED.R")

launch_step("../transformations/05_T3_40_Create_D4_ANALYTICAL_DATASET_HR_RMST_TP.R")

launch_step("../transformations/05_T3_42_Create_D4_ANALYTICAL_DATASET_HR_MICE.R")

launch_step("../transformations/05_T3_50_Create_D4_CENSORING_WEIGHTS_TRUNCATED.R") 

# launch_step("../transformations/07_T4_10_Create_D5_Table_1.R") 
# 
# launch_step("../transformations/07_T4_20_Create_D5_Table_2.R")
# 
# launch_step("../transformations/07_T4_30_Create_D5_Supplementary_Figure_1.R")
# 
# launch_step("../transformations/07_T4_32_Create_D5_Supplementary_Figure_2_MICE.R")
# 
# launch_step("../transformations/07_T4_40_Create_D5_Table_3_asd_before_after_trimming.R")

launch_step("../transformations/07_T4_50_Create_D5_Table_4_follow_up_time_and_reasons_for_censoring.R")

launch_step("../transformations/07_T4_50_Create_D5_table_S3.R")

launch_step("../transformations/07_T4_60_Create_D5_ResultsIR_HR.R")

launch_step("../transformations/07_T4_62_Create_D5_ResultsIR_MICE.R")

launch_step("../transformations/07_T4_70_Create_D5_ResultsRMST.R")

launch_step("../transformations/07_T4_80_Create_D5_ResultsIPC.R")

# launch_step("../transformations/08_T4_10_Create_D5_Table_1_masked.R")
# 
# launch_step("../transformations/08_T4_20_Create_D5_Table_2_masked.R")
# 
# launch_step("../transformations/08_T4_30_Create_D5_Supplementary_Figure_1_masked.R")
# 
# launch_step("../transformations/08_T4_32_Create_D5_Supplementary_Figure_2_masked_MICE.R")
# 
# launch_step("../transformations/08_T4_40_Create_D5_Table_3_asd_before_after_trimming_masked.R")

launch_step("../transformations/08_T4_50_Create_D5_Table_4_follow_up_time_and_reasons_for_censoring_masked.R")

launch_step("../transformations/08_T4_60_Create_D5_Table_5_crude_IR_RMST_masked.R")

# launch_step("../transformations/09_T5_10_Create_D6_Table_1_and_Figure_1_masked.R")
# 
# launch_step("../transformations/09_T5_20_Create_D6_Table_2.R")
# 
# launch_step("../transformations/09_T5_30_Create_D6_Supplementary_Figure_1.R")
# 
# launch_step("../transformations/09_T5_32_Create_D6_Supplementary_Figure_2_MICE.R")

launch_step("../transformations/09_T5_35_Create_D6_table_S3.R")

launch_step("../transformations/09_T5_40_Create_D6_Table_3.R")

launch_step("../transformations/09_T5_42_Create_D6_Table_S2.R")

launch_step("../transformations/09_T5_50_Create_D6_Table_4_follow_up_time_and_reasons_for_censoring.R")

launch_step("../transformations/09_T5_60_Create_D6_Table_5_crude_IR_RMST.R")

launch_step("../transformations/09_T5_70_Create_D6_Table_6_HR.R")

launch_step("../transformations/09_T5_80_Create_D6_Table_8_SA.R")

launch_step("../transformations/09_T5_90_Create_D6_Table_10_TP.R")

launch_step("../transformations/09_T5_100_Create_D6_Table_11_MICE.R")
