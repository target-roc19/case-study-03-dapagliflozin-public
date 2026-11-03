############################################################
#                                                          #
####   CREATE D4_ANALYTICAL_DATASET_HR_RMST_TP                              ####
# 
#                                                          #
############################################################

# author: Rosa Gini

# 1.0 19 Jul 2026

#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_34_D4_ANALYTICAL_DATASET_HR_RMST_TP"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirexp <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirexp <- direxp
}

#########################################
# load input datasets

print('CREATE D4_ANALYTICAL_DATASET_HR_RMST_TP')

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

ds <- settings$ds
recommended_end_date <- settings$recommended_end_date
study_exit_date <- settings$study_period_end_date


# use the parameter file SUBPOP and the function create_selection_rules_for_subpopulations to store the selection rule associated to each subpopulation. we handle the case when the parameter file does not exist (for example in some tests)
filesubpoppar <- file.path(thisdirsettings,"SUBPOP.rds")
if (file.exists(filesubpoppar)) {
  thisexcludemeaning <- create_selection_rules_for_subpopulations(filesubpoppar,thissubpopulations)
}else{
  thisexcludemeaning <- list()
  for (subpop in thissubpopulations) thisexcludemeaning[[subpop]] <- ""
}


subpop <- "PC_HOSP"

for (subpop in thissubpopulations) { 
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #################################
  # start
  
  processing <- readRDS(file.path(thisdirinput,paste0("D4_FUP",suffix,".rds")))

  
  # E1_outcome
  
  processing[, E1_outcome := fifelse(!is.na(outcome_date) & outcome_date <= censoring_date_E1,1,0)]
  
  # E1_fup
  
  processing[, E1_fup := as.numeric(fup_days_E1)]
  
  # E2_outcome

  processing[, E2_outcome := fifelse(!is.na(outcome_date) & outcome_date <= censoring_date_E2,1,0)]

  # E2_fup

  processing[, E2_fup := as.numeric(fup_days_E2)]
  
  
  # E3a_outcome

  processing[, E3a_outcome := fifelse(!is.na(outcome_date) & outcome_date <= censoring_date_E1,1,0)]
  
  # E3a_fup
  
  processing[, E3a_fup := as.numeric(fup_days_E1)]
  
  # E2b_outcome
  
  processing[, E3b_outcome := fifelse(!is.na(outcome_date) & outcome_date <= censoring_date_E3b,1,0)]
  
  # E2b_fup
  
  processing[, E3b_fup := as.numeric(fup_days_E3b)]
  
  # TP1_outcome
  
  processing[, TP1_outcome := fifelse(E1_outcome == 1 | (exposure == 0 & cause_of_censoring_E1 %in% c(2, 6, 7, 8) ),1,0)]
  
  # TP1_fup
  
  processing[, TP1_fup := as.numeric(censoring_date_excluding_intercurring_E1 - t0)]
  
  # TP2_outcome

  processing[, TP2_outcome := fifelse(E1_outcome == 1 | (exposure == 1 & cause_of_censoring_E1 %in% c(2, 6, 7, 8) ),1,0)]
  
  # TP2_fup

  processing[, TP2_fup := TP1_fup]
  
  # TP3_outcome
  
  processing[, TP3_outcome := fifelse(E1_outcome == 1 ,1,0)]
  
  # TP3_fup

  processing[, TP3_fup := TP1_fup]
  
  # TP4_outcome
  
  processing[, TP4_outcome := fifelse(E1_outcome == 1 | cause_of_censoring_E1 %in% c(2, 6, 7, 8), 1, 0)]
  
  # TP4_fup
  
  processing[, TP4_fup := TP1_fup]
  
    
   #########################################
   # cleaning
   
   tokeep <-  c("person_id", "exposure", "E1_outcome", "E1_fup", "E2_outcome", "E2_fup", "E3a_outcome", "E3a_fup", "E3b_outcome", "E3b_fup", "TP1_outcome", "TP1_fup", "TP2_outcome", "TP2_fup", "TP3_outcome", "TP3_fup", "TP4_outcome", "TP4_fup") #, "matched_set_id", "E2_outcome", "E2_fup"
   
   processing <- processing[,..tokeep]
   
   
   #########################################
   # save
   
   outputfile <- processing
   
   nameoutput <- paste0("D4_ANALYTICAL_DATASET_HR_RMST_TP")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
   

   }
