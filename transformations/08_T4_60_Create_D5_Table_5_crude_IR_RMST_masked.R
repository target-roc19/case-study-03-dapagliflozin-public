############################################################
#                                                          #
####         D5_Table_5_crude_IR_RMST_masked           #####
#                                                          #
############################################################

# authors: Claudia Bartolini

# v 1.0 23 Jul 2026


print('CREATE D5_Table_5_crude_IR_RMST_masked')

#########################################
# assign directories

if (TEST){
  print("TEST non available for this step D5_Table_5_crude_IR_RMST_masked")
  # testname <- "test_06_D5_Table_1b_baseline_cohort_with_diagnosis"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
  thisdirsettings <- dirpargen
}

#########################################
# load

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))




#########################################
# base functions


subpop <- "all"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  processing <- readRDS(file.path(thisdirinput,paste0("D5_ResultsRMST",suffix,".rds")))
  
  
  vecstring <- names(processing)
  vars_to_mask <- c("N_","N_outcome_")
  
  for (varN in vars_to_mask) {
    processing[, check := 0]
    processing[get(varN) > 0 & get(varN) < 5, check := 1]
    processing[, (varN) := as.character(get(varN))]
    processing[check == 1, (varN) := "< 5"]
    # varPassociated <- sub(".{2}$", "perc_", varN)
    # if (varPassociated %in% names(processing)) {
    #   processing[, (varPassociated) := as.character(get(varPassociated))]
    #   processing[check == 1, (varPassociated) := "NR"]
    # }
  }
  
  
  for (varN in names(processing)) {
    processing[, (varN) := as.character(get(varN))]
    
    
  }
  
  ##############################
  # cleaning
  #   
 
  #########################################
  # save
  
  nameoutput <- paste0("D5_Table_5_crude_IR_RMST_masked")
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, processing)
  #    saveRDS(processing, file = file.path(thisdiroutput, nameoutputext))
  nameoutputcsv <- paste0(nameoutput,suffix,".csv")
  fwrite(processing, file = file.path(thisdiroutput, nameoutputcsv))
  
  
}
