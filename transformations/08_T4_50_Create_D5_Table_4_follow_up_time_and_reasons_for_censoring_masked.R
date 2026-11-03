############################################################
#                                                          #
#########       D5_Table_4_follow_up_time_and_reasons_for_censoring_masked     ######
#                                                          #
############################################################

# authors: Claudia Bartolini, Rosa Gini

# v 1.1 4 Sep 2026

# since we have totals, if one stratum has low numbers the other is censored as well


# v 1.0 26 Jul 2026

print('CREATE D5_Table_4_follow_up_time_and_reasons_for_censoring_masked')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  # no test for this
  # testname <- "test_17_Table_2_masked"
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
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

subpop <- "all"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  processing <- readRDS(file.path(thisdirinput,paste0("D5_Table_4_follow_up_time_and_reasons_for_censoring",suffix,".rds")))   
  
  vecstring <- names(processing)
  vars_to_mask <- vecstring[grepl("N_$", vecstring)] 
  # vars_to_mask <- c("N_exp_", vars_to_mask)
  
  # since we have 2 strata and totals, if one of the 2 strata has low numbers we must mask the other as well
  processing[, strata_to_be_masked_in_pairs := fifelse(stratum < 99, 1, 0)]
  
  for (varN in vars_to_mask) {
    processing[, check := 0]
    processing[get(varN) > 0 & get(varN) < 5, check := 1]
    # mask both strata if at least one must be masked, uless the stratum has 0 counts
    processing[, checkpair := max(check), by = "strata_to_be_masked_in_pairs"]
    processing[get(varN) == 0, checkpair := 0]
    # make character and mask percentage
    processing[, (varN) := as.character(get(varN))]
    processing[check == 1, (varN) := "< 5"]
    processing[check == 0 & checkpair == 1, (varN) := "NR"]
    varPassociated <- sub(".{2}$", "perc_", varN)
    if (varPassociated %in% names(processing)) {
      processing[, (varPassociated) := as.character(get(varPassociated))]
      processing[check == 1, (varPassociated) := "NR"]
    }
  }
  
  
  for (varN in names(processing)) {
    processing[, (varN) := as.character(get(varN))]
    
    
  }
  
  
  
  #########################################
  # save
  
  outputfile <- processing
  
  nameoutput <- paste0("D5_Table_4_follow_up_time_and_reasons_for_censoring_masked",suffix)
  nameoutputext <- paste0(nameoutput,".rds")
  assign(nameoutput, outputfile)
  # saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
}