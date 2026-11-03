############################################################
#                                                          #
###  07_T4_04_Create_D5_Supplementary_Figure_1_masked      #
#                                                          #
############################################################


# authors: Sofia Miniati

# v 1.0 17 Jun 2026

# from CS5

print('CREATE D5_Supplementary_Figure_2_MICE_masked')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_D5_Supplementary_Figure_2_MICE_masked"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
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


for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  
  input <- readRDS(file.path(thisdirinput,paste0("D5_Supplementary_Figure_2_MICE",suffix,".rds")))
  
  #########################################
  # start_processing
  
  #Masked Frequency
  input[, Frequency := fifelse(Frequency > 0 & Frequency < 5, "<5", as.character(Frequency))]
  
  
  #########################################
  # clean
  
  listvartokeep <- c("ds",
                     "imputation",
                     "ps_variable",
                     "exposure_stratum",
                     "bin",
                     "Frequency",
                     "pc_1",
                     "pc_99",
                     "pc_25",
                     "pc_50",
                     "pc_75",
                     "ps_mean")
  input <- input[,..listvartokeep]
  
  #########################################
  # save
  
  outputfile <- input
  
  nameoutput <- paste0("D5_Supplementary_Figure_2_MICE_masked",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
}

