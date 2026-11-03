############################################################
#                                                          #
#########       D5_Table_3_asd_before_after_trimming_masked     ######
#                                                          #
############################################################

# authors: Claudia Bartolini, Rosa Gini

# v 1.0 26 Jul 2026

print('CREATE D5_Table_3_asd_before_after_trimming_masked')


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
  #thisdirexp <- direxp
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
  
  table2 <- readRDS(file.path(thisdirinput,paste0("D5_Table_3_asd_before_after_trimming",suffix,".rds")))   
  
  #########################################
  
  # convert everything to character
  
  table2[, (names(table2)) := lapply(.SD, as.character)]
  
  #########################################
  
  # identify N columns
  
  n_cols <- grep("N_BF$", names(table2), value = TRUE)
  
  #########################################
  # masking
  
  for(col in n_cols){
    
    p_col <- sub("N_BF$", "perc_BF", col)
    
    n_val <- suppressWarnings(as.numeric(table2[[col]]))
    
    mask <- !is.na(n_val) & n_val > 0 & n_val < 5
    
    table2[[col]][mask] <- "< 5"
    
    if(p_col %in% names(table2)){
      table2[[p_col]][mask] <- ""
    }
    
    
  }
  
  
  
  #########################################
  # save
  
  outputfile <- table2
  
  nameoutput <- paste0("D5_Table_3_asd_before_after_trimming_masked",suffix)
  nameoutputext <- paste0(nameoutput,".rds")
  assign(nameoutput, outputfile)
  #saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
}