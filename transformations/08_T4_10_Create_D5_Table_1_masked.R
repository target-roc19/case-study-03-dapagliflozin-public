############################################################
#                                                          #
###         07_T4_12_Create_D5_Table_1_masked           ####
#                                                          #
############################################################

# authors: Claudia Bartolini

# v 0.1 08 Jun 2026


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_17_Table1_2_masked"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
  thisdirsettings <- dirpargen
}

print('CREATE D5_Table_1_masked')

#########################################
# load parameters
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

#subpop="all" subpop="PC_HOSP"
for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #import input files
  table1 <- readRDS(file.path(thisdirinput,paste0("D5_Table_1",suffix,".rds"))) 
  excluded_counts <- readRDS(file.path(thisdirinput,paste0("excluded_counts",suffix,".rds"))) 
  
 
  #########################################
  # masking excluded counts
  excluded_counts_masked<- 
    excluded_counts[, names(excluded_counts) := lapply(.SD, function(x) {
    ifelse(x < 5 & x > 0, "<5", as.character(x))
  })]
  

  #########################################
  # identify N columns to be masked in table 1
  n_cols <- grep("^N_", names(table1), value = TRUE)
  
  # masking
  
  for (col in n_cols) {
    
    n_val <- suppressWarnings(as.numeric(table1[[col]]))
    mask <- !is.na(n_val) & n_val > 0 & n_val < 5
    
    
    table1[[col]] <- as.character(table1[[col]])
    table1[[col]][mask] <- "<5"
    
    pct_col <- sub("^N_", "PERCENTAGE_", col)
    if (pct_col %in% names(table1)) {
      table1[[pct_col]][mask] <- NA_character_
    }
  }
  #########################################
  # save
  
  outputfile <- table1
  
  nameoutput <- paste0("D5_Table_1_masked",suffix)
  nameoutputext <- paste0(nameoutput,".rds")
  assign(nameoutput, outputfile)
  
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  
  fwrite(excluded_counts_masked, file = file.path(thisdiroutput, paste0("excluded_counts_masked", suffix, ".csv")))
}
