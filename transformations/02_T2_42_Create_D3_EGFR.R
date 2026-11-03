############################################################
#                                                          #
####        Create D3_EGFR             #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.0 2 Jun 2026


print('CREATE D3_EGFR')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_13_D3_EGFR"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  
  #########################################
  # load input datasets
  
  LAB <- readRDS(file.path(thisdirinput,paste0("D3_CLEAN_LAB",suffix,".rds")))
  LAB_EGFR <- copy(LAB)[concept_id == "LAB_EGFR" & included == 1,]
  LAB_CREATININE_EGFR <- copy(LAB)[concept_id == "LAB_CREATININE_SERUM" & included == 1,]
  persons <- readRDS(file.path(thisdirinput,paste0("D3_ENTRY_FLOWCHART",suffix,".rds")))
  
  #########################################
  # start_processing
  
  # LAB_CREATININE must be in mg/dL

  LAB_CREATININE_EGFR <- merge(LAB_CREATININE_EGFR,persons[,.(person_id,birth_year
,sex_at_instance_creation)], all = F)
  LAB_CREATININE_EGFR <- LAB_CREATININE_EGFR[sex_at_instance_creation == "M", a1 := -0.302 ]
  LAB_CREATININE_EGFR <- LAB_CREATININE_EGFR[sex_at_instance_creation == "F", a1 := -0.241 ]
  LAB_CREATININE_EGFR[, a2 := -1.2 ]
  LAB_CREATININE_EGFR[, c := 0.9938 ]
  LAB_CREATININE_EGFR[sex_at_instance_creation == "M", d := 1 ]
  LAB_CREATININE_EGFR[sex_at_instance_creation == "F", d := 1.012 ]
  LAB_CREATININE_EGFR <- LAB_CREATININE_EGFR[sex_at_instance_creation == "M", k := 0.9 ]
  LAB_CREATININE_EGFR <- LAB_CREATININE_EGFR[sex_at_instance_creation == "F", k := 0.7 ]
  LAB_CREATININE_EGFR[, Age := year(date) - birth_year ]
  LAB_CREATININE_EGFR[, mink := pmin(value/k,1) ]
  LAB_CREATININE_EGFR[, maxk := pmax(value/k,1) ]
  LAB_CREATININE_EGFR[, value := 142 * mink^a1 * maxk^a2 * c^Age * d ]

  processing <- rbind(LAB_CREATININE_EGFR, LAB_EGFR, fill = T)
  
  
  #########################################
  # clean
  
  listvartokeep <- c("person_id", "concept_id","date","value")
  processing <- processing[,..listvartokeep]
  
  #########################################
  # save
  
  outputfile <- processing
  
  nameoutput <- paste0("D3_EGFR",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}
