############################################################
#                                                          #
####   CREATE D3_OUTCOMES                              ####
# 
#                                                          #
############################################################

# author: Rosa Gini


# 1.0 13 Jul 2026

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_25_D3_OUTCOMES"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirexp <- thisdiroutput
  thisdirsettings <- thisdirinput
  thisconcept_ids <- c("N_STROKE_AESI","C_AMI_AESI")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisconcept_ids <- c("N_STROKE_AESI","C_AMI_AESI")
  thisdirexp <- direxp
}

#########################################
# load input datasets

print('CREATE D3_OUTCOMES')

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

ds <- settings$ds


# in this file, all the meanings are used, filtering is done later
# # use the parameter file SUBPOP and the function create_selection_rules_for_subpopulations to store the selection rule associated to each subpopulation. we handle the case when the parameter file does not exist (for example in some tests)
# filesubpoppar <- file.path(thisdirsettings,"SUBPOP.rds")
# if (file.exists(filesubpoppar)) {
#   thisexcludemeaning <- create_selection_rules_for_subpopulations(filesubpoppar,thissubpopulations)
# }else{
#   thisexcludemeaning <- list()
#   for (subpop in thissubpopulations) thisexcludemeaning[[subpop]] <- ""
# }


for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  persons <- readRDS(file.path(thisdirinput,paste0("D3_STUDY_POPULATION",suffix,".rds")))
  
  data <- data.table(
    person_id = character(),
    date = as.Date(character()),
    meaning_renamed = character(),
    concept_id = character()
  )
  for (conc in thisconcept_ids) {
    fileparquet <- file.path(thisdirinput,paste0(conc,"_dataset",".parquet"))
    if (file.exists(fileparquet)) {
      dt <- as.data.table(as.data.frame(polars::pl$read_parquet(fileparquet)))
      dt[, concept_id := conc]
      dt <- dt[,.(person_id,date,meaning_renamed,concept_id)]
      data <- rbind(data,dt, fill = T)
    }
    
  } 

  data <- merge(data, persons[,.(person_id, exit_spell_category, t0)], by = "person_id", all = F )
  data <- data[ date > t0 & date <= exit_spell_category,]
  data <- data[, .(outcome_date = min(date)), by = c("person_id", "t0","concept_id", "meaning_renamed") ]
  setnames(data, c("concept_id", "meaning_renamed"), c("outcome_type", "outcome_prompt"))
  
  # add death
  toadd <- persons[!is.na(death_date), .(person_id, t0, death_date)]
  setnames(toadd, "death_date", "outcome_date")
  toadd[, outcome_type := "DEATH"]
  data <- rbind(data, toadd, fill = T)
  
  # 
  
  processing <- data
  
  # check meanings
  check <- processing[, .N, by = c("outcome_type", "outcome_prompt")]
  setorder(check,outcome_type,outcome_prompt)

  check_masked <- check
  check_masked <-  check_masked[, (names(check_masked)) := lapply(.SD, as.character)]
  check_masked <-  check_masked[as.numeric(N) > 0 & as.numeric(N) < 5, N := "< 5"]
  
  
    
   #########################################
   # cleaning
   
   tokeep <-  c("person_id","t0", "outcome_date","outcome_type", "outcome_prompt")
   
   processing <- processing[,..tokeep]
   
   
   #########################################
   # save
   
   outputfile <- processing
   
   nameoutput <- paste0("D3_OUTCOMES")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
   
   outputfile <- check
   
   nameoutput <- paste0("D5_DIAGNOSTIC_MEANING_OUTCOMES")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

   outputfile <- check_masked
   
   nameoutput <- paste0("D5_DIAGNOSTIC_MEANING_OUTCOMES_masked")
   nameoutputext <- paste0(nameoutput,suffix,".csv")
   assign(nameoutput, outputfile)
   fwrite(outputfile, file = file.path(thisdirexp, nameoutputext))
   
   }
