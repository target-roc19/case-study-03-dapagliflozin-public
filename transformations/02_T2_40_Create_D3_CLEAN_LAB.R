############################################################
#                                                          #
####   CREATE D3_CLEAN_LAB                              ####
# 
#                                                          #
############################################################

# author: Rosa Gini


# 1.2 17 Jun 2026

# added to export masked diagnostic of cleaning

# 1.1 2 Jun 2026

# added restriction to persons in D3_ENTRY_FLOWCHART

# 1.0 1 Jun 2026

# copied from CS 9

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_12_D3_CLEAN_LAB"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirexp <- thisdiroutput
  thisdirsettings <- thisdirinput
  thisconcept_ids_lab <- c("LAB_CREATININE_SERUM","LAB_HBA1C","LAB_EGFR")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisconcept_ids_lab <- fread(file.path(dirparlab, "LAB_target_units.csv"))$concept_id 
  thisdirexp <- direxp
}

#########################################
# load input datasets

print('CREATE D3_CLEAN_LAB')

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

ds <- settings$ds

# use the parameter file SUBPOP and the function create_selection_rules_for_subpopulations to store the selection rule associated to each subpopulation. we handle the case when the parameter file does not exist (for example in some tests)
filesubpoppar <- file.path(thisdirsettings,"SUBPOP.rds")
if (file.exists(filesubpoppar)) {
  thisexcludemeaning <- create_selection_rules_for_subpopulations(filesubpoppar,thissubpopulations)
}else{
  thisexcludemeaning <- list()
  for (subpop in thissubpopulations) thisexcludemeaning[[subpop]] <- ""
}


for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  persons <- readRDS(file.path(thisdirinput,paste0("D3_ENTRY_FLOWCHART",suffix,".rds")))
  
  data <- data.table(
    person_id = character(),
    date = as.Date(character()),
    mo_source_value = character(),
    mo_unit = character(),
    concept_id = character()
  )
  for (conc in thisconcept_ids_lab) {
    fileparquet <- file.path(thisdirinput,paste0(conc,"_dataset",".parquet"))
    if (file.exists(fileparquet)) {
      dt <- as.data.table(as.data.frame(polars::pl$read_parquet(fileparquet)))
      #dt <- merge(dt,persons[,.(person_id)], by = "person_id", all = F)
      # if the susbpopulation requires to exclude some meanings, here we do so
      thisexcludemeaningcommand <- fifelse(subpop == "all","",thisexcludemeaning[[subpop]])
      if (thisexcludemeaningcommand != "") {
        dt <- dt[eval(parse(text = thisexcludemeaningcommand)),]
      }
      dt[, concept_id := conc]
      dt <- dt[,.(person_id,date,mo_source_value,mo_unit,concept_id)]
      data <- rbind(data,dt, fill = T)
    }
    
  } 
  setnames(data,c("mo_source_value","mo_unit"),c("value","unit"))

  data <- merge(data, persons[,.(person_id)], by = "person_id", all = F )
  path_lab_target_units <- file.path(dirparlab,"LAB_target_units.csv")
  path_unit_conversion <- file.path(dirparlab,"LAB_unit_conversion.csv")
  
  # # START PATCH 
  # # there is a bug in CleanLabValues that does not handle correctly conversions for multiple datasources. let's keep the instructions for conversion for the current ds only
  # conversionds <- fread(path_unit_conversion)[datasource == "" | datasource == ds,]
  # fwrite(conversionds, file = file.path(dirpargen, "LAB_unit_conversion_ds.csv" ))
  # path_unit_conversion <- file.path(dirpargen, "LAB_unit_conversion_ds.csv" )
  # # END PATCH
  
  path_lab_thresholds <- file.path(dirparlab,"LAB_threshold_with_unit.csv")
  
  processing <- CleanLabValuesDataset(
     dataset = data,
     datasource = ds,
     lab_target_units = path_lab_target_units,
     lab_unit_conversion = path_unit_conversion,
     lab_thresholds = path_lab_thresholds
   )
  
  
  check <- processing[, .N, by = c("concept_id", "included", "conversion", "rule_applied")]
  setorder(check,concept_id,included,conversion,rule_applied)

  check_masked <- check
  check_masked <-  check_masked[, (names(check_masked)) := lapply(.SD, as.character)]
  check_masked <-  check_masked[as.numeric(N) > 0 & as.numeric(N) < 5, N := "< 5"]
  
  
    
   #########################################
   # cleaning
   
   tokeep <-  c("person_id","concept_id","date","value_origin", "unit_origin", "included", "value", "unit_target", "conversion", "rule_applied")
   
   processing <- processing[,..tokeep]
   
   
   #########################################
   # save
   
   outputfile <- processing
   
   nameoutput <- paste0("D3_CLEAN_LAB")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
   
   outputfile <- check
   
   nameoutput <- paste0("D5_DIAGNOSTIC_CLEAN_LAB")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

   outputfile <- check_masked
   
   nameoutput <- paste0("D5_DIAGNOSTIC_CLEAN_LAB_masked")
   nameoutputext <- paste0(nameoutput,suffix,".csv")
   assign(nameoutput, outputfile)
   fwrite(outputfile, file = file.path(thisdirexp, nameoutputext))
   
   }
