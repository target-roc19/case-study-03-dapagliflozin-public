##%##################################################################################%##
#                                                                                      #
####       CREATE D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART       ####
#                                                                                      #
##%######################################################%##############################

# author: Claudia Bartolini, Rosa Gini, Olga Paoletti

# v 1.1 19 Jun 2026

# fixed bug that did not remove records without spells from D3_ENTRY_FLOWCHART

# v 1.0 7 Jun 2026

# fixed dependencies for output_spells_category_meaning_set and D3_OBSPERIODS_PC

# v 0.1 30 Apr 2026

############################################################

print('CREATE D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_04_D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART"
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
study_start_date<- settings$study_period_start_date
study_end_date<- settings$study_period_end_date
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

# D3_OBSPERIODS_PC <- readRDS(file.path(thisdirinput,"D3_OBSPERIODS.rds"))[category_subpop == "PC_HOSP",]

listofinput <- c("D3_PERSONS", "D3_OBSPERIODS")#,"output_spells_category_meaning_set"
output_spells_category_meaning_set <- readRDS(file.path(thisdirinput,"output_spells_category_meaning_set.rds")) #[category_subpop == "PC_HOSP",]

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  for (file in listofinput) {
    # if (file %in% listofinput_parquet) {
    #   
    #   temp <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0(file,".parquet")))))
    # }else{
      if(file == "D3_PERSONS") {
        temp <- readRDS(file.path(thisdirinput,paste0(file,".rds")))
      } else {
      temp <- readRDS(file.path(thisdirinput,paste0(file,suffix,".rds")))  
      }
    assign(file,temp)
    rm(temp)
  }
  
  D3_OBSPERIODS_PC <- output_spells_category_meaning_set$PC

  #########################################
  # start_processing
  
  processing <- unique(D3_PERSONS[ , sel_sex_missing := fifelse(is.na(sex_at_instance_creation) | !(sex_at_instance_creation == "M" | sex_at_instance_creation == "F" | sex_at_instance_creation == "O" ), 1, 0, 0)][, sel_partial_date_of_death := fifelse(missing_only_death_year == 1, 1, 0)]) 

  processing <- merge(processing, D3_OBSPERIODS_PC[, .(person_id,entry_spell_category)], by = "person_id", all.x=T, allow.cartesian = T)
  
  processing <- unique(processing[ , sel_no_spells := fifelse(is.na(entry_spell_category), 1, 0)][,sel_no_spells := min(sel_no_spells,na.rm = T), by="person_id"][,entry_spell_category:=NULL])  #spells removed?
  
#CREATE a variable to take track of the present of the person in HES
  processing <- merge(processing, D3_OBSPERIODS[, .(person_id,entry_spell_category,exit_spell_category)], by = "person_id", all.x=T, allow.cartesian = T)
  
  processing <- unique(processing[ , sel_no_spell_in_PC_HOSP := fifelse(is.na(entry_spell_category), 1, 0)][,sel_no_spell_in_PC_HOSP := min(sel_no_spell_in_PC_HOSP,na.rm=T), by="person_id"])
  
#   #check overlap with study period
#   processing <- merge(processing, D3_OBSPERIODS[is_the_study_spell == 1, .(person_id,entry_spell_category,exit_spell_category,not_overlap_study_period)], by = "person_id", all.x=T, allow.cartesian = T)
#   
# processing <- processing[,sel_no_spell_overlap_study_period := not_overlap_study_period, by="person_id"][is.na(sel_no_spell_overlap_study_period),sel_no_spell_overlap_study_period:=1]
  
  sel_cols <- grep("^sel_", names(processing), value = TRUE)
  
  # apply cumulative OR row-wise
  
  processing[, (sel_cols) := {
    m <- as.matrix(.SD)
    res <- t(apply(m, 1, function(x) as.integer(cummax(x == 1))))
    as.data.table(res)
  }, .SDcols = sel_cols]
  
  processing <- processing[, .(person_id,
                               sex_at_instance_creation,
                               birth_year,
                               sel_sex_missing,
                               sel_partial_date_of_death,
                               sel_no_spells,
                               sel_no_spell_in_PC_HOSP,
                               entry_spell_category,
                               exit_spell_category

  )]
  


 #add a variable marking if the subject is included in D3_ENTRY_FLOWCHART
  processing <- processing[ , included_in_D3_ENTRY_FLOWCHART := fifelse (sel_no_spell_in_PC_HOSP == 1 ,0,1)]
  
  #########################################
  # save

  outputfile <- processing

  nameoutput <- paste0("D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART",suffix)
  assign(nameoutput, outputfile)
  #xlsx::write.xlsx(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))

  #save D3_ENTRY_FLOWCHART --------------------
  cols_for_first_filter <- c("sel_sex_missing", "sel_partial_date_of_death","sel_no_spells")
  
  person_ids <- processing[
    processing[, Reduce(`&`, lapply(.SD, \(x) x == 0)), .SDcols = cols_for_first_filter],
    person_id
  ]
  processing <- processing[person_id %in% person_ids, .(person_id,sex_at_instance_creation,birth_year,entry_spell_category,exit_spell_category)]

  outputfile <- processing

  nameoutput <- paste0("D3_ENTRY_FLOWCHART",suffix)
  assign(nameoutput, outputfile)
  #fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  #xlsx::write.xlsx(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
}

