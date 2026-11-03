

print('D5_diagnostics_flowchart_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_06_D5_diagnostics_flowchart_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirexp <- direxp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

listofinput <- c( "D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART") 

#subpop <- "all"
for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  for (file in listofinput) {
    temp <- readRDS(file.path(thisdirinput,paste0(file,suffix,".rds")))
    assign(file,temp)
    rm(temp)
  }
  
  #########################################
  # start_processing
  processing <-
    D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART[
      , .SD, .SDcols = c("person_id", grep("^sel_", names(D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART), value = TRUE))
    ]
  

  # define order of selection criteria
  sel_cols <- c("sel_sex_missing",
                "sel_partial_date_of_death",
                "sel_no_spells")
  
  # apply dependency (cascade)
  for (i in 2:length(sel_cols)) {
    prev_col <- sel_cols[i - 1]
    curr_col <- sel_cols[i]
    
    processing[
      get(prev_col) == 1,
      (curr_col) := 1
    ]
  }
  
  # N entering flowchart
  N_entering_ds <- processing[, uniqueN(person_id)]
  
  sel_criteria <- grep("^sel_", names(processing), value = TRUE)
  dt_unique <- processing[, c("person_id", sel_criteria), with = FALSE]
  dt_unique[, first_exclusion_step := NA_integer_]
  

  for (i in seq_along(sel_criteria)) {
    
    var <- sel_criteria[i]
    
    dt_unique[
      is.na(first_exclusion_step) & get(var) == 1,
      first_exclusion_step := i
    ]
  }
  
  # count excluded for each criterion
  counts_dt <- dt_unique[!is.na(first_exclusion_step),.N, by =first_exclusion_step]
  
  excluded_counts <- integer(length(sel_criteria))
  excluded_counts[counts_dt$first_exclusion_step] <- counts_dt$N
  
  # N entering flowchart
  N_entering_flowchart <- dt_unique[is.na(first_exclusion_step), .N]
  
  
  # put all variable together
  
  data <- data.table(
    ds = settings$ds,
    N_entering_ds = N_entering_ds)
  
  for (i in seq_along(sel_criteria)) {
    data[, sub("_episode", "",paste0("N_", sel_criteria[i])) := excluded_counts[i]]
  }
  
  data[ , N_entering_flowchart:= N_entering_flowchart]
  
  data[, PERCENTAGE_sel_sex_missing :=
         round(as.numeric(N_sel_sex_missing) / as.numeric(N_entering_ds) * 100, 2)]
  
  data[, PERCENTAGE_sel_partial_date_of_death :=
         round(as.numeric(N_sel_partial_date_of_death) / as.numeric(N_entering_ds) * 100, 2)]
  
  data[, PERCENTAGE_sel_no_spells :=
         round(as.numeric(N_sel_no_spells) / as.numeric(N_entering_ds) * 100, 2)]
  
  data[, PERCENTAGE_entering_flowchart :=
         round(as.numeric(N_entering_flowchart) / as.numeric(N_entering_ds) * 100, 2)]
  
  #########################################
  # clean
  
  listvartokeep <- c("ds",
                     "N_entering_ds",
                     "N_sel_sex_missing",
                     "PERCENTAGE_sel_sex_missing",
                     "N_sel_partial_date_of_death",
                     "PERCENTAGE_sel_partial_date_of_death",
                     "N_sel_no_spells",
                     "PERCENTAGE_sel_no_spells",
                     "N_entering_flowchart",
                     "PERCENTAGE_entering_flowchart")
  
  processing <- data[,..listvartokeep]
  
  
  
  #########################################
  # save
  
  outputfile <- processing

  nameoutput <- paste0("D5_diagnostics_flowchart_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART",suffix)
  assign(nameoutput, outputfile)
  fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}





