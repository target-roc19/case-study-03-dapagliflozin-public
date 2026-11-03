############################################################
#                                                          #
####   CREATE D4_FUP                              ####
# 
#                                                          #
############################################################

# author: Rosa Gini

# v 1.3 4 Sep 2026

# - added counts of persons with intercurring events not ending fup
# - added time to event stratified per cause


# 1.2 29 Jul 2026

# allow for shorter fup window (E3b)

# 1.1 26 Jul 2026

# debug after bug found in CS4

# 1.0 14 Jul 2026

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_26_D4_FUP"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirexp <- thisdiroutput
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirexp <- direxp
}

#########################################
# load input datasets

print('CREATE D4_FUP')

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

ds <- settings$ds
settings_recommended_end_date <- settings$recommended_end_date
study_exit_date <- settings$study_period_end_date


# use the parameter file SUBPOP and the function create_selection_rules_for_subpopulations to store the selection rule associated to each subpopulation. we handle the case when the parameter file does not exist (for example in some tests)
filesubpoppar <- file.path(thisdirsettings,"SUBPOP.rds")
if (file.exists(filesubpoppar)) {
  thisexcludemeaning <- create_selection_rules_for_subpopulations(filesubpoppar,thissubpopulations)
}else{
  thisexcludemeaning <- list()
  for (subpop in thissubpopulations) thisexcludemeaning[[subpop]] <- ""
}

thisconcept_ids <- c("DP_METFORMIN", "DP_SULFONYLUREA",  "DP_SULONAMIDE", "DP_ALPHAGLUCOINH", "DP_TZD", "DP_OTHERGLD", "DP_INSULIN", "DP_SGLT2I")

# helper function: vcauses of censoring have always the same labels

compute_cause_censoring <- function(namecause,namedate,vector){
  processing[, (namecause) := NA_integer_]
  j <- 1
  for (cause in c("outcome_date",
                  "death_date",
                  "recommended_end_date",
                  "end_time_window_fup",
                  "study_exit_date",
                  "discontinuation_date",
                  "new_medicine_date",
                  "exit_spell_category")) {
    if (j %in% vector) {
      processing[is.na(get(namecause)) & !is.na(get(cause)) & get(cause) == get(namedate), (namecause) := j]
    }
    j <- j + 1
  }
}

compute_cause_censoring_shorter <- function(namecause,namedate,vector){
  processing[, (namecause) := NA_integer_]
  j <- 1
  for (cause in c("outcome_date",
                  "death_date",
                  "recommended_end_date",
                  "end_time_window_fup_shorter",
                  "study_exit_date",
                  "discontinuation_date",
                  "new_medicine_date",
                  "exit_spell_category")) {
    if (j %in% vector) {
      processing[is.na(get(namecause)) & !is.na(get(cause)) & get(cause) == get(namedate), (namecause) := j]
    }
    j <- j + 1
  }
}





subpop <- "all"

for (subpop in thissubpopulations) { 
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #################################
  # start
  
  persons <- readRDS(file.path(thisdirinput,paste0("D3_STUDY_POPULATION",suffix,".rds")))

  processing <- persons

  #########################################
  # recommended_end_date
  
  # this is different for CPRD: in CPRD this depends on person
  
  if (thisdatasource == "CPRD") {
    temp <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0("END_PRACTICE_dataset",".parquet")))))
    temp <- temp[,.(person_id,date)]
    temp <- temp[,.(recommended_end_date = max(date, na.rm = T)), by = "person_id"]
    processing <- merge(processing, temp, by = "person_id", all.x = T)
    
    # write a check file to count how many missing and year distribution
    tocheck <- copy(processing)[,.(recommended_end_date)]
    tocheck[, year_recommended_end_date := year(recommended_end_date)]
    tocheck <- tocheck[,.N, by = c("year_recommended_end_date")]
    fwrite(tocheck, file = file.path(thisdiroutput,"D5_check_recommended_end_date.csv"))
    
    tocheck <- tocheck[,N_records_cat := as.character(N)]
    tocheck <- tocheck[N < 5 ,N_records_cat := "< 5"]
    tocheck[,N := NULL]
    
    fwrite(tocheck, file = file.path(thisdirexp,"D5_check_recommended_end_date_masked.csv"))
    
    # replace recommended end date with the one stored in CDM_SOURCE if missing
    processing[is.na(recommended_end_date), recommended_end_date := settings_recommended_end_date]
    
    # replace recommended end date with the minimum between the observed and the one stored in CDM_SOURCE
    
    processing[, recommended_end_date := pmin(settings_recommended_end_date, recommended_end_date)]
    
  }else{
    # outside of CPRD this is always the date stored in CDM_SOURCE
    processing[, recommended_end_date := settings_recommended_end_date]
  }
  
  
  ######################################
  # discontinuation_date
  
  eligibility <- readRDS(file.path(thisdirinput,paste0("D3_ELIGIBILITY_EPISODE",suffix,".rds")))
  eligibility <- eligibility[is_study_episode == 1,]
  processing <-   merge(processing,eligibility[,.(person_id, episode_end)], by = "person_id", all = F )
  setnames(processing, "episode_end", "discontinuation_date")
  rm(eligibility)
  
  ######################################
  # new_medicine_date
  # is_new_switch
  # switch_date
  # addition_date
  
  
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
      if (conc == "DP_SGLT2I") {
        # remove from DP_SGLT2I the records of dapagliflozin
        dt <- dt[!(codvar %in% unlist(concept_set_codes_our_study[["DP_DAPAGLIFLOZIN"]])),]
      }
      dt <- dt[,.(person_id,date,meaning_renamed,concept_id)]
      data <- rbind(data,dt, fill = T)
    }
    
  } 

  data <- merge(data, persons[,.(person_id, exit_spell_category, t0)], by = "person_id", all = F )
  data <- data[ date > t0 & date <= exit_spell_category,]

  # calculate first and second date per person of antiglycemic drugs and reshape wide
  data <- unique(data[, .(person_id, date, concept_id)])

  setorder(data, person_id, concept_id, date)
  
  data[, n := rowid(person_id, concept_id) ]
  
  second_date <- copy(data)[n == 2,] 
  first_date <- copy(data)[n == 1,]
  rm(data)
  
  first_date <- dcast(
    first_date,
    person_id ~ concept_id,
    value.var = "date"
  )
  
  for (conc in thisconcept_ids) { 
    if (!(conc %in% names(first_date))) {
      first_date[, (conc) := NA_Date_]
    }
  }
  
  old_names <- setdiff(names(first_date), "person_id")
  setnames(first_date, old_names, paste0("first_date_", old_names))
  
  second_date <- dcast(
    second_date,
    person_id ~ concept_id,
    value.var = "date"
  )
  
  for (conc in thisconcept_ids) { 
    if (!(conc %in% names(second_date))) {
      second_date[, (conc) := NA_Date_]
    }
  }
  
  old_names <- setdiff(names(second_date), "person_id")
  setnames(second_date, old_names, paste0("second_date_", old_names))
  
  # calculate minimum of other medicine
 
  episodes <- readRDS(file.path(thisdirinput,paste0("D3_EPISODES_STUDY_DRUGS",suffix,".rds")))
  episodes <-   merge(episodes, persons[,.(person_id, exit_spell_category, t0, exposure)], by = "person_id", all = F )
  episodes <- episodes[ episode_start > t0 & episode_start <= exit_spell_category,]
  episodes <- episodes[ (exposure == 0 & exposure_episode == 1) |(exposure == 1 & exposure_episode == 0),]
  
  setorder(episodes, person_id, episode_start)
  episodes[, n := rowid(person_id) ]
  
  episodes <- episodes[ n == 1, ]
  episodes <- episodes[, .(person_id, episode_start, episode_end)]
  episodes[, concept_id := "other_study_drug"]
  

  # load parameters linking concept_id's with variables of baseline use
  
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_SIMPLE_TD.xlsx")),1)))
  varname_concept_id <- unique(parameters_this_step[,.(varname,ingredient)])
  setnames(varname_concept_id, "ingredient", "concept_id")
  varname_concept_id <- varname_concept_id[concept_id %in% thisconcept_ids, ]

  #  load baseline
  
  baseline <- readRDS(file.path(thisdirinput,paste0("D3_EXP_ACT_COV",suffix,".rds")))
  varname_baseline <- paste0(varname_concept_id$varname,"_at_t0")
  tokeep <- c("person_id", varname_baseline)
  baseline <- baseline[, ..tokeep]
    
  # use baseline and -first_data- and -episodes- to define start of new antiglycemic
  
  processing <- merge(processing, baseline, by = "person_id", all.x = T)
  processing <- merge(processing, first_date, by = "person_id", all.x = T)
  processing <- merge(processing, second_date, by = "person_id", all.x = T)
  
  processing[, new_medicine_date := NA_Date_]
  processing[, concept_new_antiglycemic := NA_character_]
  for (conc in setdiff(thisconcept_ids, "DP_SGLT2I")) {
    varconc <- unname(unlist(varname_concept_id[concept_id == conc,.(varname) ]))
    varname <- paste0(varconc,"_at_t0")
    concdate <- paste0("first_date_",conc)
    processing[get(varname) == 0 & ((is.na(new_medicine_date) & !is.na(get(concdate))) | (!is.na(new_medicine_date) & !is.na(get(concdate)) & get(concdate) < new_medicine_date)), concept_new_antiglycemic := conc]
    
    processing[get(varname) == 0, new_medicine_date := pmin(new_medicine_date, get(concdate), na.rm = T)]
  }

  # case of DP_SGLT2I
  
  conc <- "DP_SGLT2I"
  concdate <- paste0("first_date_",conc)
  processing[(is.na(new_medicine_date) & !is.na(get(concdate))) | (!is.na(new_medicine_date) & !is.na(get(concdate)) & get(concdate) < new_medicine_date), concept_new_antiglycemic := conc]
  processing[concept_new_antiglycemic == conc, new_medicine_date := get(concdate)]
  
  # case of other study drug

  processing <- merge(processing, episodes, by = "person_id", all.x = T)
  
  conc <- "other_study_drug"
  concdate <- "episode_start"
  processing[(is.na(new_medicine_date) & !is.na(get(concdate))) | (!is.na(new_medicine_date) & !is.na(get(concdate)) & get(concdate) < new_medicine_date), concept_new_antiglycemic := conc]
  processing[concept_new_antiglycemic == conc, new_medicine_date := get(concdate)]
  
  # new_is_switch
  
  processing[, new_is_switch := NA_integer_]
  for (conc in thisconcept_ids) {
    print(conc)
    print(varconc)
    varname <- paste0("value_of_",varconc)
    concdate <- paste0("second_date_",conc)
    processing[concept_new_antiglycemic == conc &  
                 (is.na(get(concdate)) | get(concdate) <= discontinuation_date)
               , new_is_switch := 0 ]
    processing[concept_new_antiglycemic == conc & get(concdate) > discontinuation_date, new_is_switch := 1]
  }
  
  # case of other study drug
  conc <- "other_study_drug"
  concdate <- "episode_end"
  processing[concept_new_antiglycemic == conc & 
               (is.na(get(concdate)) | get(concdate) <= discontinuation_date)
                , new_is_switch := 0 ]
  processing[concept_new_antiglycemic == conc & get(concdate) > discontinuation_date, new_is_switch := 1]
  
  # switch_date
  # addition_date
  
  processing[new_is_switch == 1, switch_date := new_medicine_date]
  processing[new_is_switch == 0, addition_date := new_medicine_date]
  
  
  ###########################
  # end_time_window_fup
  
  processing[, end_time_window_fup := as.Date(t0 + 1825)]
  
  ###########################
  # end_time_window_fup_shorter
  
  processing[, end_time_window_fup_shorter := as.Date(t0 + 1095)]
  
  
  #################################
  # outcome_date
    
  outcomes <- readRDS(file.path(thisdirinput,paste0("D3_OUTCOMES",suffix,".rds")))
  outcomes <- outcomes[ outcome_date > t0,]
  outcomes <- outcomes[, .(outcome_date = min(outcome_date)), by = "person_id"]
  processing <- merge(processing, outcomes, by = "person_id", all.x = T)
  
  ##########################################
  # check: distribution if meaning of outcomes across study subjects in 2018 
  
  check <- copy(processing[ year(t0) == 2018,])
  
  components <- readRDS(file.path(thisdirinput,paste0("D3_OUTCOMES",suffix,".rds")))
  components <- merge(components[, t0 := NULL], check[,.(person_id, t0, end_time_window_fup)], by = "person_id", all = F)
  components <- components[outcome_date > t0 & outcome_date <= end_time_window_fup, ]
  components[, end_time_window_fup := NULL]
  components[is.na(outcome_prompt), outcome_prompt := "missing_prompt" ]
  components[, outcome_type := as.character(outcome_type) ]
  components[outcome_type == "", outcome_type := "missing_concept" ]
  components <- components[, .(person_id, outcome_type, outcome_prompt)]
  
  # reshape wide
  
  components <- dcast(
    components,
    person_id ~ outcome_type + outcome_prompt,
    fun.aggregate = length,
    value.var = "outcome_prompt",
    fill = 0
  )
  
  # convert in binary
  
  count_cols <- setdiff(names(components), "person_id")
  components[, (count_cols) := lapply(.SD, function(x) as.integer(x > 0)), .SDcols = count_cols]
  
  binary_vars <- setdiff(names(components), "person_id")
  
  # merge with exposure
  
  components <- merge(components, check[,.(person_id, exposure)], by = "person_id")
  
  # aggrgeate, mask and save
  
  components <- components[, .N, by = c(binary_vars, "exposure")]
  
  fwrite(components, file = file.path(thisdiroutput,"D5_check_components_outcome.csv"))
  
  components <- components[,N_records_cat := as.character(N)]
  components <- components[N < 5 ,N_records_cat := "< 5"]
  components[,N := NULL]
  
  fwrite(components, file = file.path(thisdirexp,"D5_check_components_outcome_masked.csv"))
  
  

  ##################################
  # censoring_date
  # cause_of_censoring
  
  processing[, censoring_date := pmin(outcome_date,
                                      death_date,
                                      recommended_end_date,
                                      end_time_window_fup,
                                      study_exit_date,
                                      discontinuation_date,
                                      new_medicine_date,
                                      exit_spell_category, 
                                      na.rm = T)]
  
  
  compute_cause_censoring("cause_of_censoring", "censoring_date", 1:8)
  
  causes_of_censoring <- list()
  
  causes_of_censoring[["E1"]] <- c(1,3,4,5,8)
  causes_of_censoring[["E2"]] <- 1:8
  causes_of_censoring[["E3a"]] <- causes_of_censoring[["E1"]]
  causes_of_censoring[["E3b"]] <- causes_of_censoring[["E1"]]
  
  ##########################################
  # ESTIMAND 1
  ##########################################

  ##########################################
  # censoring_date_E1
  # cause_of_censoring_E1
  # fup_days_E1
  # date_intercurring_events_E1
  
  processing[, censoring_date_E1 := pmin(outcome_date,
                                      death_date,
                                      recommended_end_date,
                                      end_time_window_fup,
                                      study_exit_date,
                                      exit_spell_category, 
                                      na.rm = T)]
  
  compute_cause_censoring("cause_of_censoring_E1", "censoring_date_E1", causes_of_censoring[["E1"]])
  
  processing[, fup_days_E1 := as.integer(censoring_date_E1 - t0)]
  processing[, date_intercurring_events_E1 := pmin(exit_spell_category, 
                                                  na.rm = T)]
  
  processing[, censoring_date_excluding_intercurring_E1 := pmin(outcome_date,
                                                                death_date,
                                                                recommended_end_date,
                                                                end_time_window_fup,
                                                                study_exit_date,
                                                                na.rm = T)]
  
  
  ###################
  # E1: fup time separately per cause of end
  
  
  for (cause in causes_of_censoring[["E1"]]) {
    processing[cause_of_censoring_E1 == cause, fup_days := fup_days_E1]
    setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E1_",cause))
  } 
  
  ###################
  # E1: fup time until intercurring events that do not cause end of follow up (events 6 and 7)
  
  processing[, intercurring_not_ending_fup_E1_6 := fifelse(!is.na(discontinuation_date) & discontinuation_date < censoring_date_E1, 1, 0)]
  
  processing[intercurring_not_ending_fup_E1_6 == 1, fup_days_to_intercurring_not_ending_fup_E1_6 := as.integer(discontinuation_date - t0)]
  
  processing[, intercurring_not_ending_fup_E1_7 := fifelse(!is.na(new_medicine_date) & new_medicine_date < censoring_date_E1, 1, 0)]
  
  processing[intercurring_not_ending_fup_E1_7 == 1, fup_days_to_intercurring_not_ending_fup_E1_7 := as.integer(new_medicine_date - t0)]
  
  
  
  ##########################################
  # ESTIMAND 2
  ##########################################
  
  ###########################################
  # censoring_date_E2
  # cause_of_censoring_E2
  # fup_days_E2
  
  processing[, censoring_date_E2 := censoring_date]  
  
  processing[, cause_of_censoring_E2 := cause_of_censoring]  
  processing[, fup_days_E2 := as.integer(censoring_date_E2 - t0)]  
  
  ###################
  # E2: fup time separately per cause of end
  
  for (cause in causes_of_censoring[["E2"]]) {
    processing[cause_of_censoring_E2 == cause, fup_days := fup_days_E2]
    setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E2_",cause))
  }
  
  
  ##########################################
  # ESTIMAND 3b
  ##########################################
  
  ###########################################
  # censoring_date_E3b
  # cause_of_censoring_E3b
  # fup_days_E3b
  
  processing[, censoring_date_E3b :=  pmin(outcome_date,
                                           death_date,
                                           recommended_end_date,
                                           end_time_window_fup_shorter,
                                           study_exit_date,
                                           exit_spell_category, 
                                           na.rm = T)]  
  
  compute_cause_censoring_shorter("cause_of_censoring_E3b", "censoring_date_E3b", causes_of_censoring[["E3b"]]) 
  processing[, fup_days_E3b := as.integer(censoring_date_E3b - t0)]  
  
  ###################
  # E3b: fup time separately per cause of end
  
  
  for (cause in causes_of_censoring[["E3b"]]) {
    processing[cause_of_censoring_E3b == cause, fup_days := fup_days_E3b]
    setnames(processing, "fup_days", paste0("fup_days_cause_of_censoring_E3b_",cause))
  }
  
  ###################
  # E3b: fup time until intercurring events that do not cause end of follow up (events 6 and 7)
  
  processing[, intercurring_not_ending_fup_E3b_6 := fifelse(!is.na(discontinuation_date) & discontinuation_date < censoring_date_E3b, 1, 0)]
  
  processing[intercurring_not_ending_fup_E3b_6 == 1, fup_days_to_intercurring_not_ending_fup_E3b_6 := as.integer(discontinuation_date - t0)]
  
  processing[, intercurring_not_ending_fup_E3b_7 := fifelse(!is.na(new_medicine_date) & new_medicine_date < censoring_date_E3b, 1, 0)]
  
  processing[intercurring_not_ending_fup_E3b_7 == 1, fup_days_to_intercurring_not_ending_fup_E3b_7 := as.integer(new_medicine_date - t0)]
  
  
  
    
   #########################################
   # cleaning
   
   tokeep <-  c("person_id", "t0", "exposure", "birth_year", "exit_spell_category", "death_date", "discontinuation_date", "new_medicine_date", "new_is_switch", "switch_date", "addition_date", "end_time_window_fup", "outcome_date", "censoring_date", "cause_of_censoring", "censoring_date_E1", "cause_of_censoring_E1", "fup_days_E1", "date_intercurring_events_E1", "censoring_date_excluding_intercurring_E1", paste0("fup_days_cause_of_censoring_E1_",causes_of_censoring[["E1"]]),  "intercurring_not_ending_fup_E1_6", "fup_days_to_intercurring_not_ending_fup_E1_6", "intercurring_not_ending_fup_E1_7", "fup_days_to_intercurring_not_ending_fup_E1_7", "censoring_date_E2", "cause_of_censoring_E2", "fup_days_E2", paste0("fup_days_cause_of_censoring_E2_",causes_of_censoring[["E2"]]),  "censoring_date_E3b", "cause_of_censoring_E3b", "fup_days_E3b", paste0("fup_days_cause_of_censoring_E3b_",causes_of_censoring[["E3b"]]),  "intercurring_not_ending_fup_E3b_6", "fup_days_to_intercurring_not_ending_fup_E3b_6", "intercurring_not_ending_fup_E3b_7", "fup_days_to_intercurring_not_ending_fup_E3b_7")
   
   processing <- processing[,..tokeep]
   
   
   #########################################
   # save
   
   outputfile <- processing
   
   nameoutput <- paste0("D4_FUP")
   nameoutputext <- paste0(nameoutput,suffix,".rds")
   assign(nameoutput, outputfile)
   saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
   

   }
