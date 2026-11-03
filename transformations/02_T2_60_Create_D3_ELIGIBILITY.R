###################################
#                                 #
#      Create D3_ELIGIBILITY      #
#                                 #
###################################


# author: Claudia Bartolini, Rosa Gini

#v1.3 26 Jun 2026
#fixed bug: correct how to reduce information from episode level to person level 

# v1.2 12 Jun 2026

# added date of T2DM
# ixed bug that did not check for dates to be non-missing

# v1.1 7 Jun 2026

# fixed typos:

# D3_PERSONS is without subpop
# D3_EPISODES_STUDY_DRUGS -> D3_EPISODES_STUDY_DRUGSS
# parquet datasets are named XXXX_dataset
# DEPAGLIFLOZIN -> DAPAGLIFLOZIN


# v 1.0

# 28 May 2026


print('D3_ELIGIBILITY_EPISODE')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_07_D3_ELIGIBILITY"
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
enrol_start <- as.Date("2013-01-01")
enrol_end   <- as.Date("2024-12-31")

listofinput <- c("D3_EPISODES_STUDY_DRUGS","D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART","D3_ENTRY_FLOWCHART")
# listofinput_prq <- c("DP_DAPAGLIFLOZIN_dataset","DP_DDP4I_dataset", 
#                      "E_T1DM_COV_dataset","E_T2DM_COV_dataset", 
#                      "C_IHD_COV_dataset","V_ICVD_COV_dataset","V_PAD_COV_dataset", 
#                      "E_HYPERLIPIDAEMIA_COV_dataset","DP_LIPIDLOWERING_dataset", 
#                      "V_HYPERTENSION_COV_dataset","DP_ANTIHYPERTENSIVE_dataset","L_CURRENTSMOKER_COV_dataset", 
#                      "DP_SGLT2I_dataset",
#                      "C_AMI_AESI_dataset", "C_UNSTANGINA_AESI_dataset", "N_STROKE_AESI_dataset") 

load_simplified_concepts <- function(x) {
  as.data.table(as.data.frame(
    polars::pl$scan_parquet(file.path(thisdirinput, paste0(x, ".parquet")))$select("person_id", "date")$unique()$collect()
  ))
}

D3_PERSONS <- readRDS(file.path(thisdirinput,paste0("D3_PERSONS.rds")))

subpop <- thissubpopulations
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
  
  #persons from D3_ENTRY_FLOWCHART
  processing <- unique(D3_ENTRY_FLOWCHART[ , .(person_id,
                                               entry_spell_category,
                                               exit_spell_category)])
  #add death_date
  processing <- merge(processing, D3_PERSONS[ , .(person_id,
                                                  sex_at_instance_creation,
                                                  birth_year,
                                                  death_date)], all.x = T, by = "person_id") #, allow.cartesian = T
  
  #######################################################################################################
  # start calculated variables
  
  processing <- processing[!is.na(birth_year) & birth_year>1905 ,
                           sel_not_valid_birth_date := 0][is.na(sel_not_valid_birth_date),
                                                          sel_not_valid_birth_date:=1] 
  
  #retrieve selection variables from  D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART
  processing <- merge(processing, unique(D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART[ , .(person_id, sel_no_spell_in_PC_HOSP)]), by= "person_id", all.x = T) #, allow.cartesian = T
  
  processing <- processing[ ,sel_no_HES_linkage := sel_no_spell_in_PC_HOSP ] 
  
  #the person has no spell overlapping the study period by at least one day (2012-2024)
  
  processing <- processing[ entry_spell_category <= enrol_end & exit_spell_category >= enrol_start ,
                            sel_no_spells_in_enrolment_period :=0]
  processing <- processing[ is.na(sel_no_spells_in_enrolment_period) ,
                            sel_no_spells_in_enrolment_period :=1]
  
  processing_epi <- merge(processing, unique(D3_EPISODES_STUDY_DRUGS[ , .(person_id,
                                                                          episode_start,
                                                                          episode_end,
                                                                          exposure_episode,
                                                                          type_medicine
  )]), by= "person_id", all.x = T , allow.cartesian = T) 
  
  
  #no dapagliflozin or Dpp4i or during the enrolment period 1 Jan 2013 to 31 December 2024*
  processing_epi[,  sel_with_no_episode_in_eligibility_period := fifelse(episode_start <= enrol_end & episode_end >= enrol_start , 0, 1, 1)]
  
  #this is 1 if the person does not have 365 days of lookback at episode_start sel_not_enough_lookback
  
  processing_epi <- processing_epi[ , entry_spell_category := as.Date(entry_spell_category)]
  
  processing_epi[, sel_not_enough_lookback := fifelse(episode_start - entry_spell_category >= 365 , 0, 1, 1)]
  
  E_T1DM_COV_dataset <- load_simplified_concepts("E_T1DM_COV_dataset")
  
  #T1D_diag
  concepts_T1D <- E_T1DM_COV_dataset[, .(date = min(date, na.rm = TRUE)), by = person_id]
  setnames(concepts_T1D, "date", "T1D_diag")
  
  rm(E_T1DM_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],
                           concepts_T1D, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(concepts_T1D)
  
  processing_temp <- processing_temp[ ,sel_T1D_diagnosis :=
                                        fifelse(!is.na(T1D_diag) & T1D_diag <= episode_start, 1, 0,0),
                                      by = c("person_id", "episode_start")]
  
  processing_temp <- unique(processing_temp[ , sel_T1D_diagnosis := 
                                               max(sel_T1D_diagnosis),  by = c("person_id", "episode_start")][, .(person_id, sel_T1D_diagnosis,episode_start,T1D_diag)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  
  #sel_no_too_young
  processing_epi <- processing_epi[, age_at_start_episode:=year(episode_start)- birth_year, by="person_id"]
  processing_epi <- processing_epi[ ,sel_no_too_young := as.integer(age_at_start_episode < 40),, by="person_id"]
  
  E_T2DM_COV_dataset <- load_simplified_concepts("E_T2DM_COV_dataset")
  
  #sel_no_T2D_diagnosis
  concepts_T2D <- E_T2DM_COV_dataset[, .(date = min(date, na.rm = TRUE)), by = person_id]
  
  rm(E_T2DM_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],
                           concepts_T2D, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(concepts_T2D)
  
  processing_temp <- processing_temp[ ,sel_no_T2D_diagnosis :=
                                        fifelse( !is.na(date) & date <= episode_start, 0,1,0),
                                      by = c("person_id", "episode_start")]
  
  processing_temp <- unique(processing_temp[ , sel_no_T2D_diagnosis := 
                                               max(sel_no_T2D_diagnosis),  by = c("person_id", "episode_start", "date")][, .(person_id, sel_no_T2D_diagnosis,episode_start, date)])
  
  setnames(processing_temp, "date", "T2D_diag")
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  C_IHD_COV_dataset <- load_simplified_concepts("C_IHD_COV_dataset")
  V_ICVD_COV_dataset <- load_simplified_concepts("V_ICVD_COV_dataset")
  V_PAD_COV_dataset <- load_simplified_concepts("V_PAD_COV_dataset")
  
  # NEW CRITERIA:
  #sel_establish_ACVD_or_high_risk_ACVD
  concepts_ACVD <- rbind(C_IHD_COV_dataset, V_ICVD_COV_dataset, V_PAD_COV_dataset)
  
  rm(C_IHD_COV_dataset, V_ICVD_COV_dataset, V_PAD_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],
                           concepts_ACVD, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(concepts_ACVD)
  
  processing_temp <- processing_temp[ ,sel_establish_ACVD:=
                                        fifelse(!is.na(date) & date <= episode_start, 1L, 0L, 0L),
                                      by = c("person_id", "episode_start")] #corrected
  
  
  processing_temp <- unique(processing_temp[ , sel_establish_ACVD := 
                                               max(sel_establish_ACVD),  by = c("person_id", "episode_start")][, .(person_id, sel_establish_ACVD,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  #sel_high_risk_ACVD
  #age_sex_for_cv
  processing_epi <- processing_epi[(sex_at_instance_creation == "M" & age_at_start_episode >= 55) |
                                     (sex_at_instance_creation == "F" & age_at_start_episode >= 60),
                                   age_sex_for_cv:= 1]
  processing_epi <- processing_epi[is.na(age_sex_for_cv), age_sex_for_cv:=0]
  
  #sel_establish_ACVD==0
  
  E_HYPERLIPIDAEMIA_COV_dataset <- load_simplified_concepts("E_HYPERLIPIDAEMIA_COV_dataset")
  
  #dyslipidaemia
  dyslipidaemia <- unique(E_HYPERLIPIDAEMIA_COV_dataset[ ,.(person_id,date)])
  
  rm(E_HYPERLIPIDAEMIA_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],dyslipidaemia, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(dyslipidaemia)
  
  processing_temp <- processing_temp[, dyslipidaemia :=
                                       fifelse(date <= episode_start, 1, 0, 0),
                                     by = c("person_id", "episode_start")
  ]
  
  
  processing_temp <- unique(processing_temp[ , dyslipidaemia := max(dyslipidaemia),  by = c("person_id", "episode_start")][, .(person_id, dyslipidaemia,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  DP_LIPIDLOWERING_dataset <- load_simplified_concepts("DP_LIPIDLOWERING_dataset")
  
  #lipid lowering therapy
  lipid_lower_therapy <- unique(DP_LIPIDLOWERING_dataset[ ,.(person_id,date)])
  
  rm(DP_LIPIDLOWERING_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],lipid_lower_therapy, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(lipid_lower_therapy)
  
  processing_temp <- processing_temp[, lipid_lower_therapy :=
                                       fifelse(date <= episode_start , 1, 0,0),
                                     by = c("person_id", "episode_start") #corrected ok
  ]
  
  processing_temp <- unique(processing_temp[ , lipid_lower_therapy := max(lipid_lower_therapy),  by = c("person_id", "episode_start")][, .(person_id, lipid_lower_therapy,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  V_HYPERTENSION_COV_dataset <- load_simplified_concepts("V_HYPERTENSION_COV_dataset")
  
  #hypertension
  hypertension <- unique(V_HYPERTENSION_COV_dataset[ ,.(person_id,date)])
  
  rm(V_HYPERTENSION_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],hypertension, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(hypertension)
  
  processing_temp <- processing_temp[, hypertension :=
                                       fifelse(date <= episode_start, 1, 0,0),
                                     by = c("person_id", "episode_start")
  ]
  
  processing_temp <- unique(processing_temp[ , hypertension := max(hypertension),  by = c("person_id", "episode_start")][, .(person_id, hypertension,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  DP_ANTIHYPERTENSIVE_dataset <- load_simplified_concepts("DP_ANTIHYPERTENSIVE_dataset")
  
  #anti-hypertensive
  anti_hypertensive <- unique(DP_ANTIHYPERTENSIVE_dataset[ ,.(person_id,date)])
  
  rm(DP_ANTIHYPERTENSIVE_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],anti_hypertensive, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(anti_hypertensive)
  
  processing_temp <- processing_temp[, anti_hypertensive :=
                                       fifelse(date <= episode_start , 1, 0,0),
                                     by = c("person_id", "episode_start")  #corrected ok
  ]
  
  processing_temp <- unique(processing_temp[ , anti_hypertensive := max(anti_hypertensive),  by = c("person_id", "episode_start")][, .(person_id, anti_hypertensive,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  L_CURRENTSMOKER_COV_dataset <- load_simplified_concepts("L_CURRENTSMOKER_COV_dataset")
  
  #Tobacco use
  tobacco <- unique(L_CURRENTSMOKER_COV_dataset[ ,.(person_id,date)])
  
  rm(L_CURRENTSMOKER_COV_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],tobacco, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(tobacco)
  
  processing_temp <- processing_temp[, tobacco :=
                                       fifelse((date >= (episode_start - 365) & date <= episode_start) | (date >= episode_start & date<=episode_end), 1, 0,0),
                                     by = c("person_id", "episode_start")
  ]
  
  processing_temp <- unique(processing_temp[ , tobacco := max(tobacco),  by = c("person_id", "episode_start")][, .(person_id, tobacco,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  ### ACVD final
  # Inclusion criterion:
  # Keep subjects who have either:
  # 1) established ACVD prior to index date, OR
  # 2) high risk of ACVD at index.
  #
  # High ACVD risk is defined as:
  # no established ACVD,
  # age >= 55 years in men or >= 60 years in women,
  # and at least one of:
  # dyslipidaemia prior to index,
  # lipid-lowering therapy current at index,
  # hypertension prior to index,
  # anti-hypertensive medication current at index,
  # tobacco use within last year or current use at index.
  
  processing_epi[, any_ACVD_risk_factor := as.integer(
    dyslipidaemia == 1 |
      lipid_lower_therapy == 1 |
      hypertension == 1 |
      anti_hypertensive == 1 |
      tobacco == 1
  )]
  
  processing_epi[, high_risk_ACVD := as.integer(
    sel_establish_ACVD == 0 &
      age_sex_for_cv == 1 &
      any_ACVD_risk_factor == 1
  )]
  
  processing_epi[, has_ACVD_or_risk_of_ACVD := as.integer(
    sel_establish_ACVD == 1 |
      high_risk_ACVD == 1
  )]
  
  # Exclusion criterion:
  # 1 = exclude because the subject has neither established ACVD nor high ACVD risk
  # 0 = keep because the subject has established ACVD or high ACVD risk
  processing_epi[, sel_no_ACVD_or_risk_of_ACVD := as.integer(
    has_ACVD_or_risk_of_ACVD == 0
  )]
  
  DP_SGLT2I_dataset <- load_simplified_concepts("DP_SGLT2I_dataset")
  DP_DDP4I_dataset <- load_simplified_concepts("DP_DDP4I_dataset")
  
  #N_no_prevalent_SGLT2I_or_DPP4
  concepts_SGLT2I_or_DPP4 <- rbind(DP_SGLT2I_dataset, DP_DDP4I_dataset)
  
  rm(DP_SGLT2I_dataset, DP_DDP4I_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],concepts_SGLT2I_or_DPP4, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(concepts_SGLT2I_or_DPP4)
  
  processing_temp <- processing_temp[, sel_prevalent_SGLT2I_or_DPP4 :=
                                       fifelse(date >= (episode_start - 365) & date < episode_start, 1, 0,0),
                                     by = c("person_id", "episode_start")
  ]
  
  processing_temp <- unique(processing_temp[ , sel_prevalent_SGLT2I_or_DPP4 := max(sel_prevalent_SGLT2I_or_DPP4),  by = c("person_id", "episode_start")][, .(person_id, sel_prevalent_SGLT2I_or_DPP4,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  C_AMI_AESI_dataset <- load_simplified_concepts("C_AMI_AESI_dataset")
  C_UNSTANGINA_AESI_dataset <- load_simplified_concepts("C_UNSTANGINA_AESI_dataset")
  N_STROKE_AESI_dataset <- load_simplified_concepts("N_STROKE_AESI_dataset")
  
  #sel_acute_CV_history
  concepts_CV_history <- rbind(C_AMI_AESI_dataset, C_UNSTANGINA_AESI_dataset, N_STROKE_AESI_dataset)
  
  rm(C_AMI_AESI_dataset, C_UNSTANGINA_AESI_dataset, N_STROKE_AESI_dataset)
  
  processing_temp <- merge(processing_epi[,.(person_id, episode_start, episode_end)],
                           concepts_CV_history, all.x = T, by= "person_id",allow.cartesian = T )
  
  rm(concepts_CV_history)
  
  processing_temp <- processing_temp[ ,sel_acute_CV_history:=
                                        fifelse((date >= episode_start - 365 & date < episode_start), 1, 0, 0),
                                      by = c("person_id", "episode_start")]
  
  processing_temp <- unique(processing_temp[ , sel_acute_CV_history := 
                                               max(sel_acute_CV_history),  by = c("person_id", "episode_start")][, .(person_id, sel_acute_CV_history,episode_start)])
  
  processing_epi <- merge(processing_epi, processing_temp, all.x=T, by = c("person_id", "episode_start"), allow.cartesian = T)
  
  # # define order of selection criteria
  # sel_cols <- c(
  #   "sel_not_valid_birth_date",
  #   "sel_no_HES_linkage",
  #   "sel_not_enough_lookback",
  #   "sel_no_episode_in_enrolment_period",
  #   "sel_prevalent_dapagliflozin_or_DPP4i",
  #   "sel_T1D_diagnosis",
  #   "sel_no_too_young",
  #   "sel_no_T2D_diagnosis",
  #   "sel_no_ACVD_or_risk_of_ACVD",
  #   "sel_prevalent_SGLT2I_or_DPP4",
  #   "sel_acute_CV_history")
  # 
  # # apply dependency (cascade)
  # for (i in 2:length(sel_cols)) {
  #   prev_col <- sel_cols[i - 1]
  #   curr_col <- sel_cols[i]
  #   
  #   processing[
  #     get(prev_col) == 1,
  #     (curr_col) := 1
  #   ]
  # }
  # 
  # eligible <- processing[,
  #                        Reduce(`&`, lapply(.SD, \(x) x == 0)),
  #                        .SDcols = sel_cols]
  # 
  # processing[, is_study_episode := 0L]
  # 
  # idx <- processing[
  #   eligible,
  #   .I[which.min(episode_start)],
  #   by = person_id
  # ]$V1
  
  # define order of selection criteria
  sel_cols <- c(
    "sel_not_valid_birth_date",
    "sel_no_HES_linkage",
    "sel_no_spells_in_enrolment_period",
    "sel_with_no_episode_in_eligibility_period",
    "sel_not_enough_lookback",
    "sel_T1D_diagnosis",
    "sel_no_too_young",
    "sel_no_T2D_diagnosis",
    "sel_no_ACVD_or_risk_of_ACVD",
    "sel_prevalent_SGLT2I_or_DPP4",
    "sel_acute_CV_history"
  )
  
  
  # criteria where ANY episode-level occurrence excludes the person
  person_level_any_cols <- c(
    "sel_T1D_diagnosis",
    "sel_prevalent_SGLT2I_or_DPP4",
    "sel_acute_CV_history"
  )
  
  # collapse selected criteria to person level
  processing_epi[, (person_level_any_cols) :=
                   lapply(.SD, function(x) as.integer(any(x == 1, na.rm = TRUE))),
                 by = person_id,
                 .SDcols = person_level_any_cols]
  
  # apply dependency cascade
  for (i in 2:length(sel_cols)) {
    prev_col <- sel_cols[i - 1]
    curr_col <- sel_cols[i]
    
    processing_epi[
      get(prev_col) == 1,
      (curr_col) := 1L
    ]
  }
  
  # define eligible episode rows
  eligible <- processing_epi[
    ,
    Reduce(`&`, lapply(.SD, function(x) x == 0)),
    .SDcols = sel_cols
  ]
  
  # select one study episode per person among eligible episodes
  processing_epi[, is_study_episode := 0L]
  
  idx <- processing_epi[
    eligible == TRUE,
    .I[which.min(episode_start)],
    by = person_id
  ]$V1
  
  processing_epi[idx, is_study_episode := 1L]
  
  
  #keep only specific variables
  processing1 <- processing_epi[, .(person_id,
                                    episode_start,
                                    episode_end,
                                    exposure_episode,
                                    type_medicine,
                                    sex_at_instance_creation,
                                    birth_year,
                                    death_date,
                                    entry_spell_category,
                                    exit_spell_category,
                                    sel_not_valid_birth_date,
                                    sel_no_HES_linkage,
                                    sel_no_spells_in_enrolment_period,
                                    sel_with_no_episode_in_eligibility_period,
                                    sel_not_enough_lookback,
                                    T1D_diag,
                                    sel_T1D_diagnosis,
                                    sel_no_too_young,
                                    sel_no_T2D_diagnosis,
                                    T2D_diag,
                                    sel_no_ACVD_or_risk_of_ACVD,
                                    sel_prevalent_SGLT2I_or_DPP4,
                                    sel_acute_CV_history,
                                    is_study_episode)]
  
  
  outputfile <- processing1
  
  nameoutput <- paste0("D3_ELIGIBILITY_EPISODE",suffix)
  assign(nameoutput, outputfile)
  #fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  
  
  print('D3_ELIGIBILITY')
  
  
  
  setorder(processing_epi, person_id, episode_start)
  
  dt_first <- processing_epi[, {
    idx <- which(is_study_episode == 1)
    
    if (length(idx) > 0) {
      .SD[idx[1]]
    } else {
      .SD[1]
    }
    
  }, by = person_id]
  
  processing2 <- dt_first[ , included := is_study_episode]
  
  #keep only specific variables
  outputfile <- processing2[, .(person_id,
                                sex_at_instance_creation,
                                birth_year,
                                death_date,
                                entry_spell_category,
                                exit_spell_category,
                                exposure_episode,
                                sel_not_valid_birth_date,
                                sel_no_HES_linkage,
                                sel_no_spells_in_enrolment_period,
                                sel_with_no_episode_in_eligibility_period,
                                sel_not_enough_lookback,
                                T1D_diag,
                                sel_T1D_diagnosis,
                                sel_no_too_young,
                                sel_no_T2D_diagnosis,
                                T2D_diag,
                                sel_no_ACVD_or_risk_of_ACVD,
                                sel_prevalent_SGLT2I_or_DPP4,
                                sel_acute_CV_history,
                                included)]
  
  nameoutput <- paste0("D3_ELIGIBILITY",suffix)
  assign(nameoutput, outputfile)
  #fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}