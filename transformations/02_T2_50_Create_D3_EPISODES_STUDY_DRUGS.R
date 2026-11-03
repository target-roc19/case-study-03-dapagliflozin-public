############################################################
#                                                          #
####         02_T2_32_Create_D3_EPISODES_STUDY_DRUGS        ####
#                                                          #
############################################################


# author: Rosa Gini

# v 1.3 30 Jul 2026

# keeping only episodes starting during the study period

# v 1.2 17 Jun 2026

# replace in diagnostic file the more granular N

# v 1.1 7 Jun 2026

# replace medicinal_product_atc_code with codvar

# v 1.0 7 Mar 2026

# completed for test FinReg

# v 0.1 26 Feb 2026


print('CREATE D3_EPISODES_STUDY_DRUGS')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_05_D3_EPISODES_STUDY_DRUGS"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirexp <- thisdiroutput
  thisdirsettings <- thisdirinput
  thisdirarchive <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirexp <- direxp
  thisdirarchive <- dirpargen
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

thisds <- settings$ds
study_start_date <- settings$study_period_start_date
study_end_date <- settings$study_period_end_date

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  
  persons <- readRDS(file.path(thisdirinput,paste0("D3_ENTRY_FLOWCHART",suffix,".rds")))
  
  exposed_medicines <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0("DP_DAPAGLIFLOZIN_dataset",".parquet")))))
  exposed_medicines <- merge(exposed_medicines,persons, by = "person_id", all = F)
    
  control_medicines <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0("DP_DDP4I_dataset",".parquet")))))
  control_medicines <- merge(control_medicines,persons, by = "person_id", all = F)
  
  
  
  #########################################
  # start_processing
  

  exposed_medicines <- exposed_medicines[ date >= entry_spell_category & date >= study_start_date - 365 & date <= exit_spell_category & date <= study_end_date,]
  exposed_medicines[, exposure := 1]
  
  control_medicines <- control_medicines[ date >= entry_spell_category & date >= study_start_date - 365 & date <= exit_spell_category & date <= study_end_date,]
  control_medicines[, exposure := 0]
  
  
  dispensings <- rbind(exposed_medicines,control_medicines)
 
    dispensings <- dispensings[,.(person_id,date,medicinal_product_id,codvar,presc_duration_days, exposure)]
    dispensings[, duration := presc_duration_days]
    tocheck <- dispensings[,.N, by = c("medicinal_product_id", "codvar", "exposure","duration")]
    fwrite(tocheck, file = file.path(thisdiroutput,"D5_check_study_drugs_duration.csv"))
    
    tocheck <- tocheck[N > 0 & N < 5 ,N_records_cat := "< 5"]
    tocheck <- tocheck[N >= 5 ,N_records_cat := as.character(N)]
    tocheck[,N := NULL]
    
    fwrite(tocheck, file = file.path(thisdirexp,"D5_check_study_drugs_duration_masked.csv"))
    

 
  # clean the cases when duration is not created or is absurd
  
    dispensings <- dispensings[, duration := as.numeric(duration)]
    dispensings <- dispensings[is.na(duration) , duration := 30]
    dispensings <- dispensings[duration <= 0 , duration := 30]
  
  # use AdhereR
  
    # in CS5 change atc for comparator drug is a switch, here it does not seem to be so, therefore episodes are withing exposure or non exposure only, no difference epr ATC
    
  # atc_list <- unique(unlist(dispensings[exposure == 0,.(codvar)]))
  # 
  # list_indices <- c(1L, rep(0L, length(atc_list)))
  # codes <- c("", atc_list)
    
    list_indices <- c(0,1)
  
  window_duration <- as.integer(study_end_date - study_start_date) + 730
  
  processing <- data.table()
  for (i in seq_along(list_indices)) {
    j <- list_indices[i]
    # atc <- codes[i]
    temp <- dispensings[exposure == j,]
    # if (j == 0) {
    #   temp <- temp[codvar == atc,]
    # }
    temp <- compute.treatment.episodes(temp,
                                       ID.colname= "person_id",
                                       event.date.colname= "date",
                                       event.duration.colname= "duration",
                                       # medication.class.colname= "codvar",
                                       carryover.within.obs.window = TRUE, # carry-over into the OW
                                       # carry.only.for.same.medication = TRUE, # & only for same type
                                       medication.change.means.new.treatment.episode = FALSE, # & type change
                                       maximum.permissible.gap = 90, # & a gap longer than 90 days
                                       maximum.permissible.gap.unit = "days",
                                       followup.window.duration = window_duration
    )
    temp <- as.data.table(temp)
    setnames(temp,c("episode.start","episode.end"),c("episode_start",	"episode_end"))

    temp <- temp[,.(person_id,episode_start,episode_end)]
    # add 30 days at the end
    temp[, episode_end := episode_end + 90]
    temp[,exposure_episode := j]
    # temp[,type_medicine := atc]
    processing <- rbind(processing, temp, fill = T)
  }

  # since change atc is not a switch, we removed this from the computation of episodes. let's add the variable again for compatibility
  processing[, type_medicine := ""]

  # keep only episodes starting during the study period
  
  processing <- processing[episode_start >= study_start_date & episode_start <= study_end_date, ]  
 
  #########################################
  # clean

  listvartokeep <- c("person_id", "exposure_episode", "type_medicine", "episode_start", "episode_end")
  
  setorder(processing, person_id, episode_start, exposure_episode,  type_medicine)
  
  
  # listvartokeep <- c("person_id", "exposure_episode", "episode_start", "episode_end")
  # 
  # setorder(processing, person_id, episode_start, exposure_episode)
  # 
  processing <- processing[,..listvartokeep]



  #########################################
  # save

  outputfile <- processing

  nameoutput <- paste0("D3_EPISODES_STUDY_DRUGS",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  #fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
}
