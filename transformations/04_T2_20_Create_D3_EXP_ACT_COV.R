############################################################
#                                                          #
####         D3_EXP_ACT_COV                              ####
#                                                          #
############################################################

# authors: Rosa Gini
# v 1.1
# 4 Sep 2026

print('CREATE D3_EXP_ACT_COV')

#########################################
# assign directories

if (TEST){
  
  # testname <- "test_18_D3_EXP_ACT_COV"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # thiscovariates <- c("gastritis_esophagitis", "History_of_GIB_in_year_before_cohort_entry", "History_of_GIB", "Last_GIB", "BMI", "Smoking", "Obesity")
  # dir.create(thisdiroutput, showWarnings = F)
  
} else {
  
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive, "codebooks", "D3_EXP_ACT_COV.xlsx"), 1)))
  
  thiscovariates <- unlist(parameters_this_step[parameter %in% c("covar_cat", "covar_ord", "covar_cont"), .(values)])
}

#########################################
# load settings

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive, "codebooks", "D3_EXP_ACT_COV.xlsx"), 1)))

#########################################
# loop over subpopulations

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  
  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)
  
  #########################################
  # load study population
  
  totalpop <- readRDS(file.path(thisdirinput, paste0("D3_STUDY_POPULATION", suffix, ".rds")))
  
  #########################################
  # merge covariates at t0
  
  for (covar in c(thiscovariates[-1])) {
    
    covar_TD <- readRDS(file.path(thisdirinput, paste0("D3_", covar, "_TD", suffix, ".rds")))
    
    suppressWarnings(covar_TD[, c("start_look_back", "entry_spell_category", "exit_spell_category") := NULL])
    
    totalpop[, ref_date := t0]
    
    totalpop <- covar_TD[totalpop, on = .(person_id, start_d <= ref_date, end_d >= ref_date)]
    
    totalpop[, c("start_d", "end_d") := NULL]
    
    setnames(totalpop, paste0("value_of_", covar), paste0(covar, "_at_t0"))
  }
  
  processing <- totalpop
  
  ############################################
  # covariates computed / imputed
  
  processing[, Smoking_imputed_at_t0 := as.character(Smoking_at_t0)]
  processing[Smoking_at_t0 == "" | is.na(Smoking_at_t0), Smoking_imputed_at_t0 := "0"]
  
  #########################################
  # rename baseline variables
  
  setnames(processing, "gender", "gender_at_t0")
  setnames(processing, "yrsdiab", "yrsdiab_at_t0")
  
  #########################################
  # create categorical age variable
  
  processing[, age_cat_at_t0 := cut(age, breaks = c(-Inf, 65, 75, 85, Inf), right = FALSE, labels = c("<65", "65-74", "75-84", ">=85"))]
  
  processing[, age_cat_at_t0 := factor(age_cat_at_t0, levels = c("<65", "65-74", "75-84", ">=85"))]
  
  setnames(processing, "age", "age_at_t0")
  
  #########################################
  # check age distribution
  
  print(processing[, .N, by = age_cat_at_t0])
  
  #########################################
  # clean
  
  tokeep <- c("person_id", "t0", "exposure", "age_at_t0", "age_cat_at_t0", "yrsdiab_at_t0", paste0(thiscovariates, "_at_t0"), "birth_year", "entry_spell_category", "exit_spell_category")
  
  processing <- processing[, ..tokeep]
  
  #########################################
  # save
  
  nameoutput <- "D3_EXP_ACT_COV"
  nameoutputext <- paste0(nameoutput, suffix, ".rds")
  
  assign(nameoutput, processing)
  
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputext))
}