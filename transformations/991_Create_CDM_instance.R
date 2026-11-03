############################################################
#                                                          #
####   CREATE CDM_INSTANCE ####
# 
#                                                          #
############################################################

# author: Rosa Gini,

# v 1.0 4 Jun 2026

# generate synthetic CDM for TARGET-EU
# author: rosa gini

##############################

# input data model

dirinputCDM <- file.path(dirarchive,"i_CDM_data_model")

# version of this CDM

thisDEAP <- "UU"
thisDS <- "CPRD"
thiscodingsystemEVENTS <- "MEDCODEID"
thiscodingsystemMEDICINES <- "ATC"
thiscodingsystemPROCEDURES <- c("MEDCODEID")
thisdateMEDICINES <- "date_prescription"
thisdateVACCINES <- "vx_admin_date"

coding_system_list <- unique(c(thiscodingsystemEVENTS, thiscodingsystemMEDICINES, thiscodingsystemPROCEDURES))
itemsets_sheet_CDM <- itemsets_sheet_total[ds_name == thisDS,]

instance_version <- 0.2
version <- instance_version
labversion <- ""
dirversionCDM <- dirinput # paste0(dirCDM,"/v_",version)
dir.create(file.path(dirversionCDM), showWarnings = F)



settings <- readRDS(file.path(dirpargen,"SETTINGS.rds"))
study_period_start_date <- settings$study_period_start_date
study_period_end_date <- settings$study_period_end_date


# list if all concept_ids including events and medicines, excluding datasource-specific (TO ADD: procedures)

concept_id_list <- c(concept_ids_diagnosis, concept_ids_procedures,concept_ids_medicines#,concept_ids_vaccines
)

# type of concept_id

type_of_concept_id <- list()

for (conc in concept_id_list) {
  if (conc %in% concept_ids_diagnosis) {
    type_of_concept_id[[conc]] <- "Diagnosis"
  }
  if (conc %in% concept_ids_medicines) {
    type_of_concept_id[[conc]] <- "Medicines"
  }
  if (conc %in% concept_ids_procedures) {
    type_of_concept_id[[conc]] <- "Procedures"
  }
  # if (conc %in% concept_ids_vaccines) {
  #   type_of_concept_id[[conc]] <- "Vaccines"
  # }
}


# pick one code for each coding system for each concept

code_concept_id_codsys <- list()
code_concept_id_codsys_DT <- data.table()
for (conc in concept_id_list) {
  for (codsys in coding_system_list) {
    print(conc)
    print(codsys)
    # codsysext <-  c("ICD10_2019","ICD10","ICD10CM")
    # pick one short code representing the concept_id
    temp <- unique(event_codelist[concept_id == conc & coding_system %in% codsys & tags == "narrow",.(code)])
    temp2 <- unique(medicine_codelist[concept_id == conc & tags == "narrow" & coding_system == codsys, .(code)])
    temp3 <- unique(procedure_codelist[concept_id == conc & tags == "narrow" & coding_system == codsys, .(code)])
    #    temp2 <- unique(rbind(medicine_codelist[concept_id == conc & tags == "narrow" & coding_system == codsys,.(code)],vaccine_codelist[concept_id == conc & tags == "narrow" & coding_system == codsys,.(code)]))
    temp <- rbind(temp, temp2, temp3, fill = T)
    print(nrow(temp))
    if (nrow(temp) > 0) {
      temp[,len := str_length(code)]
      setorderv(temp, "len")
      chosen_code <- temp[1, code] 
      code_concept_id_codsys[[conc]][[codsys]] <- chosen_code
      temp <- temp[1,.(code)]
      temp[,concept_id := conc]
      temp[,coding_system := fifelse(codsys %in% c("ICD10_2019","ICD10","ICD10CM"),"ICD10",codsys)]
      temp[,type_of_concept_id := concept_set_domains[[conc]]]
      code_concept_id_codsys_DT <- rbind(code_concept_id_codsys_DT,temp[1, ], fill = T)
    }else{
      code_concept_id_codsys[[conc]][[codsys]] <- "AAAAAAA"
    }
  }
}

code_concept_id_codsys_DT <- unique(code_concept_id_codsys_DT)

# pick one strategy per each itemset

setorder(itemsets_sheet_CDM,cdm_table_name,concept_id)
itemsets_sheet_CDM[, ord:= seq_len(.N),by = c("cdm_table_name","concept_id")]
itemsets_sheet_CDM <- itemsets_sheet_CDM[ord == 1,] 



##############################
# create seed files
##############################

# baseline date
baselinedate <- as.Date(lubridate::ymd(20111231))

# list the 'seed' files
tablesCDM <- c("PERSONS",
               "VISIT_OCCURRENCE",
               "OBSERVATION_PERIODS",
               "EVENTS",
               "MEDICINES",
               "MEDICAL_OBSERVATIONS",
               "SURVEY_OBSERVATIONS",
               "VACCINES",
               "PROCEDURES",
               "CDM_SOURCE",
               "INSTANCE",
               "METADATA",
               "PRODUCTS")


#list the dates of each seed file
listdatesCDM <- list()
listdatesCDM[["OBSERVATION_PERIODS"]] <- c("op_start_date","op_end_date")
listdatesCDM[["MEDICINES"]] <- c("date_dispensing","date_prescription")
listdatesCDM[["VACCINES"]] <- c("vx_record_date","vx_admin_date")
listdatesCDM[["EVENTS"]] <- c("start_date_record", "end_date_record")
listdatesCDM[["PROCEDURES"]] <- c("procedure_date")
listdatesCDM[["CDM_SOURCE"]] <- c("date_creation","recommended_end_date")
listdatesCDM[["INSTANCE"]] <- c("date_when_data_last_updated","since_when_data_complete"	,"up_to_when_data_complete")
listdatesCDM[["MEDICAL_OBSERVATIONS"]] <- c("mo_date")
listdatesCDM[["SURVEY_OBSERVATIONS"]] <- c("so_date")
listdatesCDM[["VISIT_OCCURRENCE"]] <- c("visit_start_date", "visit_end_date")

listyearsCDM <- list()
listyearsCDM[["PERSONS"]] <- c("year_of_birth")

listcharCDM <- list()
listcharCDM[["OBSERVATION_PERIODS"]] <- c("op_meaning")
# listcharCDM[["MEDICINES"]] <- c("date_dispensing","date_prescription")
# listcharCDM[["VACCINES"]] <- c("vx_record_date","vx_admin_date")
# listcharCDM[["EVENTS"]] <- c("start_date_record", "end_date_record")
# listcharCDM[["PROCEDURES"]] <- c("procedure_date")
# listcharCDM[["CDM_SOURCE"]] <- c("date_creation","recommended_end_date")
# listcharCDM[["INSTANCE"]] <- c("date_when_data_last_updated","since_when_data_complete"	,"up_to_when_data_complete")
# listcharCDM[["MEDICAL_OBSERVATIONS"]] <- c("mo_date")
# listcharCDM[["SURVEY_OBSERVATIONS"]] <- c("so_date")
listcharCDM[["VISIT_OCCURRENCE"]] <- c("meaning_of_visit")

for (tab in tablesCDM){ 
  data <- data.table()
  assign(paste0(tab,"data"),data)
  data <-  as.data.table(readxl::read_excel(file.path(dirinputCDM, paste0(tab, ".xlsx"))))
  colvars <- listdatesCDM[[tab]]
  if (!is.null(colvars)) {
    data[, (colvars) := lapply(.SD, as.IDate), .SDcols = colvars]  
  }
  charvars <- listcharCDM[[tab]]
  if (!is.null(charvars)) {
    data[, (charvars) := lapply(.SD, as.character), .SDcols = charvars]  
  }
  assign(paste0(tab,"data_model"),data)
}

############
# base CDM
############

#################
# metadata tables

tab <- "CDM_SOURCE" 
df <- data.table(
  data_access_provider_name = thisDEAP,
  data_source_name = thisDS,
  instance_number = instance_version,
  recommended_end_date = 4000,
  date_creation = 4200
)
for (datevar in listdatesCDM[[tab]]) {
  print(datevar)
  df[, (datevar) := as.Date(get(datevar) + baselinedate)]
}
df <- rbind(CDM_SOURCEdata_model,df,fill = T)
date_cols <- sapply(df, is.Date)
date_cols <- names(date_cols)[date_cols]
df[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(df, file.path(dirversionCDM, paste0(paste0(tab),".csv") ))

for (tab in c("METADATA","INSTANCE","PRODUCTS")) {
  df <- get(paste0(tab,"data_model"))
  # for (datevar in listdatesCDM[[tab]]) {
  #   print(datevar)
  #   df[, (datevar) := as.Date(get(datevar) + baselinedate)]
  # }
  date_cols <- sapply(df, is.Date)
  date_cols <- names(date_cols)[date_cols]
  df[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
  fwrite(df, file.path(dirversionCDM, paste0(paste0(tab),".csv") ))
} 


#############################################################################
# CREATE  A CORE OF DATA FROM A SYTHETIC TEST DATA

testfolder <- "test_15_D4_EXP_ACT_COV_WEIGHT"
dataset_to_be_regressed <- readRDS(file.path(dirtest,testfolder,"D3_EXP_ACT_COV.rds"))
dataset_to_be_regressed <- dataset_to_be_regressed[, t0_min := min(t0), by = "person_id"]
dataset_to_be_regressed[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]

# PERSONS

tab <- "PERSONS"

# data <- copy(dataset_to_be_regressed)[,.(person_id,t0,age_at_t0,sex)]

data <- unique(copy(dataset_to_be_regressed)[,.(person_id, t0_min)])
# sex_at_instance_creation

set.seed(1234)
data[, sex := as.character(sample(1:2, .N, replace = TRUE, prob = c(.5,.5)))]
data[,sex := fifelse(sex == "1","M","F") ]

data[, sex_at_instance_creation := sex]

set.seed(15234)
data <- data[, random := runif(.N)]
data[random <.01, sex_at_instance_creation := "O"]


# birth_year

# age at t0

set.seed(5234)
age_at_t0 <- c(17, 18, 25, 45, 65, 85)
probage = c(.001,.01,.2,.5,.2)
totprob = sum(probage)
probage = c(probage, 1 - totprob)
data[, age_at_t0 := sample(age_at_t0, .N, replace = TRUE, prob = probage)]
data[, birth_date := t0_min - round(365.25*age_at_t0) + round(365 * runif(.N))]
data[,year_of_birth := year(birth_date)]
data[,month_of_birth := month(birth_date)]
data[,day_of_birth := day(birth_date)]

set.seed(74628)
data <- data[, random := runif(.N)]
set.seed(14628)
data <- data[random < .002, death_date := t0_min +  round(5* 365 * runif(.N))]
data[,year_of_death := year(death_date)]
data[,month_of_death := month(death_date)]
data[,day_of_death := day(death_date)]

data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))

# OBSERVATION_PERIODS

tab <- "OBSERVATION_PERIODS"

data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
data <- merge(data, dataset_to_be_regressed, by = "person_id")
data[, op_start_date := lubridate::make_date(year_of_birth,month_of_birth,day_of_birth)]
set.seed(154623)
data[, op_start_date := op_start_date + round((t0_min - op_start_date)* runif(.N))]
data[, death_date := lubridate::make_date(year_of_death,month_of_death,day_of_death)]
set.seed(3754623)
data[, random := runif(.N)]
data[, op_end_date := op_end_date + round(2000 * random) - 1000]
data[, op_end_date := pmin(op_end_date, death_date, na.rm = T)]
data[, op_meaning := "OP_with_hospital_linkage"]
dataPC <- copy(data)[, op_start_date := lubridate::make_date(year_of_birth,month_of_birth,day_of_birth)]
data[, op_meaning := "enlisted_with_GP"]
data <- rbind(data,dataPC)
# duplicate and translate a couple of observation periods
set.seed(546231)
data <- data[, random := runif(.N)]
toadd <- copy(data)[random < .02,]
set.seed(462313)
toadd[, op_start_date := op_start_date + round(runif(.N)*500)]
set.seed(6231)
toadd[, op_end_date := op_start_date + round(runif(.N)*500)]
data <- rbind(data, toadd)
set.seed(746231)
data <- data[, random := runif(.N)]
toadd <- copy(data)[random < .02,]
set.seed(462313)
toadd[, op_start_date := op_start_date - round(runif(.N)*1000)]
set.seed(620231)
toadd[, op_end_date := op_start_date + round(runif(.N)*500)]
data <- rbind(data, toadd)
# remove 1 op_end_date
person_to_remove <- unlist(data[10,.(person_id)])
data <- data[person_id == person_to_remove, op_end_date := NA_Date_]
# remove 1 observation period to trigger an exclusion criterion
person_to_remove <- unlist(data[50,.(person_id)])
data <- data[person_id != person_to_remove,]
obsp <- data
obsp[, birth_date := lubridate::make_date(year_of_birth,month_of_birth,day_of_birth)]
# clean and save
setorder(data,person_id,op_start_date)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))

##
# COVARIATES

# create a dataset to_CDM where all covariate concept_id are assigned to a date before t0, and remove some of them

# thisdirarchive <- file.path(thisdir, "..", "..", "run", "p_parameters", "archive_parameters")
thisdirparlab <- file.path(dirarchive,"parameters_lab_values")


ingredients_covariates <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_SIMPLE_TD.xlsx")),1)))

ingredients_covariates <- ingredients_covariates[,.(varname, ingredient)]

thiscovariates <- unlist(unique(ingredients_covariates[,.(varname)]))
thiscovariates <- setdiff(thiscovariates, "Creatinine_serum")


parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D5_Table_2_baseline_cohort.xlsx")),1)))

data <- copy(dataset_to_be_regressed)
data[, c("birth_year","gender_at_t0","age_at_t0","entry_spell_category","exit_spell_category", "exposure", "t0_min", "CKD_at_t0") := NULL]


setnames(data,paste0(thiscovariates, "_at_t0"), thiscovariates)

for (varname in thiscovariates) {
  data[, (varname) := as.character(get(varname))]
}

data <- melt(
  data,
  id.vars = c("person_id", "t0"),
  variable.name = "varname",
   measure.vars = thiscovariates,
  value.name = "value"
)

data <- data[value != "0",]


data <- merge(data,unique(obsp[,.(person_id, birth_date)]), by = "person_id")
# data[, tmp := 1]
# thisconcept_id[, tmp := 1]
to_CDM <- merge(data, ingredients_covariates, by = "varname", allow.cartesian = T)
setnames(to_CDM, "ingredient", "concept_id")

# 
# # list covariates per type
# 
# 
# 
# thiscovariates_ord <- unlist(parameters_this_step[parameter == "covar_ord",.(values)])
# thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)])
# thiscovariates_lab <- unlist(parameters_this_step[parameter == "covar_lab",.(values)])
# 
# covariates_binary <- paste0(thiscovariates_ord,"_at_t0")
# 
# covariates_lab <- paste0(thiscovariates_lab,"_at_t0")
# 
# covariates_cat <- paste0(thiscovariates_cat,"_at_t0")
# 
# # ingredients covariates
# 
# # threshold_lab <- fread(file.path(thisdirparlab, "LAB_threshold_with_unit.csv"))
# 
# thisconcept_id_lab <- unlist(unique(ingredients_covariates[varname %in% thiscovariates_lab,.(ingredient)]))
# thisconcept_id <- unique(ingredients_covariates[,.(varname,ingredient)])
# 

set.seed(452783)
to_CDM[, random := runif(.N)]
to_CDM[, date := as.Date(t0 - round(random*(t0 - study_period_start_date - 500)))]

set.seed(52783)
to_CDM[, random := runif(.N)]
to_CDM <- to_CDM[(random > .05) , ]

# add to to_CDM exposure

data <- copy(dataset_to_be_regressed)
data <- unique(data[, .(person_id, t0, exposure)])
data[exposure == 1,concept_id := "DP_DAPAGLIFLOZIN"]
data[exposure == 0,concept_id := "DP_DDP4I"]
set.seed(452783)
data[, random := runif(.N)]
data[, date := as.Date(t0)]
to_CDM <- rbind(to_CDM, data, fill = T)

# additonial study exposure during followup

set.seed(252783)
data <- copy(dataset_to_be_regressed)
data[, random := runif(.N)]
data <- data[exposure == 1 | random > .85,]
data <- unique(data[, .(person_id, t0_min)])
set.seed(1252783)
data[, random := runif(.N)]
data[, numexpo := round(20 * random)]
data <-  data[rep(seq_len(nrow(data)), times = numexpo)]
data[, numexpo := NULL]
set.seed(565812)
data[, random := rpois(.N, lambda = 120)] # rpois(.N, lambda = 5000)
data[, date := t0_min[1L] + 10 + 
       cumsum(replace(as.integer(random), 1L, 0L)),
     by = person_id]
data[, random := NULL]
data[, t0 := NULL]
data[,concept_id := "DP_DAPAGLIFLOZIN"]
to_CDM <- rbind(to_CDM, data, fill = T)

# additonal study active comparator drug  during followup

set.seed(11252783)
data <- copy(dataset_to_be_regressed)
data[, random := runif(.N)]
data <- data[exposure == 0 | random > .95,]
data <- unique(data[, .(person_id, t0_min)])
set.seed(31252783)
data[, random := runif(.N)]
data[, numexpo := round(20 * random)]
data <-  data[rep(seq_len(nrow(data)), times = numexpo)]
data[, numexpo := NULL]
set.seed(1565812)
data[, random := rpois(.N, lambda = 120)] # rpois(.N, lambda = 5000)
data[, date := t0_min[1L] + 10 + 
       cumsum(replace(as.integer(random), 1L, 0L)),
     by = person_id]
data[, random := NULL]
data[, t0 := NULL]
data[,concept_id := "DP_DDP4I"]
to_CDM <- rbind(to_CDM, data, fill = T)

# add to to_CDM diagnosis

data <- copy(dataset_to_be_regressed)
data <- unique(data[, .(person_id, t0)])
data[,concept_id := "E_T2DM_COV"]
set.seed(2783199)
data[, random := runif(.N)]
data[, date := as.Date(t0 - round(500 * random))]
set.seed(527831)
data[, random := runif(.N)]
data <- data[(random > .05) , ]
to_CDM <- rbind(to_CDM, data, fill = T)


# add to to_CDM outcome

set.seed(252783)
data <- copy(dataset_to_be_regressed)
data[, random := runif(.N)]
data <- data[(exposure == 1 & random > .9) | (exposure == 0 & random > .95),]
set.seed(565812)
data[, date := t0_min + round(1000*random)]
data[, random := NULL]
data[, t0 := NULL]
data[,concept_id := "N_STROKE_AESI"]
to_CDM <- rbind(to_CDM, data, fill = T)




# add to to_CDM

# split to_CDM across tables of CDM

# EVENTS

tab <- "EVENTS"

data <- copy(to_CDM)[concept_id %in% concept_ids_diagnosis,]
setnames(data, "date","start_date_record")
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), allow.cartesian = T)
data[,event_code := code]
data[,event_record_vocabulary := thiscodingsystemEVENTS]
data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))


# MEDICINES

tab <- "MEDICINES"

data <- copy(to_CDM)[concept_id %in% concept_ids_medicines,]
setnames(data, "date",thisdateMEDICINES)
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"))
data[,medicinal_product_atc_code := code]
# replicate bug https://github.com/target-roc19/case-study-09-tolvaptan/issues/37#issuecomment-4562315602
data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
data[, presc_quantity_per_day := as.character(presc_quantity_per_day)]
data[ , presc_quantity_per_day := "1"]
data[ 110:.N, presc_quantity_per_day := "1.5"]
# clean and save
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))

# PROCEDURES

tab <- "PROCEDURES"

data <- copy(to_CDM)[concept_id %in% concept_ids_procedures,]
setnames(data, "date", "procedure_date")
data <- merge(data, code_concept_id_codsys_DT, by = "concept_id", allow.cartesian = T)
data[, procedure_code := code]
data[, procedure_code_vocabulary := thiscodingsystemPROCEDURES]
data <- rbind(get(paste0(tab, "data_model")), data, fill = T)
tokeep <- names(get(paste0(tab, "data_model")))
data <- data[, ..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD", labversion),".csv") ))

# MEDICAL_OBSERVATIONS

tab <- "MEDICAL_OBSERVATIONS"

data <- copy(to_CDM)[concept_id %in% item_sets_of_our_study,]
setnames(data, "date","mo_date")
data <- merge(data,itemsets_sheet_CDM[cdm_table_name == tab,], by = c("concept_id"), allow.cartesian = T)
data[,mo_code := expected_value_1]
# # generate random value for so_source_value using the metadata
# set.seed(462313)
# data <- data[, random := runif(.N)]
path_lab_thresholds <- file.path(dirparlab,"LAB_threshold_with_unit.csv")
lab_thresholds <- fread(path_lab_thresholds)
data <- merge(data, lab_thresholds, by = "concept_id", all.x = T)
setnames(data, "unit_target","mo_unit")
# data[, mo_source_value := Min + random * (Max - Min)]
data[, mo_source_value := as.numeric(value) ]
# set.seed(463131)
# data <- data[, random := runif(.N)]
# data[, mo_source_value := mo_source_value + random * 2 - 1 ]
set.seed(143131)
data <- data[, random := runif(.N)]
data[random < .5,mo_unit := NA_character_]

data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))

# add creatinine

tab <- "MEDICAL_OBSERVATIONS"

data <- copy(dataset_to_be_regressed)
data <- unique(data[, .(person_id, t0)])
set.seed(452783)
data[, random := runif(.N)]
data <- data[random > .7,]
data[,concept_id := "LAB_CREATININE_SERUM"]
set.seed(132783)
data[, random := runif(.N)]
data[, date := as.Date(t0 - round(500 * random))]
setnames(data, "date","mo_date")
data <- merge(data,itemsets_sheet_CDM[cdm_table_name == tab,], by = c("concept_id"), allow.cartesian = T)
data[,mo_code := expected_value_1]
# # generate random value for so_source_value using the metadata
path_lab_thresholds <- file.path(dirparlab,"LAB_threshold_with_unit.csv")
lab_thresholds <- fread(path_lab_thresholds)
data <- merge(data, lab_thresholds, by = "concept_id", all.x = T)
setnames(data, "unit_target","mo_unit")
# set.seed(463131)
data <- data[, random := runif(.N)]
data[, mo_source_value := Min + random * (Max - Min)]
data[, mo_source_value := as.numeric(mo_source_value) ]
set.seed(463131)
data <- data[, random := runif(.N)]
data[, mo_source_value := mo_source_value + random * 2 - 1 ]
set.seed(143131)
data <- data[, random := runif(.N)]
data[random < .5,mo_unit := NA_character_]

data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"CREAT_SD",labversion),".csv") ))

# # VACCINES
# 
# tab <- "VACCINES"
# 
# data <- copy(to_CDM)[concept_id %in% concept_ids_vaccines,]
# setnames(data, "date",thisdateVACCINES)
# data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"))
# data[,vx_atc := code]
# # clean and save
# data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
# tokeep <- names(get(paste0(tab,"data_model")))
# data <- data[,..tokeep]
# date_cols <- sapply(data, is.Date)
# date_cols <- names(date_cols)[date_cols]
# data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
# fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))
# 

############################
# add visits

tab <- "VISIT_OCCURRENCE"

data <- copy(dataset_to_be_regressed)
# 
data[, visit_start_date := t0 ]
data[, meaning_of_visit := "primary_care" ]

# clean and save
data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD_EL",labversion),".csv") ))

