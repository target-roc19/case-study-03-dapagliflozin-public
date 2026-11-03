############################################################
#                                                          #
####   CREATE CDM_INSTANCE ####
# 
#                                                          #
############################################################

# author: Rosa Gini,

# v 0.1 9 Mar 2026

# generate synthetic CDM for TARGET-EU
# author: rosa gini

##############################

# input data model

dirinputCDM <- file.path(dirarchive,"i_CDM_data_model")

# version of this CDM

thisDEAP <- "AEMPS"
thisDS <- "BIFAP"
thiscodingsystemEVENTS <- "SCTSPA"
thiscodingsystemMEDICINES <- "ATC"
thiscodingsystemPROCEDURES <- c("NCMPNCSPNCRP")
thisdateMEDICINES <- "date_dispensing"

coding_system_list <- c(thiscodingsystemEVENTS,thiscodingsystemMEDICINES,thiscodingsystemPROCEDURES)
# itemsets_sheet_CDM <- itemsets_sheet_total[ds_name == thisDS,]

instance_version <- 0.1
version <- instance_version
labversion <- ""
dirversionCDM <- dirinput # paste0(dirCDM,"/v_",version)
dir.create(file.path(dirversionCDM), showWarnings = F)



# list if all concept_ids including events and medicines, excluding datasource-specific (TO ADD: procedures)

concept_id_list <- c(concept_ids_diagnosis, concept_ids_procedures,concept_ids_medicines)

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
}


# pick one code for each coding system for each concept

code_concept_id_codsys <- list()
code_concept_id_codsys_DT <- data.table()
for (conc in concept_id_list) {
  for (codsys in coding_system_list) {
    print(conc)
    print(codsys)
    # pick one short code representing the concept_id
    temp <- unique(event_codelist[concept_id == conc & coding_system == codsys & tags == "narrow",.(code)])
    temp2 <- unique(medicine_codelist[concept_id == conc & coding_system == codsys & tags == "narrow",.(code)])
    temp <- rbind(temp,temp2, fill = T)
    print(nrow(temp))
    if (nrow(temp) > 0) {
      temp[,len := str_length(code)]
      setorderv(temp, "len")
      chosen_code <- temp[1, code] 
      code_concept_id_codsys[[conc]][[codsys]] <- chosen_code
      temp <- temp[1,.(code)]
      temp[,concept_id := conc]
      temp[,coding_system := codsys]
      temp[,type_of_concept_id := concept_set_domains[[conc]]]
      code_concept_id_codsys_DT <- rbind(code_concept_id_codsys_DT,temp[1, ], fill = T)
    }else{
      code_concept_id_codsys[[conc]][[codsys]] <- "AAAAAAA"
    }
  }
}
# 
# # pick one strategy per each itemset
# 
# setorder(itemsets_sheet_CDM,cdm_table_name,concept_id)
# itemsets_sheet_CDM[, ord:= seq_len(.N),by = c("cdm_table_name","concept_id")]
# itemsets_sheet_CDM <- itemsets_sheet_CDM[ord == 1,] 
#   


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
               "METADATA" #,
               #"PRODUCTS"
               )


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

# for (tab in c("METADATA","INSTANCE","PRODUCTS")) {
for (tab in c("METADATA","INSTANCE")) {
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

thisdirtest <- file.path(thisdir,"..","..","case-study-05-asthma","tests")

testfolder <- "test_02_D4_EXP_ACT_COV_WEIGHT"
dataset_to_be_regressed <- readRDS(file.path(thisdirtest,testfolder,"D3_EXP_ACT_COV.rds"))

# PERSONS

tab <- "PERSONS"

data <- copy(dataset_to_be_regressed)[,.(person_id,t0,age_at_t0,sex)]
data <- data[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]

data[, sex_at_instance_creation := sex]

set.seed(154672)
data[, birth_date := t0 - round(365.25*age_at_t0) + round(365 * runif(.N))]
data[,year_of_birth := year(birth_date)]
data[,month_of_birth := month(birth_date)]
data[,day_of_birth := day(birth_date)]

set.seed(74628)
data <- data[, random := runif(.N)]
set.seed(14628)
data <- data[random < .002, death_date := t0 +  round(5* 365 * runif(.N))]
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
data[, op_start_date := lubridate::make_date(year_of_birth,month_of_birth,day_of_birth)]
set.seed(154623)
data[, op_start_date := op_start_date + round(365* 12 * runif(.N))]
data[, death_date := lubridate::make_date(year_of_death,month_of_death,day_of_death)]
data[, op_end_date := as.Date(4000 + baselinedate)]
data[, op_end_date := pmin(op_end_date, death_date, na.rm = T)]
data[, op_meaning := "HOSP"]
dataPC <- copy(data)[, op_start_date := lubridate::make_date(year_of_birth,month_of_birth,day_of_birth)]
data[, op_meaning := "PC"]
data <- rbind(data,dataPC)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))

# ##
# # COVARIATES
# 
# # create a dataset to_CDM where all covariate values are reported to a possible record of CDM
# 
# data <- copy(dataset_to_be_regressed)
# data <- data[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]
# 
# concept_id_cov_EVENTS <- intersect(unique(c(unlist(covariate_num_concept_id),unlist(covariate_365days_concept_id),unlist(covariate_ever_concept_id))),concept_ids_diagnosis)
# 
# 
# covariate_codebook_parameters <- rbindlist(list(
#   copy(covariate_365days_codebook_parameters)[, type := "365days"],
#   copy(covariate_ever_codebook_parameters)[, type := "ever"],
#   copy(covariate_num_codebook_parameters)[, type := "num"]
#   ),
#   use.names = T,
#   fill = T)
# setnames(covariate_codebook_parameters, "values","covariate")
# covariate_codebook_parameters <- covariate_codebook_parameters[,.(covariate,concept_id, type)]
# 
# id_cols  <- c("person_id", "t0")
# cov_cols <- c(covariate_365days,covariate_ever,covariate_num)
# tokeep <- c(id_cols,cov_cols)
# data[, (cov_cols) := lapply(.SD, as.double), .SDcols = cov_cols]
# 
# data <- data[,..tokeep]
# 
# to_CDM <- melt(
#   data,
#   id.vars = id_cols,
#   measure.vars = cov_cols,
#   variable.name = "covariate",
#   value.name = "value"
# )[value != 0]
# 
# to_CDM <- merge(to_CDM,covariate_codebook_parameters,by ="covariate", all.x = T)
# 
# 
# to_CDM[,value := pmin(fcoalesce(value, 5), 5)]
# to_expand <- copy(to_CDM)[ type == "num" & covariate != "number_of_hospital_days"  ,]
# 
# to_expand <- to_expand[rep.int(seq_len(nrow(to_expand)), as.integer(value))]
# 
# to_CDM <- rbind(copy(to_CDM)[ !(type == "num" & covariate != "number_of_hospital_days")  ,],to_expand)
# 
# set.seed(4673893)
# to_CDM[,random := runif(.N)]
# to_CDM <- to_CDM[type == "365days", date := t0 - round(365*random)]
# to_CDM <- to_CDM[type == "ever", date := t0 - round(12*365*random)]
# to_CDM <- to_CDM[covariate == "number_of_hospital_days", date := t0 - round(365*random)]
# to_CDM <- to_CDM[covariate == "number_of_hospital_days", end_date := date + value]
# 
# # split to_CDM across tables of CDM

# # EVENTS
# 
# tab <- "EVENTS"
# 
# data <- copy(to_CDM)[concept_id %in% concept_ids_diagnosis,]
# setnames(data, "date","start_date_record")
# data[,coding_system := thiscodingsystemEVENTS]
# data <- merge(data,code_concept_id_codsys_DT, by = c("coding_system","concept_id"))
# data[,event_code := code]
# data[,event_record_vocabulary := thiscodingsystemEVENTS]
# data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
# tokeep <- names(get(paste0(tab,"data_model")))
# data <- data[,..tokeep]
# date_cols <- sapply(data, is.Date)
# date_cols <- names(date_cols)[date_cols]
# data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
# fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))
# 
# 
# # MEDICINES
# 
# tab <- "MEDICINES"
# 
# data <- copy(to_CDM)[concept_id %in% concept_ids_medicines,]
# setnames(data, "date",thisdateMEDICINES)
# data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"))
# data[,medicinal_product_atc_code := code]
# data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
# tokeep <- names(get(paste0(tab,"data_model")))
# data <- data[,..tokeep]
# date_cols <- sapply(data, is.Date)
# date_cols <- names(date_cols)[date_cols]
# data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
# fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))
# 
# # VISIT_OCCURRENCE
# 
# tab <- "VISIT_OCCURRENCE"
# 
# data <- copy(to_CDM)[concept_id %in% item_sets_of_our_study,]
# setnames(data, c("date", "end_date"), c("visit_start_date","visit_end_date"))
# thisitemsets_sheet_CDM <- itemsets_sheet_CDM[cdm_table_name == tab,]
# tokeep <- c("concept_id",paste0("column_name_",1:max_n_columns_itemsets),paste0("expected_value_",1:max_n_columns_itemsets))
# thisitemsets_sheet_CDM <- thisitemsets_sheet_CDM[,..tokeep]
# data <- merge(data,thisitemsets_sheet_CDM, by = c("concept_id"))
# itemsetsnow <- unique(data$concept_id)
# data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
# for (conc in itemsetsnow) {
#   for (j in 1:max_n_columns_itemsets) {
#     n <- nrow(thisitemsets_sheet_CDM[concept_id == conc & ! is.na(get(paste0("column_name_",j))),])
#     if (n > 0) {
#       varname <- paste0("column_name_",j)
#       name_of_col <- unlist(thisitemsets_sheet_CDM[concept_id == conc,.(get(varname))])
#       print(name_of_col)
#       varname <- paste0("expected_value_",j)
#       expected_val <- unlist(thisitemsets_sheet_CDM[concept_id == conc,.(get(varname))])
#       print(expected_val)
#       data[, (name_of_col) := expected_val]
#     } 
#   }
# }
# tokeep <- names(get(paste0(tab,"data_model")))
# data <- data[,..tokeep]
# date_cols <- sapply(data, is.Date)
# date_cols <- names(date_cols)[date_cols]
# data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
# fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD",labversion),".csv") ))
# 



##
# EXPOSURE

# create a dataset to_CDM where both exposures values are reported to a possible record of CDM

data <- copy(dataset_to_be_regressed)
data <- data[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]
data[,concept_id := fifelse(exposure == 1,"DP_DDP4I","DP_DAPAGLIFLOZIN")]

tab <- "MEDICINES"

setnames(data, "t0",thisdateMEDICINES)
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), all = F)
data[,medicinal_product_atc_code := code]
if (thisDS == "FinReg") {
  input_med_id <- readRDS(file.path(dirpargen,"input_studydrugs_FinReg.rds"))
  setorder(input_med_id,medicinal_product_atc_code,medicinal_product_id)
  input_med_id[, ord := seq_len(.N), by = medicinal_product_atc_code]
  input_med_id <- input_med_id[ord == 1,]
  input_med_id <- input_med_id[,.(medicinal_product_atc_code,medicinal_product_id)]
  data <- merge(data, input_med_id, by = "medicinal_product_atc_code", all.x = T )
}
data[,disp_number_medicinal_product := 400]
data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
data <- data[1:floor(.N/2),]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD_EXP",labversion),".csv") ))

###########
# SELECTION


# create a dataset  where treatment that is a selection criterion is stored_in CDM

data <- copy(dataset_to_be_regressed)
data <- data[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]
data[,concept_id := "DP_ICS"]

tab <- "MEDICINES"

data[, (thisdateMEDICINES) := t0 - 100]
data <- merge(data,code_concept_id_codsys_DT, by = c("concept_id"), all = F)
data[,medicinal_product_atc_code := code]
data[,disp_number_medicinal_product := 400]
data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
tokeep <- names(get(paste0(tab,"data_model")))
data <- data[,..tokeep]
date_cols <- sapply(data, is.Date)
date_cols <- names(date_cols)[date_cols]
data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD_SEL",labversion),".csv") ))

# create a dataset  where diagnosis that is a selection criterion is stored_in CDM

data <- copy(dataset_to_be_regressed)
data <- data[!is.na(person_id),person_id := paste0("SD_",testfolder,"_",as.character(person_id))]
data[,concept_id := "R_ASTHMA_COV_dataset"]
# 
# tab <- "EVENTS"
# 
# data <- copy(to_CDM)[concept_id %in% concept_ids_diagnosis,]
# data[, start_date_record := t0 - 100]
# data[,coding_system := thiscodingsystemEVENTS]
# data <- merge(data,code_concept_id_codsys_DT, by = c("coding_system","concept_id"), all = F)
# data[,event_code := code]
# data[,event_record_vocabulary := fifelse(thiscodingsystemEVENTS %in% c("ICD10_2019","ICD10"),"ICD10",thiscodingsystemEVENTS)]
# data <- rbind(get(paste0(tab,"data_model")),data,fill =T)
# tokeep <- names(get(paste0(tab,"data_model")))
# data <- data[,..tokeep]
# date_cols <- sapply(data, is.Date)
# date_cols <- names(date_cols)[date_cols]
# data[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]
# fwrite(data, file.path(dirversionCDM, paste0(paste0(tab,"_SD_SEL",labversion),".csv") ))


