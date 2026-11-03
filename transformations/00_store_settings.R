############################################################
#                                                          #
####   CREATE SETTINGS ####
# 
#                                                          #
############################################################

# author: Rosa Gini

# v 1.1 7 Jun 2026

# fixed typo: SUBPOP was overwritten by SETTINGS

# v 1.0 23 Jan 2026


#########################################
# assign input and output directories

if (TEST){ 
  testname <- "test_01_SETTINGS"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdirinput_instance <- thisdirinput
}else{
  thisdirinput <- dirarchive
  thisdirinput_instance <- dirinput
  thisdiroutput <- dirpargen
}

#########################################
# load input datasets

SETTINGS <-    as.data.table(readxl::read_excel(file.path(thisdirinput, "PAR_SETTINGS.xlsx") ))
SUBPOP <-   as.data.table(readxl::read_excel(file.path(thisdirinput, "PAR_SUBPOP.xlsx") ))
CDM_SOURCE <- fread(file.path(thisdirinput_instance, "CDM_SOURCE.csv"))

#########################################

thisds <- CDM_SOURCE$data_source_name

# SETTINGS

SETTINGS <- SETTINGS[ds == thisds,]

if (nrow(SETTINGS) == 0) {
  alert <- "The name of the data source in CDM_SOURCE/data_source_name does not match the name of one of the data sources stored in PAR_SETTINGS.xlsx"
  stop(alert)
}

setnames(CDM_SOURCE,"date_creation","date_creation_CDM_instance")
setnames(CDM_SOURCE,"instance_number","instance_number_CDM_instance")


# SETTINGS[,date_creation:=ymd(date_creation)]

SETTINGS[,study_period_start_date:=ymd(study_period_start_date)][,study_period_end_date:=ymd(study_period_end_date)]

# SETTINGS[,subpop := strsplit(trimws(subpop), " +")]

CDM_SOURCE[, recommended_end_date := ymd(recommended_end_date)]
CDM_SOURCE[, date_creation_CDM_instance := ymd(date_creation_CDM_instance)]
CDM_SOURCE[, data_extraction_date := date_creation_CDM_instance]
CDM_SOURCE <- CDM_SOURCE[,.(recommended_end_date,date_creation_CDM_instance, instance_number_CDM_instance = as.character(instance_number_CDM_instance),data_extraction_date)]

SETTINGS <- cbind(SETTINGS,CDM_SOURCE)

# SUBPOP

SUBPOP <- SUBPOP[ds == thisds,]


#########################################
# save

outputfile <- SETTINGS

nameoutput <- "SETTINGS"
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))

outputfile <- SUBPOP

nameoutput <- "SUBPOP"
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))

