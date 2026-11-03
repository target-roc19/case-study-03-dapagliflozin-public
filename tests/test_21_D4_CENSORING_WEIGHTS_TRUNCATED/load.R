rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

baselinedate <- ymd(20161231)

# list of datasets
D3_covariate_TD<-c("D3_ACE_inhibitors_TD","D3_Dialysis_TD","D3_Aspirin_TD")

listdatasets <- c("SETTINGS","D4_EXP_ACT_COV_WEIGHT_TRUNCATED","D4_FUP",D3_covariate_TD)  #,"D3_OUTCOMES"

# dates variables 

listdates <- list()
listdates[["SETTINGS"]] <- c("study_period_start_date", "study_period_end_date", "recommended_end_date", "date_creation_CDM_instance", "data_extraction_date")


listdates[["D4_EXP_ACT_COV_WEIGHT_TRUNCATED"]] <- list("t0")
listdates[["D4_FUP"]] <- list( "t0","censoring_date", "date_intercurring_events") 
listdates[["D3_ACE_inhibitors_TD"]] <- list("t0","start_look_back","end_observation_period","start_d","end_d")
listdates[["D3_Dialysis_TD"]] <- list("t0","start_look_back","end_observation_period","start_d","end_d")
listdates[["D3_Aspirin_TD"]] <- list("t0","start_look_back","end_observation_period","start_d","end_d")

# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- as.Date(lubridate::ymd(baselinedate))
  }
}


# load datasets


for (namedataset in listdatasets){
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  for (datevar in listdates[[namedataset]]) {
    if (!is.null(baseline[[namedataset]][[datevar]])){
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }

  assign(namedataset,data)
  saveRDS(data, file = file.path(thisdir,paste0(namedataset,".rds")))
}

