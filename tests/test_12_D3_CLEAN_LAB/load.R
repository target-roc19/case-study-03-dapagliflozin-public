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
if (!require("S7")) install.packages("S7")
if (!require("polars")) {
  Sys.setenv(NOT_CRAN = "true")
  install.packages("polars", repos = "https://community.r-multiverse.org")
  Sys.setenv(NOT_CRAN = "")
}

baselinedate <- ymd(20121231)

# list of datasets

listdatasets <- c("SETTINGS","LAB_EGFR_dataset","LAB_CREATININE_SERUM_dataset", "LAB_HBA1C_dataset", "D3_ENTRY_FLOWCHART")

listdatasets_parquet <- c("LAB_EGFR_dataset","LAB_CREATININE_SERUM_dataset", "LAB_HBA1C_dataset")

# dates variables 

listdates <- list()


listdates[["SETTINGS"]] <- c("study_period_start_date", "study_period_end_date", "recommended_end_date", "date_creation_CDM_instance", "data_extraction_date")

listdates[["D3_ENTRY_FLOWCHART"]] <- c("entry_spell_category","exit_spell_category")

for (dataset in listdatasets_parquet) {
  listdates[[dataset]] <- c("date")
}



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
  if (namedataset %in% listdatasets_parquet) {
    data <- polars::as_polars_df(data)$write_parquet(paste0(namedataset,".parquet"))
  }else{
    saveRDS(data, file = file.path(thisdir,paste0(namedataset,".rds")))
  }
}

