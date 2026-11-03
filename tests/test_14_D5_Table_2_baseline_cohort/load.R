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

listdatasets <- c("SETTINGS","D3_EXP_ACT_COV")

listdatasets_parquet <- c()

# dates variables 

listdates <- list()


listdates[["SETTINGS"]] <- c("study_period_start_date", "study_period_end_date", "recommended_end_date", "date_creation_CDM_instance", "data_extraction_date")


listdates[["D3_STUDY_POPULATION"]] <- c("entry_spell_category", "exit_spell_category", "t0")

for (dataset in c("D3_Obesity_TD", "D3_Tobacco_use_TD", "D3_CKD_TD", "D3_HbA1c_TD")) {
  listdates[[dataset]] <- c("start_look_back", "exit_spell_category", "start_d", "end_d")
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
  print(namedataset)
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
