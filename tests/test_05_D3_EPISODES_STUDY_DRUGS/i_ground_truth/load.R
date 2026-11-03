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
if (!require("polars")) {
  Sys.setenv(NOT_CRAN = "true")
  install.packages("polars", repos = "https://community.r-multiverse.org")
  Sys.setenv(NOT_CRAN = "")
}

baselinedate <- ymd(20161231)

# list of datasets

listdatasets <- c("D3_EPISODES_STUDY_DRUGS")

listdatasets_parquet <- c()

# dates variables 

listdates <- list()


listdates[["D3_EPISODES_STUDY_DRUGS"]] <- c("episode_start", "episode_end")


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

