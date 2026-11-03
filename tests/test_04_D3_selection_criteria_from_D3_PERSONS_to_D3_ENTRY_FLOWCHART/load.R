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

listdatasets <- c("SETTINGS","SUBPOP","D3_PERSONS","D3_OBSPERIODS_PC_HOSP","D3_OBSPERIODS_PC")

# dates variables 

listdates <- list()

listdates[["D3_PERSONS"]] <- list("death_date")
listdates[["output_spells_category_meaning_set"]] <- list("entry_spell_category","exit_spell_category")
listdates[["D3_OBSPERIODS_PC_HOSP"]] <- list("entry_spell_category","exit_spell_category")
listdates[["D3_OBSPERIODS_PC"]] <- list("entry_spell_category","exit_spell_category")
listdates[["SETTINGS"]] <- c("study_period_start_date", "study_period_end_date", "recommended_end_date", "date_creation_CDM_instance", "data_extraction_date")


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
      data[, (datevar) := as.Date(baseline[[namedataset]][[datevar]]) + as.numeric(get(datevar))]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  saveRDS(data, file = file.path(thisdir,paste0(namedataset,".rds")))
}

