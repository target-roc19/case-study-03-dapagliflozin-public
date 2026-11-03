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

listdatasets_parquet <- c("DP_METFORMIN_dataset", "DP_SULFONYLUREA_dataset",  "DP_SULONAMIDE_dataset", "DP_ALPHAGLUCOINH_dataset", "DP_TZD_dataset", "DP_OTHERGLD_dataset", "DP_INSULIN_dataset", "DP_SGLT2I_dataset")

listdatasets <- c("SETTINGS","D3_STUDY_POPULATION", "D3_OUTCOMES", "D3_EXP_ACT_COV", "D3_EPISODES_STUDY_DRUGS","D3_ELIGIBILITY_EPISODE",listdatasets_parquet)

# dates variables 

listdates <- list()


listdates[["SETTINGS"]] <- c("study_period_start_date", "study_period_end_date", "recommended_end_date", "date_creation_CDM_instance", "data_extraction_date")


listdates[["D3_STUDY_POPULATION"]] <- c("entry_spell_category", "exit_spell_category", "t0", "death_date")


listdates[["D3_ELIGIBILITY_EPISODE"]] <- c("entry_spell_category", "exit_spell_category", "episode_start",	"episode_end")

listdates[["D3_OUTCOMES"]] <- c("t0", "outcome_date")

listdates[["D3_EPISODES_STUDY_DRUGS"]] <- c("episode_start", "episode_end")

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
