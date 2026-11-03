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

listdatasets <- c("SETTINGS",
                  "D3_EPISODES_STUDY_DRUGS",
                  "D3_ENTRY_FLOWCHART",
                  "D3_PERSONS",
                  "D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART",
                  "DP_DAPAGLIFLOZIN_dataset", "DP_DDP4I_dataset","E_T2DM_COV_dataset","E_T1DM_COV_dataset", 
                  "C_IHD_COV_dataset","V_ICVD_COV_dataset","V_PAD_COV_dataset", 
                  "E_HYPERLIPIDAEMIA_COV_dataset","DP_LIPIDLOWERING_dataset", 
                  "V_HYPERTENSION_COV_dataset","DP_ANTIHYPERTENSIVE_dataset","L_CURRENTSMOKER_COV_dataset", 
                  "DP_SGLT2I_dataset", "N_STROKE_AESI_dataset", "C_UNSTANGINA_AESI_dataset", "C_AMI_AESI_dataset") 

listdatasets_parquet <- c("DP_DAPAGLIFLOZIN_dataset", "DP_DDP4I_dataset","E_T2DM_COV_dataset","E_T1DM_COV_dataset", 
                          "C_IHD_COV_dataset","V_ICVD_COV_dataset","V_PAD_COV_dataset", 
                          "E_HYPERLIPIDAEMIA_COV_dataset","DP_LIPIDLOWERING_dataset", 
                          "V_HYPERTENSION_COV_dataset","DP_ANTIHYPERTENSIVE_dataset","L_CURRENTSMOKER_COV_dataset", 
                          "DP_SGLT2I_dataset", "N_STROKE_AESI_dataset", "C_UNSTANGINA_AESI_dataset", "C_AMI_AESI_dataset")


# dates variables 

listdates <- list()
listdates[["D3_ENTRY_FLOWCHART"]] <- list("entry_spell_category","exit_spell_category")
listdates[["D3_EPISODES_STUDY_DRUGS"]] <- list("episode_start","episode_end")
listdates[["D3_PERSONS"]] <- list("death_date")
listdates[["D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART"]] <- list("entry_spell_category","exit_spell_category")
listdates[["SETTINGS"]] <- list("study_period_start_date","study_period_end_date","recommended_end_date")

listdates[["N_STROKE_AESI_dataset"]] <- c("date","end_date_record")
listdates[["C_UNSTANGINA_AESI_dataset"]] <- c("date","end_date_record")
listdates[["C_AMI_AESI_dataset"]] <- c("date","end_date_record")
listdates[["DP_DAPAGLIFLOZIN_dataset"]] <- c("date","date_prescription")
listdates[["DP_DDP4I_dataset"]] <- c("date","date_prescription")
listdates[["E_T2DM_COV_dataset"]] <- c("date","end_date_record")
listdates[["E_T1DM_COV_dataset"]] <- c("date","end_date_record")
listdates[["C_IHD_COV_dataset"]] <- c("date","end_date_record")
listdates[["V_ICVD_COV_dataset"]] <- c("date","end_date_record")
listdates[["V_PAD_COV_dataset"]] <- c("date","end_date_record")
listdates[["E_HYPERLIPIDAEMIA_COV_dataset"]] <- c("date","end_date_record")
listdates[["DP_LIPIDLOWERING_dataset"]] <- c("date","date_prescription")
listdates[["V_HYPERTENSION_COV_dataset"]] <- c("date","end_date_record")
listdates[["DP_ANTIHYPERTENSIVE_dataset"]] <- c("date","date_prescription")
listdates[["L_CURRENTSMOKER_COV_dataset"]] <- c("date","end_date_record")
listdates[["DP_SGLT2I_dataset"]] <- c("date","date_prescription")



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
    polars::as_polars_df(data)$write_parquet(file.path(thisdir,paste0(namedataset,".parquet")))
  }else{
    saveRDS(data, file = file.path(thisdir,paste0(namedataset,".rds")))
  }
}

