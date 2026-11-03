thisstudy <- "case3"

# set directory with input data based in DEAP input, or to default is this is on synthetic data

if (!TEST) {
  if (REDUCTION) {
    dirinput <- ifelse(dirinput == "", file.path(thisdir,"..","data","D2_cdm","large_cdm"))
  }else{
    dirinput <- ifelse(dirinput == "", file.path(thisdir,"..","data","D2_cdm"), dirinput)
  }
  
}

# datasource name used during development

TEST_DS <- "CPRD"

# dir where METADATA are saved

if (TEST) {
  dircheckmetadata <- file.path(thisdir, "..","..", "codelist-creation", "METADATA")
} else {
  dircheckmetadata <- file.path(thisdir, "p_parameters", "archive_parameters", "METADATA")
}


# dirtestoutput <- file.path(thisdir, "g_test")
# dir.create(dirtestoutput, showWarnings = F)


# load packages
read_library <- function(...) {
  x <- c(...)
  invisible(lapply(x, library, character.only = TRUE))
}

list.of.packages <- c("nanoparquet","MASS", "haven", "tidyverse", "lubridate", "AdhereR", "stringr", "purrr", "readr", "dplyr",
                      "survival", "rmarkdown", "ggplot2", "data.table", "qpdf", "parallel", "readxl", "gtsummary",
                      "labelled", "huxtable", "metafor", "markdown", "R.utils", "RcppAlgos", "qs2","readxl","Hmisc","ggblend","officer","kableExtra","writexl","cobalt", "polars","DiagrammeR","DiagrammeRsvg","rsvg", "remotes", "survey","survminer","flexsurv", "splines", "arrow","future", "future.apply","flextable","pagedown","glue", "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if ("polars" %in% new.packages) {
  Sys.setenv(NOT_CRAN = "true") # Enable installation with pre-built Rust library binary, or enable Rust caching
  install.packages("polars", repos = "https://community.r-multiverse.org")
  Sys.setenv(NOT_CRAN = "false")
}

invisible(lapply(list.of.packages, require, character.only = T))
rm(read_library, new.packages, list.of.packages)


# set other directories


set_and_create_dir <- function(x) {
  x <- file.path(thisdir,".." ,x)
  dir.create(paste0(x), showWarnings = F)
  return(x)
}

diroutput <- set_and_create_dir("/data/")
dirtemp <- set_and_create_dir("/data/")
# dirconceptsets <- set_and_create_dir("/data/conceptset_datasets/")
# diritemsets <- set_and_create_dir("/data/itemset_datasets/")
direxp <- set_and_create_dir("data/g_export/")
dirparameters <- set_and_create_dir("data/g_parameters/")
dirmacro <- set_and_create_dir("run/p_macro/")
dirarchive <- set_and_create_dir("run/p_parameters/archive_parameters/")
dirparlab <- set_and_create_dir("run/p_parameters/archive_parameters/parameters_lab_values/")
dirreducedCDM <- file.path(thisdir,"..","data","D2_cdm")
dirtest <- file.path(thisdir, "..", "tests")
dirparchoice <- file.path(thisdir, "i_choice")

dirpargen <- dirparameters
dircodelist <- dirarchive

rm(set_and_create_dir)

# install package ClenLabValues

cleanlab_path <- file.path(thisdir,"p_macro","CleanLabValues")
cleanlab_path <- normalizePath(cleanlab_path, mustWork = FALSE)
#if (!requireNamespace("CleanLabValues", quietly = TRUE)) {
  install.packages(cleanlab_path, repos = NULL, type = "source")
#}
library(CleanLabValues)
library(data.table)
#
#
# # Install package GenerateMatchedDataset
# if (any(grepl("package:matchboots", search()))) detach("package:matchboots", unload = TRUE)
# safe_github_install <- function(x) {
#   tryCatch(
#     #this is the chunk of code we want to run
#     {
#       remotes::install_github("ARS-toscana/GenerateMatchedDataset")
#       #when it throws an error, the following block catches the error
#     }, error = function(msg){
#       if (!("matchboots" %in% installed.packages()[,"Package"])) {
#         stop(msg)
#       } else {
#         warning(paste("Error in installing the package but continuing since a version of it is already installed.
#               Original message:", msg))
#       }
#     }
#   )
# }
# safe_github_install("ARS-toscana/GenerateMatchedDataset")
# library(matchboots)

# load macros
source(file.path(dirmacro,"CreateConceptSetDatasets_polars.R"))
source(file.path(dirmacro,"CreateItemsetDatasets_v2.2.R"))
source(file.path(dirmacro,"MergeFilterAndCollapse_v5.R"))
source(file.path(dirmacro,"CreateSpells_v15.R"))
source(file.path(dirmacro,"CreateFlowChart.R"))
source(file.path(dirmacro,"CountPersonTimeV14.0.R"))
source(file.path(dirmacro,"df_to_list_of_list.R"))
source(file.path(dirmacro,"list_of_list_to_df.R"))
source(file.path(dirmacro,"dsr.R"))
source(file.path(dirmacro,"launch_step.R"))
source(file.path(dirmacro,"CompareListsOfCodes.R"))
source(file.path(dirmacro,"CountPrevalence.R"))
source(file.path(dirmacro,"Cube.R"))
source(file.path(dirmacro, "GenerateTDDataset_v0.921.R"))


# #############################################
# #SAVE METADATA TO direxp
# #############################################

if (!TEST & localprocessing == T) {
  file.copy(file.path(dirinput,'/METADATA.csv'), direxp, overwrite = T)
  file.copy(file.path(dirinput,'/CDM_SOURCE.csv'), direxp, overwrite = T)
  file.copy(file.path(dirinput,'/INSTANCE.csv'), direxp, overwrite = T)
}


#############################################
#SAVE to_run.R TO direxp
#############################################

if (!TEST & localprocessing == T) {
  file.copy(file.path(thisdir,'/to_run.R'), direxp, overwrite = T)
}

##%######################################################%##
#                                                          #
####    Extract name of csv files matching a string     ####
#                                                          #
##%######################################################%##

extract_name_csv <- function(directory, name_to_search) {
  file_list <- list.files(directory)
  file_list <- file_list[grepl(paste0("^", name_to_search, ".*csv$"), file_list)]
  return(sub('\\.csv$', '', file_list))
}

##%######################################################%##
#                                                          #
####          Load all CDM tables from a list           ####
#                                                          #
##%######################################################%##

load_CDM_tables <- function(dir_location, variables) {
  data.table::rbindlist(lapply(variables,
                               function(x) suppressWarnings(fread(file.path(dir_location, paste0(x, ".csv")),
                                                                  colClasses = list(character = "person_id")))))
}
  #############################################
  #SAVE to_run.R TO direxp
  #############################################
  
  if (!TEST) {
    file.copy(file.path(thisdir,'/to_run.R'), direxp, overwrite = T)
  }
  
  ##%######################################################%##
  #                                                          #
  ####    Extract name of csv files matching a string     ####
  #                                                          #
  ##%######################################################%##
  
  extract_name_csv <- function(directory, name_to_search) {
    file_list <- list.files(directory)
    file_list <- file_list[grepl(paste0("^", name_to_search, ".*csv$"), file_list)]
    return(sub('\\.csv$', '', file_list))
  }
  
  ##%######################################################%##
  #                                                          #
  ####          Load all CDM tables from a list           ####
  #                                                          #
  ##%######################################################%##
  
  load_CDM_tables <- function(dir_location, variables) {
    data.table::rbindlist(lapply(variables,
                                 function(x) suppressWarnings(fread(file.path(dir_location, paste0(x, ".csv")),
                                                                    colClasses = list(character = "person_id")))))
  }
  
  ##%######################################################%##
  #                                                          #
  ####              Extract vector from cell              ####
  #                                                          #
  ##%######################################################%##
  
  extract_vector_in_cell <- function(string_to_split) {
    splitted_cleaned_names <- unique(strsplit(trimws(string_to_split), " +"))
    return(unlist(splitted_cleaned_names))
  }
  
  ##%######################################################%##
  #                                                          #
  ####                ULM formula for CIs                 ####
  #                                                          #
  ##%######################################################%##
  
  # Reference at Ulm, K. A Simple Method to Calculate the Confidence Interval of a Standardized Mortality Ratio (SMR). Am. J. Epidemiol. 1990, 131 (2), 373–375.
  exactPoiCI <- function (df, X, PT, conf.level = 0.95, conversion_factor = 365.25, OoM = 3) {
    alpha <- 1 - conf.level
    IR <- df[, get(X)]
    upper <- df[, 0.5 * qchisq((1-(alpha/2)), 2*(get(X)+1))]
    lower <- df[, 0.5 * qchisq(alpha/2, 2*get(X))]
    temp_list <- lapply(list(IR, lower, upper), `/`, df[, get(PT)/conversion_factor])
    temp_list <- lapply(temp_list, `*`, (10 ^ OoM))
    temp_list <- lapply(temp_list, function(x) {fifelse(x == Inf, 0, x)})
    return(lapply(temp_list, round, 1))
  }