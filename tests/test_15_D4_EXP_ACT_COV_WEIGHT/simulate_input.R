##############################################
# simulate input for D4_EXP_ACT_COV_WEIGHT

# author: Rosa Gini, 

# v 1 4 Jun 2026

##################################################
# HEADER

rm(list = ls(all.names = TRUE))

#set the directory where the script is saved as the working directory

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

#######################################
# PARAMETERS

# study period

library(readxl)
SETTINGS <- read_excel("SETTINGS.xlsx")
study_start_date <- ymd(SETTINGS$study_period_start_date)
study_end_date <- ymd(SETTINGS$study_period_end_date)

# set # of persons

Npersons <- 5000


# set prob of exposed

probexposure <- 0.3

# input metadata

thisdirarchive <- file.path(thisdir, "..", "..", "run", "p_parameters", "archive_parameters")
thisdirparlab <- file.path(thisdirarchive,"parameters_lab_values")


# list covariates per type

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D5_Table_2_baseline_cohort.xlsx")),1)))

thiscovariates_ord <- unlist(parameters_this_step[parameter == "covar_ord",.(values)])
thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)])
thiscovariates_lab <- unlist(parameters_this_step[parameter == "covar_lab",.(values)])

covariates_binary <- paste0(thiscovariates_ord,"_at_t0")

covariates_lab <- paste0(thiscovariates_lab,"_at_t0")

covariates_cat <- paste0(thiscovariates_cat,"_at_t0")

# ingredients covariates

ingredients_covariates <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D3_COVARIATES_SIMPLE_TD.xlsx")),1)))

threshold_lab <- fread(file.path(thisdirparlab, "LAB_threshold_with_unit.csv"))



#######################################
# DATA GENERATION

# all variables

list_variables <- c("person_id", "t0",  "birth_year", "age_at_t0", "entry_spell_category", "exit_spell_category", "exposure", covariates_binary, covariates_lab, covariates_cat) # person_id 


#############
# create base 
data <- data.table::data.table(person_id = 1:Npersons)

# person_id 

data[,person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, nchar(person_id)))]

##################
# t0

set.seed(1351)
data[, t0 := as.Date(floor(runif(.N, min = study_start_date, max = study_end_date + 1)))]

# sex_at_instance_creation

set.seed(12499)
data[, sex := sample(1:3, Npersons, replace = TRUE, prob = c(.5,.49,.01))]
# data[,sex := fifelse(sex_at_instance_creation == 1,"M","F") ]
data[, gender_at_t0 := c("M", "F", "O")[sex]]
data[, sex := NULL]

# birth_year

set.seed(523634)
birth_year_list <- c(1925,1935,1945,1955,1965,1975,1985,1995,2005,2015)
probyear = c(.01,.02,.2,.05,.2,.3,.05,.1, 0.05)
totprob = sum(probyear)
probyear = c(probyear, 1 - totprob)
data[, birth_year := sample(birth_year_list, Npersons, replace = TRUE, prob = probyear)]

# age

data[, age_at_t0 := as.integer(year(t0) - birth_year)]


# entry and exit_spell

set.seed(54632)
data[, random := round(runif(.N, min = 1, max = t0 - 365 - dmy(paste0("0101",as.character(birth_year)))))]
data[, entry_spell_category  := as.Date( t0 - random)]
data[, t0 := pmax(t0, dmy(paste0("0101",as.character(birth_year))))]
set.seed(14632)
data[, random := round(rpois(.N, lambda = 5000))]
data[, exit_spell_category := pmin(t0 + random, study_end_date)]
data[,random := NULL]

##################
# exposure

set.seed(1252)
data[, exposure := rbinom(.N, size = 1, prob = probexposure)]

##################
# covariates at t0

#binary

j <- 1
for (cb in covariates_binary) {
  seedchange <- 1111 + j
  set.seed(seedchange)
  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  seedchange <- 112 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[exposure == 1 & random > .95, cov := 1]
  setnames(data,"cov",cb)
  j <- j + 1
}

# lab

for (cb in covariates_lab) {
  seedchange <- 1131 + j
  set.seed(seedchange)
  labname <- gsub("_at_t0", "", cb)
  print(labname)
  ingredient <- unlist(ingredients_covariates[varname == labname,.(ingredient)])
  temp <- threshold_lab[concept_id == ingredient,][1]
  Minlab <- temp$Min - 1
  Maxlab <- temp$Max + 1
  data[, cov := runif(.N, min = Minlab, max = Maxlab)]
  seedchange <- 1123 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[random > .5, cov := NA_real_]
  setnames(data,"cov",cb)
  j <- j + 1
}


for (cb in setdiff(covariates_cat, "gender_at_t0")) {
  seedchange <- 1131 + j
  set.seed(seedchange)
  labname <- gsub("_at_t0", "", cb)
  print(labname)
  varname <- paste0(labname,"value")
  thisvalues <-as.character( unlist(parameters_this_step[parameter == varname,.(values)]))
  data[, cov := sample(thisvalues, .N, replace = T)]
  seedchange <- 1135 + j
  set.seed(seedchange)
  data[, random := runif(.N)]
  data[random > .5, cov := NA_real_]
  has_missing <- unlist(parameters_this_step[values == labname,.(has_missing)][1])
  if (has_missing == 0) {
    data[is.na(cov), cov := "0"]
  }
  setnames(data,"cov",cb)
}

# keep variables

data <- data[, ..list_variables]



# save

namedataset <- "D3_EXP_ACT_COV"

saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))


