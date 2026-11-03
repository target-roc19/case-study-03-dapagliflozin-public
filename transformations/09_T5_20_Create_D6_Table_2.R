############################################################
#                                                          #
####         D6_Table_2_baseline_cohort                  ####
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.2
# categorical age added

print('CREATE D6_Table_2_baseline_cohort')

#########################################
# assign directories

if (TEST){ 
  testname <- "test_18_D6_Table_2"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  thisdirsettings <- thisdirinput
  settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  thisdatasources_for_postprocessing <- settings$ds
  thisdatasource <- settings$ds
  thisdirarchive <- thisdirinput
  thisfolder_submission <- list()
  thisfolder_submission[[thisdatasource]] <- thisdirinput
}else{
  if (localprocessing){
    thisdirinput <- direxp
    thisdiroutput <- direxp
    thisdirsettings <- dirpargen
    settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
    thisdatasources_for_postprocessing <- settings$ds
    thisdirarchive <- dirarchive
    thisfolder_submission <- list()
    thisfolder_submission[[thisdatasource]] <- thisdirinput
  }else{
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- paste0(direxp,"Formatted tables/")
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
    thisdirsettings <- dirpargen
  }
}

#########################################
# load

if (localprocessing) {
  settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
  thissubpopulations <- unique(settings[, .(subpop)])
  thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
}

# from parameter settings: create the list of subpopulations of all the datasources

PARSETTINGS <- as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx")))

thissubpopulations_ds <- list()

for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds, .(subpop)]
  thissubpopulations_ds[[thisds]] <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
}

PARPOP <- as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx")))

# parameters of this step

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive, "codebooks", "D6_Table_2_baseline_cohort.xlsx"), 1)))

parameter_list <- unname(unlist(unique(parameters_this_step[, .(parameter)])))

for (par in parameter_list) {
  temp <- unname(unlist(unique(parameters_this_step[parameter == par, .(get("values"))])))
  assign(par, temp)
}

#########################################

print(paste("Now creating: D6_Table_2_baseline_cohort"))

# create the list list_ds_analysed of all datasources including their subpopulations

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {
  
  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all", "", paste0("_", thissubpop))
    
    filetoread <- file.path(thisfolder_submission[[thisds]], paste0("D5_Table_2_baseline_cohort_masked", thissuffix, ".csv"))
    
    if (file.exists(filetoread)) {
      
      temp <- fread(filetoread, colClasses = "character")
      
      suffix <- paste0("_", thisds, thissuffix)
      
      name_suffix[[suffix]] <- fifelse(
        unlist(PARPOP[ds == thisds & subpop == thissubpop, .(name_in_tables)]) == "",
        suffix,
        unlist(PARPOP[ds == thisds & subpop == thissubpop, .(name_in_tables)])
      )
      
      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)
      
    }else{
      
      errmess <- paste("File", filetoread, "does not exist in the export folder of", thisds)
      stop(errmess)
    }
  }
}

##################
# HELPERS

add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_", j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar, "N_")
  varP <- paste0(covar, "perc_")
  tab_nice[, cell := paste0(get(varN), " (", get(varP), "%)")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  return(j)
}

descriptive_lab_values <- function(j, covar){
  j <- j + 1
  varM <- paste0(covar, "_mean_")
  varSD <- paste0(covar, "_sd_")
  tab_nice[, cell := paste0(get(varM), " (", get(varSD), ")")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  
  j <- j + 1
  varM <- paste0(covar, "_median_")
  varQ1 <- paste0(covar, "_q1_")
  varQ3 <- paste0(covar, "_q3_")
  tab_nice[, cell := paste0(get(varM), " (", get(varQ1), "-", get(varQ3), ")")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  
  j <- j + 1
  varN <- paste0(covar, "_missingN_")
  varP <- paste0(covar, "_missingperc_")
  tab_nice[, cell := paste0(get(varN), " (", get(varP), "%)")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  
  return(j)
}

descriptive_continuous <- function(j, covar){
  j <- j + 1
  varM <- paste0(covar,"_mean_")
  varSD <- paste0(covar,"_sd_")
  tab_nice[stratum != "asd", cell := paste0(get(varM), " (",get(varSD),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[stratum != "asd", cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  
  return(j)
}

##########################################
# WRITE TABLE(S)

#########################################
# POPULATE ROWS

tab_nice <- data.table()

for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  tab_nice <- rbind(tab_nice, temp, fill = T)
}

# row 0

row_header_1 <- c("Datasource")
j <- 0

tab_nice[, cell := name_ds_subpop]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 1

row_header_1 <- c(row_header_1, "")

j <- j + 1

tab_nice[stratum == "overall", cell := "Overall"]
tab_nice[stratum == "exp0", cell := "Dpp4-i"]
tab_nice[stratum == "exp1", cell := "Dapagliflozin"]

setnames(tab_nice, "cell", paste0("cell_", j))

# row 2

row_header_1 <- c(row_header_1, "N")

j <- j + 1

tab_nice[, cell := N_]

setnames(tab_nice, "cell", paste0("cell_", j))

# characteristics header

row_header_1 <- c(row_header_1, "Characteristics")

j <- add_empty_row(j)

#########################################
# AGE - cont

row_header_1 <- c(
  row_header_1,
  "Age (years)",
  "  Mean (SD)",
  "   Median (IQ Range)"
)

j <- add_empty_row(j)

j <- descriptive_continuous(j, "age")


#########################################
# AGE - categorical

row_header_1 <- c(
  row_header_1,
  "Age (years) N (%)",
  "   <65",
  "   65-74",
  "   75-84",
  "   >=85"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "age_cat_<65_")
j <- descriptive_N_perc(j, "age_cat_65-74_")
j <- descriptive_N_perc(j, "age_cat_75-84_")
j <- descriptive_N_perc(j, "age_cat_>=85_")

#########################################
# GENDER

row_header_1 <- c(
  row_header_1,
  "Gender N (%)",
  "   Male",
  "   Female",
  "   Other"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "gender_M_")
j <- descriptive_N_perc(j, "gender_F_")
j <- descriptive_N_perc(j, "gender_O_")

#########################################
# CALENDAR YEAR

row_header_1 <- c(row_header_1, "Calendar year at t0")

j <- add_empty_row(j)

for (year in 2013:2025) {
  row_header_1 <- c(row_header_1, year)
  j <- descriptive_N_perc(j, paste0("calendaryear_", year, "_"))
}

#########################################
# DURATION OF DIABETES

row_header_1 <- c(
  row_header_1,
  "Duration of diabetes",
  "Mean (SD)",
  "Median (IQ range)"
)

j <- add_empty_row(j)

j <- j + 1
tab_nice[, cell := paste0(yrsdiab_mean_, " (", yrsdiab_sd_, ")")]
setnames(tab_nice, "cell", paste0("cell_", j))

j <- j + 1
varM <- "yrsdiab_median_"
varQ1 <- "yrsdiab_q1_"
varQ3 <- "yrsdiab_q3_"

tab_nice[, cell := paste0(get(varM), " (", get(varQ1), "-", get(varQ3), ")")]
setnames(tab_nice, "cell", paste0("cell_", j))

#########################################
# MISSING DATA PRE-IMPUTATION

row_header_1 <- c(
  row_header_1,
  "Missing data pre-imputation",
  "HbA1c",
  "Mean (SD)",
  "Median (IQ range)",
  "Missing N (%)",
  "eGFR",
  "Mean (SD)",
  "Median (IQ range)",
  "Missing N (%)"
)

j <- add_empty_row(j)

j <- add_empty_row(j)
j <- descriptive_lab_values(j, "HbA1c")

j <- add_empty_row(j)
j <- descriptive_lab_values(j, "EGFR")

#########################################
# TOBACCO USE

row_header_1 <- c(
  row_header_1,
  "Tobacco use",
  "None",
  "Past",
  "Current",
  "missing N (%)"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "Smoking_0_")
j <- descriptive_N_perc(j, "Smoking_1_")
j <- descriptive_N_perc(j, "Smoking_2_")
j <- descriptive_N_perc(j, "Smoking_missing")

#########################################
# (CO)MORBIDITIES

row_header_1 <- c(row_header_1, "(Co)Morbidities")

j <- add_empty_row(j)

for (cov in covar_comorb) {
  name_cov <- unlist(parameters_this_step[values == cov, .(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov, "_"))
}

#########################################
# COMEDICATIONS

row_header_1 <- c(row_header_1, "Comedications")

j <- add_empty_row(j)

for (cov in covar_comed) {
  name_cov <- unlist(parameters_this_step[values == cov, .(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov, "_"))
}

#########################################
# ESTABLISHED ACVD

row_header_1 <- c(row_header_1, "Established ACVD")

j <- add_empty_row(j)

for (cov in covar_est_ACVD) {
  name_cov <- unlist(parameters_this_step[values == cov, .(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov, "_"))
}

#########################################
# ACVD RISK FACTORS

row_header_1 <- c(row_header_1, "ACVD Risk factors")

j <- add_empty_row(j)

for (cov in covar_rf_ACVD) {
  name_cov <- unlist(parameters_this_step[values == cov, .(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov, "_"))
}

#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)

tokeep <- c("ds", "stratum", cell_cols)

tab_nice <- tab_nice[, ..tokeep]

#########################################
# RESHAPE CELLS

tab_nice <- melt(
  tab_nice,
  id.vars = c("ds", "stratum"),
  measure.vars = patterns(cell = "^cell_[0-9]+$"),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, ds, stratum)

tab_nice <- dcast(
  tab_nice,
  rownum ~ ds + stratum,
  value.var = "value"
)

setorder(tab_nice, rownum)

#########################################
# ORDER COLUMNS

ordered_ds <- substring(list_ds_analysed, 2)

ordered_cols <- c(
  "rownum",
  as.vector(rbind(paste0(ordered_ds, c("_overall", "_exp1", "_exp0"))))
)

setcolorder(tab_nice, ordered_cols)

#########################################
# ADD ROW HEADER

tab_nice[, row_header := row_header_1]

tab_nice[, rownum := NULL]

#########################################
# FINAL COLUMNS

colorder <- c("row_header", setdiff(names(tab_nice), "row_header"))

tab_nice <- tab_nice[, ..colorder]

#########################################
# NAMES

new_names <- names(tab_nice)

new_names <- sub("^cell_([0-9]{4})$", "31 Dec \\1", new_names)

new_names[new_names == "row_header"] <- ""

new_names[grepl("^cell_exp_[0-9]{4}$", new_names)] <- ""

setnames(tab_nice, new_names)

#########################################
# SAVE

suffix <- "_all_datasources"

outputfile <- tab_nice

nameoutput <- paste0("D6_Table_2_baseline_cohort", suffix)

assign(nameoutput, outputfile)

saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))

fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".csv")))

write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput, ".xlsx")))

html_table <- kable(outputfile, format = "html", escape = FALSE) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))

writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput, ".html")))

doc <- read_docx() %>%
  body_add_table(outputfile, style = "table_template") %>%
  body_end_section_continuous()

print(doc, target = file.path(thisdiroutput, paste0(nameoutput, ".docx")))