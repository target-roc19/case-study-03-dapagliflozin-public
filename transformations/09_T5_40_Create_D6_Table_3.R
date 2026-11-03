############################################################
#                                                          #
####         D6_Table_3_asd_before_after_weighting      ####
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.1
# 4 Sep 2026
# age changed from continuous to categorical

print('CREATE D6_Table_3_asd_before_after_weighting')

#########################################
# assign directories

if (TEST){ 
  # testname <- "test_20_D6_Table_2"
  # thisdirinput <- paste0(file.path(dirtest, testname), "/")
  # thisdiroutput <- file.path(dirtest,testname,"g_output")
  # dir.create(thisdiroutput, showWarnings = F)
  # thisdirsettings <- thisdirinput
  # settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  # thisdatasources_for_postprocessing <- settings$ds
  # thisdatasource <- settings$ds
  # thisdirarchive <- thisdirinput
  # thisfolder_submission <- list()
  # thisfolder_submission[[thisdatasource]] <- thisdirinput
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
    thisdiroutput <- file.path(direxp,"Formatted tables/")
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
  }
}

#########################################
# load

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

PARSETTINGS <- as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx")))

thissubpopulations_ds <- list()

for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
  thissubpopulations_ds[[thisds]] <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
}

PARPOP <- as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx")))

# parameters of this step

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks","D5_Table_3.xlsx"),1)))

parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))

for (par in parameter_list) {
  temp <- unname(unlist(unique(parameters_this_step[parameter == par,.(get("values"))])))
  assign(par, temp)
}

thiscovariates_bin <- covar_bin
thiscovariates_cat <- covar_cat
thiscovariates_cont <- covar_cont

#########################################

print("Now creating: D6_Table_3_asd_before_after_weighting")

#########################################
# load datasets from all data sources

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {
  
  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
    
    filetoread <- file.path(thisfolder_submission[[thisds]], paste0("D5_Table_3_asd_before_after_trimming_masked", thissuffix, ".csv"))
    
    if (file.exists(filetoread)) {
      
      temp <- fread(filetoread, colClasses = "character")
      
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      
      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)
      
    }else{
      errmess <- paste("File",filetoread,"does not exist in the export folder of",thisds)
      stop(errmess)
    }
  }
}

##################
# HELPERS

add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar,"N_")
  varP <- paste0(covar,"perc_")
  tab_nice[stratum != "asd", cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
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

add_asd <- function(j, covar){
  varname <- paste0("cell_",j)
  varinput <- paste0(covar,"_asd_")
  tab_nice[stratum == "asd", (varname) := get(varinput)]
}

##########################################
# WRITE TABLE(S)

#########################################
# populate rows

tab_nice <- data.table()

for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  temp[, ds_subpop := suffix]
  tab_nice <- rbind(tab_nice, temp, fill = T)
}

#########################################
# add rows for ASD

asd_cols <- grep("_asd_", names(tab_nice), value = TRUE)
asd_cols <- c("name_ds_subpop", "ds_subpop", asd_cols)

asd <- copy(tab_nice)[1, ..asd_cols]
asd[, stratum := "asd"]
asd[, exposure := 98]

tab_nice <- rbind(tab_nice, asd, fill = T)

#########################################
# put in long format

# tab_nice <- melt(
#   tab_nice,
#   id.vars = c("name_ds_subpop","ds_subpop","stratum","exposure"),
#   variable.name = "metric",
#   value.name = "value"
# )
# 
# # extract BF/AF
# 
# tab_nice[, W := sub(".*_(BF|AF)$", "\\1", metric)]
# tab_nice[, metric := sub("_(BF|AF)$", "_", metric)]
# 
# #########################################
# # reshape
# 
# tab_nice <- dcast(
#   tab_nice,
#   name_ds_subpop + ds_subpop + stratum + exposure + W ~ metric,
#   value.var = "value"
# )
# 
# tab_nice[, W := factor(W, levels = c("BF", "AF"))]
# tab_nice[, stratum := factor(stratum, levels = c("exposed", "comparators", "asd"))]
# 
# setorder(tab_nice, name_ds_subpop, W, stratum)

id_vars <- c("name_ds_subpop","ds_subpop","stratum","exposure")

# list columns ending in _BF or _AF
bf_af_cols <- grep("_(BF|AF)$", names(tab_nice), value = TRUE)

# leave out of this reshape the other columns
other_cols <- setdiff(names(tab_nice), c(id_vars, bf_af_cols))

print(other_cols)  # check

# make long columns BF/AF
long <- melt(
  tab_nice,
  id.vars = id_vars,
  measure.vars = bf_af_cols,
  variable.name = "metric",
  value.name = "value"
)

# split BF/AF
long[, W := sub(".*_(BF|AF)$", "\\1", metric)]
long[, metric := sub("_(BF|AF)$", "_", metric)]

# make wide on metrics
tab_long <- dcast(long, name_ds_subpop + ds_subpop + stratum + exposure + W ~ metric, value.var = "value")

tab_long[, W := factor(W, levels = c("BF", "AF"))]

tab_nice <- tab_long

setorder(tab_nice, W, stratum)


############################################
# row 1 - datasource

j <- 1

row_header_1 <- c("Datasource")

tab_nice[, cell := name_ds_subpop]
setnames(tab_nice, "cell", paste0("cell_",j))

############################################
# row 2 - stratum

row_header_1 <- c(row_header_1, "")

j <- j + 1

tab_nice[stratum == "exposed", cell := "Exposed"]
tab_nice[stratum == "comparators", cell := "Comparators"]
tab_nice[stratum == "asd", cell := "Absolute Standardised Difference"]

setnames(tab_nice, "cell", paste0("cell_",j))

############################################
# row 3 - N

row_header_1 <- c(row_header_1, "N")

j <- j + 1

tab_nice[, cell := N_]
setnames(tab_nice, "cell", paste0("cell_",j))

############################################
# demographics

row_header_1 <- c(row_header_1, "Demographics")

j <- add_empty_row(j)

############################################
# AGE - categorical

row_header_1 <- c(
  row_header_1,
  "Age (years)",
  "< 65",
  "65-74",
  "75-84",
  "85+"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "age_cat_<65_")
add_asd(j, "age_cat")

j <- descriptive_N_perc(j, "age_cat_65-74_")
j <- descriptive_N_perc(j, "age_cat_75-84_")
j <- descriptive_N_perc(j, "age_cat_>=85_")

############################################
# gender

row_header_1 <- c(
  row_header_1,
  "Gender N (%)",
  "Male",
  "Female",
  "Other"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "gender_M_")
j <- descriptive_N_perc(j, "gender_F_")
j <- descriptive_N_perc(j, "gender_O_")

add_asd(j - 2, "gender")

############################################
# COMORBIDITIES

row_header_1 <- c(row_header_1, "Comorbidities N (%)")

j <- add_empty_row(j)

############################################
# CKD

row_header_1 <- c(
  row_header_1,
  "Chronic Kidney Disease N (%)",
  "Stage 1-4",
  "Stage 5"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "CKD_1-4_")
j <- descriptive_N_perc(j, "CKD_5_")

add_asd(j - 1, "CKD")

############################################
# binary comorbidities

for (cov in thiscovariates_bin[1:14]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  add_asd(j, cov)
}

############################################
# COMEDICATIONS

row_header_1 <- c(row_header_1, "Comedications N (%)")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[15:37]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  add_asd(j, cov)
}

############################################
# LIFESTYLE

row_header_1 <- c(row_header_1, "Lifestyle N (%)")

j <- add_empty_row(j)

############################################
# Tobacco use imputed

row_header_1 <- c(
  row_header_1,
  "Tobacco use with missing imputed as 'none'",
  "None",
  "Past",
  "Current"
)

j <- add_empty_row(j)

j <- descriptive_N_perc(j, "Smoking_imputed_0_")
j <- descriptive_N_perc(j, "Smoking_imputed_1_")
j <- descriptive_N_perc(j, "Smoking_imputed_2_")

add_asd(j - 2, "Smoking_imputed")

############################################
# ESTABLISHED ACVD

row_header_1 <- c(row_header_1, "Established ACVD")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[38:39]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  add_asd(j, cov)
}

############################################
# ACVD RISK FACTORS

row_header_1 <- c(row_header_1, "ACVD Risk factors")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[40:42]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  add_asd(j, cov)
}

#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)

tokeep <- c("ds_subpop","stratum","W",cell_cols)

tab_nice <- tab_nice[, ..tokeep]

#########################################
# RESHAPE CELLS

tab_nice <- melt(
  tab_nice,
  id.vars = c("ds_subpop","stratum","W"),
  measure.vars = patterns(cell = "^cell_[0-9]+$"),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, ds_subpop, stratum, W)

tab_nice <- dcast(
  tab_nice,
  rownum ~ ds_subpop + stratum + W,
  value.var = "value"
)

setorder(tab_nice, rownum)

#########################################
# ADD ROW HEADER

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]

#########################################
# ORDER COLUMNS

ordered_ds <- list_ds_analysed

ordered_cols <- c(
  "row_header",
  as.vector(rbind(
    paste0(
      ordered_ds,
      c(
        "_exposed_BF",
        "_comparators_BF",
        "_asd_BF",
        "_exposed_AF",
        "_comparators_AF",
        "_asd_AF"
      )
    )
  ))
)

setcolorder(tab_nice, ordered_cols)

#########################################
# SAVE

suffix <- "_all_datasources"

outputfile <- tab_nice

nameoutput <- paste0("D6_Table_3_asd_before_after_weighting",suffix)

assign(nameoutput, outputfile)

saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))

html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))

doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))