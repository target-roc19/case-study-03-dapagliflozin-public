############################################################
#                                                          #
####         D6_Table_S2_OR                #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.0

# 4 Sep 2026



print('CREATE D6_Table_S2_OR')

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

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


# from parameter settings: create the list of subpopulations of all the datasources
PARSETTINGS <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx") ))
thissubpopulations_ds <- list()
for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
  thissubpopulations_ds[[thisds]] <- thissubpopulations <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
}

PARPOP <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx") ))

# parameters of this step

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D5_Table_3.xlsx")),1)))
parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))
for (par in parameter_list) {
  temp <- unname(unlist(unique(parameters_this_step[parameter == par,.(get("values"))])))
  assign(par, temp)
}

thiscovariates_bin <- covar_bin
thiscovariates_cat <- covar_cat
thiscovariates_cont <- covar_cont

#########################################

print(paste("Now creating: D6_Table_S2_OR"))

# this header works both locally and on DRE
# create the list list_ds_analysed of all datasources including their subpopulations (ds_subpop)
# store all the data in a list of datasets indexed by ds_subpop

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {
  
  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
    
    filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_Table_3_asd_before_after_trimming_masked", thissuffix, ".csv")
    )
    
    if (file.exists(filetoread)) {
      
      temp <- fread(filetoread, colClasses = "character")
      
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)
    }else{
      errmess <- paste("File",filetoread,"does not exist in the export folder of", thisds)
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
  tab_nice[stratum != "or", cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_continuous <- function(j, covar){
  j <- j + 1
  varM <- paste0(covar,"_mean_")
  varSD <- paste0(covar,"_sd_")
  tab_nice[stratum != "or", cell := paste0(get(varM), " (",get(varSD),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[stratum != "or", cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

add_asd <- function(j, covar){
  k <- j
  varname <- paste0("cell_",k)
  varinput <- paste0(covar, "_asd_")
  tab_nice[ stratum == "asd", (varname) := get(varinput)]
}

add_or <- function(j, covar){
  k <- j
  varname <- paste0("cell_",k)
  varor <- paste0(covar, "_or_")
  varll <- paste0(covar, "_ll_")
  varul <- paste0(covar, "_ul_")
  tab_nice[ stratum == "or", (varname) := paste0(get(varor), " (", get(varll), " - ", get(varul), ")")]
}



##########################################
# WRITE TABLE(S)

#########################################
# POPULATE ROWS

tab_nice <- data.table()
for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  temp[, ds_subpop := suffix]
  tab_nice <- rbind(tab_nice, temp, fill = T)
}

# add row for or

or_cols <- grep("_or_", names(tab_nice), value = TRUE)
ll_cols <- grep("_ll_", names(tab_nice), value = TRUE)
ul_cols <- grep("_ul_", names(tab_nice), value = TRUE)
or_cols <- c("name_ds_subpop", or_cols, ll_cols, ul_cols)
or <- copy(tab_nice)[1, ..or_cols]
or[, stratum := "or"]
or[, exposure := 99]
tab_nice <- rbind(tab_nice, or, fill = T)
# 
# 
# # put in long format
# tab_nice <- melt(
#   tab_nice,
#   id.vars = c("name_ds_subpop","stratum", "exposure"),
#   variable.name = "metric",
#   value.name = "value"
# )
# 
# # extract suffix BF/AF in a column W
# tab_nice[, W := sub(".*_(BF|AF)$", "\\1", metric)]
# tab_nice[, metric := sub("_(BF|AF)$", "_", metric)]
# 
# # to long with exposure, stratum and W as keys
# 
# tab_nice <- dcast(
#   tab_nice,
#   name_ds_subpop + stratum + exposure + W ~ metric,
#   value.var = "value"
# )
# 
# # # order BF first and AF after
# # 
# # tab_nice[, W := factor(W, levels = c("BF", "AF"))]
# # tab_nice[, stratum := factor(stratum, levels = c("exposed", "comparators", "or"))]
# # setorder(tab_nice, name_ds_subpop, W, stratum)
# 
# # remove AF
# 
# tab_nice <- tab_nice[ W != "AF",]

setnames(tab_nice, names(tab_nice), gsub("BF$", "", names(tab_nice)))

# remove AF and asd from tab_nice

col_AF <- grep("_AF", names(tab_nice), value = TRUE)
tab_nice[, (col_AF) := NULL]

col_asd <- grep("_asd", names(tab_nice), value = TRUE)
tab_nice[, (col_asd) := NULL]


############################################
# row 1

j <- 1
row_header_1 <- c("Datasource")
tab_nice[, cell := name_ds_subpop]
setnames(tab_nice, "cell", paste0("cell_",j))


# row 2

row_header_1 <- c(row_header_1,                  
                  "" 
)

j <- j + 1
tab_nice[stratum == "exposed", cell := "Exposed"]
tab_nice[stratum == "comparators", cell := "Comparators"]
tab_nice[stratum == "or", cell := "OR"]
setnames(tab_nice, "cell", paste0("cell_",j))


# row 3

row_header_1 = c(row_header_1,                  
                 "N" 
)

j <- j + 1
tab_nice[, cell := N_]
setnames(tab_nice, "cell", paste0("cell_",j))

# row 4

row_header_1 <- c(row_header_1,  
                  "Demographics")

j <- add_empty_row(j)

# rows 5-7

row_header_1 <- c(row_header_1,  
                  "Age (years) N (%)", 
                  "< 65", 
                  "65-74",
                  "75-84",
                  "85+"
)

j <- add_empty_row(j)

var <- "age_cat"
values <- unname(unlist(parameters_this_step[parameter == paste0(var,"value"),.(values)]))
for (val in values) {
  j <- descriptive_N_perc(j, paste0(var,"_",val,"_"))
  if (paste0(var,"_",val,"_or_")  %in% names(tab_nice)) {
    add_or(j, paste0(var,"_",val))
  }
}


# rows 8-11

row_header_1 <- c(row_header_1,  
                  "Gender N (%)",
                  "Male", 
                  "Female",
                  "Other"
)

j <- add_empty_row(j)
# 
# j <- descriptive_N_perc(j, "gender_M_")
# j <- descriptive_N_perc(j, "gender_F_")
# j <- descriptive_N_perc(j, "gender_O_")
# if (paste0(var, "_or_")  %in% names(tab_nice)) {
#   add_or(j, var)
# }

var <- "gender"
values <- unname(unlist(parameters_this_step[parameter == paste0(var,"value"),.(values)]))
for (val in values) {
  j <- descriptive_N_perc(j, paste0(var,"_",val,"_"))
  if (paste0(var,"_",val,"_or_")  %in% names(tab_nice)) {
    add_or(j, paste0(var,"_",val))
  }
}



# COMORBIDITIES
  
# row 12

row_header_1 <- c(row_header_1,  
                  "Comorbidities N (%)")

j <- add_empty_row(j)

# CKD

row_header_1 <- c(
  row_header_1,
  "Chronic Kidney Disease N (%)",
  "Stage 0",
  "Stage 1-4",
  "Stage 5"
)

j <- add_empty_row(j)
var <- "CKD"
values <- unname(unlist(parameters_this_step[parameter == paste0(var,"value"),.(values)]))
for (val in values) {
  j <- descriptive_N_perc(j, paste0(var,"_",val,"_"))
  if (paste0(var,"_",val,"_or_")  %in% names(tab_nice)) {
    add_or(j, paste0(var,"_",val))
  }
}


# rows 13-20

for (cov in thiscovariates_bin[1:14]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  var <- cov
  if (paste0(var, "_or_")  %in% names(tab_nice)) {
    add_or(j, var)
  }
}

# comedications

row_header_1 <- c(row_header_1,  
                  "Comedications N (%)")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[15:37]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1,  
                    name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  var <- cov
  if (paste0(var, "_or_")  %in% names(tab_nice)) {
    add_or(j, var)
  }
}

# lifestyle

row_header_1 <- c(row_header_1,  
                  "Lifestyle N (%)")

j <- add_empty_row(j)

row_header_1 <- c(row_header_1, 
                  "Tobacco use with missing imputed as 'none'",
                  "None",
                  "Past",
                  "Current" )

j <- add_empty_row(j)


var <- "Smoking_imputed"
values <- unname(unlist(parameters_this_step[parameter == paste0(var,"value"),.(values)]))
for (val in values) {
  j <- descriptive_N_perc(j, paste0(var,"_",val,"_"))
  if (paste0(var,"_",val,"_or_")  %in% names(tab_nice)) {
    add_or(j, paste0(var,"_",val))
  }
}

# ESTABLISHED ACVD

row_header_1 <- c(row_header_1, "Established ACVD")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[38:39]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  var <- cov
  if (paste0(var, "_or_")  %in% names(tab_nice)) {
    add_or(j, var)
  }
}

############################################
# ACVD RISK FACTORS

row_header_1 <- c(row_header_1, "ACVD Risk factors")

j <- add_empty_row(j)

for (cov in thiscovariates_bin[40:42]) {
  name_cov <- unlist(parameters_this_step[values == cov,.(get("name in protocol"))])
  row_header_1 <- c(row_header_1, name_cov)
  j <- descriptive_N_perc(j, paste0(cov,"_"))
  var <- cov
  if (paste0(var, "_or_")  %in% names(tab_nice)) {
    add_or(j, var)
  }
}




# View(tab_nice[, .SD, .SDcols = patterns("^cell_")])



#########################################
# KEEP CELLS
  
cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("name_ds_subpop", "stratum",  cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# RESHAPE CELLS

# First reshape tab_nice from wide into long format

tab_nice <- melt(
  tab_nice,
  id.vars = c("name_ds_subpop","stratum"),
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

setorder(tab_nice, rownum, name_ds_subpop, stratum)


# then reshape from long to wide keeping rownum as the UoO

tab_nice <- dcast(
  tab_nice,
  rownum ~ name_ds_subpop + stratum,
  value.var = "value"
)


# Order rows correctly
setorder(tab_nice, rownum)

#########################################
# ADD ROW HEADER
# 

tab_nice[, row_header := row_header_1]
tab_nice[, rownum := NULL]


#########################################
# ORDER COLUMNS


# ordered_ds <- substring(list_ds_analysed, 2)

ordered_ds <- list_ds_analysed


ordered_cols <- c(
  "row_header",
  as.vector(rbind(paste0(ordered_ds, c( "_exposed", "_comparators", "_or" 
                                        #,"_exposed_AF", "_comparators_AF", "_asd_AF"
                                        )
                         )
                  ))
)

# setcolorder(tab_nice, ordered_cols)


#########################################
# SAVE

suffix <- "_all_datasources"
outputfile <- tab_nice
nameoutput <- paste0("D6_Table_S2_OR",suffix)
assign(nameoutput, outputfile)
# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# html
html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))

