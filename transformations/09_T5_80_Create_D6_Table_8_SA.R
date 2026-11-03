############################################################
#                                                          #
####              D6_Table_8_SA                         ####
#                                                          #
############################################################

# authors: Claudia Bartolini

# v 0.1

# 17 Jul 2026

print('CREATE D6_Table_8_SA')

#########################################
# assign directories

if (TEST){ 
  testname <- "test_27_D6_Table_8_SA"
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

# from settings: create the list of subpopulations
if (localprocessing) {
  settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
  thissubpopulations <- unique(settings[, .(subpop)])
  thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
}


# from parameter settings: create the list of subpopulations of all the datasources
PARSETTINGS <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx") ))
thissubpopulations_ds <- list()
for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
  thissubpopulations_ds[[thisds]] <- thissubpopulations <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
}

PARPOP <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx") ))

# # parameters of this step
# 
# parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D6_Table_2_baseline_cohort.xlsx")),1)))
# parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))
# for (par in parameter_list) {
#   temp <- unname(unlist(unique(parameters_this_step[parameter == par,.(get("values"))])))
#   assign(par, temp)
# }


#########################################

# this header works both locally and on DRE
# create the list list_ds_analysed of all datasources including their subpopulations (ds_subpop)
# store all the data in a list of datasets indexed by ds_subpop

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {
  
  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
    
    filetoread1 <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_ResultsIPC", thissuffix, ".csv")
    )
    filetoread2 <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_ResultsNDEM", thissuffix, ".csv")
    )
    
    if (file.exists(filetoread1) & file.exists(filetoread2)) {
      
      temp1 <- fread(filetoread1, colClasses = "character")[, SensitivityAnalysis := "IPCW"]
      temp2 <- fread(filetoread2, colClasses = "character")[, SensitivityAnalysis
                                                            := "Non-Differential Exposure Misclassification"]
      
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      tab_nice_all[[suffix]] <- rbind(temp1, temp2)
      list_ds_analysed <- c(list_ds_analysed, suffix)
      
    }else if (file.exists(filetoread1)  && !file.exists(filetoread2)){
      
      temp1 <- fread(filetoread1, colClasses = "character")
      temp2 <- copy(temp1)[,`:=`(HR_    = NA_real_,
                           HRll_ = NA_real_,
                           HRul_ = NA_real_,
                           HRadj_      = NA_real_,
                           HRadjll_   = NA_real_,
                           HRadjul_   = NA_real_)]
      temp2 <- temp2[, SensitivityAnalysis := "Non-Differential Exposure Misclassification"]
      temp1 <- temp1[, SensitivityAnalysis := "IPCW"]
      
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      tab_nice_all[[suffix]] <- rbind(temp1, temp2)
      list_ds_analysed <- c(list_ds_analysed, suffix)
      
    }else{
      errmess <- paste("File",filetoread1,"OR",filetoread2,"does not exist in the export folder of", thisds)
      stop(errmess)
    }
  }
}


tab1 <- data.table()
for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  tab1  <- rbind(tab1, temp, fill = T)
}

cols_hr <- c("HR_","HRll_","HRul_",
              "HRadj_","HRadjll_","HRadjul_")

tab1[, (cols_hr) := lapply( .SD,
  \(x) as.numeric(gsub(",", ".", x))
), .SDcols = cols_hr]

tab_nice <- tab1[exposure == 1,
    .(`Crude HR (95% CI)` = sprintf(
        "%.2f (%.2f–%.2f)",
        HR_,
        HRll_,
        HRul_
      ),
      `Adjusted HR 95% CI` = sprintf(
        "%.2f (%.2f–%.2f)",
        HRadj_,
        HRadjll_,
        HRadjul_
      )),
    by = .(ds,
    name_ds_subpop,
    SensitivityAnalysis)]

tab_nice <- tab_nice[, Comparison := 'Dapa vs dpp4-i']

# clean

listvartokeep <- c("ds",
                   "name_ds_subpop",
                   "SensitivityAnalysis",
                   "Comparison",
                   "Crude HR (95% CI)",
                   "Adjusted HR 95% CI") 

tab_nice <- tab_nice[,..listvartokeep]

#########################################
# SAVE

suffix <- "_all_datasources"
outputfile <- tab_nice
nameoutput <- paste0("D6_Table_8_SA",suffix)
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
