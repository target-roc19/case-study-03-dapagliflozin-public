############################################################
#                                                          #
####        D6_Table_8_SA                         #
#                                                          #
############################################################

# authors: Claudia Bartolini

# v 1.0

# 23 Jul 2026


print('CREATE D6_Table_10_TP')

#########################################
# assign directories

if (TEST){ 
  # testname <- "test_18_D6_Table_1a"
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
    thisdiroutput <- paste0(direxp,"Formatted tables/")
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

#########################################

print(paste("Now creating: D6_Table_10_TP in nice format"))

# this header works both locally and on DRE
# create the list list_ds_analysed of all datasources including their subpopulations (ds_subpop)
# store all the data in a list of datasets indexed by ds_subpop

tab_nice_all <- list()
list_ds_analysed <- c()

for (thisds in thisdatasources_for_postprocessing) {

  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
    
    filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_ResultsIR_HR", thissuffix, ".csv")
    )
    
    if (file.exists(filetoread)) {
      
      temp <- fread(filetoread, colClasses = "character")
      
      suffix <- paste0("_",thisds,thissuffix)
      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)
    } 
  }
}

# helper
format_hr <- function(hr, lower, upper, digits = 3) {
  
  to_numeric <- function(x) {
    as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
  }
  
  hr    <- to_numeric(hr)
  lower <- to_numeric(lower)
  upper <- to_numeric(upper)
  
  risultato <- sprintf(paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
                       hr, lower, upper)
  
  risultato[is.na(hr) | is.na(lower) | is.na(upper)] <- NA_character_
  risultato
}

for (suffix in list_ds_analysed) {
  tab_nice <- copy(tab_nice_all[[suffix]])

  tab_nice <- tab_nice[exposure_stratum == 1 & analysisIR_HR %chin% paste0("TP", 1:4),
    .(
      Scenario = as.integer(sub("TP", "", analysisIR_HR)),
      Dapagliflozin = fcase(
        analysisIR_HR == "TP1", "Best case (lowest event rate)",
        analysisIR_HR == "TP2", "Worst case (highest event rate)",
        analysisIR_HR == "TP3", "Best case",
        analysisIR_HR == "TP4", "Worst case"
      ),
      `DPP-4i` = fcase(
        analysisIR_HR == "TP1", "Worst case (highest event rate)",
        analysisIR_HR == "TP2", "Best case (lowest event rate)",
        analysisIR_HR == "TP3", "Best case",
        analysisIR_HR == "TP4", "Worst case"
      ),
      Interpretation = fcase(
        analysisIR_HR == "TP1", "Maximally favours intervention",
        analysisIR_HR == "TP2", "Maximally favours control",
        analysisIR_HR == "TP3", "Optimistic for both groups",
        analysisIR_HR == "TP4", "Pessimistic for both groups"
      ),
      `Hazard Ratio` = HR_,
      
      `95% CI` = format_hr(HR_, HRll_, HRul_)
      )][order(Scenario)]
  
  
  # #########################################
  # SAVE
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_10_TP",suffix)
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
}

