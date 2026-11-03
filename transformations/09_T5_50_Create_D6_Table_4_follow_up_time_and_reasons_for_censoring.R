############################################################
#                                                          #
####         D6_Table_4_follow_up_time_and_reasons_for_censoring                         #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.1 4 Sep 2026

# - added counts of persons with intercurring events not ending fup
# - added time to event stratified per cause

# v 1.0

# 26 Jul 2026


print('CREATE D6_Table_4_follow_up_time_and_reasons_for_censoring')

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

print(paste("Now creating: D6_Table_4_follow_up_time_and_reasons_for_censoring in nice format"))

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
      paste0("D5_Table_4_follow_up_time_and_reasons_for_censoring_masked", thissuffix, ".csv")
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

descriptive_N_perc <- function(j, covar) {
  j <- j + 1
  varN <- paste0(covar,"N_")
  varP <- paste0(covar,"perc_")
  tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_continuous <- function(j, covar, minmax = 1){
  # j <- j + 1
  # varM <- paste0(covar,"_mean_")
  # varSD <- paste0(covar,"_sd_")
  # tab_nice[, cell := paste0(get(varM), " (",get(varSD),")")]
  # setnames(tab_nice, "cell", paste0("cell_",j))
  j <- j + 1
  varM <- paste0(covar,"_median_")
  varQ1 <- paste0(covar,"_q1_")
  varQ3 <- paste0(covar,"_q3_")
  tab_nice[, cell := paste0(get(varM), " (",get(varQ1),"-",get(varQ3),")")]
  setnames(tab_nice, "cell", paste0("cell_",j))
  # j <- j + 1
  # varN <- paste0(covar,"_missingN_")
  # varP <- paste0(covar,"_missingperc_")
  # tab_nice[, cell := paste0(get(varN), " (",get(varP),"%)")]
  # setnames(tab_nice, "cell", paste0("cell_",j))
  if (minmax == 1) {
    j <- j + 1
    varMin <- paste0(covar,"_min_")
    varMax <- paste0(covar,"_max_")
    tab_nice[, cell := paste0(get(varMin),"-",get(varMax))]
    setnames(tab_nice, "cell", paste0("cell_",j)) 
  }
  return(j)
}


add_empty_row <- function(j){
  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

# labels of cause for censoring

# causelab <- list()
# causelab[[1]] <- "Date of outcome"
# causelab[[2]] <- "Date of death"
# causelab[[3]] <- "Date of last date of data collection in the data source"
# causelab[[4]] <- "Day 1825 following index date"
# causelab[[5]] <- "Date of end of the study"
# causelab[[6]] <- "Date of end of the index episode of treatment"
# causelab[[7]] <- "Date of switch from the index episode of treatment"
# causelab[[8]] <- "Date of end of observation of the person in the data source"
# 
# causelab_short <- causelab
# causelab_short[[4]] <- "Day 1095 following index date"


causelab <- list()
for (estimand in c("E1", "E2", "E3b")) {
  causelab[["1"]][[estimand]] <- "Outcome"
  causelab[["2"]][[estimand]]  <- "Death"
  causelab[["3"]][[estimand]]  <- "Last date of data collection in the data source"
  causelab[["4"]][[estimand]]  <- "Day 1825 following index date"
  causelab[["5"]][[estimand]]  <- "End of the study"
  causelab[["6"]][[estimand]]  <- "Discontinuation"
  causelab[["7"]][[estimand]]  <- "Switch/add on"
  causelab[["8"]][[estimand]]  <- "End of observation of the person in the data source"
}
causelab[["4"]][["E3b"]]  <- "Day 1095 following index date"


causes_of_end_fup <- list()
causes_of_end_fup[["E1"]] <- c(1,3,4,5,8)
causes_of_end_fup[["E2"]] <- 1:8
causes_of_end_fup[["E3b"]] <- causes_of_end_fup[["E1"]] 

IE_not_causes_of_end_fup <- list()
IE_not_causes_of_end_fup[["E1"]] <-  c(6,7)
IE_not_causes_of_end_fup[["E2"]] <- c()
IE_not_causes_of_end_fup[["E3b"]] <- c(6,7)


#########################################
# HEADERS

#########################################
# CELLS

for (suffix in list_ds_analysed) {
  tab_nice <- copy(tab_nice_all[[suffix]])
  
  row_header_1 <- c()
  j <- 0
  
  # row 0

  row_header_1 = c(row_header_1,
                   ""
  )

  tab_nice[ stratum == 1, cell := "Dapagliflozin"]
  tab_nice[ stratum == 0, cell := "DPP4-i"]
  tab_nice[ stratum == 99, cell := "Total"]
  setnames(tab_nice, "cell", paste0("cell_",j))

  j <- j + 1
  
  
  # row 1
  
  row_header_1 = c(row_header_1,                  
                   "N" 
  )
  
  tab_nice[, cell := N_]
  setnames(tab_nice, "cell", paste0("cell_",j))
  
  
  estimand <- "E1"
  
  for (estimand in c("E1", "E2", "E3b") ) {
    
    row_header_1 <- c(row_header_1,
                      "",
                      paste0("Estimand ",estimand)
    )
    
    j <- add_empty_row(j)
    j <- add_empty_row(j)
    
    
    # rows 2-4
    
    row_header_1 <- c(row_header_1,
                      paste0("Person-months of follow-up in ",estimand),
                      "Median (Q1 - Q3)",
                      "Min - Max"
    )
    
    j <- add_empty_row(j)
    j <- descriptive_continuous(j, paste0("pmonth_",estimand))
    
    # Causes of end of follow up
    
    row_header_1 <- c(row_header_1,
                      paste0("Cause of end of folluw up of ",estimand)
    )
    
    j <- add_empty_row(j)
    
    for (cause in causes_of_end_fup[[estimand]]) {
      row_header_1 <- c(row_header_1,
                        paste0(causelab[[cause]][[estimand]], ": occurrence N (%)"),
                        paste0("         ", " time to occurrence Median (Q1 - Q3)")
      )
      
      j <- descriptive_N_perc(j, paste0("cause_of_censoring_",estimand,"_",cause,"_"))
      j <- descriptive_continuous(j, paste0("pmonth_cause_of_censoring_",estimand,"_",cause,""), minmax = 0)
    }  
    
    # Intercurring events that do not cause end of follow up
    row_header_1 <- c(row_header_1,
                      paste0("Intercurring events that do not cause end of follow up for ",estimand)
    )
    
    j <- add_empty_row(j)
    
    
    if (length(IE_not_causes_of_end_fup[[estimand]]) == 0) {
      row_header_1 <- c(row_header_1,
                        "None"
      )
      
      j <- add_empty_row(j)
      
    }else{
      for (cause in IE_not_causes_of_end_fup[[estimand]]) {
        row_header_1 <- c(row_header_1,
                          paste0(causelab[[cause]][[estimand]], ": occurrence N (%)"),
                          paste0("         ", " time to occurrence Median (Q1 - Q3)")
        )
        
        j <- descriptive_N_perc(j, paste0("intercurring_not_ending_fup_",estimand,"_",cause,"_"))
        j <- descriptive_continuous(j, paste0("pmonth_to_intercurring_not_ending_fup_",estimand,"_",cause,""), minmax = 0)
      }
    }
    
  }
  
    # View(tab_nice[, .SD, .SDcols = patterns("^cell_")])
  
  #########################################
  # RESHAPE CELLS
  
  
  cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
  tokeep <- c("stratum", cell_cols)
  tab_nice <- tab_nice[, ..tokeep]
  
  
  #########################################
  # RESHAPE CELLS
  
  # First reshape tab_nice from wide into long format
  
  tab_nice <- melt(
    tab_nice,
    id.vars = c("stratum"),
    measure.vars = patterns(
      cell = "^cell_[0-9]+$"
    ),
    variable.name = "rownum"
  )
  
  tab_nice[, rownum := as.integer(rownum)]
  
  setorder(tab_nice, rownum, stratum)
  
  
  # then reshape from long to wide keeping rownum as the UoO
  
  tab_nice <- dcast(
    tab_nice,
    rownum ~ stratum,
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
  # FINAL COLUMNS
  
  colorder <- c("row_header",setdiff(names(tab_nice),"row_header"))
  
  tab_nice <- tab_nice[, ..colorder]
  
  # #########################################
  # # NAMES
  # 
  # tab_nice <- tab_nice[, ..colorder]
  # 
  # setnames(tab_nice,
  #          colorder,
  #          c("Criteria",
  #            "N", 
  #            "% on all matched sets"
  #           )
  #          )
  # 
  # # View(tab_nice)
  # 
  # 
  # #########################################
  # SAVE
  
  outputfile <- tab_nice
  nameoutput <- paste0("D6_Table_4_follow_up_time_and_reasons_for_censoring",suffix)
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

