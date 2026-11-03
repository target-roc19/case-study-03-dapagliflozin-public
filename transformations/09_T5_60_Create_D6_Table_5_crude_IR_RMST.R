############################################################
#                                                          #
####        D6_Table_5_crude_IR_RMST                       #
#                                                          #
############################################################

# authors: Claudia Bartolini

# v 1.0

# 23 Jul 2026


print('CREATE D6_Table_5_crude_IR_RMST')


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

print(paste("Now creating: D6_Table_5_crude_IR_RMST in nice format"))

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
      paste0("D5_Table_5_crude_IR_RMST_masked", thissuffix, ".csv")
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

clean_chr <- function(x) {
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  x
}

format_ci <- function(est, lower, upper) {
  
  est   <- clean_chr(est)
  lower <- clean_chr(lower)
  upper <- clean_chr(upper)
  
  result <- rep(NA_character_, length(est))
  
  # Stima e limiti tutti disponibili
  complete <- (
    !is.na(est) &
      !is.na(lower) &
      !is.na(upper)
  )
  
  result[complete] <- paste0(
    est[complete],
    " (",
    lower[complete],
    "–",
    upper[complete],
    ")"
  )
  
  # Se i limiti non sono disponibili, mantiene solo la stima
  estimate_only <- (
    !is.na(est) &
      (is.na(lower) | is.na(upper))
  )
  
  result[estimate_only] <- est[estimate_only]
  
  result
}

first_present <- function(x) {
  
  x <- clean_chr(x)
  x <- x[!is.na(x)]
  
  if (length(x) > 0L) {
    x[1L]
  } else {
    NA_character_
  }
}

extract_value <- function(x, analysis) {
  
  value <- x[analysisRMST == analysis]
  value <- clean_chr(value)
  value <- value[!is.na(value)]
  
  if (length(value) > 0L) {
    value[1L]
  } else {
    NA_character_
  }
}


for (suffix in list_ds_analysed) {
  tab_nice <- copy(tab_nice_all[[suffix]])
  
  DT <-tab_nice[,
                treatment := fcase(
                  exposure_stratum == "0", "DPP4-I",
                  exposure_stratum == "1", "Dapagliflozin",
                  default = NA_character_ )]
  
  DT[,
     estimand := fcase(
       analysisRMST == "E1", "Estimand 1",
       analysisRMST == "E2", "Estimand 2",
       analysisRMST %chin% c("E3_year3", "E3_year5"), "Estimand 3",
       default = NA_character_ )]
  
  DT[,
     estimand_order := match(
       estimand,
       c("Estimand 1", "Estimand 2", "Estimand 3"))]
  
  # Per Estimand 3, E3_year3 ed E3_year5 descrivono la stessa coorte:
  # si usa una sola riga (E3_year3) per evitare duplicati.
  general_info <- DT[
    analysisRMST %chin% c("E1", "E2", "E3_year3"),
    .(
      estimand,
      estimand_order,
      treatment,
      `N patients` = clean_chr(N_),
      `N events` = clean_chr(N_outcome_),
      `Person-Years` = clean_chr(PY_),
      `Incidence rate (95% CI)` = format_ci(
        est   = IR_,
        lower = IRll_,
        upper = IRul_))]
  
  #RMST 3y
  rmst_year3 <- DT[
    analysisRMST == "E3_year3",
    .(
      estimand = "Estimand 3",
      treatment,
      `Restricted mean survival time year 3 (95% CI)` =
        format_ci(
          est   = RMST_,
          lower = RMST_lb_,
          upper = RMST_ub_))]
  
  #RMST 5y
  rmst_year5 <- DT[
    analysisRMST == "E3_year5",
    .(
      estimand = "Estimand 3",
      treatment,
      `Restricted mean survival time year 5 (95% CI)` =
        format_ci(
          est   = RMST_,
          lower = RMST_lb_,
          upper = RMST_ub_) ) ]
  
  #all together
  table1_long <- merge( general_info, rmst_year3,
                        by = c("estimand", "treatment"),
                        all.x = TRUE, sort = FALSE)
  
  table1_long <- merge(table1_long,rmst_year5,
                       by = c("estimand", "treatment"),
                       all.x = TRUE, sort = FALSE)
  
  rmst_columns <- c(
    "Restricted mean survival time year 3 (95% CI)",
    "Restricted mean survival time year 5 (95% CI)"
  )
  
  table1_long[,
              (rmst_columns) := lapply(.SD,
                                       function(x) {
                                         x <- clean_chr(x)
                                         fifelse(is.na(x), "N/A", x)
                                       }
              ),.SDcols = rmst_columns]
  
  #dcast
  table1 <- dcast(
    table1_long,
    estimand + estimand_order ~ treatment,
    value.var = c(
      "N patients",
      "N events",
      "Person-Years",
      "Incidence rate (95% CI)",
      "Restricted mean survival time year 3 (95% CI)",
      "Restricted mean survival time year 5 (95% CI)"
    ),sep = " | " )
  
  setorder(table1, estimand_order)
  table1[, estimand_order := NULL]
  setnames( table1,  old = "estimand",new = "Comparison")
  
  metrics <- c(
    "N patients",
    "N events",
    "Person-Years",
    "Incidence rate (95% CI)",
    "Restricted mean survival time year 3 (95% CI)",
    "Restricted mean survival time year 5 (95% CI)"
  )
  
  dapagliflozin_columns <- paste0(
    metrics," | Dapagliflozin")
  
  dpp4i_columns <- paste0(
    metrics,  " | DPP4-I" )
  
  # Mantiene solo i nomi effettivamente presenti
  dapagliflozin_columns <- intersect(
    dapagliflozin_columns,
    names(table1)
  )
  
  dpp4i_columns <- intersect(
    dpp4i_columns,
    names(table1)
  )
  
  setcolorder(
    table1,  c(
      "Comparison",
      dapagliflozin_columns,
      dpp4i_columns
    )
  )
  
  # #########################################
  # SAVE
  
  outputfile <- table1
  nameoutput <- paste0("D6_Table_5_crude_IR_RMST",suffix)
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
  
  ##########################################################################################
  
  print('Now producing Table S2 - Crude and adjusted differences in restricted mean survival time (95% CI)')
  
  rmst_differences <- DT[ analysisRMST %chin% c("E3_year3", "E3_year5"),
                          .(
                            crude_difference = first_present(
                              format_ci(
                                est   = RMSTdiff_,
                                lower = RMSTdiffll_,
                                upper = RMSTdifful_
                              )
                            ),
                            
                            adjusted_difference = first_present(
                              format_ci(
                                est   = RMSTdiff_adj_,
                                lower = RMSTdiff_adj_ll_,
                                upper = RMSTdiff_adj_ul_
                              )
                            )
                          ),
                          by = analysisRMST
  ]
  
  table2 <- data.table(
    Comparison = " vs DPP4-i",
    
    `Crude Difference in RMST 3 year (95% CI)` =
      rmst_differences[
        analysisRMST == "E3_year3",
        first_present(crude_difference)
      ],
    
    `Adjusted Difference in RMST 3 year (95% CI)` =
      rmst_differences[
        analysisRMST == "E3_year3",
        first_present(adjusted_difference)
      ],
    
    `Crude Difference in RMST 5 year (95% CI)` =
      rmst_differences[
        analysisRMST == "E3_year5",
        first_present(crude_difference)
      ],
    
    `Adjusted Difference in RMST 5 year (95% CI)` =
      rmst_differences[
        analysisRMST == "E3_year5",
        first_present(adjusted_difference)
      ]
  )
  
  # #########################################
  # SAVE
  
  outputfile <- table2
  nameoutput <- paste0("D6_Table_S2_Crude_and_adjusted_differences_in_RMST",suffix)
  
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