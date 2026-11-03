############################################################
#                                                          #
###        09_T5_99_collect_metadata and diagnostic    #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.0 8 Aug 2026

print('collect metadata and diagnostic (only to run on DRE)')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_28_metanalysis"
  thisfolder_submission <- list()
  thisfolder_submission[["BIFAP"]] <- file.path(dirtest,testname,"folder_BIFAP","g_export")
  thisfolder_submission[["CPRD"]] <- file.path(dirtest,testname,"folder_CPRD","g_export")
  thisdatasources_for_postprocessing <- names(thisfolder_submission)
  
}else{
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- file.path(direxp,"Formatted tables/")
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
    thisdirsettings <- dirpargen
}
  
  
  #########################################
  # load
  
  # # # from settings: create the list of subpopulations
  # # settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  # # thissubpopulations <- unique(settings[,.(subpop)])
  # # thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
  # 
  # # from parameter settings: create the list of subpopulations of all the datasources
  # PARSETTINGS <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx") ))
  # thissubpopulations_ds <- list()
  # for (thisds in thisdatasources_for_postprocessing) {
  #   thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
  #   thissubpopulations_ds[[thisds]] <- thissubpopulations <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
  # }
  # 
  # PARPOP <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx") ))
  
file_BIFAP <- fread(file.path(thisfolder_submission[["BIFAP"]],"D5_ResultsIR_HR_PC_HOSP.csv"))
file_CPRD <- fread(file.path(thisfolder_submission[["CPRD"]],"D5_ResultsIR_HR_PC_HOSP.csv"))

View(file_BIFAP)    
