############################################################
#                                                          #
###        09_T5_99_collect_metadata and diagnostic    #
#                                                          #
############################################################

# authors: Rosa Gini

# v 1.0 27 Jun 2026


# v 1.0 17 Jun 2026

# from CS5


print('collect metadata and diagnostic (only to run on DRE)')


#########################################
# assign directories and other parameters used in this step

if (!localprocessing){
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- file.path(direxp,"Formatted tables/")
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
    thisdirsettings <- dirpargen
    
    listfilesdiagnostics <- c("D5_check_study_drugs_duration_masked", "D5_DIAGNOSTIC_CLEAN_LAB_masked", "excluded_counts_masked", "n_step2", "D5_Supplementary_Figure_1_masked")
  
  
  #########################################
  # load
  
  # # from settings: create the list of subpopulations
  # settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  # thissubpopulations <- unique(settings[,.(subpop)])
  # thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
  
  # from parameter settings: create the list of subpopulations of all the datasources
  PARSETTINGS <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx") ))
  thissubpopulations_ds <- list()
  for (thisds in thisdatasources_for_postprocessing) {
    thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
    thissubpopulations_ds[[thisds]] <- thissubpopulations <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
  }
  
  PARPOP <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx") ))
  

  # metadata
  
  for (filemetadata in c("CDM_SOURCE", "INSTANCE", "METADATA")) {
    processing <- data.table()
    for (thisds in thisdatasources_for_postprocessing) {
      filetoread <- file.path(
        thisfolder_submission[[thisds]],
        paste0(filemetadata, ".csv")
      )
      if (file.exists(filetoread)) {
        
        temp <- fread(filetoread, colClasses = "character")
        temp[, ds := thisds]
        processing <- rbind(processing, temp, fill = T)
      }
    }
    fwrite(processing, file.path(thisdiroutput, paste0(filemetadata, ".csv")))
    write_xlsx(processing, file.path(thisdiroutput, path = paste0(filemetadata, ".xlsx")))
    
  } 

  
  for (filediagnostics in listfilesdiagnostics) {
    processing <- data.table()
    for (thisds in thisdatasources_for_postprocessing) {
      
      for (thissubpop in thissubpopulations_ds[[thisds]]) {
        
        thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
        
        filetoread <- file.path(
          thisfolder_submission[[thisds]],
          paste0(filediagnostics, thissuffix, ".csv")
        )
        if (file.exists(filetoread)) {
          
          temp <- fread(filetoread, colClasses = "character")
          temp[, ds := thisds]
          temp[, subpop := thissubpop]
          temp[, suffix := thissuffix]
          processing <- rbind(processing, temp, fill = T)
        }
      }
    }
    fwrite(processing, file.path(thisdiroutput, paste0(filediagnostics, ".csv")))
    write_xlsx(processing, file.path(thisdiroutput, path = paste0(filediagnostics, ".xlsx")))
    
    }  
}