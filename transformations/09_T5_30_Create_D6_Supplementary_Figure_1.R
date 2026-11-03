############################################################
#                                                          #
###        Create_D6_Supplementary_Figure_1       #
#                                                          #
############################################################

# authors: Sofia Miniati, Rosa Gini

# v 1.1 27 Jun 2026

# adapted to DRE


# v 1.0 17 Jun 2026

# from CS5


print('CREATE D6_Supplementary_Figure_1')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_07_D6_Supplementary_Figure_1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
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
    
    filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_Supplementary_Figure_1_masked", thissuffix, ".csv")
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

for (suffix in list_ds_analysed) {
  # suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets

  input <- tab_nice_all[[suffix]]
#    fread(file.path(thisdirinput, paste0("D5_Supplementary_Figure_1_masked",suffix,".csv")),colClasses = "character")
  
  #########################################
  # start_processing
  
  #Numeric Frequency and others
  input[, Frequency := as.numeric(fifelse(Frequency == "<5", "2.5", Frequency))]
  input[,bin := as.numeric(bin)]
  input[,pc_1 := as.numeric(pc_1)]
  input[,pc_99 := as.numeric(pc_99)]

  #exposure_stratum as factor
  input[, exposure_fac := as.factor(exposure_stratum)]
  levels(input$exposure_fac) <- c("Comparator", "Exposure")
  
  
  #vector of percentiles and colors
  percentiles <- data.table(
    xintercept = c(
      input$pc_1[input$exposure_stratum == 1][1],
      input$pc_99[input$exposure_stratum == 1][1],
      input$pc_1[input$exposure_stratum == 0][1],
      input$pc_99[input$exposure_stratum == 0][1]
    ),
    group = c(
      "1st percentile for exposure",
      "99th percentile for exposure",
      "1st percentile for comparator", 
      "99th percentile for comparator"
    ),
    linetype = c("dotted", "dotted", "dashed", "dashed"),
    color = c("gold", "gold", "skyblue", "skyblue")
  )
  
  width <- 0.05
  
  
  ##Propensity Score Overlap
  
  plot <- ggplot(input, aes(x = bin, y = Frequency, fill = exposure_fac)) +
    geom_col(position = "identity", width = width) |> blend("multiply") +
    scale_fill_manual(values = c("skyblue", "gold")) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
    labs(title = "Propensity Score Overlap by Treatment Group",
         x = "Propensity Score",
         y = "Frequency",
         fill = NULL) +
    theme_minimal() +
    guides(fill = guide_legend(
      reverse = TRUE,
      override.aes = list(linetype = 0, color = NA),
      order = 2
    )) +
    theme(legend.position = c(0.85, 0.85), 
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.background = element_rect(fill = "white", 
                                           color = "grey",
                                           linewidth = 0.3)) +
    geom_vline(data = percentiles, 
               aes(xintercept = xintercept, color = group, linetype = group),
              linewidth=1.4, show.legend = TRUE) +
    scale_color_manual(name = NULL,
                       values = setNames(percentiles$color, percentiles$group)) +
    scale_linetype_manual(name = NULL,
                          values = setNames(percentiles$linetype, percentiles$group))
  
  
  
  #########################################
  # clean
  
  # listvartokeep <- c()
  # processing <- processing[,..listvartokeep]
  
  #########################################
  # save
  
  #figure
  ggsave(paste0("D6_Supplementary_Figure_1",suffix,".pdf"), 
         plot = plot, 
         path = thisdiroutput, 
         width = 10, 
         height = 8)
  

}

