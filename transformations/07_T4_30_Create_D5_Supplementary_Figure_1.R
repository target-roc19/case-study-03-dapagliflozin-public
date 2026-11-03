############################################################
#                                                          #
###         Create D5_Supplementary_Figure_1_distribution_PS      #
#                                                          #
############################################################


# authors: Sofia Miniati

# v 1.0 28 Jun 2026

# from CS5

print('CREATE D5_Supplementary_Figure_1_distribution_PS')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  error <- "no test available for this step"
  stop(error)
  # testname <- "test_25_D5_Supplementary_Figure_1"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(thisdirinput,"g_output")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  
  #########################################
  # load input datasets
    
  input <- readRDS(file.path(thisdirinput,paste0("D4_EXP_ACT_COV_WEIGHT",suffix,".rds")))
  
  # rename variables 
  
  # input[, ps := conditional_ps]  
  # input[, exposure := exposed_visit]  
  #########################################
  # start_processing
  
  input <- input[,.(exposure,ps)]
  
  #bin
  width <- 0.05
  input[, bin := pmin(floor(ps / width) * width, 1 - width)]
  
  all_bin <- seq(0, 1 - width, by = width)
  
  ##Frequency
  processing <- CJ(exposure_stratum = unique(input$exposure), bin = all_bin)
  
  freq_obs <- input[, .(Frequency = .N), by = .(exposure, bin)]
  setnames(freq_obs, "exposure", "exposure_stratum")
  
  processing <- freq_obs[processing, on = .(exposure_stratum, bin)]
  processing[is.na(Frequency), Frequency := 0]
 
  processing <- processing[,ds := settings$ds]
  
  #pc_1
  processing[input[, .(pc_1 = quantile(ps, 0.01)), by = .(exposure_stratum = exposure)], 
             pc_1 := i.pc_1, 
             on = .(exposure_stratum)]
  #pc_99
  processing[input[, .(pc_99 = quantile(ps, 0.99)), by = .(exposure_stratum = exposure)], 
             pc_99 := i.pc_99, 
             on = .(exposure_stratum)]
  
  ##Diagnostics
  #First quantile
  processing[input[, .(pc_25 = quantile(ps, 0.25)), by = .(exposure_stratum = exposure)], 
             pc_25 := i.pc_25, 
             on = .(exposure_stratum)]
  #Median
  processing[input[, .(pc_50 = quantile(ps, 0.5)), by = .(exposure_stratum = exposure)], 
             pc_50 := i.pc_50, 
             on = .(exposure_stratum)]
  #Third quantile
  processing[input[, .(pc_75 = quantile(ps, 0.75)), by = .(exposure_stratum = exposure)], 
             pc_75 := i.pc_75, 
             on = .(exposure_stratum)]
  #Mean
  processing[input[, .(ps_mean = mean(ps)), by = .(exposure_stratum = exposure)], 
             ps_mean := i.ps_mean, 
             on = .(exposure_stratum)]
  
  #########################################
  # clean
  
  listvartokeep <- c("ds",
                     "exposure_stratum",
                     "bin",
                     "Frequency",
                     "pc_1",
                     "pc_99",
                     "pc_25",
                     "pc_50",
                     "pc_75",
                     "ps_mean"
                     )
  processing <- processing[,..listvartokeep]
  
  #########################################
  # save

  outputfile <- processing
  
  nameoutput <- paste0("D5_Supplementary_Figure_1",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}

