############################################################
#                                                          #
###       Create D5_Supplementary_Figure_2_MICE           ###
#                                                          #
############################################################

# authors: Sofia Miniati

# v 1.0 09 Sep 2026

# adapted from 07_T4_30_Create_D5_Supplementary_Figure_1.R

print('CREATE D5_Supplementary_Figure_2_MICE')

#########################################
# assign directories and other parameters used in this step

if (TEST){
  error <- "no test available for this step"
  stop(error)
  # testname <- "test_25_D5_Supplementary_Figure_2_MICE"
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
  # load input dataset containing the 10 PS columns

  input <- readRDS(file.path(thisdirinput,paste0("D4_EXP_ACT_COV_WEIGHT_MICE",suffix,".rds")))

  #########################################
  # start processing

  ps_columns <- paste0("ps_", 1:10)

  missing_ps_columns <- setdiff(ps_columns, names(input))
  if (length(missing_ps_columns) > 0) {
    stop(paste0("Missing expected PS columns: ", paste(missing_ps_columns, collapse = ", ")))
  }

  processing_list <- vector("list", length(ps_columns))

  for (i in seq_along(ps_columns)) {

    ps_col <- ps_columns[i]
    print(paste0("Processing ", ps_col))

    processing_input <- input[, .(exposure, ps = get(ps_col))]
    processing_input <- processing_input[!is.na(ps)]

    # bin
    width <- 0.05
    processing_input[, bin := pmin(floor(ps / width) * width, 1 - width)]

    all_bin <- seq(0, 1 - width, by = width)

    ## Frequency
    processing <- CJ(exposure_stratum = unique(processing_input$exposure), bin = all_bin)

    freq_obs <- processing_input[, .(Frequency = .N), by = .(exposure, bin)]
    setnames(freq_obs, "exposure", "exposure_stratum")

    processing <- freq_obs[processing, on = .(exposure_stratum, bin)]
    processing[is.na(Frequency), Frequency := 0]

    processing[, ds := settings$ds]
    processing[, imputation := i]
    processing[, ps_variable := ps_col]

    # pc_1
    processing[processing_input[, .(pc_1 = quantile(ps, 0.01)), by = .(exposure_stratum = exposure)],
               pc_1 := i.pc_1,
               on = .(exposure_stratum)]

    # pc_99
    processing[processing_input[, .(pc_99 = quantile(ps, 0.99)), by = .(exposure_stratum = exposure)],
               pc_99 := i.pc_99,
               on = .(exposure_stratum)]

    ## Diagnostics
    # First quantile
    processing[processing_input[, .(pc_25 = quantile(ps, 0.25)), by = .(exposure_stratum = exposure)],
               pc_25 := i.pc_25,
               on = .(exposure_stratum)]

    # Median
    processing[processing_input[, .(pc_50 = quantile(ps, 0.5)), by = .(exposure_stratum = exposure)],
               pc_50 := i.pc_50,
               on = .(exposure_stratum)]

    # Third quantile
    processing[processing_input[, .(pc_75 = quantile(ps, 0.75)), by = .(exposure_stratum = exposure)],
               pc_75 := i.pc_75,
               on = .(exposure_stratum)]

    # Mean
    processing[processing_input[, .(ps_mean = mean(ps)), by = .(exposure_stratum = exposure)],
               ps_mean := i.ps_mean,
               on = .(exposure_stratum)]

    #########################################
    # clean

    listvartokeep <- c("ds",
                       "imputation",
                       "ps_variable",
                       "exposure_stratum",
                       "bin",
                       "Frequency",
                       "pc_1",
                       "pc_99",
                       "pc_25",
                       "pc_50",
                       "pc_75",
                       "ps_mean")

    processing <- processing[, ..listvartokeep]
    processing_list[[i]] <- processing
  }

  #########################################
  # combine the 10 PS distributions in one dataset

  outputfile <- rbindlist(processing_list, use.names = TRUE, fill = TRUE)

  #########################################
  # save

  nameoutput <- paste0("D5_Supplementary_Figure_2_MICE",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}
