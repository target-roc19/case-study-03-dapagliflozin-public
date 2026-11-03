############################################################
#                                                          #
###       Create_D6_Supplementary_Figure_2_MICE          ###
#                                                          #
############################################################

# authors: Sofia Miniati, Rosa Gini
# adapted for MICE propensity-score distributions

# v 1.0 09 Sep 2026

print('CREATE D6_Supplementary_Figure_2_MICE')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_D6_Supplementary_Figure_2_MICE"
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
PARSETTINGS <- as.data.table(
  readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx"))
)

thissubpopulations_ds <- list()

for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds, .(subpop)]
  thissubpopulations_ds[[thisds]] <- unlist(
    strsplit(trimws(thissubpopulations_ds[[thisds]]), " +")
  )
}

PARPOP <- as.data.table(
  readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx"))
)


#########################################
# create the list of datasets to analyse

tab_nice_all <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {

  for (thissubpop in thissubpopulations_ds[[thisds]]) {

    thissuffix <- fifelse(
      thissubpop == "all",
      "",
      paste0("_", thissubpop)
    )

    filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0(
        "D5_Supplementary_Figure_2_MICE_masked",
        thissuffix,
        ".csv"
      )
    )

    if (file.exists(filetoread)) {

      temp <- fread(
        filetoread,
        colClasses = "character"
      )

      suffix <- paste0("_", thisds, thissuffix)

      name_suffix[[suffix]] <- fifelse(
        unlist(
          PARPOP[
            ds == thisds & subpop == thissubpop,
            .(name_in_tables)
          ]
        ) == "",
        suffix,
        unlist(
          PARPOP[
            ds == thisds & subpop == thissubpop,
            .(name_in_tables)
          ]
        )
      )

      tab_nice_all[[suffix]] <- temp
      list_ds_analysed <- c(list_ds_analysed, suffix)

    }else{

      errmess <- paste(
        "File",
        filetoread,
        "does not exist in the export folder of",
        thisds
      )

      stop(errmess)
    }
  }
}


#########################################
# create one figure containing all 10 imputations

for (suffix in list_ds_analysed) {

  print(suffix)

  #########################################
  # load input dataset

  input <- copy(tab_nice_all[[suffix]])


  #########################################
  # start processing

  # Convert masked frequencies to numeric values for plotting.
  # As in the original Step 09, masked counts <5 are represented by 2.5.
  input[
    ,
    Frequency := as.numeric(
      fifelse(Frequency == "<5", "2.5", Frequency)
    )
  ]

  input[, bin := as.numeric(bin)]
  input[, pc_1 := as.numeric(pc_1)]
  input[, pc_99 := as.numeric(pc_99)]
  input[, imputation := as.integer(imputation)]

  # Treatment group
  input[
    ,
    exposure_fac := factor(
      exposure_stratum,
      levels = c("0", "1"),
      labels = c("Comparator", "Exposure")
    )
  ]

  # Keep the imputations ordered from 1 to 10
  input[
    ,
    imputation_fac := factor(
      imputation,
      levels = 1:10,
      labels = paste0("Imputation ", 1:10)
    )
  ]


  #########################################
  # percentile lines for each imputation

  percentiles <- unique(
    input[
      ,
      .(
        imputation,
        exposure_stratum,
        pc_1,
        pc_99
      )
    ]
  )

  percentiles_1 <- percentiles[
    ,
    .(
      imputation,
      exposure_stratum,
      percentile = "1st",
      xintercept = pc_1
    )
  ]

  percentiles_99 <- percentiles[
    ,
    .(
      imputation,
      exposure_stratum,
      percentile = "99th",
      xintercept = pc_99
    )
  ]

  percentiles_long <- rbind(
    percentiles_1,
    percentiles_99
  )

  percentiles_long[
    ,
    group := fifelse(
      exposure_stratum == "1",
      paste0(percentile, " percentile for exposure"),
      paste0(percentile, " percentile for comparator")
    )
  ]

  percentiles_long[
    ,
    imputation_fac := factor(
      imputation,
      levels = 1:10,
      labels = paste0("Imputation ", 1:10)
    )
  ]


  #########################################
  # plot

  width <- 0.05

  plot <- ggplot(
    input,
    aes(
      x = bin,
      y = Frequency,
      fill = exposure_fac
    )
  ) +
    geom_col(
      position = "identity",
      width = width,
      alpha = 0.65
    ) +
    geom_vline(
      data = percentiles_long,
      aes(
        xintercept = xintercept,
        color = group,
        linetype = group
      ),
      linewidth = 0.7,
      show.legend = TRUE,
      inherit.aes = FALSE
    ) +
    facet_wrap(
      ~ imputation_fac,
      ncol = 5
    ) +
    scale_fill_manual(
      values = c(
        "Comparator" = "skyblue",
        "Exposure" = "gold"
      )
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "1st percentile for exposure" = "goldenrod",
        "99th percentile for exposure" = "goldenrod",
        "1st percentile for comparator" = "steelblue",
        "99th percentile for comparator" = "steelblue"
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c(
        "1st percentile for exposure" = "dotted",
        "99th percentile for exposure" = "dotted",
        "1st percentile for comparator" = "dashed",
        "99th percentile for comparator" = "dashed"
      )
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1)
    ) +
    labs(
      title = "Propensity Score Overlap Across HbA1c Imputations",
      x = "Propensity Score",
      y = "Frequency",
      fill = NULL
    ) +
    theme_minimal() +
    guides(
      fill = guide_legend(
        reverse = TRUE,
        order = 1
      ),
      color = guide_legend(
        order = 2
      ),
      linetype = guide_legend(
        order = 2
      )
    ) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 18
      ),
      strip.text = element_text(
        size = 10,
        face = "bold"
      ),
      legend.position = "bottom",
      legend.box = "vertical",
      panel.spacing = unit(1, "lines")
    )


  #########################################
  # save

  ggsave(
    filename = paste0(
      "D6_Supplementary_Figure_2_MICE",
      suffix,
      ".pdf"
    ),
    plot = plot,
    path = thisdiroutput,
    width = 16,
    height = 8
  )
}

#########################################
