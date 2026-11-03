############################################################
#                                                          #
####         Create_D6_table_S3                         ####
#                                                          #
############################################################

# Purpose:
# Create formatted Table S3 from D5_table_S3.rds.

print("CREATE D6_table_S3")

#########################################
# assign directories

if (TEST) {
  
  testname <- "test_23_D6_table_S3"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(dirtest, testname, "g_output")
  thisdirsettings <- file.path(dirtest, testname)
  
  dir.create(thisdiroutput, showWarnings = FALSE)
  
} else {
  
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
  thisdirsettings <- dirpargen
}

#########################################
# load settings

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

#########################################
# helper functions

fmt_num <- function(x, digits = 3) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return("NA")
  }
  
  formatC(as.numeric(x), digits = digits, format = "f")
}

fmt_mean_sd <- function(mean_value, sd_value, digits = 3) {
  
  if (is.na(mean_value)) {
    return("NA")
  }
  
  paste0(
    fmt_num(mean_value, digits),
    " (",
    fmt_num(sd_value, digits),
    ")"
  )
}

fmt_range <- function(x1, x2, digits = 3) {
  
  if (is.na(x1) & is.na(x2)) {
    return("NA")
  }
  
  paste0(
    ifelse(is.na(x1), "NA", fmt_num(x1, digits)),
    " – ",
    ifelse(is.na(x2), "NA", fmt_num(x2, digits))
  )
}

format_section <- function(dt, exposure_label_value, section_value) {
  
  row <- dt[
    exposure_label == exposure_label_value &
      section == section_value
  ]
  
  if (nrow(row) == 0) {
    return(rep("NA", 6))
  }
  
  c(
    fmt_mean_sd(row$mean, row$sd),
    fmt_range(row$q1, row$q3),
    fmt_num(row$median),
    fmt_range(row$min, row$max),
    fmt_range(row$p1, row$p99),
    if (section_value == "Propensity score") {
      fmt_range(row$ps_cut_lower, row$ps_cut_upper)
    } else {
      fmt_range(row$weight_cut_lower, row$weight_cut_upper)
    }
  )
}

#########################################
# main loop

subpop <- "all"

for (subpop in thissubpopulations) {
  
  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)
  
  #########################################
  # load intermediate dataset
  
  D5_table_S3 <- readRDS(
    file.path(
      thisdirinput,
      paste0("D5_table_S3", suffix, ".rds")
    )
  )
  
  D5_table_S3 <- as.data.table(D5_table_S3)
  
  #########################################
  # exposure columns
  
  exposure_columns <- unique(D5_table_S3$exposure_label)
  
  preferred_exposure_columns <- c("Dapagliflozin", "Dpp4-i")
  
  exposure_columns <- c(
    preferred_exposure_columns[
      preferred_exposure_columns %in% exposure_columns
    ],
    setdiff(exposure_columns, preferred_exposure_columns)
  )
  
  #########################################
  # create table shell
  
  D6_table_S3 <- data.table(
    Characteristic = c(
      "Propensity score",
      "  Mean (SD)",
      "  Q1 – Q3",
      "  Median",
      "  Min – Max",
      "  P1 – P99",
      "  If truncated, cut-offs",
      "IPTW",
      "  Mean (SD)",
      "  Q1 – Q3",
      "  Median",
      "  Min – Max",
      "  P1 – P99",
      "  If truncated, cut-offs"
    )
  )
  
  #########################################
  # fill table
  
  for (exposure_label_i in exposure_columns) {
    
    ps_values <- format_section(
      D5_table_S3,
      exposure_label_i,
      "Propensity score"
    )
    
    iptw_values <- format_section(
      D5_table_S3,
      exposure_label_i,
      "IPTW"
    )
    
    D6_table_S3[
      ,
      (exposure_label_i) := c(
        "",
        ps_values[1],
        ps_values[2],
        ps_values[3],
        ps_values[4],
        ps_values[5],
        ps_values[6],
        "",
        iptw_values[1],
        iptw_values[2],
        iptw_values[3],
        iptw_values[4],
        iptw_values[5],
        iptw_values[6]
      )
    ]
  }
  
  #########################################
  # order columns
  
  preferred_cols <- c(
    "Characteristic",
    "Dapagliflozin",
    "Dpp4-i"
  )
  
  available_preferred_cols <- intersect(preferred_cols, names(D6_table_S3))
  other_cols <- setdiff(names(D6_table_S3), available_preferred_cols)
  
  setcolorder(D6_table_S3, c(available_preferred_cols, other_cols))
  
  #########################################
  # save
  
  outputfile <- D6_table_S3
  nameoutput <- paste0("D6_table_S3", suffix)
  assign(nameoutput, outputfile)
  
  saveRDS(outputfile,file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
  fwrite(outputfile,file = file.path(thisdiroutput, paste0(nameoutput, ".csv")))
}
