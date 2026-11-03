############################################################
#                                                          #
####         Create_D5_table_S3                         ####
#                                                          #
############################################################

# Purpose:
# Create intermediate dataset with all numeric summaries needed
# for Table S3: propensity scores and stabilized IPTW diagnostics.

print("CREATE D5_table_S3")

#########################################
# assign directories

if (TEST) {
  
  testname <- "test_22_D5_table_S3"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(dirtest, testname, "g_output")
  thisdirsettings <- file.path(dirtest, testname)
  thisdirchoice <- thisdirinput

  dir.create(thisdiroutput, showWarnings = FALSE)
  
} else {
  
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirchoice <- dirparchoice
}

#########################################
# load settings

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

deap <- unique(settings$ds)[1]

#########################################
# helper function

summary_stats <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(data.table(
      n = 0L,
      mean = NA_real_,
      sd = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      median = NA_real_,
      min = NA_real_,
      max = NA_real_,
      p1 = NA_real_,
      p99 = NA_real_
    ))
  }
  
  q <- quantile(
    x,
    probs = c(0.01, 0.25, 0.50, 0.75, 0.99),
    na.rm = TRUE
  )
  
  data.table(
    n = length(x),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    q1 = q[["25%"]],
    q3 = q[["75%"]],
    median = q[["50%"]],
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    p1 = q[["1%"]],
    p99 = q[["99%"]]
  )
}

#########################################
# exposure labels

exposure_labels <- data.table(
  exposure = c(1, 0),
  exposure_label = c("Dapagliflozin", "Dpp4-i")
)

#########################################
# main loop

subpop <- "all"

for (subpop in thissubpopulations) {
  
  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)
  
  #########################################
  # load input
  
  input <- readRDS(
    file.path(
      thisdirinput,
      paste0("D4_EXP_ACT_COV_WEIGHT_TRUNCATED", suffix, ".rds")
    )
  )
  
  input <- as.data.table(input)
  
  #########################################
  # checks
  
  required_cols <- c(
    "person_id",
    "t0",
    "exposure",
    "ps",
    "truncated_ps",
    "weight",
    "truncated_weight"
  )
  
  missing_cols <- setdiff(required_cols, names(input))
  
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "The following required columns are missing from D4_EXP_ACT_COV_WEIGHT_TRUNCATED: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  #########################################
  # analysis population
  # Subjects outside the PS overlap region have truncated_ps = 0
  
  analysis_dt <- input[truncated_ps > 0]
  
  #########################################
  # load trimming choices
  
  PAR_CHOICE_TRUNCATION <- fread(
    file.path(thisdirchoice, "PAR_CHOICE_TRUNCATION.csv")
  )
  
  PAR_CHOICE_TRUNCATION <- PAR_CHOICE_TRUNCATION[ds == deap]
  
  trunc_par_exposed <- PAR_CHOICE_TRUNCATION[exposure == 1]
  
  if (nrow(trunc_par_exposed) == 0) {
    stop("No row found for exposure == 1 in PAR_CHOICE_TRUNCATION")
  }
  
  if (nrow(trunc_par_exposed) > 1) {
    stop("More than one row found for exposure == 1 in PAR_CHOICE_TRUNCATION")
  }
  
  #########################################
  # PS trimming cut-offs
  
  ps_cut_lower <- NA_real_
  
  if (!is.na(trunc_par_exposed$lower_limit)) {
    
    ps_cut_lower <- quantile(
      input[exposure == 1, ps],
      probs = trunc_par_exposed$lower_limit / 100,
      na.rm = TRUE
    )
  }
  
  ps_cut_upper <- NA_real_
  
  if (!is.na(trunc_par_exposed$upper_limit)) {
    
    ps_cut_upper <- quantile(
      input[exposure == 1, ps],
      probs = trunc_par_exposed$upper_limit / 100,
      na.rm = TRUE
    )
  }
  
  #########################################
  # IPTW truncation cut-offs
  # weight = recalculated IPTW before truncation
  # truncated_weight = final truncated IPTW
  
  weight_cutoffs <- quantile(
    analysis_dt$weight,
    probs = c(0.01, 0.99),
    na.rm = TRUE
  )
  
  weight_cut_lower <- weight_cutoffs[["1%"]]
  weight_cut_upper <- weight_cutoffs[["99%"]]
  
  #########################################
  # create intermediate numeric dataset
  
  D5_table_S3 <- rbindlist(
    lapply(exposure_labels$exposure, function(a) {
      
      label <- exposure_labels[exposure == a, exposure_label]
      
      dt_a <- analysis_dt[exposure == a]
      
      ps_stats <- summary_stats(dt_a$ps)
      iptw_stats <- summary_stats(dt_a$truncated_weight)
      
      rbind(
        data.table(
          exposure = a,
          exposure_label = label,
          section = "Propensity score",
          variable = "ps",
          ps_cut_lower = ps_cut_lower,
          ps_cut_upper = ps_cut_upper,
          weight_cut_lower = NA_real_,
          weight_cut_upper = NA_real_
        ),
        data.table(
          exposure = a,
          exposure_label = label,
          section = "IPTW",
          variable = "truncated_weight",
          ps_cut_lower = NA_real_,
          ps_cut_upper = NA_real_,
          weight_cut_lower = weight_cut_lower,
          weight_cut_upper = weight_cut_upper
        )
      )[
        ,
        cbind(
          .SD,
          rbind(ps_stats, iptw_stats)
        )
      ]
    }),
    fill = TRUE
  )
  
  #########################################
  # add population counts
  
  D5_counts <- input[
    ,
    .(
      n_total = .N,
      n_in_overlap = sum(truncated_ps > 0, na.rm = TRUE),
      n_outside_overlap = sum(truncated_ps == 0, na.rm = TRUE)
    )
  ]
  
  D5_table_S3[, `:=`(
    n_total = D5_counts$n_total,
    n_in_overlap = D5_counts$n_in_overlap,
    n_outside_overlap = D5_counts$n_outside_overlap
  )]
  
  #########################################
  # save
  
  saveRDS(
    D5_table_S3,
    file.path(thisdiroutput, paste0("D5_table_S3", suffix, ".rds"))
  )
  
  # fwrite(
  #   D5_table_S3,
  #   file.path(thisdiroutput, paste0("D5_table_S3", suffix, ".csv"))
  # )
}
