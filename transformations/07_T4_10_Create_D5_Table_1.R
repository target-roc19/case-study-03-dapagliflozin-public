############################################################
#                                                          #
#######   07_T4_11_Create_D5_Table_1_and_Figure_1    #######
#                                                          #
############################################################

print("CREATE D5_Table_1")

#########################################
# assign directories

if (TEST) {
  testname <- "test_09_Flowchart"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(thisdirinput, "g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = FALSE)
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  # thisdirexp <- direxp
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

for (subpop in thissubpopulations) {

  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)

  #########################################
  # load input datasets

  eligibility <- readRDS(
    file.path(thisdirinput, paste0("D3_ELIGIBILITY", suffix, ".rds"))
  )

  pre_fc <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D3_selection_criteria_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART",
        suffix,
        ".rds"
      )
    )
  )

  setDT(eligibility)
  setDT(pre_fc)

  #########################################
  # checks: both inputs must be at person level

  if (anyDuplicated(pre_fc$person_id)) {
    stop("pre_fc contains more than one row per person_id.")
  }

  if (anyDuplicated(eligibility$person_id)) {
    stop("eligibility contains more than one row per person_id.")
  }

  #########################################
  # selection criteria
  #
  # IMPORTANT:
  # The order below is aligned with table_criteria_names.
  # Output labels and column names are intentionally unchanged.

  pre_fc_criteria <- c(
    "sel_sex_missing",
    "sel_partial_date_of_death",
    "sel_no_spells"
  )

  eligibility_criteria <- c(
    "sel_not_valid_birth_date",
    "sel_no_HES_linkage",
    "sel_no_spells_in_enrolment_period",
    "sel_with_no_episode_in_eligibility_period",
    "sel_not_enough_lookback",
    "sel_T1D_diagnosis",
    "sel_no_too_young",
    "sel_no_T2D_diagnosis",
    "sel_no_ACVD_or_risk_of_ACVD",
    "sel_prevalent_SGLT2I_or_DPP4",
    "sel_acute_CV_history"
  )

  sel_criteria <- c(pre_fc_criteria, eligibility_criteria)

  table_criteria_names <- c(
    "no_sex_missing",
    "complete_date_of_death",
    "at_least_one_spell",
    "valid_birth_date",
    "HES_linkage",
    "spells_in_enrolment_period",
    "with_episode_in_eligibility_period",
    "enough_lookback",
    "no_T1D_diagnosis",
    "too_young",
    "T2D_diagnosis",
    "ACVD_or_high_risk_ACVD",
    "prevalent_SGLT2I_or_DPP4",
    "no_acute_CV_history"
  )

  missing_pre_fc <- setdiff(
    c("person_id", pre_fc_criteria),
    names(pre_fc)
  )

  if (length(missing_pre_fc) > 0L) {
    stop(
      "Missing columns in pre_fc: ",
      paste(missing_pre_fc, collapse = ", ")
    )
  }

  missing_eligibility <- setdiff(
    c("person_id", "exposure_episode", eligibility_criteria),
    names(eligibility)
  )

  if (length(missing_eligibility) > 0L) {
    stop(
      "Missing columns in eligibility: ",
      paste(missing_eligibility, collapse = ", ")
    )
  }

  #########################################
  # starting counts

  N_entering_ds <- pre_fc[, uniqueN(person_id)]
  N_entering_flowchart <- eligibility[, uniqueN(person_id)]

  #########################################
  # initialise output vectors

  surviving_counts <- integer(length(sel_criteria))
  excluded_counts <- integer(length(sel_criteria))

  #########################################
  # first part of flowchart: use pre_fc only

  pre_processing <- copy(
    pre_fc[, c("person_id", pre_fc_criteria), with = FALSE]
  )

  pre_processing[, still_in_study := TRUE]

  for (i in seq_along(pre_fc_criteria)) {

    var <- pre_fc_criteria[i]

    n_before <- pre_processing[
      still_in_study == TRUE,
      uniqueN(person_id)
    ]

    excluded_counts[i] <- pre_processing[
      still_in_study == TRUE & !is.na(get(var)) & get(var) == 1,
      uniqueN(person_id)
    ]

    pre_processing[
      still_in_study == TRUE & !is.na(get(var)) & get(var) == 1,
      still_in_study := FALSE
    ]

    surviving_counts[i] <- pre_processing[
      still_in_study == TRUE,
      uniqueN(person_id)
    ]

    if (n_before - excluded_counts[i] != surviving_counts[i]) {
      stop(
        "Flowchart reconciliation failed for ", var, ": ",
        n_before, " - ", excluded_counts[i],
        " != ", surviving_counts[i]
      )
    }
  }

  #########################################
  # second part of flowchart: use eligibility only
  #
  # D3_ELIGIBILITY is the population entering this section.
  # This avoids carrying forward people present only in pre_fc,
  # which previously caused the counts to increase after
  # N_entering_flowchart.

  eligibility_processing <- copy(
    eligibility[
      ,
      c("person_id", "exposure_episode", eligibility_criteria),
      with = FALSE
    ]
  )

  eligibility_processing[, still_in_study := TRUE]

  offset <- length(pre_fc_criteria)

  for (j in seq_along(eligibility_criteria)) {

    var <- eligibility_criteria[j]
    i <- offset + j

    n_before <- eligibility_processing[
      still_in_study == TRUE,
      uniqueN(person_id)
    ]

    excluded_counts[i] <- eligibility_processing[
      still_in_study == TRUE & !is.na(get(var)) & get(var) == 1,
      uniqueN(person_id)
    ]

    eligibility_processing[
      still_in_study == TRUE & !is.na(get(var)) & get(var) == 1,
      still_in_study := FALSE
    ]

    surviving_counts[i] <- eligibility_processing[
      still_in_study == TRUE,
      uniqueN(person_id)
    ]

    if (n_before - excluded_counts[i] != surviving_counts[i]) {
      stop(
        "Flowchart reconciliation failed for ", var, ": ",
        n_before, " - ", excluded_counts[i],
        " != ", surviving_counts[i]
      )
    }
  }

  #########################################
  # assign output names

  names(surviving_counts) <- paste0("N_", table_criteria_names)
  names(excluded_counts) <- paste0(
    "N_excluded_",
    sub("^sel_", "", sel_criteria)
  )
  names(N_entering_ds) <- "N_entering_ds"

  excluded_counts_with_total <- c(N_entering_ds, excluded_counts)
  dt_excluded <- as.data.table(as.list(excluded_counts_with_total))

  #########################################
  # eligible counts

  N_entering_study <- eligibility_processing[
    still_in_study == TRUE,
    uniqueN(person_id)
  ]

  N_eligible_dapagliflozin <- eligibility_processing[
    still_in_study == TRUE & exposure_episode == 1,
    uniqueN(person_id)
  ]

  N_eligible_Dpp4i <- eligibility_processing[
    still_in_study == TRUE & exposure_episode == 0,
    uniqueN(person_id)
  ]

  N_eligible_missing_exposure <- eligibility_processing[
    still_in_study == TRUE & is.na(exposure_episode),
    uniqueN(person_id)
  ]

  N_eligible_other_exposure <- eligibility_processing[
    still_in_study == TRUE &
      !is.na(exposure_episode) &
      !exposure_episode %in% c(0, 1),
    uniqueN(person_id)
  ]

  if (N_eligible_missing_exposure > 0L || N_eligible_other_exposure > 0L) {
    stop(
      "The final eligible cohort contains ",
      N_eligible_missing_exposure,
      " person(s) with missing exposure_episode and ",
      N_eligible_other_exposure,
      " person(s) with exposure_episode other than 0 or 1."
    )
  }

  if (
    N_entering_study !=
      N_eligible_dapagliflozin + N_eligible_Dpp4i
  ) {
    stop(
      "Eligible cohort does not reconcile: N_entering_study = ",
      N_entering_study,
      ", while N_eligible_dapagliflozin + N_eligible_Dpp4i = ",
      N_eligible_dapagliflozin + N_eligible_Dpp4i,
      "."
    )
  }

  #########################################
  # print reconciliation table

  flowchart_check <- data.table(
    criterion = sel_criteria,
    excluded = as.integer(excluded_counts),
    remaining = as.integer(surviving_counts)
  )

  flowchart_check[
    ,
    entering := c(
      N_entering_ds,
      head(surviving_counts[seq_along(pre_fc_criteria)], -1L),
      N_entering_flowchart,
      head(surviving_counts[(offset + 1L):length(sel_criteria)], -1L)
    )
  ]

  flowchart_check[, expected_remaining := entering - excluded]
  flowchart_check[, difference := remaining - expected_remaining]

  print(flowchart_check)

  if (any(flowchart_check$difference != 0L)) {
    stop("One or more flowchart counts do not reconcile.")
  }

  #########################################
  # create Table 1 - second part

  data <- data.table(
    ds = settings$ds,
    N_entering_ds = N_entering_ds,
    N_entering_flowchart = N_entering_flowchart
  )

  for (i in seq_along(table_criteria_names)) {

    n_col <- paste0("N_", table_criteria_names[i])
    p_col <- paste0("PERCENTAGE_", table_criteria_names[i])

    data[, (n_col) := surviving_counts[i]]
    data[, (p_col) := round((surviving_counts[i] / N_entering_ds) * 100, 2)]
  }

  data[, N_entering_study := N_entering_study]
  # data[, PERCENTAGE_entering_study :=
  #        round(N_entering_study / N_entering_flowchart * 100, 2)]

  data[, N_eligible_dapagliflozin := N_eligible_dapagliflozin]
  # data[, PERCENTAGE_eligible_dapagliflozin :=
  #        round(N_eligible_dapagliflozin / N_entering_study * 100, 2)]

  data[, N_eligible_Dpp4i := N_eligible_Dpp4i]
  # data[, PERCENTAGE_eligible_Dpp4i :=
  #        round(N_eligible_Dpp4i / N_entering_study * 100, 2)]

  #########################################
  # order columns

  n_cols <- grep("^N_", names(data), value = TRUE)

  col_order <- c(
    "ds",
    "N_entering_ds",
    unlist(
      lapply(
        setdiff(n_cols, c("ds", "N_entering_ds")),
        \(x) c(x, sub("^N_", "PERCENTAGE_", x))
      )
    )
  )

  data[, PERCENTAGE_entering_flowchart :=
         round(N_entering_flowchart / N_entering_ds * 100, 2)]

  processing <- data[, intersect(col_order, names(data)), with = FALSE]

  setcolorder(
    processing,
    c("N_entering_flowchart", "PERCENTAGE_entering_flowchart"),
    after = "PERCENTAGE_at_least_one_spell"
  )

  #########################################
  # save Table 1

  nameoutput <- paste0("D5_Table_1", suffix)
  assign(nameoutput, processing)

  saveRDS(
    processing,
    file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
  )

  #########################################
  # save diagnostics table

  print("D5_diagnostics_flowchart_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART")

  nameoutput <- paste0(
    "D5_diagnostics_flowchart_from_D3_PERSONS_to_D3_ENTRY_FLOWCHART",
    suffix
  )

  assign(nameoutput, processing[, 1:9])

  # fwrite(
  #   processing[, 1:9],
  #   file = file.path(thisdirexp, paste0(nameoutput, ".csv"))
  # )

  saveRDS(
    processing[, 1:9],
    file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
  )

  #########################################
  # save counts for Figure 1

  print("Save counts for Figure 1")

  nameoutput <- paste0("excluded_counts", suffix)
  assign(nameoutput, dt_excluded)

  saveRDS(
    dt_excluded,
    file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
  )
}
