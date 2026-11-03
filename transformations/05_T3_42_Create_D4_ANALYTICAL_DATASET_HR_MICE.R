############################################################
#                                                          #
####     CREATE D4_ANALYTICAL_DATASET_HR_MICE            ####
#                                                          #
############################################################

# author: Rosa Gini
# adapted for MICE propensity-score analysis

# v 1.0 09 Sep 2026


#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_D4_ANALYTICAL_DATASET_HR_MICE"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}


#########################################
# load parameters

print('CREATE D4_ANALYTICAL_DATASET_HR_MICE')

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))

thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


#########################################
# loop over subpopulations

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {

  suffix <- fifelse(
    subpop == "all",
    "",
    paste0("_",subpop)
  )

  print(suffix)


  #########################################
  # load input datasets

  D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED",
        suffix,
        ".rds"
      )
    )
  )

  D4_FUP <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_FUP",
        suffix,
        ".rds"
      )
    )
  )

  D3_OUTCOMES <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D3_OUTCOMES",
        suffix,
        ".rds"
      )
    )
  )

  D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED <- as.data.table(
    D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED
  )

  D4_FUP <- as.data.table(D4_FUP)
  D3_OUTCOMES <- as.data.table(D3_OUTCOMES)


  #########################################
  # check identifiers

  required_ids <- c("person_id")

  missing_weight_ids <- setdiff(
    required_ids,
    names(D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED)
  )

  missing_fup_ids <- setdiff(
    required_ids,
    names(D4_FUP)
  )

  if (length(missing_weight_ids) > 0) {
    stop(
      paste0(
        "Missing identifier(s) in D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED: ",
        paste(missing_weight_ids, collapse = ", ")
      )
    )
  }

  if (length(missing_fup_ids) > 0) {
    stop(
      paste0(
        "Missing identifier(s) in D4_FUP: ",
        paste(missing_fup_ids, collapse = ", ")
      )
    )
  }


  #########################################
  # merge outcome information if needed
  #
  # D4_FUP in the original pipeline normally already contains outcome_date.
  # D3_OUTCOMES is nevertheless used explicitly here as requested.
  # If outcome_date is absent from D4_FUP, recover it from D3_OUTCOMES.

  if (!"outcome_date" %in% names(D4_FUP)) {

    if (!"outcome_date" %in% names(D3_OUTCOMES)) {
      stop(
        "outcome_date is missing from both D4_FUP and D3_OUTCOMES."
      )
    }

    outcomes_to_merge <- unique(
      D3_OUTCOMES[
        ,
        .(
          person_id,
          outcome_date
        )
      ]
    )

    D4_FUP <- merge(
      D4_FUP,
      outcomes_to_merge,
      by = "person_id",
      all.x = TRUE,
      sort = FALSE
    )
  }


  #########################################
  # create Estimand 1 outcome and follow-up only

  required_E1_cols <- c(
    "person_id",
    "exposure",
    "outcome_date",
    "censoring_date_E1",
    "fup_days_E1"
  )

  missing_E1_cols <- setdiff(
    required_E1_cols,
    names(D4_FUP)
  )

  if (length(missing_E1_cols) > 0) {
    stop(
      paste0(
        "The following Estimand 1 columns are missing from D4_FUP: ",
        paste(missing_E1_cols, collapse = ", ")
      )
    )
  }

  processing <- copy(D4_FUP)

  processing[
    ,
    E1_outcome := fifelse(
      !is.na(outcome_date) &
        outcome_date <= censoring_date_E1,
      1L,
      0L
    )
  ]

  processing[
    ,
    E1_fup := as.numeric(fup_days_E1)
  ]


  #########################################
  # keep only Estimand 1 analysis variables
  # before merging PS / weights

  fup_keep <- c(
    "person_id",
    "exposure",
    "E1_outcome",
    "E1_fup"
  )

  # Keep t0 as merge key when available in both datasets
  merge_keys <- "person_id"

  if (
    "t0" %in% names(processing) &&
    "t0" %in% names(D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED)
  ) {
    fup_keep <- c(
      "person_id",
      "t0",
      "exposure",
      "E1_outcome",
      "E1_fup"
    )

    merge_keys <- c(
      "person_id",
      "t0"
    )
  }

  processing <- processing[
    ,
    ..fup_keep
  ]


  #########################################
  # select MICE PS and weight variables

  ps_cols <- paste0(
    "ps_",
    1:10
  )

  truncated_ps_cols <- paste0(
    "truncated_ps_",
    1:10
  )

  weight_cols <- paste0(
    "weight_",
    1:10
  )

  truncated_weight_cols <- paste0(
    "truncated_weight_",
    1:10
  )

  required_mice_cols <- c(
    merge_keys,
    "exposure",
    ps_cols,
    truncated_ps_cols,
    weight_cols,
    truncated_weight_cols
  )

  missing_mice_cols <- setdiff(
    required_mice_cols,
    names(D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED)
  )

  if (length(missing_mice_cols) > 0) {
    stop(
      paste0(
        "The following MICE variables are missing from ",
        "D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED: ",
        paste(missing_mice_cols, collapse = ", ")
      )
    )
  }

  mice_weights <- D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED[
    ,
    ..required_mice_cols
  ]


  #########################################
  # avoid duplicate exposure column during merge

  mice_weights[
    ,
    exposure_weight := exposure
  ]

  mice_weights[
    ,
    exposure := NULL
  ]


  #########################################
  # merge Estimand 1 follow-up with MICE PS/weights

  processing <- merge(
    processing,
    mice_weights,
    by = merge_keys,
    all = FALSE,
    sort = FALSE
  )


  #########################################
  # check exposure consistency

  inconsistent_exposure <- processing[
    !is.na(exposure) &
      !is.na(exposure_weight) &
      exposure != exposure_weight
  ]

  if (nrow(inconsistent_exposure) > 0) {
    stop(
      paste0(
        "Exposure differs between D4_FUP and ",
        "D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED for ",
        nrow(inconsistent_exposure),
        " row(s)."
      )
    )
  }

  processing[
    ,
    exposure_weight := NULL
  ]


  #########################################
  # final column order

  tokeep <- c(
    "person_id",
    if ("t0" %in% names(processing)) "t0",
    "exposure",
    "E1_outcome",
    "E1_fup",
    ps_cols,
    truncated_ps_cols,
    weight_cols,
    truncated_weight_cols
  )

  processing <- processing[
    ,
    ..tokeep
  ]


  #########################################
  # save

  outputfile <- processing

  nameoutput <- paste0(
    "D4_ANALYTICAL_DATASET_HR_MICE"
  )

  nameoutputext <- paste0(
    nameoutput,
    suffix,
    ".rds"
  )

  assign(
    nameoutput,
    outputfile
  )

  saveRDS(
    outputfile,
    file = file.path(
      thisdiroutput,
      nameoutputext
    )
  )
}

#########################################
