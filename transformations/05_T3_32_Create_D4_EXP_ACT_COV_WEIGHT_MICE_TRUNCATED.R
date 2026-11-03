############################################################
#                                                          #
####   Create_D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED       ####
#                                                          #
############################################################

# authors: Sofia Miniati, Paoletti Olga, Rosa Gini, Azzurra Bongianni
# adapted for 10 MICE propensity-score imputations

# v 1.0 09 Sep 2026

print("CREATE D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED")


#########################################
# assign directories and other parameters used in this step

if (TEST) {
  # testname <- "test_D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED"
  # thisdirinput <- file.path(dirtest, testname)
  # thisdiroutput <- file.path(thisdirinput, "g_output")
  # thisdirsettings <- thisdirinput
  # thisdirchoice <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = FALSE)
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirchoice <- dirparchoice
}


#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

deap <- unique(settings$ds)[1]

# Use exactly the same truncation choices as the main analysis
PAR_CHOICE_TRUNCATION <- fread(
  file = file.path(thisdirchoice, "PAR_CHOICE_TRUNCATION.csv")
)

PAR_CHOICE_TRUNCATION <- PAR_CHOICE_TRUNCATION[ds == deap, ]

if (nrow(PAR_CHOICE_TRUNCATION) == 0) {
  stop(paste0("No truncation parameters found for datasource: ", deap))
}

trunc_par <- unique(
  PAR_CHOICE_TRUNCATION[, .(lower_limit, upper_limit)]
)

if (nrow(trunc_par) != 1) {
  stop(
    paste0(
      "Expected one unique pair of lower_limit/upper_limit for datasource ",
      deap,
      ", but found ",
      nrow(trunc_par)
    )
  )
}

ps_lower <- trunc_par$lower_limit
ps_upper <- trunc_par$upper_limit

print(
  paste0(
    "PS truncation limits for ", deap,
    ": lower = ", ps_lower,
    ", upper = ", ps_upper
  )
)


#########################################
# loop over subpopulations

subpop <- "all"

for (subpop in thissubpopulations) {

  suffix <- fifelse(
    subpop == "all",
    "",
    paste0("_", subpop)
  )

  print(suffix)


  #########################################
  # load input dataset

  input <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_EXP_ACT_COV_WEIGHT_MICE",
        suffix,
        ".rds"
      )
    )
  )

  input <- as.data.table(input)


  #########################################
  # keep original columns

  cols_to_keep <- colnames(input)


  #########################################
  # check required columns

  ps_cols <- paste0("ps_", 1:10)

  required_cols <- c(
    "person_id",
    "t0",
    "exposure",
    ps_cols
  )

  missing_cols <- setdiff(
    required_cols,
    names(input)
  )

  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "The following required columns are missing from input: %s",
        paste(missing_cols, collapse = ", ")
      )
    )
  }


  #########################################
  # apply main-analysis truncation separately
  # to each MICE propensity score

  for (i in 1:10) {

    print(
      paste0(
        "Applying PS trimming/truncation to imputation ",
        i
      )
    )

    ps_col <- paste0("ps_", i)
    weight_col <- paste0("weight_", i)
    truncated_ps_col <- paste0("truncated_ps_", i)
    truncated_weight_col <- paste0("truncated_weight_", i)
    overlap_col <- paste0("in_overlap_tmp_", i)


    #########################################
    # identify subjects inside PS limits

    input[, (overlap_col) := 1L]

    if (!is.na(ps_lower)) {
      input[
        !is.na(get(ps_col)) & get(ps_col) < ps_lower,
        (overlap_col) := 0L
      ]
    }

    if (!is.na(ps_upper)) {
      input[
        !is.na(get(ps_col)) & get(ps_col) > ps_upper,
        (overlap_col) := 0L
      ]
    }

    input[
      is.na(get(ps_col)),
      (overlap_col) := 0L
    ]


    #########################################
    # create truncated PS

    input[
      ,
      (truncated_ps_col) := fifelse(
        get(overlap_col) == 1L,
        get(ps_col),
        0
      )
    ]


    #########################################
    # recalculate stabilized IPTW among
    # retained subjects for this imputation

    input[, (weight_col) := 0]

    n_retained <- input[
      get(overlap_col) == 1L,
      .N
    ]

    if (n_retained == 0) {
      stop(
        paste0(
          "No subjects remain after PS trimming for imputation ",
          i,
          "."
        )
      )
    }

    p_treated_trimmed <- input[
      get(overlap_col) == 1L,
      mean(exposure == 1, na.rm = TRUE)
    ]

    if (
      is.na(p_treated_trimmed) ||
      p_treated_trimmed <= 0 ||
      p_treated_trimmed >= 1
    ) {
      stop(
        paste0(
          "Invalid treatment prevalence after PS trimming for imputation ",
          i,
          ": ",
          p_treated_trimmed
        )
      )
    }

    input[
      get(overlap_col) == 1L,
      (weight_col) := fifelse(
        exposure == 1,
        p_treated_trimmed / get(ps_col),
        (1 - p_treated_trimmed) / (1 - get(ps_col))
      )
    ]


    #########################################
    # truncate recalculated IPTW at 1st/99th
    # percentile among retained subjects

    input[, (truncated_weight_col) := 0]

    q_weight <- quantile(
      input[
        get(overlap_col) == 1L,
        get(weight_col)
      ],
      probs = c(0.01, 0.99),
      na.rm = TRUE
    )

    input[
      get(overlap_col) == 1L,
      (truncated_weight_col) := pmin(
        pmax(
          get(weight_col),
          q_weight[1]
        ),
        q_weight[2]
      )
    ]


    #########################################
    # diagnostic message

    n_excluded <- input[
      get(overlap_col) == 0L,
      .N
    ]

    print(
      paste0(
        "Imputation ", i,
        ": retained = ", n_retained,
        "; excluded = ", n_excluded,
        "; p_treated_trimmed = ",
        round(p_treated_trimmed, 6),
        "; weight p1 = ",
        round(q_weight[1], 6),
        "; weight p99 = ",
        round(q_weight[2], 6)
      )
    )
  }


  #########################################
  # cleaning

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

  keep_cols <- unique(
    c(
      cols_to_keep,
      truncated_ps_cols,
      weight_cols,
      truncated_weight_cols
    )
  )

  input <- input[, ..keep_cols]


  #########################################
  # save

  outputfile <- input

  nameoutput <- paste0(
    "D4_EXP_ACT_COV_WEIGHT_MICE_TRUNCATED",
    suffix
  )

  assign(
    nameoutput,
    outputfile
  )

  saveRDS(
    outputfile,
    file = file.path(
      thisdiroutput,
      paste0(
        nameoutput,
        ".rds"
      )
    )
  )
}

#########################################
