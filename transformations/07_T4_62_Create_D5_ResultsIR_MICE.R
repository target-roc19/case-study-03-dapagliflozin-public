############################################################
#                                                          #
#              Create D5_ResultsIR_HR_MICE                 #
#                                                          #
############################################################

# authors: Azzurra Bongianni, Rosa Gini
# adapted for MICE pooled Cox analysis

# v 1.1 09 Sep 2026

print("CREATE D5_ResultsIR_HR_MICE")

#########################################
# assign directories

if (TEST) {
  # testname <- "test_D5_ResultsIR_HR_MICE"
  # thisdirinput <- file.path(dirtest, testname)
  # thisdiroutput <- file.path(thisdirinput, "g_output")
  # thisdirexp <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = FALSE)
} else {
  thisdirinput <- dirtemp
  thisdiroutput <- diroutput
  thisdirexp <- direxp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

to_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

#########################################
# loop over subpopulations

subpop <- "all"

for (subpop in thissubpopulations) {

  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)

  #########################################
  # load analytical dataset

  analytical_dataset <- readRDS(
    file.path(thisdirinput, paste0("D4_ANALYTICAL_DATASET_HR_MICE", suffix, ".rds"))
  )
  setDT(analytical_dataset)

  #########################################
  # required variables

  weight_cols <- paste0("truncated_weight_", 1:10)
  required_variables <- c("exposure", "E1_fup", "E1_outcome", weight_cols)
  missing_variables <- setdiff(required_variables, names(analytical_dataset))

  if (length(missing_variables) > 0L) {
    stop(paste0(
      "[D5_ResultsIR_HR_MICE] Missing variable(s): ",
      paste(missing_variables, collapse = ", ")
    ))
  }

  #########################################
  # fit one weighted robust Cox model
  # for each HbA1c imputation

  m <- 10L
  fitlist <- vector("list", m)
  beta <- rep(NA_real_, m)
  robust_se <- rep(NA_real_, m)
  naive_se <- rep(NA_real_, m)
  n_used <- integer(m)
  n_events <- integer(m)

  for (j in 1:m) {

    print(paste0("Fitting MICE Cox model ", j, "/", m))

    weight_col <- paste0("truncated_weight_", j)

    input <- analytical_dataset[, .(
      exposure = to_numeric(exposure),
      FUP = to_numeric(E1_fup),
      outcome = to_numeric(E1_outcome),
      weight = to_numeric(get(weight_col))
    )]

    input <- input[
      !is.na(exposure) & exposure %in% c(0, 1) &
      !is.na(FUP) & is.finite(FUP) & FUP > 0 &
      !is.na(outcome) & outcome %in% c(0, 1) &
      !is.na(weight) & is.finite(weight) & weight > 0
    ]

    if (nrow(input) == 0L) stop(paste0("No valid observations available for imputation ", j))

    if (!all(c(0, 1) %in% unique(input$exposure))) {
      stop(paste0("Both exposure groups are not present in imputation ", j))
    }

    events_by_group <- input[, .(events = sum(outcome == 1, na.rm = TRUE)), by = exposure]

    if (any(events_by_group$events <= 0)) {
      stop(paste0("At least one exposure group has no events in imputation ", j))
    }

    fitlist[[j]] <- survival::coxph(
      survival::Surv(FUP, outcome) ~ exposure,
      data = input,
      weights = weight,
      robust = TRUE
    )

    beta[j] <- as.numeric(coef(fitlist[[j]])["exposure"])
    robust_se[j] <- sqrt(diag(vcov(fitlist[[j]])))["exposure"]

    if (!is.null(fitlist[[j]]$naive.var)) {
      naive_se[j] <- sqrt(diag(fitlist[[j]]$naive.var))["exposure"]
    }

    n_used[j] <- nrow(input)
    n_events[j] <- sum(input$outcome == 1, na.rm = TRUE)
  }

  #########################################
  # robust SE diagnostics
  #
  # Do not rely on broom::tidy() for pooling because
  # std.error may be model-based while robust.se is separate.
  # Rubin pooling is therefore performed manually using
  # the sandwich variance from vcov(coxph).

  se_check <- data.table(
    imputation = 1:m,
    logHR = beta,
    robust_se = robust_se,
    naive_se = naive_se,
    N = n_used,
    N_events = n_events
  )

  if (any(!is.finite(beta)) || any(!is.finite(robust_se)) || any(robust_se <= 0)) {
    stop("At least one imputation produced an invalid coefficient or robust SE.")
  }

  #########################################
  # Rubin pooling on the log-HR scale

  Qbar <- mean(beta)
  U <- robust_se^2
  Ubar <- mean(U)
  B <- stats::var(beta)
  total_variance <- Ubar + (1 + 1 / m) * B
  pooled_se <- sqrt(total_variance)

  #########################################
  # degrees of freedom
  #
  # Use Rubin large-sample df when B > 0.
  # If between-imputation variance is 0,
  # the pooled estimate reduces to the within-imputation result.

  if (is.finite(B) && B > 0) {
    r <- ((1 + 1 / m) * B) / Ubar
    df_old <- (m - 1) * (1 + 1 / r)^2

    dfcom <- min(n_events) - 1L
    lambda <- ((1 + 1 / m) * B) / total_variance
    df_obs <- ((dfcom + 1) / (dfcom + 3)) * dfcom * (1 - lambda)
    pooled_df <- 1 / (1 / df_old + 1 / df_obs)
  } else {
    dfcom <- min(n_events) - 1L
    pooled_df <- dfcom
  }

  if (!is.finite(pooled_df) || pooled_df <= 0) {
    stop(paste0("Invalid pooled degrees of freedom: ", pooled_df))
  }

  #########################################
  # pooled 95% confidence interval

  crit <- stats::qt(0.975, df = pooled_df)
  logHR_low <- Qbar - crit * pooled_se
  logHR_high <- Qbar + crit * pooled_se

  pooled_HR <- exp(Qbar)
  pooled_HR_low <- exp(logHR_low)
  pooled_HR_high <- exp(logHR_high)

  #########################################
  # small final table

  outputfile <- data.table(
    ds = paste0(settings$ds[1L], suffix),
    HR = round(pooled_HR, 3),
    HR_low = round(pooled_HR_low, 3),
    HR_high = round(pooled_HR_high, 3)
  )


  #########################################
  # save final pooled table

  nameoutput <- paste0("D5_ResultsIR_HR_MICE", suffix)
  assign(nameoutput, outputfile)

  fwrite(
    outputfile,
    file = file.path(thisdirexp, paste0(nameoutput, ".csv"))
  )

  saveRDS(
    outputfile,
    file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
  )

  print(outputfile)
}

#########################################
