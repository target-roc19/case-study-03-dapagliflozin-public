############################################################
#                                                          #
#                 CREATE D5_ResultsRMST                    #
#                                                          #
############################################################

# authors: Azzurra Bongianni, Rosa Gini, Claudia Bartolini
#
# E1:
#   N patients
#   N events
#   Person-years
#   Incidence rate with 95% CI
#
# E3_year3:
#   E3b_outcome and E3b_fup
#   RMST restricted at 3 year
#
# E3_year5:
#   E3a_outcome and E3a_fup
#   RMST restricted at 5 years
#
# E2:
#   N patients
#   N events
#   Person-years
#   Incidence rate with 95% CI
#
# RMST model:
#   Weibull accelerated failure-time model
#
# RMST confidence intervals:
#   1000 stratified non-parametric bootstrap samples
#   Percentile-based 95% confidence intervals
#
# Optimization:
#   - bootstrap samples run in parallel
#   - crude and IPTW models use the same bootstrap sample
#   - model Hessian is not calculated during bootstrap
#   - existing truncated IPTW weights are retained

print("CREATE D5_ResultsRMST")

############################################################

if (TEST) {
  
  # testname <- "test_33_D5_ResultsRMST"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path( thisdirinput, "g_output")
  # thisdirsettings <- thisdirinput
  # dir.create( thisdiroutput,recursive = TRUE, showWarnings = FALSE)
  
} else {
  
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}


############################################################
# 3. SETTINGS
############################################################

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
settings <- as.data.table(settings)

thissubpopulations <- unique(settings[, subpop])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations)," +"))


############################################################
# 4. ANALYSIS DEFINITIONS
############################################################

analysis_definitions <- list(
  
  E1 = list(
    outcome_col = "E1_outcome",
    fup_col = "E1_fup",
    calculate_rmst = FALSE,
    tau = NA_real_,
    label = "E1"
  ),
  
  E2 = list(
    outcome_col = "E2_outcome",
    fup_col = "E2_fup",
    calculate_rmst = FALSE,
    tau = NA_real_,
    label = "E2"
  ),
  
  E3_year3 = list(
    outcome_col = "E3b_outcome",
    fup_col = "E3b_fup",
    calculate_rmst = TRUE,
    tau = 3,
    label = "E3_year3"
  ),
  
  E3_year5 = list(
    outcome_col = "E3a_outcome",
    fup_col = "E3a_fup",
    calculate_rmst = TRUE,
    tau = 5,
    label = "E3_year5"
  )
)


############################################################
# 5. BOOTSTRAP PARAMETERS
############################################################

B <- 1000L

bootstrap_seed <- 1234L

# Use all available cores except one.
# To limit computational load, replace this with a fixed number,
# for example:

if ( settings$ds != "CPRD") {
  n_workers <- 4L
  
  available_cores <- future::availableCores()
  
  n_workers <- max(
    1L,
    available_cores - 1L
  )
  
  
  ############################################################
  # 6. SET PARALLEL PLAN
  ############################################################
  
  if (n_workers > 1L) {
    
    future::plan(
      future::multisession,
      workers = n_workers
    )
    
  } else {
    
    future::plan(
      future::sequential
    )
  }
}else{
  future::plan(
    future::sequential
  )
}



############################################################
# 7. EMPTY RMST RESULT
############################################################

empty_rmst_result <- function() {
  
  c(
    rmst_0 = NA_real_,
    rmst_1 = NA_real_,
    difference = NA_real_
  )
}


############################################################
# 8. PERCENTILE CONFIDENCE INTERVAL
############################################################

percentile_ci <- function(x) {
  
  x <- x[is.finite(x)]
  
  if (length(x) < 2L) {
    
    return( c(lower = NA_real_,  upper = NA_real_ ) )
  }
  
  stats::quantile(
    x,
    probs = c(
      0.025,
      0.975
    ),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
}


############################################################
# 9. FAST WEIBULL AFT RMST FUNCTION
############################################################

estimate_rmst_fast <- function(dt,tau,weight_col = NULL,calculate_hessian = FALSE,print_errors = FALSE) {
  
  dt <- as.data.table(dt)
  
  
  ##########################################################
  # Validate required variables
  ##########################################################
  
  required_variables <- c(
    "FUP_years",
    "outcome",
    "exposure"
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(dt)
  )
  
  if (length(missing_variables) > 0L) {
    
    if (print_errors) {
      
      message(
        "Missing variables: ",
        paste(
          missing_variables,
          collapse = ", "
        )
      )
    }
    
    return(
      empty_rmst_result()
    )
  }
  
  
  ##########################################################
  # Keep valid observations
  ##########################################################
  
  valid_rows <- (
    !is.na(dt$FUP_years) &
      is.finite(dt$FUP_years) &
      dt$FUP_years > 0 &
      !is.na(dt$outcome) &
      dt$outcome %in% c(0L, 1L) &
      !is.na(dt$exposure) &
      dt$exposure %in% c(0L, 1L)
  )
  
  dt <- dt[
    valid_rows
  ]
  
  
  ##########################################################
  # Check weight variable
  ##########################################################
  
  if (!is.null(weight_col)) {
    
    if (!weight_col %in% names(dt)) {
      
      if (print_errors) {
        
        message(
          "Weight variable not found: ",
          weight_col
        )
      }
      
      return(
        empty_rmst_result()
      )
    }
    
    valid_weight <- (
      !is.na(dt[[weight_col]]) &
        is.finite(dt[[weight_col]]) &
        dt[[weight_col]] > 0
    )
    
    dt <- dt[
      valid_weight
    ]
  }
  
  
  ##########################################################
  # Basic model checks
  ##########################################################
  
  if (nrow(dt) == 0L) {
    
    return(
      empty_rmst_result()
    )
  }
  
  if (!all(c(0L, 1L) %in% dt$exposure)) {
    
    return(
      empty_rmst_result()
    )
  }
  
  
  ##########################################################
  # At least one event in each exposure group
  ##########################################################
  
  events_by_group <- dt[
    ,
    .(
      events = sum(
        outcome == 1L,
        na.rm = TRUE
      )
    ),
    by = exposure
  ]
  
  if (
    nrow(events_by_group) < 2L ||
    any(events_by_group$events == 0L)
  ) {
    
    return(
      empty_rmst_result()
    )
  }
  
  
  ##########################################################
  # Fit Weibull AFT model
  ##########################################################
  
  fit <- tryCatch(
    {
      
      if (is.null(weight_col)) {
        
        flexsurv::flexsurvreg(
          survival::Surv(
            FUP_years,
            outcome
          ) ~ exposure,
          data = dt,
          dist = "weibull",
          hessian = calculate_hessian
        )
        
      } else {
        
        flexsurv::flexsurvreg(
          survival::Surv(
            FUP_years,
            outcome
          ) ~ exposure,
          data = dt,
          dist = "weibull",
          weights = dt[[weight_col]],
          hessian = calculate_hessian
        )
      }
    },
    error = function(e) {
      
      if (print_errors) {
        
        message(
          "flexsurvreg failed: ",
          conditionMessage(e)
        )
      }
      
      NULL
    }
  )
  
  if (is.null(fit)) {
    
    return(
      empty_rmst_result()
    )
  }
  
  
  ##########################################################
  # Predict RMST for exposure 0 and exposure 1
  ##########################################################
  
  rmst_predictions <- tryCatch(
    {
      
      predict(
        fit,
        newdata = data.frame(
          exposure = c(
            0L,
            1L
          )
        ),
        type = "rmst",
        times = tau,
        conf.int = FALSE,
        se.fit = FALSE
      )
    },
    error = function(e) {
      
      if (print_errors) {
        
        message(
          "RMST prediction failed: ",
          conditionMessage(e)
        )
      }
      
      NULL
    }
  )
  
  if (is.null(rmst_predictions)) {
    
    return(
      empty_rmst_result()
    )
  }
  
  
  ##########################################################
  # Extract prediction column
  ##########################################################
  
  prediction_column <- intersect(
    c(
      ".pred_rmst",
      "est"
    ),
    names(rmst_predictions)
  )
  
  if (length(prediction_column) == 0L) {
    
    return(
      empty_rmst_result()
    )
  }
  
  prediction_column <- prediction_column[1L]
  
  rmst_values <- as.numeric(
    rmst_predictions[[prediction_column]]
  )
  
  if (length(rmst_values) < 2L) {
    
    return(
      empty_rmst_result()
    )
  }
  
  rmst_0 <- rmst_values[1L]
  rmst_1 <- rmst_values[2L]
  
  
  ##########################################################
  # Exposed minus unexposed difference
  ##########################################################
  
  c(
    rmst_0 = rmst_0,
    rmst_1 = rmst_1,
    difference = rmst_1 - rmst_0
  )
}


############################################################
# 10. PARALLEL BOOTSTRAP FUNCTION
############################################################

bootstrap_rmst_parallel <- function(
    dt,
    tau,
    B = 1000L,
    weight_col = "weight",
    seed = 1234L
) {
  
  dt <- as.data.table(dt)
  
  
  ##########################################################
  # Exposure-specific row indices
  ##########################################################
  
  control_rows <- which(
    dt$exposure == 0L
  )
  
  exposed_rows <- which(
    dt$exposure == 1L
  )
  
  n_control <- length(
    control_rows
  )
  
  n_exposed <- length(
    exposed_rows
  )
  
  if (
    n_control == 0L ||
    n_exposed == 0L
  ) {
    
    warning(
      "The bootstrap was skipped because one exposure group is empty."
    )
    
    return(
      list(
        estimates = data.table(),
        
        ci = data.table(
          crude_rmst_0_lb = NA_real_,
          crude_rmst_0_ub = NA_real_,
          crude_rmst_1_lb = NA_real_,
          crude_rmst_1_ub = NA_real_,
          crude_difference_lb = NA_real_,
          crude_difference_ub = NA_real_,
          iptw_rmst_0_lb = NA_real_,
          iptw_rmst_0_ub = NA_real_,
          iptw_rmst_1_lb = NA_real_,
          iptw_rmst_1_ub = NA_real_,
          iptw_difference_lb = NA_real_,
          iptw_difference_ub = NA_real_,
          requested_bootstraps = B,
          successful_bootstraps_crude = 0L,
          successful_bootstraps_iptw = 0L
        )
      )
    )
  }
  
  
  ##########################################################
  # Check whether adjusted analysis is available
  ##########################################################
  
  calculate_iptw <- (
    !is.null(weight_col) &&
      weight_col %in% names(dt)
  )
  
  
  ##########################################################
  # Run bootstrap replicates
  ##########################################################
  
  bootstrap_list <- future.apply::future_lapply(
    X = seq_len(B),
    
    FUN = function(b) {
      
      sampled_control_rows <- sample(
        control_rows,
        size = n_control,
        replace = TRUE
      )
      
      sampled_exposed_rows <- sample(
        exposed_rows,
        size = n_exposed,
        replace = TRUE
      )
      
      sampled_rows <- c(
        sampled_control_rows,
        sampled_exposed_rows
      )
      
      bootstrap_data <- dt[
        sampled_rows
      ]
      
      
      ######################################################
      # Crude model
      ######################################################
      
      crude_estimate <- estimate_rmst_fast(
        dt = bootstrap_data,
        tau = tau,
        weight_col = NULL,
        calculate_hessian = FALSE,
        print_errors = FALSE
      )
      
      
      ######################################################
      # IPTW-adjusted model
      ######################################################
      
      if (calculate_iptw) {
        
        iptw_estimate <- estimate_rmst_fast(
          dt = bootstrap_data,
          tau = tau,
          weight_col = weight_col,
          calculate_hessian = FALSE,
          print_errors = FALSE
        )
        
      } else {
        
        iptw_estimate <- empty_rmst_result()
      }
      
      
      ######################################################
      # Return one bootstrap row
      ######################################################
      
      c(
        crude_rmst_0 =
          unname(
            crude_estimate["rmst_0"]
          ),
        
        crude_rmst_1 =
          unname(
            crude_estimate["rmst_1"]
          ),
        
        crude_difference =
          unname(
            crude_estimate["difference"]
          ),
        
        iptw_rmst_0 =
          unname(
            iptw_estimate["rmst_0"]
          ),
        
        iptw_rmst_1 =
          unname(
            iptw_estimate["rmst_1"]
          ),
        
        iptw_difference =
          unname(
            iptw_estimate["difference"]
          )
      )
    },
    
    future.seed = seed,
    
    future.packages = c(
      "data.table",
      "survival",
      "flexsurv"
    ),
    
    future.globals = c(
      "dt",
      "tau",
      "weight_col",
      "control_rows",
      "exposed_rows",
      "n_control",
      "n_exposed",
      "calculate_iptw",
      "estimate_rmst_fast",
      "empty_rmst_result"
    ),
    
    future.scheduling = 2
  )
  
  
  ##########################################################
  # Combine bootstrap estimates
  ##########################################################
  
  bootstrap_matrix <- do.call(
    rbind,
    bootstrap_list
  )
  
  bootstrap_results <- as.data.table(
    bootstrap_matrix
  )
  
  expected_bootstrap_names <- c(
    "crude_rmst_0",
    "crude_rmst_1",
    "crude_difference",
    "iptw_rmst_0",
    "iptw_rmst_1",
    "iptw_difference"
  )
  
  setnames(
    bootstrap_results,
    expected_bootstrap_names
  )
  
  
  ##########################################################
  # Calculate percentile confidence intervals
  ##########################################################
  
  crude_rmst_0_ci <- percentile_ci(
    bootstrap_results$crude_rmst_0
  )
  
  crude_rmst_1_ci <- percentile_ci(
    bootstrap_results$crude_rmst_1
  )
  
  crude_difference_ci <- percentile_ci(
    bootstrap_results$crude_difference
  )
  
  iptw_rmst_0_ci <- percentile_ci(
    bootstrap_results$iptw_rmst_0
  )
  
  iptw_rmst_1_ci <- percentile_ci(
    bootstrap_results$iptw_rmst_1
  )
  
  iptw_difference_ci <- percentile_ci(
    bootstrap_results$iptw_difference
  )
  
  
  ##########################################################
  # Number of successful bootstrap estimates
  ##########################################################
  
  successful_crude <- sum(
    is.finite(
      bootstrap_results$crude_difference
    )
  )
  
  successful_iptw <- sum(
    is.finite(
      bootstrap_results$iptw_difference
    )
  )
  
  
  ##########################################################
  # Bootstrap CI output
  ##########################################################
  
  bootstrap_ci <- data.table(
    
    crude_rmst_0_lb =
      crude_rmst_0_ci[1L],
    
    crude_rmst_0_ub =
      crude_rmst_0_ci[2L],
    
    crude_rmst_1_lb =
      crude_rmst_1_ci[1L],
    
    crude_rmst_1_ub =
      crude_rmst_1_ci[2L],
    
    crude_difference_lb =
      crude_difference_ci[1L],
    
    crude_difference_ub =
      crude_difference_ci[2L],
    
    iptw_rmst_0_lb =
      iptw_rmst_0_ci[1L],
    
    iptw_rmst_0_ub =
      iptw_rmst_0_ci[2L],
    
    iptw_rmst_1_lb =
      iptw_rmst_1_ci[1L],
    
    iptw_rmst_1_ub =
      iptw_rmst_1_ci[2L],
    
    iptw_difference_lb =
      iptw_difference_ci[1L],
    
    iptw_difference_ub =
      iptw_difference_ci[2L],
    
    requested_bootstraps = B,
    
    successful_bootstraps_crude =
      successful_crude,
    
    successful_bootstraps_iptw =
      successful_iptw
  )
  
  
  list(
    estimates = bootstrap_results,
    ci = bootstrap_ci
  )
}


############################################################
# 11. SUBPOPULATION LOOP
############################################################

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  
  suffix <- fifelse(
    subpop == "all","", paste0( "_",subpop) )
  
  cat(
    "\nSubpopulation:",
    subpop,
    "\n"
  )
  
  
  ##########################################################
  # Load analytical dataset
  ##########################################################
  
  analytical_dataset <- readRDS(file.path(thisdirinput, paste0("D4_ANALYTICAL_DATASET_HR_RMST_TP",suffix, ".rds")))

  analytical_dataset <- as.data.table(analytical_dataset)
   
  D4_EXP_ACT_COV_WEIGHT <-  readRDS(file.path(thisdirinput, paste0("D4_EXP_ACT_COV_WEIGHT",suffix,".rds")))
  #analytical_dataset <- readRDS(file.path(thisdirinput, paste0("analytical_dataset_test.rds")))
  analytical_dataset <- merge(analytical_dataset, D4_EXP_ACT_COV_WEIGHT[, .(person_id,weight)], by = "person_id")
  
  
  ##########################################################
  # Validate required dataset columns
  ##########################################################
  
  required_dataset_columns <- c(
    "person_id",
    "exposure",
    "E1_outcome",
    "E1_fup",
    "E2_outcome",
    "E2_fup",
    "E3a_outcome",
    "E3a_fup",
    "E3b_outcome",
    "E3b_fup"
  )
  
  missing_dataset_columns <- setdiff(
    required_dataset_columns,
    names(analytical_dataset)
  )
  
  if (length(missing_dataset_columns) > 0L) {
    
    stop(
      "Missing variables in D4_ANALYTICAL_DATASET_HR_RMST_TP",
      suffix,
      ": ",
      paste(
        missing_dataset_columns,
        collapse = ", "
      )
    )
  }
  
  
  ##########################################################
  # Initialize analysis results list
  ##########################################################
  
  results_list <- vector(
    mode = "list",
    length = length(
      analysis_definitions
    )
  )
  
  names(results_list) <- names(
    analysis_definitions
  )
  
  
  ##########################################################
  # 12. ANALYSIS LOOP
  ##########################################################
  
  for (analysis_name in names( analysis_definitions)) {
    
    definition <- analysis_definitions[[analysis_name]]
    
    outcome_col <- definition$outcome_col
    fup_col <- definition$fup_col
    calculate_rmst <- definition$calculate_rmst
    tau <- definition$tau
    analysis_label <- definition$label
    
    # cat(
    #   "\nRunning analysis:",
    #   analysis_label,
    #   "\n"
    # )
    
    
    ########################################################
    # Create analysis input
    ########################################################
    
    input <- analytical_dataset[
      ,
      .(
        person_id,
        exposure,
        FUP = as.numeric(
          get(fup_col)
        ),
        outcome = as.integer(
          get(outcome_col)
        ),
        
        weight = if (
          "weight" %in%
          names(analytical_dataset)
        ) {
          as.numeric(
            weight
          )
        } else {
          NA_real_
        }
      )
    ]
    
    input[
      ,
      FUP_years := FUP / 365.25
    ]
    
    input[
      ,
      exposure := as.integer(
        exposure
      )
    ]
    
    
    ########################################################
    # Keep valid observations
    ########################################################
    
    input <- input[
      !is.na(person_id) &
        !is.na(exposure) &
        exposure %in% c(0L, 1L) &
        !is.na(FUP) &
        is.finite(FUP) &
        FUP >= 0 &
        !is.na(outcome) &
        outcome %in% c(0L, 1L)
    ]
    
    
    ########################################################
    # Check IPTW weight availability
    ########################################################
    
    has_weight <- (
      "weight" %in%
        names(analytical_dataset) &&
        any(
          is.finite(
            input$weight
          ) &
            input$weight > 0,
          na.rm = TRUE
        )
    )
    
    
    ########################################################
    # Descriptive results by exposure group
    ########################################################
    
    processing <- input[
      ,
      .(
        N_ = uniqueN(
          person_id
        ),
        
        N_outcome_ = sum(
          outcome == 1L,
          na.rm = TRUE
        ),
        
        PY_ = sum(
          FUP_years,
          na.rm = TRUE
        )
      ),
      by = .(
        exposure_stratum = exposure
      )
    ]
    
    
    ########################################################
    # Ensure exposure 0 and exposure 1 rows
    ########################################################
    
    exposure_template <- data.table(
      exposure_stratum = c(
        0L,
        1L
      )
    )
    
    processing <- merge(
      exposure_template,
      processing,
      by = "exposure_stratum",
      all.x = TRUE,
      sort = TRUE
    )
    
    
    ########################################################
    # Incidence rate per 1,000 person-years
    ########################################################
    
    processing[
      ,
      IR_ := fifelse(
        !is.na(PY_) &
          PY_ > 0,
        1000 *
          N_outcome_ /
          PY_,
        NA_real_
      )
    ]
    
    
    ########################################################
    # Incidence-rate 95% CI
    ########################################################
    
    if (exists("exactPoiCI")) {
      
      incidence_ci <- tryCatch(
        {
          
          exactPoiCI(
            processing,
            "N_outcome_",
            "PY_",
            conversion_factor = 1,
            OoM = 3
          )
        },
        error = function(e) {
          
          cat(
            "exactPoiCI failed for ",
            analysis_label,
            ": ",
            conditionMessage(e),
            "\n",
            sep = ""
          )
          
          NULL
        }
      )
      
      if (!is.null(incidence_ci)) {
        
        processing[
          ,
          `:=`(
            IR_lb_ =
              incidence_ci[[2]],
            
            IR_ub_ =
              incidence_ci[[3]]
          )
        ]
        
      } else {
        
        processing[
          ,
          `:=`(
            IR_lb_ = NA_real_,
            IR_ub_ = NA_real_
          )
        ]
      }
      
    } else {
      
      warning(
        "Function exactPoiCI() was not found. ",
        "Incidence-rate confidence intervals were set to NA."
      )
      
      processing[
        ,
        `:=`(
          IR_lb_ = NA_real_,
          IR_ub_ = NA_real_
        )
      ]
    }
    
    
    ########################################################
    # 13. RMST FOR E3 ONLY
    ########################################################
    
    if (calculate_rmst) {
      
      cat(
        "Calculating RMST at",
        tau,
        "year(s)\n"
      )
      
      
      ######################################################
      # Crude point estimate
      ######################################################
      
      crude_point <- estimate_rmst_fast(
        dt = input,
        tau = tau,
        weight_col = NULL,
        calculate_hessian = TRUE,
        print_errors = TRUE
      )
      
      
      ######################################################
      # IPTW-adjusted point estimate
      ######################################################
      
      if (has_weight) {
        
        iptw_point <- estimate_rmst_fast(
          dt = input,
          tau = tau,
          weight_col = "weight",
          calculate_hessian = TRUE,
          print_errors = TRUE
        )
        
      } else {
        
        warning(
          "No valid weight values were found for ",
          analysis_label,
          ". Adjusted estimates were set to NA."
        )
        
        iptw_point <- empty_rmst_result()
      }
      
      
      ######################################################
      # Parallel bootstrap
      ######################################################
      
      bootstrap_result <- bootstrap_rmst_parallel(
        dt = input,
        tau = tau,
        B = B,
        weight_col = if (
          has_weight
        ) {
          "weight"
        } else {
          NULL
        },
        seed = bootstrap_seed +
          match(
            analysis_name,
            names(
              analysis_definitions
            )
          )
      )
      
      bootstrap_ci <- bootstrap_result$ci
      
      
      ######################################################
      # Crude RMST by exposure group
      ######################################################
      
      processing[
        ,
        RMST_ := fifelse(
          exposure_stratum == 0L,
          unname(
            crude_point["rmst_0"]
          ),
          unname(
            crude_point["rmst_1"]
          )
        )
      ]
      
      processing[
        ,
        RMST_lb_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            crude_rmst_0_lb,
          bootstrap_ci$
            crude_rmst_1_lb
        )
      ]
      
      processing[
        ,
        RMST_ub_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            crude_rmst_0_ub,
          bootstrap_ci$
            crude_rmst_1_ub
        )
      ]
      
      
      ######################################################
      # Crude RMST difference
      #
      # Exposed minus unexposed
      ######################################################
      
      processing[
        ,
        Diff_RMST_crude_ := fifelse(
          exposure_stratum == 1L,
          unname(
            crude_point["difference"]
          ),
          NA_real_
        )
      ]
      
      processing[
        ,
        Diff_RMST_crude_lb_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            crude_difference_lb,
          NA_real_
        )
      ]
      
      processing[
        ,
        Diff_RMST_crude_ub_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            crude_difference_ub,
          NA_real_
        )
      ]
      
      
      ######################################################
      # IPTW-adjusted RMST by exposure group
      ######################################################
      
      processing[
        ,
        RMST_adj_ := fifelse(
          exposure_stratum == 0L,
          unname(
            iptw_point["rmst_0"]
          ),
          unname(
            iptw_point["rmst_1"]
          )
        )
      ]
      
      processing[
        ,
        RMST_adj_lb_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            iptw_rmst_0_lb,
          bootstrap_ci$
            iptw_rmst_1_lb
        )
      ]
      
      processing[
        ,
        RMST_adj_ub_ := fifelse(
          exposure_stratum == 0L,
          bootstrap_ci$
            iptw_rmst_0_ub,
          bootstrap_ci$
            iptw_rmst_1_ub
        )
      ]
      
      
      ######################################################
      # IPTW-adjusted RMST difference
      ######################################################
      
      processing[
        ,
        Diff_RMST_adj_ := fifelse(
          exposure_stratum == 1L,
          unname(
            iptw_point["difference"]
          ),
          NA_real_
        )
      ]
      
      processing[
        ,
        Diff_RMST_adj_lb_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            iptw_difference_lb,
          NA_real_
        )
      ]
      
      processing[
        ,
        Diff_RMST_adj_ub_ := fifelse(
          exposure_stratum == 1L,
          bootstrap_ci$
            iptw_difference_ub,
          NA_real_
        )
      ]
      
      
      ######################################################
      # Bootstrap diagnostics
      ######################################################
      
      processing[
        ,
        `:=`(
          bootstrap_samples =
            bootstrap_ci$
            requested_bootstraps,
          
          successful_bootstraps_crude =
            bootstrap_ci$
            successful_bootstraps_crude,
          
          successful_bootstraps_iptw =
            bootstrap_ci$
            successful_bootstraps_iptw
        )
      ]
      
    } else {
      
      
      ######################################################
      # No RMST for E1 and E2
      ######################################################
      
      processing[
        ,
        `:=`(
          RMST_ = NA_real_,
          RMST_lb_ = NA_real_,
          RMST_ub_ = NA_real_,
          
          Diff_RMST_crude_ =
            NA_real_,
          
          Diff_RMST_crude_lb_ =
            NA_real_,
          
          Diff_RMST_crude_ub_ =
            NA_real_,
          
          RMST_adj_ = NA_real_,
          RMST_adj_lb_ = NA_real_,
          RMST_adj_ub_ = NA_real_,
          
          Diff_RMST_adj_ =
            NA_real_,
          
          Diff_RMST_adj_lb_ =
            NA_real_,
          
          Diff_RMST_adj_ub_ =
            NA_real_,
          
          bootstrap_samples =
            NA_integer_,
          
          successful_bootstraps_crude =
            NA_integer_,
          
          successful_bootstraps_iptw =
            NA_integer_
        )
      ]
    }
    
    
    ########################################################
    # Analysis metadata
    ########################################################
    
    processing[
      ,
      `:=`(
        analysisRMST =
          analysis_label,
        
        outcome_variable =
          outcome_col,
        
        followup_variable =
          fup_col,
        
        RMST_year =
          tau,
        
        ds = paste0(
          settings$ds,
          suffix
        )
      )
    ]
    
    
    ########################################################
    # Store analysis output
    ########################################################
    
    results_list[[analysis_name]] <- processing
  }
  
  
  ##########################################################
  # 14. COMBINE ALL ANALYSES
  ##########################################################
  
  results_list <- Filter(
    Negate(
      is.null
    ),
    results_list
  )
  
  if (length(results_list) == 0L) {
    
    stop(
      "No analysis results were produced."
    )
  }
  
  D5_ResultsRMST <- rbindlist(
    results_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  
  ##########################################################
  # 15. ROUND OUTPUT VARIABLES
  ##########################################################
  
  columns_to_round <- c(
    "PY_",
    "IR_",
    "IR_lb_",
    "IR_ub_",
    "RMST_",
    "RMST_lb_",
    "RMST_ub_",
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    "Diff_RMST_crude_",
    "Diff_RMST_crude_lb_",
    "Diff_RMST_crude_ub_",
    "Diff_RMST_adj_",
    "Diff_RMST_adj_lb_",
    "Diff_RMST_adj_ub_"
  )
  
  columns_to_round <- intersect(
    columns_to_round,
    names(
      D5_ResultsRMST
    )
  )
  
  D5_ResultsRMST[
    ,
    (columns_to_round) := lapply(
      .SD,
      round,
      digits = 2
    ),
    .SDcols = columns_to_round
  ]
  
  
  ##########################################################
  # 16. RENAME FINAL VARIABLES
  ##########################################################
  
  old_names <- c(
    "IR_lb_",
    "IR_ub_",
    "Diff_RMST_crude_",
    "Diff_RMST_crude_lb_",
    "Diff_RMST_crude_ub_",
    "Diff_RMST_adj_",
    "Diff_RMST_adj_lb_",
    "Diff_RMST_adj_ub_"
  )
  
  new_names <- c(
    "IRll_",
    "IRul_",
    "RMSTdiff_",
    "RMSTdiffll_",
    "RMSTdifful_",
    "RMSTdiff_adj_",
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_"
  )
  
  variables_present <- old_names %in%
    names(
      D5_ResultsRMST
    )
  
  setnames(
    D5_ResultsRMST,
    old = old_names[
      variables_present
    ],
    new = new_names[
      variables_present
    ]
  )
  
  
  ##########################################################
  # 17. SELECT FINAL OUTPUT COLUMNS
  ##########################################################
  
  variables_to_keep <- c(
    "ds",
    "analysisRMST",
    "outcome_variable",
    "followup_variable",
    "exposure_stratum",
    "RMST_year",
    
    "N_",
    "N_outcome_",
    "PY_",
    "IR_",
    "IRll_",
    "IRul_",
    
    "RMST_",
    "RMST_lb_",
    "RMST_ub_",
    
    "RMSTdiff_",
    "RMSTdiffll_",
    "RMSTdifful_",
    
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    
    "RMSTdiff_adj_",
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_",
    
    "bootstrap_samples",
    "successful_bootstraps_crude",
    "successful_bootstraps_iptw"
  )
  
  variables_to_keep <- intersect(
    variables_to_keep,
    names(
      D5_ResultsRMST
    )
  )
  
  D5_ResultsRMST <- D5_ResultsRMST[
    ,
    ..variables_to_keep
  ]
  
  
  ##########################################################
  # 18. SET FINAL ANALYSIS ORDER
  ##########################################################
  
  D5_ResultsRMST[
    ,
    analysis_order := match(
      analysisRMST,
      c(
        "E1",
        "E2",
        "E3_year3",
        "E3_year5"
      )
    )
  ]
  
  setorder(
    D5_ResultsRMST,
    analysis_order,
    exposure_stratum
  )
  
  D5_ResultsRMST[
    ,
    analysis_order := NULL
  ]
  
  
  #########################################
  # cleaning
  
  keep_cols <- c(
    "ds",
    "analysisRMST",
    "exposure_stratum",
    "N_",
    "N_outcome_",
    "PY_",
    "IR_" ,
    "IRll_",
    "IRul_" ,
    "RMST_" ,
    "RMST_lb_",
    "RMST_ub_",
    "RMSTdiff_" ,
    "RMSTdiffll_",
    "RMSTdifful_",
    "RMST_adj_",
    "RMST_adj_lb_",
    "RMST_adj_ub_",
    "RMSTdiff_adj_" ,
    "RMSTdiff_adj_ll_",
    "RMSTdiff_adj_ul_"
  )   #, covariate_after_intervals, do not keep since we have different values in time
  
  
  D5_ResultsRMST <- D5_ResultsRMST[, ..keep_cols]
  
         

  ##########################################################
  # 19. SAVE OUTPUT
  ##########################################################
  
  outputfile <- D5_ResultsRMST
  
  nameoutput <- paste0(
    "D5_ResultsRMST",
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


############################################################
# 20. CLOSE PARALLEL WORKERS
############################################################

future::plan(
  future::sequential
)
