############################################################
#                                                          #
#            Create D5_ResultsIR_HR                         #
#                                                          #
############################################################

# authors: Azzurra Bongianni, Rosa Gini
# corrected version:
# - robust validation of exposure, follow-up and outcome
# - always retains exposure strata 0 and 1
# - safe Cox, unweighted log-rank, cloglog and Schoenfeld diagnostics
# - avoids ggsurvplot error: "'from' must be a finite number"

print("CREATE D5_ResultsIR_HR")

#########################################
# assign directories and other parameters used in this step

if (TEST) {
  # testname <- "test_32_D5_ResultsIR_HR"
  # thisdirinput <- file.path(dirtest, testname)
  # thisdiroutput <- file.path(thisdirinput, "g_output")
  # thisdirsettings <- thisdirinput
  # 
  # dir.create(
  #   thisdiroutput,
  #   showWarnings = FALSE,
  #   recursive = TRUE
  # )
  
} else {
  
  thisdirinput <- dirtemp
  thisdirexp <- direxp
  thisdiroutput <- diroutput
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(
  file.path(thisdirsettings, "SETTINGS.rds")
)

thissubpopulations <- unique(settings[, .(subpop)])
thissubpopulations <- unlist(
  strsplit(trimws(thissubpopulations), " +")
)

analysisIR_HR <- c(
  "E1",
  "E2",
  "TP1",
  "TP2",
  "TP3",
  "TP4"
)

# Define the follow-up and outcome variables used by each analysis.
# Change these mappings when an estimand must use different variables.
analysis_variables <- list(
  E1 = list(
    fup = "E1_fup",
    outcome = "E1_outcome"
  ),
  E2 = list(
    fup = "E2_fup",
    outcome = "E2_outcome"
  ),
  TP1 = list(
    fup = "TP1_fup",
    outcome = "TP1_outcome"
  ),
  TP2 = list(
    fup = "TP2_fup",
    outcome = "TP2_outcome"
  ),
  TP3 = list(
    fup = "TP3_fup",
    outcome = "TP3_outcome"
  ),
  TP4 = list(
    fup = "TP4_fup",
    outcome = "TP4_outcome"
  )
)

expected_exposure_strata <- c(0, 1)

#########################################
# helper functions

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

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
  
  analytical_dataset <- readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_ANALYTICAL_DATASET_HR_RMST_TP",
        suffix,
        ".rds"
      )
    )
  )
  
  D4_EXP_ACT_COV_WEIGHT <-  readRDS(
    file.path(
      thisdirinput,
      paste0(
        "D4_EXP_ACT_COV_WEIGHT",
        suffix,
        ".rds"
      )
    )
  )
  
  analytical_dataset <- merge(analytical_dataset, D4_EXP_ACT_COV_WEIGHT[, .(person_id,weight)], by = "person_id")
  
  setDT(analytical_dataset)
  
  output <- data.table()
  
  plot_analyses <- c("E1", "E2")
  
  #########################################
  # loop over analyses
  
  for (analysis in analysisIR_HR) {
    
    print(analysis)
    
    # TP1-TP4 never enter any plotting or plot-saving block.
    make_diagnostic_plots <- analysis %in% plot_analyses
    
    if (!analysis %in% names(analysis_variables)) {
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "no follow-up/outcome mapping defined"
          ),
          subpop,
          analysis
        )
      )
      next
    }
    
    fup_variable <- analysis_variables[[analysis]]$fup
    outcome_variable <- analysis_variables[[analysis]]$outcome
    
    required_variables <- c(
      "exposure",
      fup_variable,
      outcome_variable
    )
    
    missing_variables <- setdiff(
      required_variables,
      names(analytical_dataset)
    )
    
    if (length(missing_variables) > 0L) {
      
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "missing variable(s): %s"
          ),
          subpop,
          analysis,
          paste(missing_variables, collapse = ", ")
        )
      )
      
      next
    }
    
    
    #########################################
    # prepare input
    
    input <- analytical_dataset[
      ,
      .(
        exposure = to_numeric(exposure),
        FUP = to_numeric(get(fup_variable)),
        outcome = to_numeric(get(outcome_variable)),
        weight
      )
    ]
    
    
    invalid_input <- input[
      is.na(exposure) |
        !exposure %in% c(0, 1) |
        is.na(FUP) |
        !is.finite(FUP) |
        FUP <= 0 |
        is.na(outcome) |
        !outcome %in% c(0, 1)
    ]
    
    if (nrow(invalid_input) > 0L) {
      
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "excluding %s invalid record(s)"
          ),
          subpop,
          analysis,
          nrow(invalid_input)
        )
      )
      
      
    }
    
    input <- input[
      !is.na(exposure) &
        exposure %in% c(0, 1) &
        !is.na(FUP) &
        is.finite(FUP) &
        FUP > 0 &
        !is.na(outcome) &
        outcome %in% c(0, 1)
    ]
    
    #########################################
    # initialise both exposure strata
    
    processing <- data.table(
      exposure_stratum = expected_exposure_strata
    )
    
    #########################################
    # N, events and person-years
    
    if (nrow(input) > 0L) {
      
      counts <- input[
        ,
        .(
          N_ = .N,
          N_outcome_ = sum(outcome == 1, na.rm = TRUE),
          PY_ = sum(FUP, na.rm = TRUE) / 365.25
        ),
        by = .(
          exposure_stratum = exposure
        )
      ]
      
      processing <- merge(
        processing,
        counts,
        by = "exposure_stratum",
        all.x = TRUE,
        sort = FALSE
      )
    }
    
    processing[
      is.na(N_),
      `:=`(
        N_ = 0L,
        N_outcome_ = 0L,
        PY_ = 0
      )
    ]
    
    processing[
      ,
      `:=`(
        N_ = as.integer(N_),
        N_outcome_ = as.integer(N_outcome_),
        PY_ = round(PY_, digits = 1)
      )
    ]
    
    #########################################
    # incidence rates
    
    processing[
      ,
      IR_ := fifelse(
        PY_ > 0,
        1000 * N_outcome_ / PY_,
        NA_real_
      )
    ]
    
    CI <- tryCatch(
      exactPoiCI(
        processing,
        "N_outcome_",
        "PY_",
        conversion_factor = 1,
        OoM = 3
      ),
      error = function(e) {
        
        message(
          sprintf(
            paste0(
              "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
              "exactPoiCI failed: %s"
            ),
            subpop,
            analysis,
            conditionMessage(e)
          )
        )
        
        NULL
      }
    )
    
    if (!is.null(CI)) {
      
      processing[
        ,
        `:=`(
          IR_lb_ = as.numeric(CI[[2L]]),
          IR_ub_ = as.numeric(CI[[3L]])
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
    
    processing[
      ,
      c("IR_", "IR_lb_", "IR_ub_") := lapply(
        .SD,
        round,
        digits = 3
      ),
      .SDcols = c(
        "IR_",
        "IR_lb_",
        "IR_ub_"
      )
    ]
    
    #########################################
    # check whether Cox/log-rank are estimable
    
    events_by_group <- input[
      ,
      .(
        N = .N,
        events = sum(outcome == 1, na.rm = TRUE),
        min_FUP = min(FUP),
        max_FUP = max(FUP)
      ),
      by = exposure
    ]
    
    can_fit_cox <-
      nrow(input) > 0L &&
      all(c(0, 1) %in% events_by_group$exposure) &&
      all(
        events_by_group[
          match(c(0, 1), exposure),
          N
        ] > 0L
      ) &&
      all(
        events_by_group[
          match(c(0, 1), exposure),
          events
        ] > 0L
      )
    
    #########################################
    # crude Cox model
    
    cox_model <- NULL
    
    if (can_fit_cox) {
      
      cox_model <- tryCatch(
        survival::coxph(
          survival::Surv(FUP, outcome) ~ exposure,
          data = input,
          robust = TRUE
        ),
        error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "coxph failed: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
    } else {
      
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "Cox model skipped because both exposure groups ",
            "must be present and contain at least one event"
          ),
          subpop,
          analysis
        )
      )
    }
    
    #########################################
    # HR and confidence interval
    
    hr_ci <- NULL
    
    if (!is.null(cox_model)) {
      
      hr_ci <- tryCatch(
        list(
          hr = as.numeric(exp(stats::coef(cox_model))),
          ci = exp(stats::confint(cox_model))
        ),
        error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "HR/confint failed: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
    }
    
    if (!is.null(hr_ci)) {
      
      processing[
        ,
        `:=`(
          HR_crude_ = fifelse(
            exposure_stratum == 1,
            round(hr_ci$hr, 3),
            1
          ),
          
          HR_crude_lb_ = fifelse(
            exposure_stratum == 1,
            round(as.numeric(hr_ci$ci[1L, 1L]), 3),
            NA_real_
          ),
          
          HR_crude_ub_ = fifelse(
            exposure_stratum == 1,
            round(as.numeric(hr_ci$ci[1L, 2L]), 3),
            NA_real_
          )
        )
      ]
      
    } else {
      
      processing[
        ,
        `:=`(
          HR_crude_ = fifelse(
            exposure_stratum == 0,
            1,
            NA_real_
          ),
          HR_crude_lb_ = NA_real_,
          HR_crude_ub_ = NA_real_
        )
      ]
    }
    
    
    #HR_adj, HR_adj_lb_ and HR_adj_ub_
    cox_iptw <- survival::coxph(
      survival::Surv(FUP, outcome) ~ exposure,
      data = input,
      weights = weight,
      robust = TRUE
    )
    
    processing[, `:=`(
      HR_adj_ = ifelse(exposure_stratum == 1,
                       round(exp(coef(cox_iptw)), digits = 3),
                       1),
      HR_adj_ll_ = ifelse(exposure_stratum == 1,
                          round(exp(confint(cox_iptw))[,1], digits = 3),
                          NA),
      HR_adj_ul_ = ifelse(exposure_stratum == 1,
                          round(exp(confint(cox_iptw))[,2], digits = 3),
                          NA)
    )]
    
    #########################################
    # unadjusted log-rank test
    #
    # survdiff() is used instead of svydesign()/svylogrank() because this
    # is the unweighted analysis. This avoids the warning:
    # "No weights or probabilities supplied, assuming equal probability".
    
    logrank_p <- NA_real_
    
    if (can_fit_cox) {
      
      logrank_p <- tryCatch({
        
        lr <- survival::survdiff(
          survival::Surv(FUP, outcome) ~ exposure,
          data = input
        )
        
        df_logrank <- length(lr$n) - 1L
        
        if (
          is.na(lr$chisq) ||
          !is.finite(lr$chisq) ||
          df_logrank < 1L
        ) {
          NA_real_
        } else {
          stats::pchisq(
            q = lr$chisq,
            df = df_logrank,
            lower.tail = FALSE
          )
        }
        
      }, error = function(e) {
        
        message(
          sprintf(
            paste0(
              "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
              "log-rank failed: %s"
            ),
            subpop,
            analysis,
            conditionMessage(e)
          )
        )
        
        NA_real_
      })
    }
    
    processing[
      ,
      logrank_unadj := round(logrank_p, 3)
    ]
    
    #########################################
    # diagnostic: log(-log) survival plot
    
    log_unadj <- NULL
    
    plot_input <- input[
      !is.na(FUP) &
        is.finite(FUP) &
        FUP > 0 &
        !is.na(outcome) &
        outcome %in% c(0, 1) &
        exposure %in% c(0, 1)
    ]
    
    plot_check <- plot_input[
      ,
      .(
        N = .N,
        events = sum(outcome == 1, na.rm = TRUE),
        min_FUP = min(FUP),
        max_FUP = max(FUP),
        unique_FUP = uniqueN(FUP)
      ),
      by = exposure
    ]
    
    # can_plot_cloglog <-
    #   nrow(plot_input) > 0L &&
    #   all(c(0, 1) %in% plot_check$exposure) &&
    #   all(is.finite(plot_check$min_FUP)) &&
    #   all(is.finite(plot_check$max_FUP)) &&
    #   all(plot_check$min_FUP > 0) &&
    #   all(plot_check$max_FUP > plot_check$min_FUP) &&
    #   all(plot_check$unique_FUP >= 2L) &&
    #   all(plot_check$events > 0L)
    
    if (make_diagnostic_plots ) {  #&& can_plot_cloglog
      
      surv_fit_plot <- tryCatch(
        survival::survfit(
          survival::Surv(FUP, outcome) ~ exposure,
          data = plot_input
        ),
        error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "survfit failed: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
      if (!is.null(surv_fit_plot)) {
        
        log_unadj <- tryCatch(
          survminer::ggsurvplot(
            surv_fit_plot,
            data = plot_input,
            fun = "cloglog",
            pval = if (is.finite(logrank_p)) logrank_p else FALSE,
            conf.int = TRUE,
            palette = c("skyblue", "gold"),
            title = paste0(
              analysis,
              ": unadjusted log(-log) survival curves"
            ),
            legend.labs = c(
              "Dpp4-i", 
              "Dapagliflozin"
            ),
            risk.table = FALSE
          ),
          error = function(e) {
            
            message(
              sprintf(
                paste0(
                  "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                  "ggsurvplot skipped: %s"
                ),
                subpop,
                analysis,
                conditionMessage(e)
              )
            )
            
            NULL
          }
        )
      }
      
    } else if (make_diagnostic_plots) {
      
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "cloglog plot skipped because follow-up/event data ",
            "are insufficient"
          ),
          subpop,
          analysis
        )
      )
      
      print(plot_check)
    }
    
    #########################################
    # cumulative incidence plots: unadjusted and IPTW-adjusted
    #
    # Kaplan-Meier estimates are transformed from survival probability
    # S(t) to cumulative incidence 1 - S(t).
    
    cumulative_incidence_unadj <- NULL
    cumulative_incidence_adj <- NULL
    
    if (make_diagnostic_plots ) {  #&& can_plot_cloglog
      
      cumulative_incidence_unadj_fit <- tryCatch(
        survival::survfit(
          survival::Surv(FUP, outcome) ~ exposure,
          data = plot_input,
          conf.type = "log"
        ),
        error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "unadjusted cumulative-incidence survfit failed: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
      if (!is.null(cumulative_incidence_unadj_fit)) {
        
        cumulative_incidence_unadj <- tryCatch(
          survminer::ggsurvplot(
            cumulative_incidence_unadj_fit,
            data = plot_input,
            fun = "event",
            conf.int = TRUE,
            censor = TRUE,
            censor.shape = 3,
            censor.size = 2.5,
            palette = c("#F8766D", "#00BFC4"),
            legend.labs = c(
              "Dpp4-i", 
              "Dapagliflozin"
            ),
            xlab = "Day since start of risk period",
            ylab = "Cumulative incidence",
            title = paste0(
              analysis,
              ": unadjusted cumulative incidence"
            ),
            risk.table = FALSE,
            ggtheme = ggplot2::theme_minimal()
          ),
          error = function(e) {
            
            message(
              sprintf(
                paste0(
                  "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                  "unadjusted cumulative-incidence plot skipped: %s"
                ),
                subpop,
                analysis,
                conditionMessage(e)
              )
            )
            
            NULL
          }
        )
      }
    }
    
    cumulative_incidence_input <- input[
      !is.na(FUP) &
        is.finite(FUP) &
        FUP > 0 &
        !is.na(outcome) &
        outcome %in% c(0, 1) &
        exposure %in% c(0, 1) &
        !is.na(weight) &
        is.finite(weight) &
        weight > 0
    ]
    
    cumulative_incidence_check <- cumulative_incidence_input[
      ,
      .(
        N = .N,
        events = sum(outcome == 1, na.rm = TRUE),
        unique_FUP = uniqueN(FUP)
      ),
      by = exposure
    ]
    
    # can_plot_cumulative_incidence <-
    #   nrow(cumulative_incidence_input) > 0L &&
    #   all(c(0, 1) %in% cumulative_incidence_check$exposure) &&
    #   all(cumulative_incidence_check$N > 0L) &&
    #   all(cumulative_incidence_check$events > 0L) &&
    #   all(cumulative_incidence_check$unique_FUP >= 2L)
    
    if (make_diagnostic_plots ) {  #&& can_plot_cumulative_incidence
      
      cumulative_incidence_fit <- tryCatch(
        survival::survfit(
          survival::Surv(FUP, outcome) ~ exposure,
          data = cumulative_incidence_input,
          weights = weight,
          robust = TRUE,
          conf.type = "log"
        ),
        error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "weighted cumulative-incidence survfit failed: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        }
      )
      
      if (!is.null(cumulative_incidence_fit)) {
        
        cumulative_incidence_adj <- tryCatch({
          
          cumulative_incidence_plot <- survminer::ggsurvplot(
            cumulative_incidence_fit,
            data = cumulative_incidence_input,
            fun = "event",
            conf.int = TRUE,
            censor = TRUE,
            censor.shape = 3,
            censor.size = 2.5,
            palette = c("#F8766D", "#00BFC4"),
            legend.labs = c(
              "Dpp4-i", 
              "Dapagliflozin"
            ),
            xlab = "Day since start of risk period",
            ylab = "Cumulative incidence",
            title = paste0(
              analysis,
              ": IPTW-adjusted cumulative incidence"
            ),
            risk.table = FALSE,
            ggtheme = ggplot2::theme_minimal()
          )
          
          cumulative_incidence_plot$plot <-
            cumulative_incidence_plot$plot +
            ggplot2::theme(
              panel.grid.minor = ggplot2::element_line(
                colour = "grey92",
                linewidth = 0.3
              ),
              legend.title = ggplot2::element_blank()
            )
          
          cumulative_incidence_plot
          
        }, error = function(e) {
          
          message(
            sprintf(
              paste0(
                "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
                "cumulative-incidence plot skipped: %s"
              ),
              subpop,
              analysis,
              conditionMessage(e)
            )
          )
          
          NULL
        })
      }
      
    } else if (make_diagnostic_plots) {
      
      message(
        sprintf(
          paste0(
            "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
            "weighted cumulative-incidence plot skipped because ",
            "follow-up, event or weight data are insufficient"
          ),
          subpop,
          analysis
        )
      )
    }
    #########################################
    # diagnostic: Schoenfeld residuals
    
    schonefeld_unadj <- NULL
    
    if (make_diagnostic_plots && !is.null(cox_model)) {
      
      schonefeld_unadj <- tryCatch({
        
        zph <- survival::cox.zph(cox_model)
        
        n_event_times <- length(
          unique(
            input$FUP[
              input$outcome == 1
            ]
          )
        )
        
        df_use <- max(
          1L,
          min(
            4L,
            n_event_times - 1L
          )
        )
        
        survminer::ggcoxzph(
          zph,
          main = paste0(
            analysis,
            ": unadjusted Schoenfeld residuals"
          ),
          df = df_use
        )
        
      }, error = function(e) {
        
        message(
          sprintf(
            paste0(
              "[D5_ResultsIR_HR] subpop='%s' analysis='%s': ",
              "cox.zph/ggcoxzph failed: %s"
            ),
            subpop,
            analysis,
            conditionMessage(e)
          )
        )
        
        NULL
      })
    }
    
    #########################################
    # add metadata and clean
    
    processing[
      ,
      ds := paste0(
        settings$ds[1L],
        suffix
      )
    ]
    
    processing[
      ,
      analysisIR_HR := analysis
    ]
    
    listvartokeep <- c(
      "ds",
      "exposure_stratum",
      "analysisIR_HR",
      "N_",
      "N_outcome_",
      "PY_",
      "IR_",
      "IR_lb_",
      "IR_ub_",
      "HR_crude_",
      "HR_crude_lb_",
      "HR_crude_ub_",
      "HR_adj_",
      "HR_adj_ll_",
      "HR_adj_ul_",
      "logrank_unadj"
    )
    
    processing <- processing[
      ,
      ..listvartokeep
    ]
    
    output <- rbind(
      output,
      processing,
      fill = TRUE
    )
    
    #########################################
    # save diagnostic plots
    
    if (make_diagnostic_plots && !is.null(log_unadj)) {
      
      ggsave(
        filename = paste0(
          "D5_",
          analysis,
          "_Figure_log_unadjusted",
          suffix,
          ".pdf"
        ),
        plot = log_unadj$plot,
        path = thisdiroutput,
        width = 10,
        height = 6
      )
    }
    
    if (make_diagnostic_plots && !is.null(cumulative_incidence_adj)) {
      
      ggsave(
        filename = paste0(
          "D5_",
          analysis,
          "_Figure_cumulative_incidence_adjusted",
          suffix,
          ".pdf"
        ),
        plot = cumulative_incidence_adj$plot,
        path = thisdiroutput,
        width = 10,
        height = 6
      )
    }
    
    if (make_diagnostic_plots && !is.null(cumulative_incidence_unadj)) {
      
      ggsave(
        filename = paste0(
          "D5_",
          analysis,
          "_Figure_cumulative_incidence_unadjusted",
          suffix,
          ".pdf"
        ),
        plot = cumulative_incidence_unadj$plot,
        path = thisdiroutput,
        width = 10,
        height = 6
      )
    }
    
    if (make_diagnostic_plots && !is.null(schonefeld_unadj)) {
      
      schonefeld_plot <- if (
        inherits(schonefeld_unadj, "ggplot")
      ) {
        schonefeld_unadj
      } else if (
        is.list(schonefeld_unadj) &&
        length(schonefeld_unadj) >= 1L
      ) {
        schonefeld_unadj[[1L]]
      } else {
        NULL
      }
      
      if (!is.null(schonefeld_plot)) {
        
        ggsave(
          filename = paste0(
            "D5_",
            analysis,
            "_Figure_Schonefeld_residuals_unadjusted",
            suffix,
            ".pdf"
          ),
          plot = schonefeld_plot,
          path = thisdiroutput,
          width = 10,
          height = 6
        )
      }
    }
  }
  
  #########################################
  # final naming and validation
  
  if (nrow(output) == 0L) {
    
    stop(
      sprintf(
        "[D5_ResultsIR_HR] subpop='%s': no analysis produced output",
        subpop
      )
    )
  }
  
  setnames(
    output,
    old = c(
      "IR_lb_",
      "IR_ub_",
      "HR_crude_",
      "HR_crude_lb_",
      "HR_crude_ub_"
    ),
    new = c(
      "IRll_",
      "IRul_",
      "HR_",
      "HRll_",
      "HRul_"
    )
  )
  
  setorder(
    output,
    analysisIR_HR,
    exposure_stratum
  )
  
  if (
    anyDuplicated(
      output[
        ,
        .(
          analysisIR_HR,
          exposure_stratum
        )
      ]
    )
  ) {
    
    stop(
      sprintf(
        paste0(
          "[D5_ResultsIR_HR] subpop='%s': ",
          "duplicated analysis/exposure key"
        ),
        subpop
      )
    )
  }
  
  #########################################
  # save output
  
  outputfile <- output
  
  nameoutput <- paste0("D5_ResultsIR_HR",suffix)
  assign(nameoutput,outputfile)
  
  fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  
}