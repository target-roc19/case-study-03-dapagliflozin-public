############################################################
#                                                          #
#######         Create_D5_ResultsIPC         ######
#                                                          #
############################################################


# authors: Claudia Bartolini

# v 1.0 20 Jul 2026

# from CS 3

print('CREATE D5_ResultsIPC')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  # testname <- "test_24_D5_Results_IPC"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(thisdirinput,"g_output")
  # thisdiroutputplots <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdirexp <- direxp
  thisdiroutput <- diroutput
  thisdiroutputplots <- direxp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

subpop = "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  
  input <- readRDS(file.path(thisdirinput,paste0("D4_CENSORING_WEIGHTS_TRUNCATED",suffix,".rds")))
  
  input <- input[ truncated_product_weight > 0 , ]

  #########################################
  # start_processing
  
  #fup_days numeric
  input[, FUP := as.numeric(fup_days_E1)]

  # set.seed(123)
  # 
  # input[
  #   !is.na(FUP),
  #   FUP :=
  #     FUP + sample(30:720, .N, replace = TRUE)
  # ]
  
  # Survival object
  surv_obj <- Surv(time  = input$FUP, event = input$intercurrent_event)
  
  # Preliminary checks

  invalid_input <- input[
    is.na(exposure) |
      !exposure %in% c(0, 1) |
      is.na(FUP) |
      !is.finite(FUP) |
      FUP <= 0 
  ]
  

  
  input <- input[
    !is.na(exposure) &
      exposure %in% c(0, 1) &
      !is.na(FUP) &
      is.finite(FUP) &
      FUP > 0 
  ]
  
  valid_survival <- with(
    input,
    !is.na(FUP) &
      is.finite(FUP) &
      !is.na(intercurrent_event) &
      !is.na(exposure)
  )
  
  n_valid <- sum(valid_survival)
  
  n_events <- sum(
    input$intercurrent_event[valid_survival] == 1,
    na.rm = TRUE
  )
  
  n_exposure_groups <- uniqueN(
    input$exposure[valid_survival]
  )
  
  
  # Check that each exposure group has at least one event
  events_by_exposure <- input[
    valid_survival,
    .(n_events = sum(intercurrent_event == 1, na.rm = TRUE)),
    by = exposure
  ]
  print(events_by_exposure)
  
  has_event_each_exposure_group <-
    nrow(events_by_exposure) >= 2L &&
    all(events_by_exposure$n_events > 0L)
  
  fup_check <- input[
    ,
    .(
      min_FUP = min(FUP),
      max_FUP = max(FUP),
      n_unique_FUP = uniqueN(FUP),
      n_unique_event_times = uniqueN(FUP[intercurrent_event == 1])
    ),
    by = exposure
  ]
  
  print(fup_check)
  
  
  has_FUP_variation <- uniqueN(input$FUP[valid_survival]) >= 2L
  
  
  can_fit_survival <- (
    n_valid > 0L &&
      n_events > 0L &&
      n_exposure_groups >= 2L &&
      has_event_each_exposure_group &&
      has_FUP_variation
  ) 


  if (!can_fit_survival) {
    
    ##########################################################
    # Cox e log-rank NOT estimable
    ##########################################################
    
    reason_not_estimable <- if (n_valid == 0L) {
      "No valid survival observations"
    } else if (n_events == 0L) {
      "No events observed"
    } else {
      "Fewer than two exposure groups"
    }
    
    warning(paste0(
      "Survival analyses not estimable for subpopulation '",
      subpop,"': ", reason_not_estimable))
    
    # Results:
    # reference = 1; group exposed = NA 
    processing <- unique(input[,.(exposure)])
    
    processing[, `:=`(
      HR_crude_    = fifelse(exposure == 0, 1, NA_real_),
      HR_crude_lb_ = NA_real_,
      HR_crude_ub_ = NA_real_,
      HR_adj_      = fifelse(exposure == 0, 1, NA_real_),
      HR_adj_lb_   = NA_real_,
      HR_adj_ub_   = NA_real_,
      ds           = settings$ds
    )]
    
    # objects not estimable
    cox_model    <- NULL
    cox_ipcw     <- NULL
    logrank      <- NULL
    logrank_IPCW <- NULL
    
    # Bookmark graphic to prevent ggsave() from being interrupted
    empty_survival_plot <- ggplot() +
      annotate("text", x = 0, y = 0,
               label = paste0( "Analysis not estimable\n", reason_not_estimable, "\nN valid = ", n_valid, "; events = ", n_events ),
               size = 5) +
      xlim(-1, 1) +
      ylim(-1, 1) +
      theme_void()
    
    
    log_unadj <- list(plot = empty_survival_plot)
    log_ipcw  <- list(plot = empty_survival_plot)
    
    schonefeld_unadj <- list(empty_survival_plot)
    schonefeld_ipcw  <- list(empty_survival_plot)
    
  } else {
    
    ##########################################################
    # Analysis 
    ##########################################################
    
    analysis_input <- input[valid_survival]
    
    surv_obj <- Surv(time  = analysis_input$FUP,event = analysis_input$intercurrent_event)
    
    # HR crude
    cox_model <- coxph(surv_obj ~ exposure, data = analysis_input, robust = TRUE)
    
    crude_coef <- unname(coef(cox_model)[1])
    crude_ci   <- exp(confint(cox_model)[1,])
    
    processing1 <- unique(analysis_input[, .(exposure)])
    
    processing1[, `:=`(
      HR_crude_ = fifelse(
        exposure == 1, round(exp(crude_coef), 2), 1),
      HR_crude_lb_ = fifelse(
        exposure == 1, round(crude_ci[1], 2), NA_real_),
      HR_crude_ub_ = fifelse(
        exposure == 1, round(crude_ci[2], 2), NA_real_)
    )]
    
    # HR IPCW
    cox_ipcw <- coxph(surv_obj ~ exposure, data = analysis_input, weights = truncated_product_weight, robust  = TRUE )
    
    adj_coef <- unname(coef(cox_ipcw)[1])
    adj_ci   <- exp(confint(cox_ipcw)[1,])
    
    processing2 <- unique(analysis_input[, .(exposure)])
    
    processing2[, `:=`(
      HR_adj_ = fifelse(
        exposure == 1, round(exp(adj_coef), 2), 1),
      HR_adj_lb_ = fifelse(
        exposure == 1, round(adj_ci[1], 2), NA_real_),
      HR_adj_ub_ = fifelse(
        exposure == 1, round(adj_ci[2], 2), NA_real_)
    )]
    
    processing <- merge(processing1, processing2, by = "exposure" )[, ds := settings$ds]
    
    # # Log-rank not adj
    # design <- svydesign(ids = ~1, weights = ~1, data = analysis_input)
    # logrank <- svylogrank(surv_obj ~ exposure, design = design)
    # 
    # # Log-rank IPCW
    # design_ipcw <- svydesign(ids = ~1, weights = ~truncated_product_weight, data = analysis_input )
    # logrank_IPCW <- svylogrank( surv_obj ~ exposure, design = design_ipcw)
    # 
    # # Log(-log) non aggiustato
    # log_unadj <- ggsurvplot(survfit(
    #   surv_obj ~ exposure, data = analysis_input, robust = TRUE),
    #   data        = analysis_input,
    #   fun         = "cloglog",
    #   pval        = round(logrank[[2]]["p"], 2),
    #   conf.int    = TRUE,
    #   palette     = c("skyblue", "gold"),
    #   title       = paste0("Unadjusted survival curves with log(-log) transformation",
    #                        "\nEvents = ", n_events ),
    #   legend.labs = c("Non-Exposed", "Exposed")
    # )
    # 
    # # Log(-log) IPCW
    # log_ipcw <- ggsurvplot(survfit(
    #   surv_obj ~ exposure,data = analysis_input, weights = truncated_product_weight,robust = TRUE),
    #   data        = analysis_input,
    #   fun         = "cloglog",
    #   pval        = round(logrank_IPCW[[2]]["p"], 2),
    #   conf.int    = TRUE,
    #   palette     = c("skyblue", "gold"),
    #   title       = paste0("IPCW survival curves with log(-log) transformation",
    #                        "\nEvents = ", n_events),
    #   legend.labs = c("Non-Exposed", "Exposed")
    # )
    # 
    # # Schoenfeld residuals
    # schonefeld_unadj <- ggcoxzph(cox.zph(cox_model),main = "Unadjusted Schoenfeld residuals")
    # 
    # schonefeld_ipcw <- ggcoxzph(cox.zph(cox_ipcw), main = "IPCW Schoenfeld residuals")
  }
  
  #########################################
  # clean
  
  listvartokeep <- c("ds",
                     "exposure",
                     "HR_crude_",
                     "HR_crude_lb_",
                     "HR_crude_ub_",
                     "HR_adj_",
                     "HR_adj_lb_",
                     "HR_adj_ub_") 
                     
  processing <- processing[,..listvartokeep]
  
  setnames(processing,
           c("HR_crude_", "HR_crude_lb_", "HR_crude_ub_", "HR_adj_", "HR_adj_lb_", "HR_adj_ub_"),
           c("HR_", "HRll_", "HRul_", "HRadj_", "HRadjll_", "HRadjul_")
           )
  
  
  
  # #########################################
  # # save
  # 
  # ggsave(paste0("Figure_log_unadjusted",suffix,".pdf"),
  #        plot = log_unadj$plot,
  #        path = thisdiroutput,
  #        width = 10,
  #        height = 6)
  # 
  # ggsave(paste0("Figure_log_ipcw",suffix,".pdf"),
  #        plot = log_ipcw$plot,
  #        path = thisdiroutput,
  #        width = 10,
  #        height = 6)
  # 
  # ggsave(paste0("Figure_Schonefeld_residuals_unadjusted",suffix,".pdf"),
  #        plot = schonefeld_unadj[[1]],
  #        path = thisdiroutput,
  #        width = 10,
  #        height = 6)
  # 
  # ggsave(paste0("Figure_Schonefeld_residuals_ipcw",suffix,".pdf"),
  #        plot = schonefeld_ipcw[[1]],
  #        path = thisdiroutput,
  #        width = 10,
  #        height = 6)
  
  outputfile <- processing
  
  nameoutput <- paste0("D5_ResultsIPC",suffix)
  assign(nameoutput, outputfile)
  
  fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
  
  
}
