############################################################
#                                                          #
####     CREATE D4_CENSORING_WEIGHTS_TRUNCATED          ####
#                                                          #
#                                                          #
############################################################

# authors: Olga Paoletti

# v 0.1 20 Apr 2026

#########################################
# assign the parameter CHECK: during test with dummy data, this is set to T, otherwise to F

#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_21_D4_CENSORING_WEIGHTS_TRUNCATED"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
  # thisdirarchive <-dirarchive
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirarchive <-dirarchive
  
}

#########################################
# load input datasets

print('CREATE D4_CENSORING_WEIGHTS_TRUNCATED')

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


# list covariates per type

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D4_EXP_ACT_COV_WEIGHT.xlsx")),1)))

covariate <- unname(unlist(parameters_this_step[parameter == "covariate",.(values)]))
covariate_no_TD<-covariate[1:2]
covariate_TD<-covariate[3:46]
covariate_no_TD <- paste0(covariate_no_TD,"_at_t0")
covariate_TD <- paste0(covariate_TD,"_TD")


#correct uppercase covariate names
covariate_TD <- tolower(covariate_TD)

# covariate_TD[covariate_TD %in% c("saba_td", "sama_td")] <-
#   c("SABA_TD", "SAMA_TD")

#remove _TD from covariates name
covariate_after_intervals <- covariate[3:46]

#reduce covariate list for TEST 
if(TEST == T) {
  covariate_TD<-c("ACE_inhibitors_TD","Dialysis_TD","Aspirin_TD")
  listofinput <- c("D4_EXP_ACT_COV_WEIGHT_TRUNCATED","D4_FUP",covariate_TD)
  covariate_after_intervals <- sub("_TD$", "", covariate_TD)
}

covariate<-append(covariate_no_TD,covariate_TD)

thiscovariates_cat <- unlist(parameters_this_step[type == "cat",.(values)])
thiscovariates_bin <- unlist(parameters_this_step[type == "bin",.(values)])
thiscovariates_cont <- unlist(parameters_this_step[type == "cont",.(values)])


listofinput <- c("D4_EXP_ACT_COV_WEIGHT_TRUNCATED","D4_FUP",paste0("D3_",tolower(covariate_TD)))  #,"D3_OUTCOMES"

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  

  
  #import input files
  for (file in listofinput) {
    temp <- readRDS(file.path(thisdirinput,paste0(file,suffix,".rds")))
    assign(file,temp)
    rm(temp)
  }
  
  # transform list of coavariate in name inside dataset
  #covariate<-sub("_TD$", "_at_t0", sub("^D3_", "", D3_covariate_TD)) 
  

  
  # start from cols in D4_EXP_ACT_COV_WEIGHT_TRUNCATED
  cols <- c("person_id", "t0", "exposure", covariate_no_TD,"truncated_weight")  
  data<-D4_EXP_ACT_COV_WEIGHT_TRUNCATED[, ..cols]
  
  # #take fup_days from D4_FUP 
  # base_dt<-merge(data, D4_FUP[,.(person_id, censoring_date_excluding_intercurring, cause_of_censoring, censoring_date_excluding_intercurring
  # )], by="person_id", all.x = T)

  
  # =========================================================
  # 2. MERGE BASE DATA
  # =========================================================
  
  dt <- merge(
    data,
    D4_FUP[, .(person_id, t0,censoring_date_E1, date_intercurring_events_E1, cause_of_censoring_E1,fup_days_E1)],
    by = c("person_id", "t0"),
    all = FALSE
  )
  
  # for (v in c("t0", "censoring_date")) {
  #   if (inherits(dt[[v]], "Date")) {
  #     dt[, (v) := as.integer(get(v))]
  #   }
  # }
  # 
  # dt <- dt[!is.na(t0) & !is.na(censoring_date) & censoring_date >= t0]
  
  # nonadministrative censoring for IPCW
  dt[, intercurrent_event := as.integer(!is.na(date_intercurring_events_E1) & censoring_date_E1 == date_intercurring_events_E1)]  #admin are 3,4 and 5 and 1 is the outcome
  
  #general summary
  
  # dt_persons (1 riga/paz) dt_labs (N misure/paz)
  # id, start, end, trt, ... id, date_meas, creatinina, hb
  # │ │
  # ▼ │
  # expand_person_period() │
  # (1 riga per intervallo 30gg) │
  # │ │
  # ▼ │
  # dt_pp ◄────── rolling join (LOCF) ─────────┘
  # + event / censored per intervallo
  # + creatinina_lag, hb_lag
  # │
  # ▼
  # glm denominatore → p_uncens_denom
  # glm numeratore → p_uncens_num
  # │
  # ▼
  # cumprod() by id → ipcw_stab → troncatura
  # │
  # ▼
  # glm outcome pesato → OR + IC 95%
  
  # =========================================================
  # 3. BUILD 30-DAY PERSON-INTERVAL DATASET
  # =========================================================
  
  interval_days <- 30L
  
  long_dt <- dt[, {
  total_days <- censoring_date_E1 - t0  # or  date_intercurring_events 
    n_intervals <- floor(total_days / interval_days) + 1L
    idx <- 0:(n_intervals - 1L)
    
    .(
      interval = idx + 1L,
      start_day = idx * interval_days,
      stop_day  = pmin((idx + 1L) * interval_days, total_days + 1L)
    )
  }, by = .(person_id, t0)]
  
  long_dt <- merge(long_dt, dt, by = c("person_id", "t0"), all.x = TRUE)
  
  long_dt[, interval_start_date := t0 + start_day]
  long_dt[, interval_end_date   := pmin(t0 + stop_day - 1L, date_intercurring_events_E1)]
  
  setorder(long_dt, person_id, t0, interval)
  long_dt[, last_interval := as.integer(interval == max(interval)), by = .(person_id, t0)]
  long_dt[, censored_interval := as.integer(last_interval == 1 & intercurrent_event == 1)]
  
  long_dt[, time_since_index_30d := interval - 1L] ##???
  
  # =========================================================
  # 4. ATTACH TD COVARIATES AT INTERVAL START
  # =========================================================
  
  attach_td_covariate <- function(long_dt, td_dt) {
    td_dt <- copy(td_dt)
    long_dt <- copy(long_dt)
    
    value_cols <- grep("^value_of_", names(td_dt), value = TRUE)
    
    if (length(value_cols) != 1) {
      stop(sprintf(
        "Expected one value_of_ column, found: %s",
        paste(value_cols, collapse = ", ")
      ))
    }
    
    value_col <- value_cols[1]
    cov_name <- sub("^value_of_", "", value_col)

    setnames(td_dt, value_col, cov_name)

    long_dt[, interval_start_date_join := interval_start_date]
    
    
    
    tmp <- td_dt[
      long_dt,
      on = .(
        person_id,
        #t0,
        start_d <= interval_start_date_join,
        end_d >= interval_start_date_join
      ),
      mult = "last"
    ][
      ,
      .(
        person_id,
        t0,
        interval_start_date = start_d,
        value = get(cov_name)
      )
    ]
    
    setnames(tmp, "value", cov_name)
    

    out <- merge(
      long_dt,
      tmp,
      by = c("person_id", "t0", "interval_start_date"),
      all.x = TRUE
    )
    
    out[, interval_start_date_join := NULL]
    
    if (is.numeric(out[[cov_name]]) || is.integer(out[[cov_name]])) {
      out[is.na(get(cov_name)), (cov_name) := 0]
    }
    
    out
  }
  
  suffix_clean <- sub("^_+", "", suffix)
  
  suffix_regex <- gsub(
    "([][{}()+*^$|\\\\?.])",
    "\\\\\\1",
    suffix_clean
  )
  
  td_pattern <- paste0(
    "^D3_.*_TD",
    if (nzchar(suffix_regex)) paste0("_", suffix_regex) else "",
    "\\.rds$"
  )
  
  td_files <- list.files(
    path = thisdirinput,
    pattern = td_pattern,
    full.names = TRUE
  )
  
  TD_list <- lapply(td_files, function(f) {
    as.data.table(readRDS(f))
  })
  
  names(TD_list) <- basename(td_files)

  
  for (nm in names(TD_list)) {
    long_dt <- attach_td_covariate(long_dt, TD_list[[nm]])
  }
  

  if ("gender" %in% names(long_dt)) long_dt[, gender := as.factor(gender)]
  

  # =========================================================
  # 5. POOLED LOGISTIC MODELS
  # =========================================================
  
  denom_formula <- as.formula(
    paste(
      "censored_interval ~ ns(time_since_index_30d, df = 4)",  #ns model time flexibly using a natural cubic spline with 4 degrees of freedom
      if (length(c(covariate_no_TD, covariate_after_intervals)) > 0)
        paste("+", paste(c(covariate_no_TD, covariate_after_intervals), collapse = " + "))
    )
  )
  
  numer_formula <- as.formula(
    paste(
      "censored_interval ~ ns(time_since_index_30d, df = 4)",  
      if (length(covariate_no_TD) > 0)
        paste("+", paste(covariate_no_TD, collapse = " + "))
    )
  )
  
  fit_denom <- glm(denom_formula, data = long_dt, family = binomial())
  fit_numer <- glm(numer_formula, data = long_dt, family = binomial())
  
  long_dt[, p_cens_denom := predict(fit_denom, type = "response")]
  long_dt[, p_cens_numer := predict(fit_numer, type = "response")]
  
  eps <- 1e-6
  long_dt[, p_uncens_denom := pmax(1 - p_cens_denom, eps)]
  long_dt[, p_uncens_numer := pmax(1 - p_cens_numer, eps)]
  
  # =========================================================
  # 7. CUMULATIVE STABILIZED IPCW
  # =========================================================
  
  setorder(long_dt, person_id, t0, interval)
  
  long_dt[, cum_uncens_denom := cumprod(p_uncens_denom), by = .(person_id, t0)]
  long_dt[, cum_uncens_numer := cumprod(p_uncens_numer), by = .(person_id, t0)]
  
  long_dt[, weight_censoring := cum_uncens_numer / cum_uncens_denom]
  
  q <- quantile(long_dt$weight_censoring, probs = c(0.01, 0.99), na.rm = TRUE)
  long_dt[, truncated_weight_censoring := pmin(pmax(weight_censoring, q[1]), q[2])]
  
  # =========================================================
  # 8. CREATE OUTPUT
  # =========================================================
  
  D4_CENSORING_WEIGHTS_TRUNCATED <- long_dt[, .SD[.N], by = .(person_id, t0)]
  
  
  if ("truncated_weight" %in% names(D4_CENSORING_WEIGHTS_TRUNCATED)) {
    D4_CENSORING_WEIGHTS_TRUNCATED[, truncated_product_weight := truncated_weight * truncated_weight_censoring]
  }

  #########################################
  # cleaning
  
  keep_cols <- c(
    "person_id",
    "t0",
    "exposure",
    "fup_days_E1",
    "intercurrent_event",
    "date_intercurring_events_E1",
    "truncated_product_weight"
  )
  
  
  D4_CENSORING_WEIGHTS_TRUNCATED <- D4_CENSORING_WEIGHTS_TRUNCATED[, ..keep_cols]
  
  #########################################
  # save
  
  outputfile <- D4_CENSORING_WEIGHTS_TRUNCATED
  
  nameoutput <- "D4_CENSORING_WEIGHTS_TRUNCATED"
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}


#########################################
# during check, save output to QC repository
# 
# if (TEST & CHECK){
#   thisdircheck <- file.path(thisdirinput,"g_output_QC")
#   dir.create(thisdircheck, showWarnings = F)
#   saveRDS(outputfile, file = file.path(thisdircheck, nameoutputext))
# }
# 
# if (!TEST & CHECKSEQ){
#   saveRDS(outputfile, file = file.path(dirtestseq,nameoutput,"g_output_QC", nameoutputext))
# }



