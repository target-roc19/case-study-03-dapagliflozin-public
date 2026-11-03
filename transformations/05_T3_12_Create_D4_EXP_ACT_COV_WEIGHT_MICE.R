############################################################
#                                                          #
####     Create_D4_EXP_ACT_COV_WEIGHT_IMPUTED             ####
#                                                          #
############################################################

# authors: Sofia Miniati, Paoletti Olga, Rosa Gini
# adapted for multiple HbA1c imputations

print('CREATE D4_EXP_ACT_COV_WEIGHT_IMPUTED')

#########################################
# assign directories and other parameters used in this step

if (TEST){
  # testname <- "test_02_D4_EXP_ACT_COV_WEIGHT_IMPUTED"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(thisdirinput,"g_output")
  # thisdiroutputplots <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # thisdirarchive <- dirarchive
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdiroutputplots <- direxp
  thisdirsettings <- dirpargen
  thisdirarchive <- dirarchive
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
ds <- settings$ds

parameters_this_step <- as.data.table(
  unique(
    readxl::read_excel(
      file.path(
        thisdirarchive,
        "codebooks",
        "D4_EXP_ACT_COV_WEIGHT_IMPUTED.xlsx"
      ),
      1
    )
  )
)

covariate <- unname(unlist(parameters_this_step$values))
covariate <- covariate[!is.na(covariate)]

# HbA1c is handled separately because there are 10 imputed versions
covariate_no_HbA1c <- setdiff(covariate, "HbA1c")
covariate_at_t0 <- paste0(covariate_no_HbA1c, "_at_t0")

#########################################
# loop over subpopulations

subpop <- "all"

for (subpop in thissubpopulations) {

  suffix <- fifelse(subpop == "all", "", paste0("_", subpop))
  print(suffix)

  #########################################
  # load input dataset produced by Step 04

  input <- readRDS(
    file.path(
      thisdirinput,
      paste0("D3_EXP_ACT_COV_IMPUTATION", suffix, ".rds")
    )
  )

  data.table::setDT(input)
  
  #########################################
  # variables common to all 10 PS models

  to_keep_common <- c("exposure", covariate_at_t0)

  if (ds == "DANREG") {
    to_keep_common <- setdiff(to_keep_common, "Smoking_imputed_at_t0")
  } else {
    if ("Smoking_imputed_at_t0" %in% names(input)) {
      input[, Smoking_imputed_at_t0 := as.factor(Smoking_imputed_at_t0)]
    }
  }

  if ("gender_at_t0" %in% names(input)) {
    input[, gender_at_t0 := as.factor(gender_at_t0)]
  }

  if ("CKD_at_t0" %in% names(input)) {
    input[, CKD_at_t0 := as.factor(CKD_at_t0)]
  }

  #########################################
  # marginal probability of treatment
  # same for all imputations

  p_treated <- mean(input$exposure == 1, na.rm = TRUE)

  #########################################
  # containers for model coefficients

  all_or_tables <- list()

  #########################################
  # fit one PS model for each HbA1c imputation

  for (i in 1:10) {

    hba1c_col <- paste0("HbA1c_imputed_at_t0_", i)
    ps_col <- paste0("ps_", i)
    weight_col <- paste0("weight_", i)

    if (!hba1c_col %in% names(input)) {
      stop(paste0("Missing expected variable: ", hba1c_col))
    }

    print(paste0("Fitting PS model using ", hba1c_col))

    to_keep_i <- c(
      to_keep_common,
      hba1c_col
    )

    variables <- input[, ..to_keep_i]

    #########################################
    # remove constant variables

    const_cols <- sapply(variables, function(x) {
      if (is.factor(x)) {
        nlevels(droplevels(x)) <= 1
      } else {
        length(unique(x[!is.na(x)])) <= 1
      }
    })

    if (any(const_cols)) {
      print(
        paste0(
          "Imputation ", i,
          " - constant variables removed: ",
          paste(names(const_cols)[const_cols], collapse = ", ")
        )
      )
    }

    variables_clean <- variables[, !const_cols, with = FALSE]

    #########################################
    # logistic PS model

    model <- glm(
      exposure ~ .,
      data = variables_clean,
      family = binomial(link = "logit")
    )

    #########################################
    # propensity score

    input[, (ps_col) := predict(model, newdata = variables_clean, type = "response")]

    #########################################
    # stabilized IPTW

    input[, (weight_col) := fifelse(
      exposure == 1,
      p_treated / get(ps_col),
      (1 - p_treated) / (1 - get(ps_col))
    )]

    #########################################
    # save model summary

    summary_file <- file.path(
      thisdiroutputplots,
      paste0("D5_PS_model_summary_imputation_", i, suffix, ".txt")
    )

    capture.output(
      summary(model),
      file = summary_file
    )

    #########################################
    # save model parameters

    coefs <- coef(model)
    ci <- suppressMessages(confint(model))
    se <- summary(model)$coefficients[, "Std. Error"]
    pval <- summary(model)$coefficients[, "Pr(>|z|)"]

    coef_names <- names(coefs)
    model_vars <- attr(model$terms, "term.labels")

    match_var <- function(cn) {
      if (cn == "(Intercept)") return("(Intercept)")
      matches <- model_vars[sapply(model_vars, function(v) startsWith(cn, v))]
      if (length(matches) == 0) return(NA_character_)
      matches[which.max(nchar(matches))]
    }

    variable <- sapply(coef_names, match_var)

    level <- rep(NA_character_, length(coef_names))
    non_intercept <- variable != "(Intercept)" & !is.na(variable)
    level[non_intercept] <- substring(
      coef_names[non_intercept],
      nchar(variable[non_intercept]) + 1
    )
    level[level == ""] <- NA_character_

    or_table <- data.table(
      imputation = i,
      HbA1c_variable = hba1c_col,
      variable = variable,
      level = level,
      OR = exp(coefs),
      CI_lower = exp(ci[, 1]),
      CI_upper = exp(ci[, 2]),
      p_value = pval
    )

    all_or_tables[[i]] <- or_table
  }

  #########################################
  # combine and save OR results

  or_table_all <- rbindlist(all_or_tables, use.names = TRUE, fill = TRUE)

  or_file <- file.path(
    thisdiroutputplots,
    paste0("D5_PS_model_OR_IMPUTED", suffix, ".rds")
  )

  #saveRDS(or_table_all, file = or_file)

  #########################################
  # clean output
  #
  # keep one unique dataset with:
  # - identifiers
  # - exposure
  # - all baseline covariates
  # - all 10 imputed HbA1c values
  # - ps_1 ... ps_10
  # - weight_1 ... weight_10

  hba1c_cols <- paste0("HbA1c_imputed_at_t0_", 1:10)
  ps_cols <- paste0("ps_", 1:10)
  weight_cols <- paste0("weight_", 1:10)

  listvartokeep <- unique(c(
    "person_id",
    "t0",
    "exposure",
    covariate_at_t0,
    hba1c_cols,
    ps_cols,
    weight_cols
  ))

  listvartokeep <- listvartokeep[listvartokeep %in% names(input)]

  outputfile <- input[, ..listvartokeep]

  #########################################
  # save

  nameoutput <- paste0("D4_EXP_ACT_COV_WEIGHT_MICE", suffix)

  assign(nameoutput, outputfile)

  saveRDS(
    outputfile,
    file = file.path(
      thisdiroutput,
      paste0(nameoutput, ".rds")
    )
  )
}

#########################################
