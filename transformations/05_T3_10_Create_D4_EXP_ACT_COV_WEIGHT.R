############################################################
#                                                          #
####         Create_D4_EXP_ACT_COV_WEIGHT         #
#                                                          #
############################################################


# authors: Sofia Miniati, Paoletti Olga, Rosa Gini


# v 1.1 4 Sep 2026

# save in a dataset the coefficients of the PS model


# v 1.0 17 Jun 2026

# from CS5


print('CREATE D4_EXP_ACT_COV_WEIGHT')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  # testname <- "test_02_D4_EXP_ACT_COV_WEIGHT"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(thisdirinput,"g_output")
  # thisdiroutputplots <- thisdiroutput
  # thisdirsettings <- thisdirinput
  # thisdirarchive <-dirarchive
  # 
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdiroutputplots <- direxp
  thisdirsettings <- dirpargen
  thisdirarchive <-dirarchive
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

ds <- settings$ds

# list covariates per type

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(thisdirarchive,"codebooks",paste0("D4_EXP_ACT_COV_WEIGHT.xlsx")),1)))

covariate <- unname(unlist(parameters_this_step[parameter == "covariate",.(values)]))
covariate_at_t0 <- paste0(covariate,"_at_t0")

subpop <- "all"
for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  
  input <- readRDS(file.path(thisdirinput,paste0("D3_EXP_ACT_COV",suffix,".rds")))
  
  #########################################
  # start_processing
  
  to_keep<-c("exposure", covariate_at_t0)
  # handle missing
  

  if (ds == "DANREG") {
       to_keep <- setdiff(to_keep, "Smoking_imputed_at_t0")
}else{
  input[, Smoking_imputed_at_t0 := as.factor(Smoking_imputed_at_t0)]
  
     }
  variables<-input[,..to_keep]
  
  variables[, gender_at_t0 := as.factor(gender_at_t0)]
  variables[, CKD_at_t0 := as.factor(CKD_at_t0)]
  
  
  # remove from the model variables that are constant
  
  const_cols <- sapply(variables, function(x) {
    if (is.factor(x)) nlevels(droplevels(x)) <= 1
    else length(unique(x)) <= 1
  })
  
  variables_clean <- variables[, !const_cols, with = FALSE]
  
  #logistic model
  model <- glm(exposure ~ ., 
               data = variables_clean,  
               family = binomial(link="logit"))
  
  #propensity score
  input$ps <- model$fitted.values

  summary_file <- file.path(thisdiroutput, paste0("D5_PS_model_summary", suffix, ".txt"))
  
  capture.output(
    summary(model),
    file = summary_file
  )
  
  ################################
  # save model parameters in a dataset
  
  coefs <- coef(model)
  ci <- confint(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  pval <- summary(model)$coefficients[, "Pr(>|z|)"]
  
  coef_names <- names(coefs)
  
  # recover variable names from model (excluding the intercept)
  model_vars <- attr(model$terms, "term.labels")
  
  # assign each coefficient to its origin variable
  match_var <- function(cn) {
    if (cn == "(Intercept)") return("(Intercept)")
    matches <- model_vars[sapply(model_vars, function(v) startsWith(cn, v))]
    if (length(matches) == 0) return(NA_character_)
    matches[which.max(nchar(matches))]
  }
  
  variable <- sapply(coef_names, match_var)
  level <- substring(coef_names, nchar(variable) + 1)
  level[level == ""] <- NA_character_  # continuous variables: no level
  
  or_table <- data.table(
    variable = variable,
    level = level,
    OR = exp(coefs),
    CI_lower = exp(ci[, 1]),
    CI_upper = exp(ci[, 2]),
    p_value = pval
  )
  
  or_file <- file.path(thisdiroutputplots, paste0("D5_PS_model_OR", suffix, ".rds"))
  saveRDS(or_table, file = or_file)
    #stabilized weights
  
  # marginal probability of treatment
  p_treated <- mean(input$exposure == 1, na.rm = TRUE)
  
  # stabilized weights
  input[, weight := ifelse(
    exposure == 1,
    p_treated / ps,
    (1 - p_treated) / (1 - ps)
  )]
  
  
  # #########################################
  # clean
  
  listvartokeep <- c("person_id",
                     "t0",
                     "exposure",
                     covariate_at_t0,
                     "ps",
                     "weight"
  )
  input <- input[,..listvartokeep]
  
  
  
  #########################################
  # save
  
  outputfile <- input
  
  nameoutput <- paste0("D4_EXP_ACT_COV_WEIGHT",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}

#########################################

