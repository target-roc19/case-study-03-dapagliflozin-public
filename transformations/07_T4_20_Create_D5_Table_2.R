############################################################
#                                                          #
####         D5_Table_1b_baseline_cohort_with_diagnosis         #
#                                                          #
############################################################

# authors: Rosa Gini, Claudia Bartolini

# v 1.1

# add calendar year

# v 1.0

# 3 Jun 2026

print('CREATE D5_Table_2_baseline_cohort')

#########################################
# assign directories

if (TEST){
  testname <- "test_14_D5_Table_2_baseline_cohort"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirsettings <- thisdirinput
  thiscovariates_ord <- c("Obesity")
  thiscovariates_cat <- c("Tobacco_use", "CKD")
  thiscovariates_lab <- c("HbA1c")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D5_Table_2_baseline_cohort.xlsx")),1)))
  thiscovariates_ord <- unlist(parameters_this_step[parameter == "covar_ord",.(values)])
  thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)]) # [-c(1,2)]
  thiscovariates_cont <- unlist(parameters_this_step[parameter == "covar_cont",.(values)]) 
  thiscovariates_cat_var <- paste0(thiscovariates_cat,"_at_t0")
  thiscovariates_ord_var <- paste0(thiscovariates_ord,"_at_t0")
  
}

#########################################
# load

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D5_Table_2_baseline_cohort.xlsx")),1)))



#########################################
# base functions

# med <- function(x) paste0(round(median(as.numeric(x), na.rm=TRUE),1))
# q1  <- function(x) paste0(round(quantile(x,0.25,na.rm=TRUE),1))
# q3  <- function(x) paste0(round(quantile(x,0.75,na.rm=TRUE),1))
# 
# n <- function(x) paste0(sum(x, na.rm=TRUE))
# 
# p <- function(x,y){
#   n1 <- sum(x==1, na.rm=TRUE)
#   tot <- sum(!is.na(x))
#   paste0(round(100*n1/y,1))
# }


########################
# functions to describe covariates

# binary
desc_binary <- function(dataset, inputvar, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = F) {
  temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1, .(var_N = .N), by = "stratum" ]
  processing <- merge(processing,temp, by =  "stratum", all.x = T)
  processing[is.na(var_N), var_N := 0]
  thislist <- c("N")
  if (nchar(vartot) > 0) {
    processing[, var_perc := round(100* var_N/get(vartot), digits = 1)]
    thislist <- c(thislist,"perc")
  }
  setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}

#categorical
desc_cat <- function(dataset, inputvar, listofvalues, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = T) {
  for (valuevar in listofvalues) {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar, .(var_N = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    processing[is.na(var_N), var_N := 0]
    thislist <- c("N")
    if (nchar(vartot) > 0) {
      processing[, var_perc := round(100* var_N/get(vartot), digits = 1)]
      thislist <- c(thislist,"perc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",valuevar,"_",thislist,"_"))
  }
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}

# continuous
desc_continuous <- function(dataset, inputvar, outputvar, vartot = "", condition = "!is.na(person_id)", compute_Nmissing = F) {
  temp <- dataset[, (inputvar) := as.numeric(get(inputvar))]
  temp <- dataset[eval(parse(text = condition)) , .(var_mean = round(mean(get(inputvar), na.rm = T),1),
                                                    var_sd = round(sd(get(inputvar), na.rm = T),1),
                                                    var_median = round(median(get(inputvar), na.rm = T),1),
                                                    var_q1 = round(quantile(get(inputvar), probs = 0.25, na.rm = T),1),
                                                    var_q3 = round(quantile(get(inputvar), probs = 0.75, na.rm = T),1),
                                                    var_min = round(min(get(inputvar), na.rm = T),1),
                                                    var_max = round(max(get(inputvar), na.rm = T),1)
  ), by = "stratum" ]
  processing <- merge(processing,temp, by =  "stratum", all.x = T)
  # processing[is.na(var1), var1 := 0]
  # processing[, var2 := round(100* var1/N_, digits = 1)]
  thislist <- c("mean", "sd", "median", "q1", "q3", "min", "max")
  setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  if (compute_Nmissing) {
    temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum" ]
    processing <- merge(processing,temp, by =  "stratum", all.x = T)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100* var_missingN/get(vartot), digits = 1)]
      thislist <- c(thislist,"missingperc")
      
    }
    setnames(processing,paste0("var_",thislist),paste0(outputvar,"_",thislist,"_"))
  }
  return(processing)
}


# description of variables

parameter_list <- unname(unlist(unique(parameters_this_step[,.(parameter)])))
for (par in parameter_list) {
  temp <- unname(unlist(unique(parameters_this_step[parameter  == par,.(get("values"))])))
  assign(par, temp)
}


subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  totalpop <- readRDS(file.path(thisdirinput,paste0("D3_EXP_ACT_COV",suffix,".rds")))
 
  exposed <- copy(totalpop)[exposure == 1,]
  exposed[, stratum := "exp1"]
  unexposed <- copy(totalpop)[exposure == 0,]
  unexposed[, stratum := "exp0"]
  
  
  allstrata <- rbind(totalpop, exposed, unexposed, fill = T)
  allstrata[is.na(stratum), stratum := "overall"]
  
  #create calendaryear
  
  allstrata <- allstrata[, calendaryear_at_t0 := year(t0)]
  
  #########################################
  # BASE TABLE
  
  processing <- unique(allstrata[,.(stratum)])
  
  # N_
  
  varname1 <- "N_"
  temp <- allstrata[, .(var1 = .N), by = "stratum" ]
  processing <- merge(processing,temp, by =  "stratum", all.x = T)
  processing[is.na(var1), var1 := 0]
  setnames(processing,"var1",varname1)
  

  for (varname in thiscovariates_cat) {
    varnamein <- paste0(varname, "_at_t0")
    computemissing <- unname(unlist(unique(parameters_this_step[values == varname, .(has_missing)])))
    listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter == paste0(varname, "value"), .(values)])))
    computemissing <- ifelse(computemissing == 1, T, F)
    processing <- desc_cat(dataset = allstrata, inputvar = varnamein, listofvalues = listofvaluesthisvar, outputvar = varname, vartot = "N_", compute_Nmissing = computemissing)
  }

  for (varname in thiscovariates_cont) { #,"yrsdiab"
    varnamein <- paste0(varname, "_at_t0")
    # print(varnamein)
    computemissing <- unname(unlist(unique(parameters_this_step[values  == varname,.(has_missing)])))
    #listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter  == paste0(varname,"value"),.(values)])))
    computemissing <- ifelse(computemissing == 1, T, F)
    processing <- desc_continuous(dataset = allstrata, inputvar = varnamein, outputvar = varname, vartot = "N_", compute_Nmissing = computemissing)  
  }

  for (varname in thiscovariates_ord) { 
    varnamein <- paste0(varname, "_at_t0")
    processing <- desc_binary(dataset = allstrata, inputvar = varnamein, outputvar = varname, vartot = "N_", compute_Nmissing = F)
    
  }

  # add datasource
  
  processing[,ds := paste0(settings$ds,suffix)]
  
  ##############################
  # # cleaning
  # 
  # tokeep <-  c("ds", "stratum")
  # 
  # tokeep <- c(tokeep,"N_", "age_at_t0_mean_", "age_at_t0_sd_", "age_at_t0_median_", "age_at_t0_q1_", "age_at_t0_q3_","gender_at_t0_M_N_", "gender_at_t0_M_perc_", "gender_at_t0_F_N_", "gender_at_t0_F_perc_", "gender_at_t0_O_N_", "gender_at_t0_O_perc_") #, "yrsdiab_mean_", "yrsdiab_sd_","yrsdiab_median_", "yrsdiab_q1_", "yrsdiab_q3_"
  # 
  # # for (lab in thiscovariates_lab) {
  # #   tokeep <- c(tokeep, paste0(lab,c("_mean_","_sd_", "_median_", "_q1_", "_q3_", "_missingN_", "_missingperc_")))
  # # }
  # 
  # for (varname in thiscovariates_cat) {
  #   computemissing <- unname(unlist(unique(parameters_this_step[values  == varname,.(has_missing)])))
  #   listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter  == paste0(varname,"value"),.(values)])))
  #   computemissing <- ifelse(computemissing == 1, T, F)
  #   for (val in listofvaluesthisvar) {
  #     tokeep <- c(tokeep, paste0(varname,"_at_t0_",val,"_N_"), paste0(varname,"_at_t0_",val,"_perc_"))
  #   }
  #   if (computemissing == 1) {
  #     tokeep <- c(tokeep, paste0(varname,"_at_t0_missingN_"), paste0(varname,"_at_t0_missingperc_"))
  #   }
  # }
  # 
  # for (lab in thiscovariates_ord_var) {
  #   tokeep <- c(tokeep, paste0(lab,c("_N_","_perc_")))
  # }
  # 
  # processing <- processing[,..tokeep]
  # 
  # 
  #########################################
  # save
  
  nameoutput <- paste0("D5_Table_2_baseline_cohort")
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, processing)
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputext))
  
  
  
}

