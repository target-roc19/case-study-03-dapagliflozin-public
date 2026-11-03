############################################################
#                                                          #
####   CREATE D3_<covariate_imputer>_TD ####
# 
#                                                          #
############################################################

# author: Rosa Gini,

# 1.0 24 Jul 2026


#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_09_D3_COVARIATES_COMPLEX"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # thisdirparlab <- thisdirinput
  # thiscovariates <- c("Smoking","CKD")
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirparlab <- dirparlab
  # parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_COMPLEX_TD.xlsx")),1)))
  # thiscovariates <- unlist(unique(parameters_this_step[,.(varname)]))
}

#########################################
# load input datasets

print('CREATE D3_<covariate_imputed>_TD')

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

# use the parameter file SUBPOP and the function create_selection_rules_for_subpopulations to store the selection rule associated to each subpopulation. we handle the case when the parameter file does not exist (for example in some tests)
filesubpoppar <- file.path(thisdirsettings,"SUBPOP.rds")
if (file.exists(filesubpoppar)) {
  thisexcludemeaning <- create_selection_rules_for_subpopulations(filesubpoppar,thissubpopulations)
}else{
  thisexcludemeaning <- list()
  for (subpop in thissubpopulations) thisexcludemeaning[[subpop]] <- ""
}


# parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_COMPLEX_TD.xlsx")),1)))
# 
# thisingredient_table <- unlist(unique(parameters_this_step[is_table
#                                                            == 1,.(ingredient)]))
# 
# thisingredient_lab <- unlist(unique(parameters_this_step[LAB == 1,.(ingredient)]))

# thiscovariate_lab <- unlist(unique(parameters_this_step[LAB == 1,.(varname)]))

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  var_start_window <- "start_look_back"
  
  
  #########################################
  
  covar  <- "Smoking_imputed"
  variable <- paste0("value_of_Smoking")
  processing <- readRDS(file.path(thisdirinput,paste0("D3_Smoking_TD",suffix,".rds")))
  processing <- processing[ is.na(get(variable)), (variable) := 0]
  setnames(processing, variable, paste0("value_of_",covar))
  variable <- covar
  
  #########################################
  # cleaning
  
  tokeep <-  c("person_id",var_start_window, "exit_spell_category", paste0("value_of_",variable),"start_d","end_d")
  
  processing <- processing[,..tokeep]
  
  #########################################
  # save
  
  outputfile <- processing
  
  nameoutput <- paste0("D3_",covar,"_TD")
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

  
  
  #########################################
  # 
  # covar  <- "Obesity_composite"
  # variable <- covar
  # processing1 <- readRDS(file.path(thisdirinput,paste0("D3_Obesity_TD",suffix,".rds")))
  # processing2 <- readRDS(file.path(thisdirinput,paste0("D3_BMI_TD",suffix,".rds")))
  # 
  # 
  # 
  # # merge the two TD datasets
  # TD_variables_added <- c("value_of_Obesity")
  # processing2[, c(var_start_window,"exit_spell_category") := NULL]
  # listbase <- list(var_start_window,"exit_spell_category")
  # TDvar_first_dataset <- c(listbase, TD_variables_added)
  # TD_variables_toadd <- list("value_of_BMI")
  # processing <- GenerateTDDataset(datasets = list(processing1,processing2),
  #                                 UoO_vars = c("person_id","person_id"),
  #                                 start_d_vars = c("start_d","start_d"),
  #                                 end_d_vars = c("end_d","end_d"),
  #                                 keep_auxiliary_variables = F,
  #                                 TD_variables = list(TDvar_first_dataset,TD_variables_toadd),
  #                                 keep_periods_observed_by = "first" 
  # )
  # 
  # processing[, variable := value_of_Obesity]
  # processing[value_of_BMI >= 30, variable := 1]
  # 
  # setnames(processing, "variable", paste0("value_of_",covar))
  # 
  # 
  # 
  # #########################################
  # # cleaning
  # 
  # tokeep <-  c("person_id",var_start_window, "exit_spell_category", paste0("value_of_",variable),"start_d","end_d")
  # 
  # processing <- processing[,..tokeep]
  # 
  # 
  # 
  # #########################################
  # # save
  # 
  # outputfile <- processing
  # 
  # nameoutput <- paste0("D3_",covar,"_TD")
  # nameoutputext <- paste0(nameoutput,suffix,".rds")
  # assign(nameoutput, outputfile)
  # saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

  
}
