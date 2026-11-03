############################################################
#                                                          #
####   CREATE D3_<covaroate_complex>_TD ####
# 
#                                                          #
############################################################

# author: Rosa Gini,

# 1.2 20 Jun 2026

# fixed bug in Tobacco_use: if past smoking, cannot be never smoker later

# 1.1 18 Jun 2026

# fixed bug: if concept sets are empty, then the dataset entering CreateSpells is empty as well

# 1.0 3 Jun 2026


#########################################
# assign input and output directories

if (TEST){
  testname <- "test_11_D3_COVARIATES_COMPLEX"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirsettings <- thisdirinput
  thisdirparlab <- thisdirinput
  thiscovariates <- c("Tobacco_use","CKD")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirparlab <- dirparlab
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_COMPLEX_TD.xlsx")),1)))
  thiscovariates <- unlist(unique(parameters_this_step[,.(varname)]))
}

#########################################
# load input datasets

print('CREATE D3_<covariate_complex>_TD')

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


parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_COMPLEX_TD.xlsx")),1)))

thisingredient_table <- unlist(unique(parameters_this_step[is_table
 == 1,.(ingredient)]))

thisingredient_lab <- unlist(unique(parameters_this_step[LAB == 1,.(ingredient)]))

# thiscovariate_lab <- unlist(unique(parameters_this_step[LAB == 1,.(varname)]))


for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
   persons <- readRDS(file.path(thisdirinput,paste0("D3_ENTRY_FLOWCHART",suffix,".rds")))
   
   persons[, start_look_back := entry_spell_category]
   var_start_window <- "start_look_back"
   
   
   for (covar in thiscovariates) {
     processing <- persons
     
     
     tokeep <- c("person_id","birth_year" ,var_start_window,"exit_spell_category")
     processing <- processing[,..tokeep]
     processing[, start_d := get(var_start_window)]
     processing[, end_d := exit_spell_category]
     
     TD_variables_added <- c()
     
     variable <- covar
     record <- parameters_this_step[varname == covar,]
     print(variable)
     concept_ids <- unname(unlist(record[,.(ingredient)]))
     data <- data.table()
     for (conc in concept_ids) {
       if (conc %in% thisingredient_lab) {
         dt <- cleanlab[concept_id == conc & included == 1,]
       }else if(conc %in% thisingredient_table){
         dt <- readRDS(file.path(thisdirinput,paste0(conc,suffix,".rds")))
         
         }else{
         dt <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0(conc,"_dataset",".parquet")))))
         # if the susbpopulation requires to exclude some meanings, here we do so
        thisexcludemeaningcommand <- fifelse(subpop == "all","",thisexcludemeaning[[subpop]])
        if (thisexcludemeaningcommand != "") {
           dt <- dt[eval(parse(text = thisexcludemeaningcommand)),]
        }
         }
       suppressWarnings( dt[, concept_id := NULL])
       dt[, concept_id := conc]
       dt <- merge(dt,persons[,.(person_id)], by = "person_id", all = F)
       data <- rbind(data,dt, fill = T)
     } 
    # keep only persons in the population
    temp <- merge(data,persons, by = "person_id")
    # create records of validity of the variable starting from var_start_window, based on the assessment window stored in the parameters
    window_assessment <- record[1]$window
    missingis0 <- record[1]$missing_is_0
    temp <- temp[date >= get(var_start_window) - window_assessment & date <= exit_spell_category,]
    if (nrow(temp) > 0) {
      # LIST SPECIFIC ALGORITHM PER EACH COMPLEX COVARIATE - output must be temp[,.(person_id, -variable-, date, exit_spell_date)]
      if (covar == "Smoking") {
        temp[concept_id %in% c("L_NEVERSMOKED_COV" , "L_NONSMOKER_COV"), variable := 0]
        temp[concept_id %in% c("L_EXSMOKER_COV"), variable := 1]
        temp[concept_id %in% c("L_CURRENTSMOKER_COV"), variable := 2]
        temp <- temp[, .(variable = max(as.numeric(variable))), by = c("person_id", "date","exit_spell_category")]
        }
      if (covar == "CKD") {
        temp[concept_id %in% c("D3_EGFR") & value >= 90, variable := 0]
        temp[concept_id %in% c("G_CKDST14_COV"), variable := 1]
        temp[concept_id %in% c("D3_EGFR") & value < 90, variable := 1]
        temp[concept_id %in% c("G_CKDST5_COV"), variable := 5]
        temp[concept_id %in% c("D3_EGFR") & value < 15, variable := 5]
        temp <- temp[, .(variable = max(as.numeric(variable))), by = c("person_id", "date","exit_spell_category")]
      }
      setorder(temp,person_id, date)
      temp[, start_d := date + record[1]$start]
      temp[, end_d := shift(start_d, n = 1, type = "lead"), by = person_id]
      temp[, end_d := end_d - 1]
      temp <- temp[is.na(end_d), end_d := exit_spell_category]
      temp[, end_d := pmin(end_d, start_d + window_assessment)]
      tokeep <- c("person_id",  "start_d","end_d","variable")
      temp <- temp[ ,..tokeep]
      if (covar == "Smoking") { 
        # for smoking if a person is 'never smoker' but has a recording of smoking in the past, they become 'ex smokers'
        setorder(temp,person_id, start_d)
        temp[, highest_past_smoking := {
          x <- variable
          x_no_na <- fifelse(is.na(x), -Inf, x)
          out <- cummax(x_no_na)
          out[out == -Inf] <- NA_real_
          out
        }, by = person_id]
        temp[highest_past_smoking > 0, smoking_new := pmax(variable, 1)]
        temp[!is.na(smoking_new), variable := smoking_new]
        tokeep <- c("person_id",  "start_d","end_d","variable")
        temp <- temp[ ,..tokeep]
      }
      if (covar == "CKD") {
        setorder(temp,person_id, start_d)
        temp[, variable := cummax(variable), by = person_id]
        temp[, variable := as.character(variable)]
        temp[ variable == 1, variable := "1-4"]
        
      }
    } else{
      tokeep <- c("person_id", var_start_window, "exit_spell_category")
      temp <- copy(persons)[,..tokeep]
      temp <- temp[is.na(person_id),]
      temp[, start_d := get(var_start_window)]
      temp[, end_d := exit_spell_category]
      temp[, exit_spell_category := NULL]
      temp[, (var_start_window) := NULL]
      temp[, variable := fifelse(missingis0 == 1,0,NA_integer_)]
      
    }

    setnames(temp, "variable", variable)

    temp <- CreateSpells(
      dataset = temp,
      id = "person_id",
      start_date = "start_d",
      end_date = "end_d",
      category = variable
    )
    temp <- temp[ get(variable) != "_overall",]
    temp[, num_spell := NULL]
    setnames(temp, c("entry_spell_category","exit_spell_category"), c("start_d","end_d"))
    setorder(temp,person_id, start_d)
    
    # merge with the TD dataset
    listbase <- list(var_start_window,"exit_spell_category")
    TDvar_first_dataset <- c(listbase, TD_variables_added)
    TD_variables_toadd <- list(variable)
    # thismissingval <- fifelse(missingis0 == 1,0,NA_integer_)
   processing <- GenerateTDDataset(datasets = list(processing,temp),
                                   UoO_vars = c("person_id","person_id"),
                                   start_d_vars = c("start_d","start_d"),
                                   end_d_vars = c("end_d","end_d"),
                                   keep_auxiliary_variables = F,
                                   TD_variables = list(TDvar_first_dataset,TD_variables_toadd),
                                   keep_periods_observed_by = "first" 
    )
   if (missingis0 == 1) {
     processing <- processing[is.na(get(variable)), (variable) := 0]
   }
   
  # TD_variables_added <- c(TD_variables_added,variable)
     
   
   # if(variable %in% thiscovariate_lab) {
   #   processing[, (variable) := as.numeric(get(variable))]
   # }
   setnames(processing, variable, paste0("value_of_",variable))
   
   
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
   }
   
}
