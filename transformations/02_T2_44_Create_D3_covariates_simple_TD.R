############################################################
#                                                          #
####   CREATE D3_COVARIATES_TD ####
# 
#                                                          #
############################################################

# author: Rosa Gini,


# 1.2 17 Jun 2026

# fixed bug: if concept sets are empty, then the dataset entering CreateSpells is empty as well

# 1.1 3 Jun 2026

# removed calculation of smoking


# 1.0 27 May 2026


#########################################
# assign input and output directories

if (TEST){
  testname <- "test_10_D3_COVARIATES_SIMPLE"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  thisdirsettings <- thisdirinput
  thisdirparlab <- thisdirinput
  thiscovariates <- c("Obesity","Heart_transplant","Metformin", "Lower_limb_amputations" ,"HbA1c", "EGFR")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
  thisdirparlab <- dirparlab
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_SIMPLE_TD.xlsx")),1)))
  thiscovariates <- unlist(unique(parameters_this_step[,.(varname)]))
}

#########################################
# load input datasets

print('CREATE D3_COVARIATES_TD')

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


parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_COVARIATES_SIMPLE_TD.xlsx")),1)))

thisingredient_table <- unlist(unique(parameters_this_step[is_table
                                                                                     == 1,.(ingredient)]))

thisingredient_lab <- unlist(unique(parameters_this_step[LAB == 1,.(ingredient)]))

thiscovariate_lab <- unlist(unique(parameters_this_step[LAB == 1,.(varname)]))


# subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
   persons <- readRDS(file.path(thisdirinput,paste0("D3_ENTRY_FLOWCHART",suffix,".rds")))
   
   cleanlab <- readRDS(file.path(thisdirinput,paste0("D3_CLEAN_LAB",suffix,".rds")))
  
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
         dt <- merge(dt,persons[,.(person_id)], by = "person_id", all = F)
         # if the subpopulation requires to exclude some meanings, here we do so
        thisexcludemeaningcommand <- fifelse(subpop == "all","",thisexcludemeaning[[subpop]])
        if (thisexcludemeaningcommand != "") {
           dt <- dt[eval(parse(text = thisexcludemeaningcommand)),]
        }
        dt[, concept_id := conc]
       }
       data <- rbind(data,dt, fill = T)
     } 
    # keep only persons in the population
    temp <- merge(data,persons, by = "person_id")
    # create records of validity of the variable starting from var_start_window, based on the assessment window stored in the parameters
    window_assessment <- record[1]$window
    missingis0 <- record[1]$missing_is_0
    temp <- temp[date >= get(var_start_window) - window_assessment & date <= exit_spell_category,]
    if (nrow(temp) > 0) {
      #if record contains a value: each record starts at the end of the next
      if ("value" %in% names(temp)) {
        temp <- temp[, .(variable = max(as.numeric(value))), by = c("person_id", "date","exit_spell_category")]
        setnames(temp,"variable",variable)
      }else{
        # otherwise: variable is 1"
        temp[,(variable) := 1]
      }
      setorder(temp,person_id, date)
      temp[, start_d := date + record[1]$start]
      temp[, end_d := shift(start_d, n = 1, type = "lead"), by = person_id]
      temp[, end_d := end_d - 1]
      temp <- temp[is.na(end_d), end_d := exit_spell_category]
      temp[, end_d := pmin(end_d, start_d + window_assessment)]
      tokeep <- c("person_id",  "start_d","end_d",variable)
      temp <- temp[ ,..tokeep]
    }else{
      
      tokeep <- c("person_id", var_start_window, "exit_spell_category")
      temp <- copy(persons)[,..tokeep]
      temp <- temp[is.na(person_id),]
      temp[, start_d := get(var_start_window)]
      temp[, end_d := exit_spell_category]
      temp[, exit_spell_category := NULL]
      temp[, (var_start_window) := NULL]
      temp[, (variable) := fifelse(missingis0 == 1,0,NA_integer_)]
    }
    

  
    # temp[,person_id_var := paste0(person_id,"_",get(variable))]
    # tokeep <- c("person_id", variable, "person_id_var")
    # id <- unique(copy(temp)[,..tokeep])
    # temp <- CreateSpells(
    #   dataset = temp,
    #   id = c("person_id_var"),
    #   start_date = "start_d",
    #   end_date = "end_d"
    # )
    # temp[, num_spell := NULL]
    # setnames(temp, c("entry_spell_category","exit_spell_category"), c("start_d","end_d"))
    # temp <- merge(temp,id, by = "person_id_var")
    # tokeep <- c("person_id",  "start_d","end_d",variable)
    # temp <- temp[ ,..tokeep]
    # setorder(temp,person_id, start_d)
    
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
     
   
   if(variable %in% thiscovariate_lab) {
     processing[, (variable) := as.numeric(get(variable))]
   }
   
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
   rm(outputfile,processing)
   }
   
}
