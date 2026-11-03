##%######################################################%##
#                                                          #
####  EXTRACT FROM CDM TABLES ONE DATASET PER ITEMSET   ####
#### CONTAINING ONLY RECORDS WITH A STRING OF INTEREST  ####
#                                                          #
##%######################################################%##

# author: Rosa Gini, Olga Paoletti


# v 1.2 18 Jun 2026

# if CDM instance in parquet, output is still in RData: introduced a temporary fix

# v 1.1 10 Jun 2026

# added support for CDM instance in parquet, output is in parquet as well

# v 1.0 10 Mar 2026


# Structure based on parameter 04_itemset.R: all itemsets are retrieved together, irrespective of combination of columns used to retrieve them, and they are classified according to the group of tables/columns that are needed for extraction. All possible combinations are stored in column_combinations_for_itemsets. There can be more than 1 group and in this case the itemset is split in > 1 subitemsets, that are then reconciled in the next row


print('CREATE itemset datasets')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_16_CreateItemsetsFromParquet"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
  extensionthisinstance <- "csv"
  thiscolumn_combinations_for_itemsets <- list(c("MEDICINES","medicinal_product_atc_code"))
  this_combination_tables <- list()
  # this_combination_tables[["MEDICINES_medicinal_product_atc_code"]] <- c("MEDICINES_SD_EXP_00000000.parquet")
  # this_combination_tables <- list()
  this_combination_tables[["MEDICINES_medicinal_product_atc_code"]][["MEDICINES_SD_EXP_00000000"]] <- "medicinal_product_atc_code"
  this_combination_tables[["MEDICINES_medicinal_product_atc_code"]][["MEDICINES_SD_SEL_00000000"]] <- "medicinal_product_atc_code"
  this_combination_dates <- list()
   this_combination_dates[["MEDICINES_medicinal_product_atc_code"]][["MEDICINES_SD_EXP_00000000"]] <- list("date_dispensing")
   this_combination_dates[["MEDICINES_medicinal_product_atc_code"]][["MEDICINES_SD_SEL_00000000"]] <- list("date_dispensing")
  this_rename_var <- list()
  this_study_variable_names <- list()
  this_study_variable_names[["MEDICINES_medicinal_product_atc_code"]] <- c("DP_DAPAGLIFLOZIN", "XXX")
  this_itemset_rules <- list()
  this_itemset_rules[["MEDICINES_medicinal_product_atc_code"]][["DP_DAPAGLIFLOZIN"]][["MEDICINES_SD_EXP_00000000"]] <- list(list("A10BK01"))
  this_itemset_rules[["MEDICINES_medicinal_product_atc_code"]][["XXX"]][["MEDICINES_SD_EXP_00000000"]] <- list(list("CCC"))
  this_itemset_rules[["MEDICINES_medicinal_product_atc_code"]][["DP_DAPAGLIFLOZIN"]][["MEDICINES_SD_SEL_00000000"]] <- list(list("A10BK01"))
  this_itemset_rules[["MEDICINES_medicinal_product_atc_code"]][["XXX"]][["MEDICINES_SD_SEL_00000000"]] <- list(list("CCC"))
  
  this_item_sets_of_our_study <- c("DP_DAPAGLIFLOZIN", "XXX")
  this_concept_id_itemset_retrieval <- list()
  for (conc in this_item_sets_of_our_study) {
    this_concept_id_itemset_retrieval[[conc]] <- conc  
  }

}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
  thiscolumn_combinations_for_itemsets <- column_combinations_for_itemsets
  this_combination_tables <- EAVtables_CreateItemsetDatasets
  this_combination_dates <- datevar_CreateItemsetDatasets
  this_rename_var <- list(person_id=person_id_retrieve,date=date_retrieve,meaning_renamed = meaning_retrieve)
  this_study_variable_names <- study_variable_names_CreateItemsetDatasets
  this_itemset_rules <- itemset_CreateItemsetDatasets
  this_item_sets_of_our_study <- item_sets_of_our_study
  this_concept_id_itemset_retrieval <- concept_id_itemset_retrieval
}

thiscolumn_combinations_for_itemsets<- list(c("MEDICAL_OBSERVATIONS","mo_meaning"))
this_item_sets_of_our_study <- "END_PRACTICE"


print("RETRIEVE ITEMSET DATASETS")

extensionthisinstance <- unique(ifelse(length(list.files(thisdirinput, pattern = "\\.parquet$")) > 0, "parquet","csv" ))


for (column_comb in thiscolumn_combinations_for_itemsets) {
  column_comb_string <- paste(unlist(column_comb), collapse = "_")
  print(paste("extracting (sub)itemsets from the combination of tables/columns",column_comb_string,"whose list is:"))
  print(this_study_variable_names[[column_comb_string]])
  CreateItemsetDatasets(EAVtables = this_combination_tables[[column_comb_string]],
                        datevar = this_combination_dates[[column_comb_string]],
                        dateformat= "YYYYmmdd",
                        rename_col = this_rename_var, 
                        study_variable_names = this_study_variable_names[[column_comb_string]],
                        itemset = this_itemset_rules[[column_comb_string]],
                        dirinput = thisdirinput,
                        diroutput = thisdiroutput,
                        extension = c(extensionthisinstance))
}



extensionthisinstance <-"csv" 

print("Reconcile the files that form a same itemset dataset, and save in parquet")

for (concept in this_item_sets_of_our_study){
  print(paste0("Reconcile and save in parquet the itemset dataset ",concept,"_dataset"))
  data <- data.table(
    person_id = character(),
    date = as.Date(character()),
    mo_source_value = character(),
    mo_unit = character()
  )
  
  for (concept_file in this_concept_id_itemset_retrieval[[concept]]){

    if (extensionthisinstance == "csv") {
      rdatafile <- file.path(thisdiroutput,paste0(concept_file,".RData"))
      if (file.exists(rdatafile)) {
        load(rdatafile)
        data <- rbind(data,get(concept_file), fill = T)
      }
    }else{
      parquetfile <- file.path(thisdiroutput,paste0(concept_file,"_dataset.parquet"))
      if (file.exists(parquetfile)) {
        dt <- as.data.table(as.data.frame(polars::pl$read_parquet(parquetfile))) 
        data <- rbind(data, dt, fill = T)
      }
    }
    

  }
  if(nrow(data) > 0) data[, AVpair := NULL]
  polars::as_polars_df(data)$write_parquet(file.path(thisdiroutput,paste0(concept,"_dataset.parquet")))
}

