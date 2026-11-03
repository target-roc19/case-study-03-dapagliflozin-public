###################################################################
# CREATE RULE TO EXTRACT ITEMSETS DATASETS FROM ALL COMBINATIONS OF CDM TABLES AND COLUMNS
###################################################################

if (TEST) {
  thisdatasource <- TEST_DS
}else{
  CDM_SOURCE <- fread(file.path(dirinput, "CDM_SOURCE.csv"))
  thisdatasource <- CDM_SOURCE$data_source_name
}


# -item_sets_of_our_study- is the list of itemsets that will contribute to study variables

itemsets_sheet_total <- fread(file.path(dirarchive,"itemsets_cs3.csv"))
# 

itemsets_sheet_total[, ds_name := as.character(ds_name)]

itemsets_sheet_total[cdm_table_name == "MEDICAL_OBSERVATION", cdm_table_name := "MEDICAL_OBSERVATIONS"]


itemsets_sheet <- itemsets_sheet_total[,ds_name := thisdatasource]


LAB_concept_ids <- unname(unlist(unique(itemsets_sheet[grepl("LAB",concept_id),.(concept_id)])))
  

#############################DO NOT CHANGE FROM HERE

# maximum number of columns to be used to filter the itemsets

max_n_columns_itemsets <- 3


# clean when missing in column_name_X and expected_value_X is coded as string 'NA' and convert it to true missing

for (j in 1:max_n_columns_itemsets) {
  varname <- paste0("column_name_",j)
  itemsets_sheet <- itemsets_sheet[get(varname) == "NA", (varname) := NA]
  itemsets_sheet <- itemsets_sheet[get(varname) == "", (varname) := NA]
  varname <- paste0("expected_value_",j)
  itemsets_sheet <- itemsets_sheet[get(varname) == "NA", (varname) := NA]
  itemsets_sheet <- itemsets_sheet[get(varname) == "", (varname) := NA]
}

tokeep <- c("concept_id","ds_name","cdm_table_name",paste0("column_name_",1:max_n_columns_itemsets),paste0("expected_value_",1:max_n_columns_itemsets))
itemsets_sheet <- unique(itemsets_sheet[,..tokeep])


############################
# check some expected things
# column and expected values 1 are always non-empty

for (varname in c("cdm_table_name","column_name_1","expected_value_1")) {
  if (nrow(itemsets_sheet[is.na(get(varname)),]) > 0) {
    errmess <- paste("In the dap_specific_concept_map file there are rows where",varname,"is missing, which should not be possible, you can see them by printing -View(itemsets_sheet[is.na(get(varname)),])- ")
    stop(errmess)
  }
}

# if a column is empty, the next is empty too

for (j in 2:max_n_columns_itemsets) {
  jp <- j - 1
  var <- "column_name_"
  varname <- paste0(var,j)
  varnamep <- paste0(var,jp)
  if (nrow(itemsets_sheet[is.na(get(varnamep)) & !is.na(get(varname)),]) > 0) {
    errmess <- paste("In the dap_specific_concept_map file there are rows where",varnamep,"is missing, but ",varname,"is not, which should not be possible, you can see them by printing -itemsets_sheet[is.na(get(varnamep)) & !is.na(get(varname)),])- ")
    stop(errmess)
  }
}




##############################
# list of itemsets 

item_sets_of_our_study <- unique(unlist(itemsets_sheet[!is.na(concept_id),.(concept_id)]))

# list of combination of columns to be inspected to retrieve itemsets
column_combinations_for_itemsets <- itemsets_sheet[, {
  cols <- c(cdm_table_name, column_name_1, column_name_2, column_name_3)
  # Remove empty strings
  cols <- cols[cols != ""]
  cols <- cols[!is.na(cols)]
  # Return as list
  list(list(cols))
}, by = .(cdm_table_name, column_name_1, column_name_2, column_name_3)]$V1
column_combinations_for_itemsets <- unique(column_combinations_for_itemsets)

# for each itemset, identify the combination of columns used to retrieved it. if it's > 1, create a subitemset for each combination of columns

concept_id_itemset_retrieval <- list()
itemset_of_subitemset <- list()
column_combinations_for_itemset <- list()

for (concept in item_sets_of_our_study){
  column_combinations_for_itemset[[concept]] <- itemsets_sheet[concept_id == concept, {
    cols <- c(cdm_table_name, column_name_1, column_name_2, column_name_3)
    # Remove empty strings
    cols <- cols[cols != ""]
    cols <- cols[!is.na(cols)]
    # Return as list
    list(list(cols))
  }, by = .(cdm_table_name, column_name_1, column_name_2, column_name_3)]$V1
  column_combinations_for_itemset[[concept]] <- unique(column_combinations_for_itemset[[concept]])
  if (length(column_combinations_for_itemset[[concept]]) == 1 ) {
    concept_id_itemset_retrieval[[concept]] <- concept
    itemset_of_subitemset[[concept]] <- concept
  }else{
    # concept_id_itemset_retrieval[[concept]] <- paste0(paste0(concept,"_"),paste(column_combinations_for_itemset[[concept]],collapse = "_"))
    concept_id_itemset_retrieval[[concept]] <- paste0(paste0(concept,"_"), sapply(column_combinations_for_itemset[[concept]], paste, collapse = "_"))
    for (subitemset in concept_id_itemset_retrieval[[concept]]) {
      itemset_of_subitemset[[subitemset]] <- concept 
    }
  }
}

###############################
# create the list of subitemset datasets that will be created

# the overall list of subitemset datasets

item_sets_of_our_study_subitemsets <- unique(unlist(concept_id_itemset_retrieval, use.names = FALSE))

# list of itemsets and of subitemsets for each combination of columns (argument is not the combination per se, but the string obtained collapsing it with '_')

itemsets_column_comb <- list()
subitemsets_column_comb <- list()

for (column_comb in column_combinations_for_itemsets) {
  column_comb_string <- paste(unlist(column_comb), collapse = "_")
  itemsets_column_comb[[column_comb_string]] <- c()
  subitemsets_column_comb[[column_comb_string]] <- c()
  for (concept in item_sets_of_our_study) {
    column_combinations_concept_string <- sapply(column_combinations_for_itemset[[concept]], paste, collapse = "_")
    if (column_comb_string %in% column_combinations_concept_string) {
      itemsets_column_comb[[column_comb_string]] <- c(itemsets_column_comb[[column_comb_string]],concept)
      if (length(column_combinations_for_itemset[[concept]]) == 1 ) {
        subitemsets_column_comb[[column_comb_string]] <- c(subitemsets_column_comb[[column_comb_string]],concept)   
      }else{
        subitemsets_column_comb[[column_comb_string]] <- c(subitemsets_column_comb[[column_comb_string]], paste0(paste0(concept,"_",column_comb_string)))
      }
    }
  }
}

################################################
# CreateItemsetDatasets will be called for each combination of columns, so we must create its arguments for each combination

EAVtables_CreateItemsetDatasets <- list()
datevar_CreateItemsetDatasets <- list()
study_variable_names_CreateItemsetDatasets <- list()
itemset_CreateItemsetDatasets <- list()


for (column_comb in column_combinations_for_itemsets) {
  column_comb_string <- paste(unlist(column_comb), collapse = "_")
  vec_column_comb <- unlist(column_comb)
  for (tab in files_ConcePTION_CDM_tables[[vec_column_comb[1]]]) {
    EAVtables_CreateItemsetDatasets[[column_comb_string]][[tab]] <- vec_column_comb[-1]
    # exception: in 02_parameters_CDM we create the list 'meaning_retrieve' that we then use in CreateItemsetDataset, it renames the columns meaning_retrieve[[tab]] as 'meaning_renamed'. so if in vec_column_comb[-1] we list a column of type 'meaning....' this later throws an error when we invoke CreateItemsetDataset. so we must handle this case
    for (s in 1:length(EAVtables_CreateItemsetDatasets[[column_comb_string]][[tab]])) {
      if (EAVtables_CreateItemsetDatasets[[column_comb_string]][[tab]][s] == meaning_retrieve[[tab]]) {
        EAVtables_CreateItemsetDatasets[[column_comb_string]][[tab]][s] <- "meaning_renamed"
      }
    }
    datevar_CreateItemsetDatasets[[column_comb_string]][[tab]] <- ConcePTION_CDM_datevar_retrieve[[tab]]
    study_variable_names_CreateItemsetDatasets[[column_comb_string]] <- subitemsets_column_comb[[column_comb_string]]
    j <- length(vec_column_comb) - 1 
    column_names <- paste0("column_name_",1:j)
    expected_values <- paste0("expected_value_",1:j)
    condition <- c()
    for (s in 1:j) {
      condition <- c(condition,c(paste0("column_name_",s," == '",vec_column_comb[-1][s],"'")))
    }
    if (j < max_n_columns_itemsets) {
      s <- j + 1
      condition <- c(condition,c(paste0("is.na(column_name_",s,")")))
    }
    condition <- paste(condition, collapse = " & ")
    for (subitemset in subitemsets_column_comb[[column_comb_string]]) {
      conditionsub <- paste0(condition , " & concept_id == '", itemset_of_subitemset[[subitemset]],"' & cdm_table_name == '",  vec_column_comb[1],"'")
      distinct_values <- unique( itemsets_sheet[eval(parse(text = conditionsub)) , ..expected_values])
      itemset_CreateItemsetDatasets[[column_comb_string]][[subitemset]][[tab]] <- lapply(seq_len(nrow(distinct_values)), function(i) unname(as.list(distinct_values[i])))
    }
  }
}




# CreateItemsetDatasets(EAVtables = EAVtables_CreateItemsetDatasets[[column_comb_string]],
#                       datevar= datevar_CreateItemsetDatasets[[column_comb_string]],
#                       dateformat= "YYYYmmdd",
#                       rename_col = list(person_id=person_id_retrieve,date=date_retrieve,meaning_renamed = meaning_retrieve),
#                       study_variable_names = study_variable_names_CreateItemsetDatasets[[column_comb_string]],
#                       itemset = itemset_CreateItemsetDatasets[[column_comb_string]],
#                       dirinput = dirinput,
#                       diroutput = "C:/temp/",
#                       extension = c("csv"))

