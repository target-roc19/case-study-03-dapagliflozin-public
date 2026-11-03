# the output of this step are several lists and lists of lists to be used by CreateConceptsetDatasets

# concept_set_domains
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 domain

# concept_set_codes_our_study,
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes included in that conceptset for that coding system

# concept_set_codes_our_study_excl
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes to be excluded from that conceptset for that coding system

# the lists of all conceptsets 
# concept_sets_of_our_study


# read the list of study variables

study_variables_sheet <- fread(file.path(dircheckmetadata,"STUDY_CODELISTS.csv"))


##############################
# read the codelists and stores them in rds

full_codelist <- unique(rbind(fread(file.path(dircodelist,"ROC19_codelist.csv")),fread(file.path(dircodelist,"ROC19_Dapagliflozin.csv")),fread(file.path(dircodelist,"ROC19_DPP4i.csv"))))

full_codelist[system == "DP" | system == "LAB" , concept_id := paste0(system, "_", event_abbreviation) ]
full_codelist[concept_id == "" | is.na(concept_id), concept_id := paste0(system, "_", event_abbreviation,"_", type) ]



#####
# events

event_codelist <- full_codelist[system != "DP" & system != "TP",]
 
# # Create the new variable 'concept_id'
# event_codelist[, concept_id := paste(system, event_abbreviation, type, sep = "_")]

# # save as rds
# saveRDS(event_codelist, file = file.path(dircodelist,"event_codelist.rds"))

####
# medicines

medicine_codelist <- full_codelist[system == "DP",]

# # Create the new variable 'concept_id'
# medicine_codelist<-medicines_codelist[, concept_id := paste(system, event_abbreviation, sep = "_")]
# 
# # save as rds
# saveRDS(medicines_codelist, file = file.path(dircodelist,"medicines_codelist.rds"))

####
# procedures
procedure_codelist <- full_codelist[system == "TP",]

# # Create the new variable 'concept_id'
# procedure_codelist[, concept_id := paste(system, event_abbreviation, type, sep = "_")]

# # save as rds
# saveRDS(procedure_codelist, file = file.path(dircodelist,"procedure_codelist_codelist.rds"))

###################################
# initialize the lists to be created

concept_set_codes_our_study <- vector(mode="list")

concept_set_codes_our_study_excl <- vector(mode="list")

concept_sets_of_our_study <- c()


concept_set_domains <- list()

###############################
# CODELISTS OF DIAGNOSIS
# list of concept_id's that refer to this case study and to codelists of diagnosis

concept_ids_diagnosis <- unique(unlist(study_variables_sheet[case_study == thisstudy & (type_of_concept_id == "diagnosis codelist" | type_of_concept_id== "diagnosis_codelist") & !is.na(concept_id),.(concept_id)]))

# codelist of diagnosis restricted to diagnosis of interest

event_codelist <- event_codelist[concept_id %in% concept_ids_diagnosis,]

# concept_set_codes_our_nar <- df_to_list_of_list(event_codelist[tags == "narrow", ], codying_system_recode = "auto", type_col = "type")
recoding_coding_systems_to_from <- list()
if (thisdatasource == "BIFAP") {
  recoding_coding_systems_to_from[["ICD10CM"]] <- c("ICD10", "ICD10CM","ICD10_2019")
}else{
  recoding_coding_systems_to_from[["ICD10"]] <- c("ICD10", "ICD10CM","ICD10_2019")
}
recoding_coding_systems_to_from[["ICD9CM"]] <- c("ICD9", "ICD9CM")
recoding_coding_systems_to_from[["SCTSPA"]] <- c("SNOMEDCT_US")

concept_set_codes_our_nar <- df_to_list_of_list(event_codelist[tags == "narrow", ], codying_system_recode = recoding_coding_systems_to_from, type_col = "type")

for (concept in concept_ids_diagnosis) {
     concept_set_domains[[concept]] = "Diagnosis"
     concept_set_codes_our_study[[concept]] <- concept_set_codes_our_nar[[paste0(concept,"_narrow")]]
}

# exclusion is only for coding systems 'start with'

coding_systems_start_with <- c("ICD10CM", "ICD9CM")

event_codelist_excl <- event_codelist[(tags == "exclude" | tags == "possible") & coding_system %in% coding_systems_start_with, ] 
event_codelist_excl[, tags := "exclude"]
concept_set_codes_our_excl <- df_to_list_of_list(event_codelist_excl, codying_system_recode = recoding_coding_systems_to_from, type_col = "type")



concept_sets_of_our_study <- c(concept_sets_of_our_study, concept_ids_diagnosis)


###############################
# CODELISTS OF PROCEDURES
# list of concept_id's that refer to codelists of diagnosis

concept_ids_procedures <- unique(unlist(study_variables_sheet[case_study == thisstudy & (type_of_concept_id == "diagnosis and procedure codelist" | type_of_concept_id == "diagnosis_and_procedure_codelist"| type_of_concept_id == "procedure_codelist") & !is.na(concept_id),.(concept_id)]))

concept_ids_procedures<- unique(c(concept_ids_procedures, "TP_HEARTTRANSPLANT_COV"))


# codelist of procedures
 

procedure_codelist <- procedure_codelist[concept_id %in% concept_ids_procedures,]

concept_set_codes_our_proc <- df_to_list_of_list(procedure_codelist, code_col = "code", concepts_col = "concept_id", codying_system_col = T, codying_system_recode = F)

for (concept in concept_ids_procedures) {
  concept_set_domains[[concept]] = "ProcDiag"
  concept_set_codes_our_study[[concept]] <- concept_set_codes_our_proc[[paste0("TP_",concept,"_COV_narrow")]]
  if (is.null(concept_set_codes_our_study[[concept]])) {
    concept_set_codes_our_study[[concept]] <- concept_set_codes_our_proc[[paste0("TP_",concept,"_AESI_narrow")]]
  }
}

concept_sets_of_our_study <- c(concept_sets_of_our_study, concept_ids_procedures)


###############################
# CODELISTS OF MEDICINES
# list of concept_id's that refer to codelists of medicines

concept_ids_medicines <- unique(unlist(study_variables_sheet[case_study == thisstudy & (type_of_concept_id == "medicine codelist" | type_of_concept_id == "medicine_codelist" | type_of_concept_id == "medicines_codelists" | type_of_concept_id == "medicine") & !is.na(concept_id),.(concept_id)]))

concept_ids_medicines <- unique( c(concept_ids_medicines,"DP_DDP4I","DP_DAPAGLIFLOZIN"))

# medicine_codelist <- readRDS(file = file.path(dircodelist,"medicines_codelist.rds"))

medicine_codelist <- medicine_codelist[tags == "Narrow",tags := "narrow"]

medicine_codelist <- medicine_codelist[concept_id %in% concept_ids_medicines,]

medicine_codelist <- medicine_codelist[,type := "DP"]

concept_set_codes_our_med <- df_to_list_of_list(medicine_codelist, code_col = "code", concepts_col = "concept_id", codying_system_col = F,codying_system_recode = "auto")

for (concept in concept_ids_medicines) {
  concept_set_domains[[concept]] =   "Medicines"
  #if(thisdatasource == "CPRD"){
  #  concept_set_domains[[concept]] =   "MedicinesMedProd"
  #}
  # concept_set_codes_our_study[[concept]] <- concept_set_codes_our_med[[paste0("system_",concept,"_DP_Narrow")]]
  concept_set_codes_our_study[[concept]] <- concept_set_codes_our_med[[paste0("DP_",concept,"_DP_narrow")]]
}

concept_sets_of_our_study <- c(concept_sets_of_our_study, concept_ids_medicines)

#################################
# normally the same concept_id is retrieved just from one concept sets, but there are exceptions, for example if a concept_id has multiple domains (e.g., both diagnosis and procedures). let's create a variable to handle this, to be used in step create_D2_CONCEPTS

concept_id_retrieval <- vector(mode="list")

for (concept in concept_sets_of_our_study){
  concept_id_retrieval[[concept]] <- concept
}

#################################
# coding systems start with and  exact search

coding_systems_start_with <- c("ATC")
coding_systems_exact_search <- setdiff(unlist(unique(event_codelist[,.(coding_system)])),coding_systems_start_with)