##%######################################################%##
#                                                          #
#### EXTRACT FROM CDM TABLES ONE DATASET PER CONCEPTSET ####
####  CONTAINING ONLY RECORDS WITH A CODE OF INTEREST   ####
#                                                          #
##%######################################################%##


print('RETRIEVE FROM CDM RECORDS CORRESPONDING TO CONCEPT SETS THAT WERE MISSING IN PREVIOUS RELEASES')

missing_concept_ids <- c("TP_HEARTTRANSPLANT_COV")

CreateConceptSetDatasets(concept_set_names = missing_concept_ids,
                         dataset = ConcePTION_CDM_tables,
                         codvar = ConcePTION_CDM_codvar,
                         datevar = ConcePTION_CDM_datevar,
                         EAVtables = ConcePTION_CDM_EAV_tables,
                         EAVattributes = ConcePTION_CDM_EAV_attributes_this_datasource,
                         dateformat= "YYYYmmdd",
                         vocabulary = ConcePTION_CDM_coding_system_cols,
                         rename_col = list(person_id = person_id, date = date,
                                           meaning_renamed = meaning_renamed),
                         concept_set_domains = concept_set_domains,
                         concept_set_codes =	concept_set_codes_our_study,
                         concept_set_codes_excl = concept_set_codes_our_study_excl,
                         dirinput = dirinput,
                         diroutput = dirtemp,
                         # extension = c("csv"),
                         vocabularies_with_dot_wildcard = c("READ"),
                         vocabularies_with_exact_search_not_dot = coding_systems_exact_search,
                         suffix = "_dataset")
#
