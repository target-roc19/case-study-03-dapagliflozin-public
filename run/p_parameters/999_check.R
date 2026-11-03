###############################################
#
# CHECK 1: compare concept_ids requested in the metadata 
# with the concept_ids stored in the codelist and itemset specifications
# in both directions


# all concept_id's in codelist

full_codelist_to_check <- full_codelist

# full_codelist_to_check[system == "DP" | system == "LAB" , concept_id := paste0(system, "_", event_abbreviation) ]
# full_codelist_to_check[concept_id == "" | is.na(concept_id), concept_id := paste0(system, "_", event_abbreviation,"_", type) ]

concept_ids_in_codelist <- unname(unlist(unique(full_codelist_to_check[,.(concept_id)])))

# all concept_id's in itemset

itemsets_sheet_total_to_check <- itemsets_sheet_total

concept_ids_in_itemset <- unname(unlist(unique(itemsets_sheet_total_to_check[,.(concept_id)])))

# concept_id's in either codelist or itemsets

concept_ids_with_extraction_rules <- unique(c(concept_ids_in_codelist, concept_ids_in_itemset))

# concept_id's in metadata

study_variables_sheet_to_check <- fread(file.path(dircheckmetadata,"STUDY_CODELISTS.csv"))[case_study == thisstudy,]

concept_ids_in_metadata <- unname(unlist(unique(study_variables_sheet_to_check[,.(concept_id)])))


# compare codelist with concept_id's in metadata

concept_ids_in_metadata_without_extraction_rules <- sort(setdiff(concept_ids_in_metadata, concept_ids_with_extraction_rules))

concept_ids_with_extraction_rules_not_in_metadata <- sort(setdiff( concept_ids_with_extraction_rules, concept_ids_in_metadata))

inboth <- intersect(concept_sets_of_our_study, concept_ids_in_codelist)

print(paste("Concept id's requested by the metadata but without extraction rules are the following"))
print(concept_ids_in_metadata_without_extraction_rules)

print(paste("Concept id's with extraction rules but not stored in the metadata are the following"))
print(concept_ids_with_extraction_rules_not_in_metadata)


#############################################
# CHECK 2
# check that concept_set_codes_our_study_excl does not have ambiguities, and if it does not, remove codes that have a 'narrow' descendant

has_descendant_in_include <- function(x, include) {
  any(startsWith(include, x) & include != x)
}

has_ascendant_in_include <- function(x, include) {
  any(startsWith(x, include) & include != x)
}

for (concept in concept_ids_diagnosis) {
  for (codsys in names(concept_set_codes_our_study_excl[[concept]])) {
    LIST_EXCLUDE <- concept_set_codes_our_study_excl[[concept]][[codsys]]
    LIST_INCLUDE <- concept_set_codes_our_study[[concept]][[codsys]]
    WITH_DESCENDANTS <- LIST_EXCLUDE[
      sapply(LIST_EXCLUDE, has_descendant_in_include, include = LIST_INCLUDE)
    ]
    AMBIGUOUS <- LIST_EXCLUDE[
      sapply(LIST_EXCLUDE, function(x) {
        has_descendant_in_include(x, LIST_INCLUDE) &&
          has_ascendant_in_include(x, LIST_INCLUDE)
      })
    ]
    if (length(WITH_DESCENDANTS) > 0) {
      if (length(AMBIGUOUS) > 0) { 
        errmess <- paste0("In the concept_id ",concept," for the coding system ",codsys," the codes ",AMBIGUOUS, " are to be excluded but have both ascendants and descendants that must be included. The simple 'start with' option of CreateConceptsetDataset cannot be applied. A more complex procedure must be used instead: the list of unique codes in the data source must be extracted in this step, and each of the codes found must be labelled as 'narrow' or 'exclude' based on the tag of its closest ascendant. The resulting extended codelist of 'narrow' codes can then be used in CreateConceptsetDataset with 'exact match'")
        stop(errmess)
      }else{
        print("No ambiguous code")
        # remove from the 'exclude' codelist the codes that have a 'narrow' descendant 
        concept_set_codes_our_study_excl[[concept]][[codsys]] <- setdiff(concept_set_codes_our_study_excl[[concept]][[codsys]], WITH_DESCENDANTS)
      }
    }
  }
}


