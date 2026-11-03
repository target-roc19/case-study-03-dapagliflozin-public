
#Author: Olga Paoletti, Davide Messina, Rosa Gini

#Date: 04/02/2024
#version 23: Fixed memory leak for EAV tables thanks to @SHayati

#Date: 06/04/2023
#version 22: addition of the parameter add_conceptset_name as an option to have the name of the conceptset in the file

#Date: 20/09/2022
#version 21: addition of the parameter add_conceptset_name as an option to have the name of the conceptset in the file

#'CreateConceptSetDatasets
#'
#' The function CreateConceptSetDatasets inspects a set of input tables af data and creates a group of datasets, each corresponding to a concept set. Each dataset contains the records of the input tables that match the corresponding concept set and is named out of it.
#'
#'
#' @param dataset a 2-level list containing, for each domain, the names of the corresponding input tables of data
#' @param codvar a 3-level list containing, for each input table of data and each domain, the name(s) of the column(s) containing the codes of interest
#' @param datevar (optional): a 2-level list containing, for each input table of data, the name(s) of the column(s) containing dates (only if extension=”csv”), to be saved as dates in the output
#' @param EAVtables (optional): a 2-level list specifying, for each domain, tables in a Entity-Attribute-Value structure; each table is listed with the name of two columns: the one contaning attributes and the one containing values
#' @param EAVattributes (optional): a 3-level list specifying, for each domain and table in a Entity-Attribute-Value structure, the attributes whose values should be browsed to retrieve codes belonging to that domain; each attribute is listed along with its coding system
#' @param dateformat (optional): a string containing the format of the dates in the input tables of data (only if -datevar- is indicated); the string must be in one of the following:
# YYYYDDMM...
#' @param rename_col (optional) this is a list of 3-level lists; each 3-level list contains a column name for each input table of data (associated to a data domain) to be renamed in the output (for instance: the personal identifier, or the date); in the output all the columns will be renamed with the name of the list.
#' @param filter_expression (optional) this is a 2-level lists: this is a logical condition in the columns that are specified in -rename_col-. This conditions is to be used to filter the input datasets before starting to filter the concept sets
#' @param concept_set_domains a 2-level list containing, for each concept set, the corresponding domain
#' @param concept_set_codes a 3-level list containing, for each concept set, for each coding system, the list of the corresponding codes to be used as inclusion criteria for records: records must be included if the their code(s) starts with at least one string in this list; the match is executed ignoring points
#' @param concept_set_codes_excl (optional) a 3-level list containing, for each concept set, for each coding system, the list of the corresponding codes to be used as exclusion criteria for records: records must be excluded if the their code(s) starts with at least one string in this list; the match is executed ignoring points
#' @param concept_set_names (optional) a vector containing the names of the concept sets to be processed; if this is missing, all the concept sets included in the previous lists are processed
#' @param vocabulary (optional) a 3-level list containing, for each table of data and data domain, the name of the column containing the vocabulary of the column(s) -codvar-
#' @param addtabcol a logical parameter, by default set to TRUE: if so, the columns "Table_cdm" and "Col" are added to the output, indicating respectively from which original table and column the code is taken.
#' @param verbose a logical parameter, by default set to FALSE. If it is TRUE additional intermediate output datasets will be shown in the R environment
#' @param discard_from_environment (optional) a logical parameter, by default set to FALSE. If it is TRUE, the output datasets are removed from the global environment
#' @param dirinput (optional) the directory where the input tables of data are stored. If not provided the working directory is considered.
#' @param diroutput (optional) the directory where the output concept sets datasets will be saved. If not provided the working directory is considered.
#' @param extension (optional) the extension of the input tables of data (csv and dta are supported)
#' @param vocabularies_with_dot_wildcard a list containing the vocabularies in which treat the character dot in codes as wildcard. If vocabulary this option can be set to "any"
#' @param vocabularies_with_keep_dot a list containing the vocabularies in which treat the character dot in codes as itself. If vocabulary this option can be set to "any"
#' @param vocabularies_with_exact_search a list containing the vocabularies in which the codes must match exactly. If vocabulary this option can be set to "any"
#' @param use_qs use package qs to compress final datasets and decrease computation time
#' @importFrom data.table :=
#'
#'
#' @details
#'
#' A concept set is a set of medical concepts (eg the concept set "DIABETES" may contain the concepts "type 2 diabetes" and "type 1 diabetes") that may be recorded in the tables of data in some coding systems (for instance, "ICD10", or "ATC"). Each concept set is associated to a data domain (eg "diagnosis" or "medication") which is the topic of one or more tables of data. When calling CreateConceptSetDatasets, the concept sets, their domains and the associated codes are listed as input in the format of multi-level lists.
#'
#' @seealso
#'
#' We open the table, add a column named "general" initially set to 0. For each concept set linked to the domain, we create a column named "Filter_conceptset" that takes the value 1 for each row that match the concept set codes. After checking for each concept set, the column general is updated and only the rows for which general=1 are kept. The dataset is saved locally as "FILTERED_table" (you will have these datasets in the global environment only if verbose=T).
#' We split each of the new FILTERED_table relying on the column "Filter_conceptset" and we create one dataset for each concept set and each dataset. (you will have these datasets in output only if verbose=T).
#' Finally we put together all the datasets related to the same concept set and we save it in the -dirtemp- given as input with the extenstion .R .
#'
#'
#'#'CHECK VOCABULARY
CreateConceptSetDatasets <- function(dataset, codvar, datevar, EAVtables, EAVattributes, dateformat, rename_col,
                                     filter_expression, concept_set_domains, concept_set_codes, concept_set_codes_excl,
                                     concept_set_names, vocabulary, addtabcol = T, verbose = T,
                                     discard_from_environment = F, dirinput = getwd(), diroutput = getwd(),
                                     extension = F, vocabularies_with_dot_wildcard, vocabularies_with_keep_dot,
                                     vocabularies_with_exact_search, vocabularies_with_exact_search_not_dot, use_qs = F,
                                     aggregate_concepts=NULL, add_conceptset_name=T, suffix=NULL) {
  
  # TODO fix verbose
  if (!verbose) defer(options(warn = 1))
  
  #Check that output folder exist otherwise create it
  if (grepl("/$", diroutput)) {diroutput <- substr(diroutput, 1, nchar(diroutput) - 1)}
  dir.create(file.path(diroutput), showWarnings = FALSE)
  
  if (extension == F) {extension_flag = T} else {extension_flag = F}
  
  if (missing(concept_set_names)) {
    concept_set_names = unique(names(concept_set_domains))
  } else {
    concept_set_domains <- concept_set_domains[names(concept_set_domains) %in% concept_set_names]
    dataset <- dataset[names(dataset) %in% unique(concept_set_domains)]
  }
  
  if (use_qs) {n_threads <- data.table::getDTthreads()}
  
  used_domains <- unique(concept_set_domains)
  
  concept_set_dom <- split(names(concept_set_domains), unlist(concept_set_domains))
  
  partial_concepts <- vector(mode = "list")
  
  for (dom in used_domains) {
    
    dataset_in_dom <- dataset[[dom]]
    if (!missing(EAVtables) && !missing(EAVattributes) && dom %in% names(EAVtables) && length(EAVattributes)!=0) {
      for (EAVtab_dom in names(EAVattributes[[dom]])) {
        dataset_in_dom <- append(dataset_in_dom, EAVtab_dom[[1]][[1]])
      }
    }
    
    print(paste("I'm analysing domain", dom))
    
    #for (df2 in list(dataset_in_dom[[23]])) {
    for (df2 in dataset_in_dom) {
      print(paste0("I'm analysing table ", df2, " [for domain ", dom, "]"))
      
      # if(all(sapply(concept_set_dom[[dom]],function(concept)file.exists(paste0(diroutput, "/",concept, "~", df2, "~", dom,".RData"))))){
      #   for(conc in concept_set_dom[[dom]])
      #     partial_concepts <- append(partial_concepts, paste0(conc, "~", df2, "~", dom))
      #   next
      # }
      
      polars_calls_list <- list()
      
      if (extension_flag) {
        files <- list.files(dirinput)
        file_name <- files[stringr::str_detect(files, df2)][[1]]
        extension <- paste0(stringr::str_extract(file_name, "(?<=\\.).*"))
      } else {
        file_name <- paste0(df2, ".", extension)
      }
      path = paste0(dirinput, "/", file_name)
      if (extension == "dta") {used_df <- data.table::as.data.table(haven::read_dta(path))
      } else if (extension == "csv") {
        
        # Need to use schema_overrides at least for codvar at importation to retain leading/trailing 0s in codes which seems float
        newlist <- list()
        for (change_cols in c("person_id", codvar[[dom]][[df2]])) {
          newlist[[change_cols]] <- polars::pl$String
        }
        
        
        
        lazy_frame_df2 <- polars::pl$scan_csv(path, infer_schema = FALSE)
        
      } else if (extension == "RData") {assign('used_df', get(load(path)))
      } else if (extension == "parquet") {
        lazy_frame_df2 <- polars::pl$scan_parquet(path)
      } else {stop("File extension not recognized. Please use a supported file")}
      
      if (!missing(vocabulary) && dom %in% names(vocabulary) && df2 %in% names(vocabulary[[dom]])) {
        # Exclude those records with no specified vocabulary
        lazy_frame_df2 <- lazy_frame_df2$filter(polars::pl$col(vocabulary[[dom]][[df2]]) != "")
        lazy_frame_tmp <- copy(lazy_frame_df2)
        cod_system_indataset1 <- as.list(lazy_frame_tmp$select(polars::pl$col(vocabulary[[dom]][[df2]]))$unique()$collect(engine = "streaming"), as_series = FALSE)
        cod_system_indataset1 <- unlist(cod_system_indataset1)
        cod_system_indataset1_excl <- unlist(cod_system_indataset1)
      }
      
      if (!missing(dateformat)){
        for (datevar_dom_df2 in datevar[[dom]][[df2]]) {
          
          test_vect <- list()
          test_vect[[datevar_dom_df2]] <- polars::pl$String
          lazy_frame_df2 <- lazy_frame_df2$cast(!!!test_vect)
          
          first_char <- substring(dateformat, 1,1)
          if (stringr::str_count(dateformat, "m") == 3 || stringr::str_count(dateformat, "M") == 3) {
            new_dateformat <- "%d%b%Y"
          } else if (first_char %in% c("Y", "y")) {
            new_dateformat <- "%Y%m%d"
          } else if (first_char %in% c("D", "d")) {
            new_dateformat <- "%d%m%Y"
          }
          
          lazy_frame_df2 <- lazy_frame_df2$with_columns(polars::pl$col(datevar_dom_df2)$str$to_date(new_dateformat))
        }
      }
      
      # TODO add test, then convert to polars
      if(!missing(rename_col)){
        
        ###################RENAME THE COLUMNS ID AND DATE
        for (elem in names(rename_col)) {
          data <- rename_col[[elem]]
          if (data[[dom]][[df2]] %in% names(lazy_frame_df2$collect_schema())) {
            # data.table::setnames(used_df, data[[dom]][[df2]], elem)
            test_vect <- elem
            names(test_vect) <- data[[dom]][[df2]]
            lazy_frame_df2 <- lazy_frame_df2$rename(!!!test_vect)
          }
        }
      }
      
      # TODO add test, then convert to polars
      if (!missing(filter_expression) && !is.null(filter_expression)) {
        #if (!is.null(filter_expression)) {
        # used_df <- used_df[eval(parse(text = filter_expression)), ]
        lazy_frame_df2 <- lazy_frame_df2$filter(eval(parse(text = filter_expression)))
      }
      
      # TODO add test, then convert to polars
      #Pre computing used_dfAEV
      used_dfAEVs = vector(mode="list")
      if (!missing(EAVtables)){
        for(dom2 in used_domains){
          used_dfAEV<-data.table::data.table()
          
          for (p in seq_along(EAVtables[[dom]])) {
            if (df2 %in% EAVtables[[dom]][[p]][[1]][[1]]) {
              
              for (elem1 in names(EAVattributes[[dom2]][[df2]])) {
                #TODO improve naming of lenght_first_df2, df2_elem and EAV_concept_p
                lenght_first_df2 <- length(EAVattributes[[dom2]][[df2]][[elem1]][[1]])
                EAV_concept_p <- EAVtables[[dom2]][[p]]
                for (df2_elem in EAVattributes[[dom2]][[df2]][[elem1]]) {
                  if (lenght_first_df2 >= 2){
                    used_dfAEV <- rbind(used_dfAEV, used_df[get(EAV_concept_p[[1]][[2]]) == df2_elem[[1]] & get(EAV_concept_p[[1]][[3]])==df2_elem[[2]],],fill=T)
                  }else{
                    used_dfAEV <- rbind(used_dfAEV, used_df[get(EAV_concept_p[[2]]) == df2_elem[[1]],])
                  }
                }
              }
              
            }
          }
          used_dfAEVs[[dom2]] <- data.table::copy(used_dfAEV)
        }
      }
      
      #for each dataset search for the codes in all concept sets
      for (concept in concept_set_dom[[dom]]) {
        
        # if(file.exists(paste0(diroutput, "/",concept, "~", df2, "~", dom, ".RData"))){
        #   partial_concepts <- append(partial_concepts, paste0(concept, "~", df2, "~", dom))
        #   next
        # }
        
        # TODO add test for two or more concepts
        lazy_frame <- lazy_frame_df2$clone()
        
        col_concept <- paste0("Col_",concept)
        conc_dom <- concept_set_domains[[concept]]
        
        print(paste("concept set", concept))
        
        if (!missing(EAVtables)) {
          # TODO add test, then convert to polars
          # NOTE correct place and method for assignment?
          for (p in seq_along(EAVtables[[dom]])) {
            if (df2 %in% EAVtables[[dom]][[p]][[1]][[1]]) {
              rm(used_df)
              gc()
              used_df <- data.table::copy(used_dfAEVs[[conc_dom]])
            }
          }
        }
        
        # TODO to be removed. When using lazyframes this might increase computation time
        if (!missing(vocabulary) && dom %in% names(vocabulary) && df2 %in% names(vocabulary[[dom]])) {
          cod_system_indataset <- intersect(cod_system_indataset1,names(concept_set_codes[[concept]]))
        } else {
          cod_system_indataset <- names(concept_set_codes[[concept]])
        }
        
        if (length(cod_system_indataset) == 0) {
          
          # TODO write test and then activate polars modification to test it
          # used_df <- used_df[, c(col_concept, "Filter") := 0, ]
          
          test_vect <- list()
          test_vect[[col_concept]] <- polars::pl$lit(NA_character_)
          test_vect[["Filter"]] <- polars::pl$lit(0L)
          
          lazy_frame <- lazy_frame$with_columns(!!!test_vect)
        } else {
          for (col in codvar[[conc_dom]][[df2]]) {
            lazy_frame <- lazy_frame$with_columns(polars::pl$col(col)$str$replace("\\.", "")$alias(paste0(col, "_tmp")))
            
            for (type_cod in cod_system_indataset) {
              codes_rev <- concept_set_codes[[concept]][[type_cod]]
              
              lower_codes_rev <- tolower(as.character(codes_rev))
              all_codes_str <- c("all", "all codes", "all_codes")
              
              # TODO add tests
              if (any(all_codes_str %in% lower_codes_rev)) {
                print(paste("Using all codes for concept", concept))
                # used_df[, Filter:=1]
                # NOTE next or break? all codes is for all type of codes or just one?
                # used_df[, list(col_concept) := codvar[[dom]][[df2]][1]]
                
                test_vect <- list()
                test_vect[["Filter"]] <- polars::pl$lit(1L)
                test_vect[[col_concept]] <- codvar[[dom]][[df2]][1]
                lazy_frame <- lazy_frame$with_columns(!!!test_vect)
                
                next
              }
              
              if (df2 %in% dataset[[dom]]) {################### IF I GIVE VOCABULARY IN INPUT
                pattern_base <- paste0("^", codes_rev)
                pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                column_to_search <- paste0(col, "_tmp")
                pattern <- gsub("\\*", ".", pattern_no_dot)
                vocab_dom_df2_eq_type_cod <- TRUE
                
                if (!missing(vocabularies_with_dot_wildcard) && (type_cod %in% vocabularies_with_dot_wildcard || "any" %in% vocabularies_with_dot_wildcard)) {
                  pattern <- paste(pattern_base, collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_keep_dot) && (type_cod %in% vocabularies_with_keep_dot || "any" %in% vocabularies_with_keep_dot)) {
                  pattern <- paste(gsub("\\.", "\\\\.", pattern_base), collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_exact_search) && (type_cod %in% vocabularies_with_exact_search || "any" %in% vocabularies_with_exact_search)) {
                  pattern <- paste0(pattern_base, "$", collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_exact_search_not_dot) && (type_cod %in% vocabularies_with_exact_search_not_dot || "any" %in% vocabularies_with_exact_search_not_dot)) {
                  pattern <- paste0(gsub("\\.", "", pattern_base), "$", collapse = "|")
                }
                
                # if (!missing(vocabulary) && dom %in% names(vocabulary)) {
                #   vocab_dom_df2_eq_type_cod <- used_df[, get(vocabulary[[dom]][[df2]])] == type_cod
                # }
                
                #                 used_df[vocab_dom_df2_eq_type_cod & stringr::str_detect(get(column_to_search), pattern),
                #                         c("Filter", col_concept) := list(1, col)]
                
                test_vect <- list()
                test_vect[["Filter"]] <- 1L
                test_vect[[col_concept]] <- polars::pl$lit(col)
                test_vect_oth <- list()
                test_vect_oth[["Filter"]] <- "Filter"
                test_vect_oth[[col_concept]] <- col_concept
                test_vect_base <- list()
                test_vect_base[["Filter"]] <- polars::pl$coalesce(polars::pl$col("^Filter$"), 0L)
                test_vect_base[[col_concept]] <- polars::pl$coalesce(polars::pl$col(paste0("^", col_concept, "$")), 0L)
                
                if (!missing(vocabulary) && dom %in% names(vocabulary)) {
                  lazy_frame <- lazy_frame$with_columns(
                    !!!test_vect_base
                  )$with_columns(
                    polars::pl$when(
                      polars::pl$col(vocabulary[[dom]][[df2]]) == type_cod & polars::pl$col(column_to_search)$str$contains(pattern)
                    )$then(
                      polars::pl$struct(!!!test_vect))$otherwise(
                        polars::pl$struct(!!!test_vect_oth)
                      )$struct$unnest()
                  )
                } else {
                  lazy_frame <- lazy_frame$with_columns(
                    !!!test_vect_base
                  )$with_columns(
                    polars::pl$when(
                      polars::pl$col(column_to_search)$str$contains(pattern)
                    )$then(
                      polars::pl$struct(!!!test_vect))$otherwise(
                        polars::pl$struct(!!!test_vect_oth)
                      )$struct$unnest()
                  )
                }
                
              } else {
                for (EAVtab_dom in EAVtables[[dom]]) {
                  if (df2 %in% EAVtab_dom[[1]][[1]]) {
                    # TODO add test to check if changes to polars are working
                    rn0_ = used_df[,get(vocabulary[[dom]][[df2]])] == type_cod
                    if(!any(rn0_))
                      next
                    #rnx_ = gsub("\\*", ".", paste(gsub("\\.", "", paste0("^", codes_rev)), collapse = "|"))
                    rnx_ = paste0("^(",gsub("\\*", ".", paste(gsub("\\.", "", codes_rev), collapse = "|")),")")
                    rn1_ = stringr::str_detect(used_df[rn0_,get(paste0(col, "_tmp"))], rnx_)
                    rn0_[rn0_] = rn1_
                    # used_df[rn0_, c("Filter", paste0("Col_", concept)) := list(1, paste0(EAVtab_dom[[1]][[2]], EAVtab_dom[[1]][[3]]))]
                    test_vect <- list()
                    test_vect[["Filter"]] <- 1L
                    test_vect[[col_concept]] <- polars::pl$lit(paste0(EAVtab_dom[[1]][[2]], EAVtab_dom[[1]][[3]]))
                    lazy_frame <- lazy_frame$with_columns(polars::pl$when(vocab_dom_df2_eq_type_cod & polars::pl$col(column_to_search)$str$contains(pattern)
                    )$then(polars::pl$struct(!!!test_vect))$struct$unnest())
                  }
                }
              }
            }
            
            # TODO add tests regarding exclusion of codes
            if (!missing(concept_set_codes_excl)){
              if (!missing(vocabulary) && dom %in% names(vocabulary) && df2 %in% names(vocabulary[[dom]])) {
                cod_system_indataset_excl <- intersect(cod_system_indataset1_excl,names(concept_set_codes_excl[[concept]]))
              } else {
                cod_system_indataset_excl <- names(concept_set_codes_excl[[concept]])
              }
              for (type_cod_2 in cod_system_indataset_excl) {
                codes_rev <- concept_set_codes_excl[[concept]][[type_cod_2]]
                pattern_base <- paste0("^", codes_rev)
                pattern_no_dot <- paste(gsub("\\.", "", pattern_base), collapse = "|")
                pattern <- gsub("\\*", ".", pattern_no_dot)
                column_to_search <- paste0(col, "_tmp")
                vocab_dom_df2_eq_type_cod <- TRUE
                
                if (!missing(vocabularies_with_dot_wildcard) && (type_cod_2 %in% vocabularies_with_dot_wildcard || "any" %in% vocabularies_with_dot_wildcard)) {
                  pattern <- paste(pattern_base, collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_keep_dot) && (type_cod_2 %in% vocabularies_with_keep_dot || "any" %in% vocabularies_with_keep_dot)) {
                  pattern <- paste(gsub("\\.", "\\\\.", pattern_base), collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_exact_search) && (type_cod_2 %in% vocabularies_with_exact_search || "any" %in%vocabularies_with_exact_search)) {
                  pattern <- paste0(pattern_base, "$", collapse = "|")
                  column_to_search <- col
                } else if (!missing(vocabularies_with_exact_search_not_dot) && (type_cod_2 %in% vocabularies_with_exact_search_not_dot || "any" %in% vocabularies_with_exact_search_not_dot)) {
                  pattern <- paste0(gsub("\\.", "", pattern_base), "$", collapse = "|")
                }
                
                # if (!missing(vocabulary) && df2 %in% dataset[[dom]] && dom %in% names(vocabulary)) {
                #   vocab_dom_df2_eq_type_cod <- used_df[, get(vocabulary[[dom]][[df2]])] == type_cod_2
                # }
                
                # test_vect_base can be skipped here: first we include then exclude
                test_vect <- list()
                test_vect[["Filter"]] <- 0L
                test_vect[[col_concept]] <- polars::pl$lit(col)
                test_vect_oth <- list()
                test_vect_oth[["Filter"]] <- "Filter"
                test_vect_oth[[col_concept]] <- col_concept
                lazy_frame <- lazy_frame$with_columns(
                  polars::pl$when(
                    polars::pl$col(vocabulary[[dom]][[df2]]) == type_cod_2 & polars::pl$col(column_to_search)$str$contains(pattern)
                  )$then(
                    polars::pl$struct(!!!test_vect))$otherwise(
                      polars::pl$struct(!!!test_vect_oth)
                    )$struct$unnest()
                )
                
              }
            }
            lazy_frame <- lazy_frame$drop(paste0(col, "_tmp"))
          }
        }
        
        if (addtabcol == F) {
          lazy_frame <- lazy_frame$drop(col_concept)
          lazy_frame <- lazy_frame$filter(polars::pl$col("Filter") == 1)$drop("Filter")
        } else {
          if ("Col" %in% names(lazy_frame)) {
            # Col <- NULL
            # used_df[, Col := NULL]
            # lazy_frame <- lazy_frame$drop("Col")
          }
          # data.table::setnames(used_df, col_concept, "Col")
          # filtered_concept <- data.table::copy(used_df[Filter == 1, ])[, c("Filter", "Table_cdm") := list(NULL, df2)]
          # used_df[, "Filter" := NULL]
          test <- "Col"
          names(test) <- col_concept
          lazy_frame <- lazy_frame$rename(!!!test)
          lazy_frame <- lazy_frame$filter(polars::pl$col("Filter") == 1)$drop("Filter")$with_columns(Table_cdm = polars::pl$lit(df2))
        }
        
        # used_df <- data.table::data.table(as.data.frame(lazy_frame$collect()))
        
        for (col in codvar[[dom]][[df2]]) {
          if (col %in% names(lazy_frame)) {
            test_vect <- "codvar"
            names(test_vect) <- col
            lazy_frame <- lazy_frame$rename(!!!test_vect)
          }
        }
        
        if (!missing(add_conceptset_name)) {
          if (add_conceptset_name==T) lazy_frame <- lazy_frame$with_columns(Conceptset = polars::pl$lit(concept))
        }
        
        name_export_df <- paste0(concept, "~", df2, "~", dom)
        partial_concepts <- append(partial_concepts, name_export_df)
        
        disk_path <- paste0(diroutput, "/", concept, "~", df2, "~", dom, ".parquet")
        lazy_frame <- lazy_frame$lazy_sink_parquet(disk_path)
        
        polars_calls_list <- append(polars_calls_list, lazy_frame)
        
        # filtered_concept <- data.table::data.table(as.data.frame(lazy_frame$collect()))
        rm(lazy_frame)
      }
      
      # rm(used_df)
      if (!missing(EAVtables))
        rm(used_dfAEVs)
      
      polars::pl$collect_all(polars_calls_list, engine = "streaming")
      
    }
  }
  
  for (concept in concept_set_names) {
    
    print(paste("Merging and saving the concept", concept))
    final_concept <- data.table::data.table()
    list_final_concept <- list(final_concept)
    
    final_concept_name <- if (!is.null(suffix)) paste0(concept, suffix) else concept
    
    list_partial_concept <- list.files(diroutput)[grepl(paste0(concept, "~", ".*", ".parquet"), list.files(diroutput))]
    list_partial_concept <- lapply(list_partial_concept, function(x) polars::pl$scan_parquet(file.path(diroutput, x)))
    pl$concat(!!!list_partial_concept, how = "diagonal_relaxed")$sink_parquet(paste0(diroutput, "/", final_concept_name, ".parquet"))
    
    # tryCatch(
    #   error = function(cnd) {
    #     warning(paste0("There was an error in reading and savinf partial concept"))
    #     polars::pl$LazyFrame()$sink_parquet(paste0(diroutput, "/", final_concept_name, ".parquet"))
    #   },
    #   lazy_frame <- polars::pl$scan_parquet(paste0(diroutput, "/", concept, "~", "*", ".parquet"))$sink_parquet(paste0(diroutput, "/", final_concept_name, ".parquet"))
    # )
    
    # if (use_qs) {
    #   qs::qsave(get(final_concept),
    #             file = paste0(diroutput, "/", concept, ".qs"),
    #             preset = "high", nthreads = n_threads)
    # } else {
    #   save(final_concept, file = paste0(diroutput, "/", concept, ".RData"))
    # }
    # rm(final_concept)
    
    for (single_file in partial_concepts[stringr::str_detect(sub("~.*", "", partial_concepts), paste0("^", concept, "$"))]) {
      file.remove(paste0(diroutput, "/", single_file, ".parquet"))
    }
    
  }
  print(paste("Concept set datasets saved in",diroutput))
}
