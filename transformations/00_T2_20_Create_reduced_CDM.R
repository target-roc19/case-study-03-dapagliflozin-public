############################################################
#                                                          #
####   CREATE REDUCED CDM_INSTANCE ####
# 
#                                                          #
############################################################

# author: Rosa Gini, Davide Messina

# v 0.1 18 Mar 2026

# generate reduced CDM instance for TARGET-EU


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_04_D3_OBSPERIODS"
  thisdirinput <- dirtemp
  thisdirinput1 <- dirinput
  thisdirreducedCDM <- dirreducedCDM
  thisfiles_ConcePTION_CDM_tables <- list()
  thisfiles_ConcePTION_CDM_tables <- files_ConcePTION_CDM_tables
  dir.create(thisdirreducedCDM, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdirinput1 <- dirinput
  thisdirreducedCDM <- dirreducedCDM 
  thisfiles_ConcePTION_CDM_tables <- list()
  thisfiles_ConcePTION_CDM_tables <- files_ConcePTION_CDM_tables
}
# copy the metadata tables

file.copy(file.path(dirinput,'/METADATA.csv'), thisdirreducedCDM, overwrite = T)
file.copy(file.path(dirinput,'/CDM_SOURCE.csv'), thisdirreducedCDM, overwrite = T)
file.copy(file.path(dirinput,'/INSTANCE.csv'), thisdirreducedCDM, overwrite = T)
if (file.exists(file.path(dirinput,'/PRODUCTS.csv'))) {
  file.copy(file.path(dirinput,'/PRODUCTS.csv'), thisdirreducedCDM, overwrite = T)
}

# Retrieve the date columns irrespective of domain
date_simplified <- data.table::copy(date)
names(date_simplified) <- NULL
date_simplified <- unlist(date_simplified, recursive = FALSE)

date_simplified <- tapply(unlist(date_simplified, use.names = FALSE),
                          rep(names(date_simplified), lengths(date_simplified)), FUN = c)


date_simplified <- lapply(date_simplified, unique)

# # list the 'seed' files
tablesCDM <- c("PERSONS",
               "VISIT_OCCURRENCE",
               "OBSERVATION_PERIODS",
               "EVENTS",
               "MEDICINES",
               "MEDICAL_OBSERVATIONS",
               "SURVEY_ID",
               "SURVEY_OBSERVATIONS",
               "VACCINES",
               "PROCEDURES")

thisfiles_ConcePTION_CDM_tables <- thisfiles_ConcePTION_CDM_tables[names(thisfiles_ConcePTION_CDM_tables) %in% tablesCDM]

print("Retriving person_ids using DP_DDP4I or DP_DAPAGLIFLOZIN")
list_person_ids <- pl$scan_parquet(file.path(thisdirinput,
                                             paste0(c("DP_DDP4I_dataset", "DP_DAPAGLIFLOZIN_dataset"), ".parquet"))
)$select(pl$col("person_id"))$unique()$collect()$to_series()

CDM_file_sizes <- sapply(unique(unlist(thisfiles_ConcePTION_CDM_tables)),
                         function(x) file.size(file.path(thisdirinput1, paste0(x, ".csv"))))

pb <- progress::progress_bar$new(
  format = "  Reducing :what [:bar] :percent eta: :eta",
  total = sum(CDM_file_sizes), clear = FALSE, width = 80)
pb$tick(0)

for (table_type in names(thisfiles_ConcePTION_CDM_tables)) {
  for (single_file in thisfiles_ConcePTION_CDM_tables[[table_type]]) {
    # Load the csv
    single_lf <- pl$scan_csv(file.path(thisdirinput1, paste0(single_file, ".csv")), infer_schema = FALSE)
    
    if (table_type %in% c("PERSONS", "OBSERVATION_PERIODS")) {
      
      # Restrict person_id to previously created list
      single_lf <- single_lf$filter(pl$col("person_id")$is_in(list_person_ids$implode()))
      
    } else {
      
      filter_date_vect <- c()
      test_vect <- list()
      for (date_col in date_simplified[[single_file]]) {
        filter_date_vect[[date_col]] <- pl$lit(20090101, dtype = polars::pl$Int32)
        test_vect[[date_col]] <- polars::pl$Int32
      }
      
      
      single_lf <- single_lf$cast(!!!test_vect)
      
      # Keep all values Greater Equal than the ones defined in filter_date_vect
      # Restrict person_id to previously created list
      # Keep filter of person together the one for dates to improve performance
      single_lf <- single_lf$filter(pl$all_horizontal(
        !!!sapply(names(filter_date_vect), function(x) pl$col(x)$ge(filter_date_vect[[x]])),
        pl$col("person_id")$is_in(list_person_ids$implode())
      ))
      
    }
    
    # Save to a parquet file
    single_lf <- single_lf$lazy_sink_parquet(
      pl$PartitionBy(file.path(dirreducedCDM, single_file),
                     approximate_bytes_per_file = 2147483647)
    )
    
    # Append the LazyFrame to the main list
    single_lf$collect(engine = "streaming")
    
    filename_split <- list.files(file.path(dirreducedCDM, single_file))
    file.rename(file.path(dirreducedCDM, single_file, filename_split),
                file.path(dirreducedCDM, single_file, paste(single_file, filename_split, sep = "_")))
    file.copy(from = file.path(dirreducedCDM, single_file, paste(single_file, filename_split, sep = "_")),
              to   = file.path(dirreducedCDM, paste(single_file, filename_split, sep = "_")),
              overwrite = TRUE)
    unlink(file.path(dirreducedCDM, single_file), recursive = TRUE)
    
    pb$tick(CDM_file_sizes[[single_file]], tokens = list(what = single_file))
    
  }
}



