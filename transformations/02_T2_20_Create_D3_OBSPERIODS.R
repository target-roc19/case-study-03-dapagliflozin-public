############################################################
#                                                          #
####         02_T2_02_Create_D3_OBSPERIODS             #
#                                                          #
############################################################


# authors: Claudia Bartolini, Rosa Gini

# v 1.3 3 Sep 2026

# replaced end of study period with min between end of study period and death


# v 1.2 17 Jun 2026

# support for data instance in parquet

# v 1.1 7 Jun 2026

# from CS9: fixed the wrong assumption that length(thissubpopulations) == 1 implies that subpop == "all": created new parameter this_list_of_categories_to_be_computed

# v 1.0 30 Apr 2026

# from scripts of ECVM, CVM


print('CREATE D3_OBSPERIODS')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_03_D3_OBSPERIODS"
  thisdirinput <- file.path(dirtest,testname)
  thisdirinputRDS <- thisdirinput
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  thisfiles_ConcePTION_CDM_tables <- list()
  thisfiles_ConcePTION_CDM_tables[["OBSERVATION_PERIODS"]] <- extract_name_csv(file.path(dirtest, testname), "OBSERVATION_PERIODS")
  dir.create(thisdiroutput, showWarnings = F)
  extensionthisinstance <- "csv" 
  }else{
  thisdirinput <- dirinput
  thisdirinputRDS <- dirtemp
  thisdiroutput <- dirtemp
  thisfiles_ConcePTION_CDM_tables <- list()
  thisfiles_ConcePTION_CDM_tables[["OBSERVATION_PERIODS"]] <- files_ConcePTION_CDM_tables[["OBSERVATION_PERIODS"]] 
  thisdirsettings <- dirpargen
  extensionthisinstance <- unique(ifelse(length(list.files(thisdirinput, pattern = "\\.parquet$")) > 0, "parquet","csv" ))
  
  
}


#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
thisdatasource <- settings$ds
thissubpopulations <- extract_vector_in_cell(settings[, subpop])
thisrecommendedend <- settings[, pmin(study_period_end_date, recommended_end_date,
                                      date_creation_CDM_instance, data_extraction_date, na.rm = T)]
gap_allowed = 1
subpop_param <- readRDS(file.path(thisdirsettings, "SUBPOP.rds"))

this_list_of_categories_to_be_computed <- unique(c(unlist(subpop_param[subpop %in% thissubpopulations,.(unlist(strsplit(trimws(sets), " +")))]),thissubpopulations))

#########################################
# load input datasets

# OBSERVATION_PERIODS <- load_CDM_tables(thisdirinput, thisfiles_ConcePTION_CDM_tables[["OBSERVATION_PERIODS"]])

OBSERVATION_PERIODS <- data.table()
for (file in thisfiles_ConcePTION_CDM_tables[["OBSERVATION_PERIODS"]]) {
  if (extensionthisinstance == "csv") {
    temp <- fread(file.path(thisdirinput, paste0( file, ".csv")),
                  colClasses = list(character = "person_id")
    )
  }else{
    temp <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0(file,".parquet")))))
    for (varname in c("op_start_date", "op_end_date")) {
      temp[, (varname) := lubridate::ymd(get(varname))]
    }
  }
  OBSERVATION_PERIODS <- rbind(OBSERVATION_PERIODS, temp, fill = T)
  rm(temp)
}

##%######################################################%##
#                                                          #
####       Remove impossible observation periods        ####
#                                                          #
##%######################################################%##

# remove observation periods without start date
OBSERVATION_PERIODS <- OBSERVATION_PERIODS[!is.na(op_start_date), ]

# if the person has a death date, set end observation period to min between end obs period and death date

persons <- readRDS(file.path(thisdirinputRDS, "D3_PERSONS.rds"))
persons <- persons[,.(person_id, death_date)]

OBSERVATION_PERIODS <- merge(OBSERVATION_PERIODS, persons, by = "person_id", all.x = T)

OBSERVATION_PERIODS[, op_end_date:= pmin(op_end_date, death_date, na.rm = T)]

# remove observation periods that start after ending

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date) | !(op_end_date < op_start_date), ]

# TODO check SETTINGS and CDM_SOURCE
if (length(this_list_of_categories_to_be_computed) == 1) {
  
  OBSERVATION_PERIODS <- OBSERVATION_PERIODS[, op_meaning := "all"]
  D3_output_spells_category <- CreateSpells(
    dataset = OBSERVATION_PERIODS,
    id = "person_id" ,
    start_date = "op_start_date",
    end_date = "op_end_date",
    category ="op_meaning",
    replace_missing_end_date = thisrecommendedend,
    gap_allowed = gap_allowed
  )
  
  D3_output_spells_category <- as.data.table(D3_output_spells_category)
  setkeyv(
    D3_output_spells_category,
    c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning")
  )
  
} else {
  
  empty_spells <- data.table(person_id = character(),
                             op_meaning = character(),
                             entry_spell_category = as.Date(integer()),
                             exit_spell_category = as.Date(integer()),
                             num_spell = integer())
  
  # for each op_meaning_set, create the dataset of the corresponding spells
  output_spells_category_meaning_set <- vector(mode="list")
  for (op_meaning_set in subpop_param[ds == thisdatasource & !grepl(" ", sets), sets]){
    periods_op_meaning_set <- OBSERVATION_PERIODS
    cond_op_meaning_set <- ""
    op_meanings_list_per_set <- subpop_param[ds == thisdatasource & sets == op_meaning_set, ]
    op_meanings_list_per_set <- extract_vector_in_cell(op_meanings_list_per_set[, op_meanings_list_per_set])
    for (op_meaning in op_meanings_list_per_set){
      if (cond_op_meaning_set=="") {cond_op_meaning_set = paste0("op_meaning=='",op_meaning,"'")
      }else{
        cond_op_meaning_set=paste0(cond_op_meaning_set, " | op_meaning=='",op_meaning,"'")
      }
    }
    periods_op_meaning_set <- periods_op_meaning_set[eval(parse(text = cond_op_meaning_set)),]
    periods_op_meaning_set <- periods_op_meaning_set[,op_meaning:= op_meaning_set]
    print(paste0("COMPUTE SPELLS OF TIME FOR ",op_meaning_set,": ",cond_op_meaning_set))
    output_spells_op_meaning_set <- CreateSpells(
      dataset=periods_op_meaning_set,
      id="person_id" ,
      start_date = "op_start_date",
      end_date = "op_end_date",
      category ="op_meaning",
      replace_missing_end_date = thisrecommendedend,
      gap_allowed = gap_allowed
    )
    if (nrow(output_spells_op_meaning_set)>0){
      output_spells_op_meaning_set<-as.data.table(output_spells_op_meaning_set)
      setkeyv(
        output_spells_op_meaning_set,
        c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning")
      )
    }else{
      output_spells_op_meaning_set <- empty_spells
    }
    output_spells_category_meaning_set[[op_meaning_set]] <- output_spells_op_meaning_set
  }
  saveRDS(output_spells_category_meaning_set, file.path(thisdiroutput, "output_spells_category_meaning_set.rds"))
  
  # creates spells of overlapping op_meaning sets
  # load(paste0(thisdiroutput,"output_spells_category_meaning_set.RData"))
  for (this_subpop in thissubpopulations){
    op_meaning_sets_in_subpop <- extract_vector_in_cell(subpop_param [ds == thisdatasource & subpop == this_subpop, sets])
    if (length(op_meaning_sets_in_subpop)>1){
      runninglen = 1
      while (runninglen < length(op_meaning_sets_in_subpop)) {
        op_meaning_set_first = op_meaning_sets_in_subpop[1]
        if (runninglen > 1){
          for (j in 2:runninglen) {
            print(j)
            op_meaning_set_first = paste0(op_meaning_set_first,'_',op_meaning_sets_in_subpop[j])
          }
        }
        op_meaning_set_second = op_meaning_sets_in_subpop[runninglen+1]
        overlap_op_meaning_sets <- paste0(op_meaning_set_first,'_',op_meaning_set_second)
        # compute spells overlap corresponding to overlap_op_meaning_sets, unless it has been already computed
        if (!(overlap_op_meaning_sets %in% names(output_spells_category_meaning_set))){
          print(paste0("COMPUTE SPELLS OF TIME FOR ",overlap_op_meaning_sets))
          inputfirst <- output_spells_category_meaning_set[[op_meaning_set_first]]
          inputsecond <- output_spells_category_meaning_set[[op_meaning_set_second]]
          # check whether one of the two composing input files is empty (if so, the overlap is also empty), otherwise use CreateSpells again
          if(nrow(inputfirst)==0 | nrow(inputsecond)==0){
            output_spells_category_meaning_set[[overlap_op_meaning_sets]] <- empty_spells
          }else{
            input_observation_periods_overlap <- as.data.table(rbind(inputfirst,inputsecond,fill = T))
            temp <- CreateSpells(
              dataset=input_observation_periods_overlap,
              id="person_id" ,
              start_date = "entry_spell_category",
              end_date = "exit_spell_category",
              category = "op_meaning",
              gap_allowed = gap_allowed,
              overlap = T,
              dataset_overlap = "overlap",
              replace_missing_end_date = thisrecommendedend,
              only_overlaps = T
            )
            output_spells_category_meaning_set[[overlap_op_meaning_sets]] <- get("overlap")
          }
        }
        runninglen = runninglen + 1
      }
      
    }
  }
  saveRDS(output_spells_category_meaning_set, file.path(thisdiroutput, "output_spells_category_meaning_set.rds"))
  
  # readRDS(file.path(thisdiroutput, "output_spells_category_meaning_set.rds"))
  D3_output_spells_category <- vector(mode="list")
  for (this_subpop in thissubpopulations){
    print(this_subpop)
    op_meaning_sets_in_subpop <- extract_vector_in_cell(subpop_param [ds == thisdatasource & subpop == this_subpop, sets])
    print(op_meaning_sets_in_subpop)
    print(length(op_meaning_sets_in_subpop))
    if (length(op_meaning_sets_in_subpop)==1){
      D3_output_spells_category[[this_subpop]] <- output_spells_category_meaning_set[[op_meaning_sets_in_subpop]]
    }
    if (length(op_meaning_sets_in_subpop)>1){
      concat_op_meaning_sets_in_subpop = op_meaning_sets_in_subpop[1]
      for (j in 2:length(op_meaning_sets_in_subpop)){
        concat_op_meaning_sets_in_subpop = paste0(concat_op_meaning_sets_in_subpop,'_',op_meaning_sets_in_subpop[j])
      }
      D3_output_spells_category[[this_subpop]] <- output_spells_category_meaning_set[[concat_op_meaning_sets_in_subpop]]
    }
  }
  D3_output_spells_category <- data.table::rbindlist(D3_output_spells_category, use.names = TRUE)
}

#D3_output_spells_category[, op_meaning := paste0("_", op_meaning)]
setnames(D3_output_spells_category, "op_meaning", "category_subpop")
setcolorder(D3_output_spells_category, c("person_id", "category_subpop", "entry_spell_category", "exit_spell_category"))

##%######################################################%##
#                                                          #
####    Find spells not overlapping the study period    ####
#                                                          #
##%######################################################%##

D3_output_spells_category[, not_overlap_study_period := fifelse(exit_spell_category < settings$study_period_start_date |
                                                                  entry_spell_category > thisrecommendedend, 1, 0)]

# Retain for each person/subpop a homogeneous value of not_overlap_study_period
D3_output_spells_category <- D3_output_spells_category[D3_output_spells_category[order(not_overlap_study_period), .I[1L], c("person_id", "category_subpop")]$V1]

##%######################################################%##
#                                                          #
####                Find the study spell                ####
#                                                          #
##%######################################################%##

D3_output_spells_category <- D3_output_spells_category[D3_output_spells_category[order(num_spell), .I[1L], c("person_id", "category_subpop")]$V1]
D3_output_spells_category <- D3_output_spells_category[, is_the_study_spell := fifelse(not_overlap_study_period == 0, 1, 0)]

#########################################
# save

saveRDS(D3_output_spells_category, file = file.path(thisdiroutput, "D3_OBSPERIODS.rds"))


for (subpop in thissubpopulations) { 
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  
  outputfile <- copy(D3_output_spells_category)[category_subpop == subpop,]
  
  nameoutput <- paste0("D3_OBSPERIODS")
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
  
}