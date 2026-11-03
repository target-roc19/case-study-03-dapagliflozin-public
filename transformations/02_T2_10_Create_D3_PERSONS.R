############################################################
#                                                          #
########         02_T2_01_CREATE D3_PERSONS       ##########
#                                                          #
#                                                          #
############################################################

# author: Claudia Bartolini, Rosa Gini

# v 1.1 17 Jun 2026

# support for data instance in parquet

# v 0.1 30 Apr 2026

#########################################
# assign the parameter CHECK: during test with dummy data, this is set to T, otherwise to F

#########################################
# assign input and output directories

if (TEST){
  testname <- "test_02_D3_PERSONS"
  thislistofPERSONS <- c("PERSONS")
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output/")
  dir.create(thisdiroutput, showWarnings = F)
  extensionthisinstance <- "csv" 
  
}else{
  thislistofPERSONS <- files_ConcePTION_CDM_tables[["PERSONS"]]
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
  extensionthisinstance <- unique(ifelse(length(list.files(thisdirinput, pattern = "\\.parquet$")) > 0, "parquet","csv" ))
  
}

#########################################
# load input datasets



PERSONS <- data.table()
for (file in thislistofPERSONS) {
  if (extensionthisinstance == "csv") {
    temp <- fread(file.path(thisdirinput, paste0( file, ".csv")),
                  colClasses = list(character = "person_id",
                                    integer = c("day_of_birth", "month_of_birth", "year_of_birth", "day_of_death", "month_of_death", "year_of_death"))
    )
  }else{
    temp <- as.data.table(as.data.frame(polars::pl$read_parquet(file.path(thisdirinput,paste0(file,".parquet")))))
    for (varname in c("day_of_birth", "month_of_birth", "year_of_birth", "day_of_death", "month_of_death", "year_of_death")) {
      temp[, (varname) := as.integer(get(varname))]
    }
  }
  PERSONS <- rbind(PERSONS, temp, fill = T)
  rm(temp)
}



# missing_birth_year 
PERSONS[, missing_birth_year  := fifelse(is.na(year_of_birth), 1, 0)]

# missing_only_death_year
PERSONS[, missing_only_death_year := fifelse(is.na(year_of_death) & ((!is.na(day_of_death) | !is.na(month_of_death))), 1, 0)]

PERSONS[, missing_only_death_month := fifelse(is.na(month_of_death) & ((!is.na(day_of_death) | !is.na(year_of_death))), 1, 0)]

PERSONS[, missing_only_death_day := fifelse(is.na(day_of_death) & ((!is.na(month_of_death) | !is.na(year_of_death))), 1, 0)]

if (sum(PERSONS$missing_only_death_day)>0) warning("Day, month or year of death is missing in CDM table PERSONS. Please check")

if (sum(PERSONS$missing_only_death_month)>0) warning("Day, month or year of death is missing in CDM table PERSONS. Please check")

if (sum(PERSONS$missing_only_death_year)>0) warning("Day, month or year of death is missing in CDM table PERSONS. Please check")

# death_date
PERSONS[missing_only_death_year == 0 , death_date := lubridate::ymd(paste(year_of_death, month_of_death, day_of_death, sep = "-"))]

setnames(PERSONS,"year_of_birth","birth_year")

PERSONS <- PERSONS[, .(person_id,
                       sex_at_instance_creation,
                       birth_year,
                       death_date,
                       missing_birth_year,
                       missing_only_death_year)]



#########################################
# save

outputfile <- PERSONS

nameoutput <- "D3_PERSONS"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))

#########################################
# during check, save output to QC repository
# 
# if (TEST & CHECK){
#   thisdircheck <- file.path(thisdirinput,"g_output_QC")
#   dir.create(thisdircheck, showWarnings = F)
#   saveRDS(outputfile, file = file.path(thisdircheck, nameoutputext))
# }
# 
# if (!TEST & CHECKSEQ){
#   saveRDS(outputfile, file = file.path(dirtestseq,nameoutput,"g_output_QC", nameoutputext))
# }


