##%######################################################%##
#                                                          #
####     Lists of parameetrs for variables      ####
#                                                          #
##%######################################################%##


# retrieve parameters of Codebook D3_covariate_ever_TD, D3_covariate_num, D3_covariate_365days to be used in cretaing the corresponding datasets

for (listcov in c("covariate_num", "covariate_ever","covariate_365days")) {
  data <- as.data.table(unique(read.xlsx2(file.path(dirarchive,"codebooks",paste0("D3_{",listcov,"}_TD.xlsx")),1)))
  assign(paste0(listcov,"_codebook_parameters"),data)
  listcov_names <- unique(data$values)
  assign(listcov,listcov_names)
  listcov_concept_id <-  split(data$concept_id, data$values)
  assign(paste0(listcov,"_concept_id"),listcov_concept_id)
}


# meanings of hospitalisazions

meaning_hospitalisations <- c("hospitalization","hospital_epi")
