############################################################
#                                                          #
####         0x_TX_0X_Create_D6_XXXXXXXXXXXXXX             #
#                                                          #
############################################################

# authors: ...

# v 0.1 01 Jan 2026


print('CREATE D6_XXXXXXXXXXXXXX')


#########################################
# assign directories and other parameters used in this step

if (TEST){ 
  # this happens during development
  testname <- "test_XXX_T5_XXX"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output_program")
  dir.create(thisdiroutput, showWarnings = F)
  thisdatasources_for_postprocessing <- c("TEST","TEST2")
  thisfolder_submission <- folders_submission
  thisfolder_submission[["TEST"]] <- paste0(thisdirinput,"folder_TEST/")
  thisfolder_submission[["TEST2"]] <- paste0(thisdirinput,"folder_TEST2/")
}else{
  if (localprocessing){
    # this happens at DEAPs
    thisdirinput <- dirtablesubpop[[subpop]]
    thisdiroutput <- dirD6
    thisdatasources_for_postprocessing <- thisdatasource
    thisfolder_submission <- folders_submission
    thisfolder_submission[[thisdatasource]] <- thisdirinput
  }else{
    # this happens at DRE
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- paste0(direxp,"Formatted tables/")
  }
}


# Table X

print(paste("Now creating: Table X in nice format"))

tab_nice <- data.table()
for (ds in thisdatasources_for_postprocessing) {
  filetoread <- paste0(thisfolder_submission[[ds]], "D5_Table_XXX_XXXX_masked.csv")
  if (file.exists(filetoread)) {
    print(paste0(ds, " has D5 for this table"))
    temp <- fread(filetoread)  
    tab_nice <- rbind(tab_nice,temp, fill = T)
  }else{
    print(paste0(ds, " has no D5 for this table"))
  }
}


oldnames <- names(tab_nice)
newnames <- gsub(paste0("_Cov_",immdis), "", oldnames)
setnames(tab_nice,oldnames, newnames)




row_header_1 = c(
  "Total cohort population", "Female (n/%)", "Male (n/%)", "Other (n/%)", "Age in year (IQR): PCT 25", "Age in year (IQR): PCT 50", "Age in year (IQR): PCT 75", "Age in categories (n/%): < 18", "Age in categories (n/%): 18-59", "Age in categories (n/%): > 60", "Number of persons who started the follow-up", "Total follow-up time regardless of interruptions from start of follow-up (PY)", "Number of vaccinations during follow-up (median, IQR)", "Number of people with vaccinations (n/%): 0", "Number of people with vaccinations (n/%): 1", "Number of people with vaccinations (n/%): 2", "Number of people with vaccinations (n/%): 3 or more", "Number of pregnancies" )

for (j in c(1)) {
  tab_nice[,(paste0("cell_",j)) := get(paste0("n_",j))]
}

for (j in c("5","6","7")) {
  tab_nice[,(paste0("cell_",j)) := get(paste0("pc_",j)) ] 
}

for (j in c(12)) {
  tab_nice[,(paste0("cell_",j)) := get(paste0("py_",j)) ] 
}

for (j in c(13)) {
  tab_nice[,(paste0("cell_",j)) := get(paste0("pc_",j)," (",get(paste0("pc_",j,"_2")),"-",  get(paste0("pc_",j,"_3")),")") ] 
}


for (j in c(2,3,4,8,9,10,11,14,15,16,17)) {
  cellj <- paste0("cell_",j)
  nj <- paste0("n_",j)
  pj <- paste0("p_",j)
  if (is.numeric(tab_nice[[pj]] )) {
    tab_nice[ , thispj := (pj)]
  }else{
    tab_nice[ , thispj := as.numeric((pj))]
  }
  
  tab_nice <- tab_nice[!is.na(thispj), (cellj) := paste0(get(nj), " (", get(pj),"%)") ]
  tab_nice <- tab_nice[is.na(thispj), (cellj) := paste0( as.character(format_with_comma(get(nj))), " (", get(pj),"%)") ]
  checkvar <- paste0("n_",j,"_check")
  tab_nice[get(cellj) == "0 (NA%)",(cellj) := "NA"]
  tab_nice[ , thispj := NULL]
  
}

tab_nice[ , summary_check_gender := n_2_check + n_3_check + n_4_check]
tab_nice <- tab_nice[ summary_check_gender == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_2 := "NC"]
tab_nice <- tab_nice[ summary_check_gender == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_3 := "NC"]
tab_nice <- tab_nice[ summary_check_gender == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_4 := "NC"]

tab_nice[ , summary_check_ageband := n_8_check + n_9_check + n_10_check]
tab_nice <- tab_nice[ summary_check_ageband == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_8 := "NC"]
tab_nice <- tab_nice[ summary_check_ageband == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_9 := "NC"]
tab_nice <- tab_nice[ summary_check_ageband == 1 & ds %in% ds_rm_higher_level_if_one_smallcount, cell_10 := "NC"]

tokeep <- c("ds",names(tab_nice)[grep("^cell_",names(tab_nice))])

tab_nice <- tab_nice[, ..tokeep]


# Reshape 
tab_nice <- melt(tab_nice, id.vars = "ds", measure.vars = patterns("^cell_"), variable.name = "cell", value.name = "value")



# Cast to one column per 'ds'
tab_nice <- dcast(tab_nice, cell ~ ds, value.var = "value")

# transform cell into a number
tab_nice[, cell := gsub("cell_", "", cell)]
tab_nice[, cell := as.numeric(gsub("_", ".", cell))]


# order
setorder(tab_nice, cell)
tab_nice[, ord := seq_len(.N)]


# add row_header_1
tab_nice[, row_header := row_header_1[ord]]



# remove cell  
tab_nice[, cell := NULL]  
tab_nice[, ord := NULL]    

# rename
old_col_names = c("row_header",setdiff( names(tab_nice),"row_header"))
new_col_names <- unlist(c("Cohort characteristics", name_ds[setdiff( names(tab_nice),"row_header")]))
setnames(tab_nice, old_col_names,  new_col_names)

tab_nice <- tab_nice[, ..new_col_names]


########################################
# save

outputfile <- tab_nice
nameoutput <- paste0("D6_Table_X_XXX")
assign(nameoutput, outputfile)
# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# html
html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template") %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))
  




# #########################################
# # load parameters
# 
# settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
# thissubpopulations <- unique(settings[,.(subpop)])
# thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
# 
# 
# for (subpop in thissubpopulations) {
#   suffix <- fifelse(subpop == "all","",paste0("_",subpop))
#   
#   #########################################
#   # load input datasets
#   
#   # input1 <- readRDS(file.path(thisdirinput,paste0("XXX.rds")))
#   # input2 <- readRDS(file.path(thisdirinput,paste0("YYY",suffix,".rds")))
#   
#   #########################################
#   # start_processing
#   
#   
#   # processing <- ...
#   
#   
#   
#   #########################################
#   # clean
#   
#   listvartokeep <- c()
#   processing <- processing[,..listvartokeep]
#   
#   #########################################
#   # save
#   
#   outputfile <- processing
#   
#   nameoutput <- paste0("D_N_XXXXX",suffix)
#   assign(nameoutput, outputfile)
#   saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# }
