############################################################
#                                                          #
####         03_T3_10_Create_D3_STUDY_POPULATION        ####
#                                                          #
############################################################


# authors: Claudia Bartolini

# v 0.1 09 Apr 2026


print('CREATE D3_STUDY_POPULATION')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "test_08_D3_STUDY_POPULATION"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(thisdirinput,"g_output")
  thisdirsettings <- thisdirinput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}

#########################################
# load parameters

settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input datasets
  
  eligibility <- readRDS(file.path(thisdirinput,paste0("D3_ELIGIBILITY",suffix,".rds")))
  eligibility_episode <- readRDS(file.path(thisdirinput,paste0("D3_ELIGIBILITY_EPISODE",suffix,".rds")))
  
  #########################################
  # start_processing
  data<-eligibility[included==1,.(person_id, sex_at_instance_creation,birth_year,entry_spell_category,death_date, exit_spell_category,included)]

  data_included<- merge(data, eligibility_episode[is_study_episode==1,
                                                .(person_id,episode_start,episode_end,exposure_episode,type_medicine,T2D_diag,is_study_episode)],
                        by="person_id", all.x = T)
  
  # - t0
  data_included<-data_included[, t0:=episode_start]
  data_included<-data_included[,.(person_id, t0, exposure_episode, type_medicine, 
                                  birth_year, sex_at_instance_creation, entry_spell_category,exit_spell_category,death_date,T2D_diag)]
  
  # - exposure 
  setnames(data_included, "exposure_episode","exposure")
    
  # - age_at_t0
  #data_included<-data_included[,age_at_t0:=year(t0)-birth_year]
  data_included<-data_included[,age:=year(t0)-birth_year]
  
  # - time since T2D_diag
  data_included<-data_included[!is.na(T2D_diag), yrsdiab := as.numeric(t0 - T2D_diag) / 365.25]
  
  #########################################
  # clean
  
  setnames(data_included,"sex_at_instance_creation", "gender")
             
  listvartokeep <- c("person_id",
                     "gender",
                     "birth_year",
                     "entry_spell_category",
                     "death_date",
                     "exit_spell_category",
                     "t0",
                     "exposure",
                     "age",
                     "yrsdiab")
  
  processing <- data_included[,..listvartokeep]



  #########################################
  # save

  outputfile <- processing

  nameoutput <- paste0("D3_STUDY_POPULATION",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}






