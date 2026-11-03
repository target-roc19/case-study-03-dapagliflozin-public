############################################################
#                                                          #
####         0x_TX_0X_Create_DX_XXXXXXXXXXXXXX             #
#                                                          #
############################################################

# authors: ...

# v 0.1 01 Jan 2026


print('CREATE DX_XXXXXXXXXXXXXX')


#########################################
# assign directories and other parameters used in this step

if (TEST){
  testname <- "XXXX"
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
  
  #########################################
  # load input datasets
  
  # input1 <- readRDS(file.path(thisdirinput,paste0("XXX.rds")))
  # input2 <- readRDS(file.path(thisdirinput,paste0("YYY",suffix,".rds")))
  
  #########################################
  # start_processing
  
  
  # processing <- ...
  
  
  
  #########################################
  # clean
  
  listvartokeep <- c()
  processing <- processing[,..listvartokeep]
  
  #########################################
  # save
  
  outputfile <- processing
  
  nameoutput <- paste0("D_N_XXXXX",suffix)
  assign(nameoutput, outputfile)
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
}
