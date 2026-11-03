##%######################################################%##
#                                                          #
####             D3_EXP_ACT_COV_IMPUTATION              ####
#                                                          #
##%######################################################%##

# authors: Davide Messina

# v 1.0

# 08 Sep 2026

print('CREATE D3_EXP_ACT_COV_IMPUTATION')

#########################################
# assign directories

if (TEST){
  # testname <- "test_18_D3_EXP_ACT_COV"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # thiscovariates <- c("gastritis_esophagitis", "History_of_GIB_in_year_before_cohort_entry","History_of_GIB","Last_GIB", "BMI", "Smoking","Obesity")
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdiroutput_1 <- direxp
  thisdirsettings <- dirpargen
  parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",
                                                                            paste0("D3_EXP_ACT_COV_IMPUTATION.xlsx")),1)))
  # thiscovariates_ord <- unlist(parameters_this_step[parameter == "covar_ord",.(values)])
  # thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)])[-1]
  # thiscovariates_lab <- unlist(parameters_this_step[parameter == "covar_lab",.(values)])
  thiscovariates <- unlist(parameters_this_step[parameter %in% c("covar_cat","covar_ord", "covar_cont") ,.(values)])
}

#########################################
# load

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_EXP_ACT_COV_IMPUTATION.xlsx")),1)))

subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all", "",paste0("_",subpop))
  print(suffix)
  
  # Load input df
  pop_with_cov <- readRDS(file.path(thisdirinput,paste0("D3_EXP_ACT_COV",suffix,".rds")))
  
  # Find constant variables
  constant_cols <- names(pop_with_cov)[sapply(pop_with_cov, function(x) uniqueN(x) == 1)]
  
  # Create vector of columns to exclude
  exclude_cols <- setdiff(colnames(pop_with_cov), c(paste0(thiscovariates, "_at_t0"), "HbA1c_at_t0"))
  exclude_cols <- c(constant_cols, exclude_cols)
  
  # Find the predictors
  preds <- setdiff(colnames(pop_with_cov), exclude_cols)
  
  # Transform character to factor
  chr_preds <- preds[sapply(pop_with_cov[, ..preds], is.character)]
  pop_with_cov[, (chr_preds) := lapply(.SD, factor), .SDcols = chr_preds]
  
  # make.predictorMatrix() crea la matrice con il default di mice
  # (tutti 1 fuori diagonale). La uso solo per avere le dimensioni e i
  # nomi di riga/colonna corretti.
  pred <- mice::make.predictorMatrix(pop_with_cov)
  
  # Azzero TUTTO: nessuna variabile predice nessun'altra.
  # Da qui in poi accendo solo quello che mi serve.
  pred[, ] <- 0
  
  # Accendo una sola riga: quella di HbA1c_at_t0, cioè la variabile da
  # imputare. Sulle colonne indico i predittori scelti.
  # Le righe delle variabili con method == "" vengono comunque ignorate,
  # ma tenerle a 0 rende la matrice leggibile.
  pred["HbA1c_at_t0", setdiff(preds, "HbA1c_at_t0")] <- 1
  
  meth <- mice::make.method(pop_with_cov) # crea il vettore con i nomi giusti
  meth[] <- "" # disattivo l'imputazione ovunque
  meth["HbA1c_at_t0"] <- "pmm" # la riattivo solo dove serve
  
  # Main MICE imputation
  imp <- mice::mice(pop_with_cov, maxit = 50, m = 10,
                    method = meth, seed = 123,
                    predictorMatrix = pred)
  
  # Create diagnostic plots
  trace_plot <- plot(imp, layout = c(1, 2))
  strip_plot <- mice::stripplot(imp, HbA1c_at_t0~.imp, pch = 20, cex = 2)
  density_plot <- mice::densityplot(imp, ~ HbA1c_at_t0 | .imp)
  
  # Add all imputed columns in one single df
  list_imputed_dfs <- mice::complete(copy(imp), action = "all")
  for (i in 1:10){
    colnames(list_imputed_dfs[[i]])[which(names(list_imputed_dfs[[i]]) == "HbA1c_at_t0")] <- paste0("HbA1c_imputed_at_t0_", i)
  }
  MergedDT = Reduce(function(...) merge(..., by = setdiff(colnames(pop_with_cov), "HbA1c_at_t0")), list_imputed_dfs)
 
  #########################################
  # save
  
  # Save figures
  pdf(file.path(thisdiroutput_1, paste0("D6_MICE_Trace_plot", suffix, ".pdf")), width = 10, height = 8) 
  print(trace_plot)
  dev.off()
  pdf(file.path(thisdiroutput_1, paste0("D6_MICE_Strip_plot", suffix, ".pdf")), width = 10, height = 8)   
  print(strip_plot)
  dev.off()
  pdf(file.path(thisdiroutput_1, paste0("D6_MICE_Density_plot", suffix, ".pdf")), width = 10, height = 8)  
  print(density_plot)
  dev.off()
  
  # Save main dataset
  fileConn<-file(file.path(thisdiroutput_1, "MICE predictors.txt"))
  writeLines(paste(setdiff(preds, "HbA1c_at_t0"), collapse = ", "), fileConn)
  close(fileConn)
  
  # Save main dataset
  nameoutput <- paste0("D3_EXP_ACT_COV_IMPUTATION")
  nameoutputext <- paste0(nameoutput, suffix, ".rds")
  saveRDS(MergedDT, file = file.path(thisdiroutput, nameoutputext))
}

