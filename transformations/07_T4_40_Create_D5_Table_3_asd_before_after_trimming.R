############################################################
#                                                          #
####         Create 07_T4_40_Create_D5_Table_3_asd_before_after_trimming.R         #
#                                                          #
############################################################

# authors: Sofia Miniati, Rosa Gini


# v 1.1 (with OR)


# v 0.2 (with aSD)

print('CREATE D5_Table_3_asd_before_after_trimming.R')

#########################################
# assign directories

if (TEST){
  # testname <- "test_23_D5_Table_3"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output/")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdirexp <- direxp
  thisdiroutput <- dirtemp
  thisdirsettings <- dirpargen
}
#########################################
# load

# from settings: create the list of subpopulations
settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
thissubpopulations <- unique(settings[,.(subpop)])
thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))


parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D5_Table_3.xlsx")),1)))


thiscovariates_bin <- unlist(parameters_this_step[parameter == "covar_bin",.(values)])
thiscovariates_cat <- unlist(parameters_this_step[parameter == "covar_cat",.(values)]) #[-1]
thiscovariates_cont <- unlist(parameters_this_step[parameter == "covar_cont",.(values)])

wtd_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

wtd_var <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  m <- sum(w * x) / sum(w)
  sum(w * (x - m)^2) / sum(w)
}

wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

asd_bin <- function(x1, x0, w1 = NULL, w0 = NULL){
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  p1 <- wtd_mean(as.numeric(x1 == 1), w1)
  p0 <- wtd_mean(as.numeric(x0 == 1), w0)
  val <- abs(p1 - p0) / sqrt((p1*(1-p1) + p0*(1-p0))/2)
  round(val,1)
}

asd_cont <- function(x1, x0, w1 = NULL, w0 = NULL){
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  m1 <- wtd_mean(x1, w1); m0 <- wtd_mean(x0, w0)
  v1 <- wtd_var(x1, w1);  v0 <- wtd_var(x0, w0)
  val <- abs(m1 - m0) / sqrt((v1 + v0)/2)
  round(val,1)
}

asd_cat <- function(x1, x0, w1 = NULL, w0 = NULL) {
  if (is.null(w1)) w1 <- rep(1, length(x1))
  if (is.null(w0)) w0 <- rep(1, length(x0))
  cats <- union(unique(x1), unique(x0))
  p1 <- sapply(cats, function(c) wtd_mean(as.numeric(x1 == c), w1))
  p0 <- sapply(cats, function(c) wtd_mean(as.numeric(x0 == c), w0))
  num <- sum((p1 - p0)^2)
  denom <- sum((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  val <- sqrt(num / denom)
  round(val, 1)
}

desc_binary <- function(processing, dataset, inputvar, outputvar, vartot = "",
                        condition = "!is.na(person_id)", compute_Nmissing = FALSE,
                        weightvar = NULL) {
  if (is.null(weightvar)) {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1, .(var_N = .N), by = "stratum"]
  } else {
    temp <- dataset[eval(parse(text = condition)) & get(inputvar) == 1,
                    .(var_N = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
  }
  processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
  processing[is.na(var_N), var_N := 0]
  thislist <- c("N")
  if (nchar(vartot) > 0) {
    processing[, var_perc := round(100 * var_N / get(vartot), digits = 1)]
    thislist <- c(thislist, "perc")
  }
  setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}

desc_cat <- function(processing, dataset, inputvar, listofvalues, outputvar, vartot = "",
                     condition = "!is.na(person_id)", compute_Nmissing = TRUE,
                     weightvar = NULL) {
  for (valuevar in listofvalues) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar, .(var_N = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & get(inputvar) == valuevar,
                      .(var_N = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    processing[is.na(var_N), var_N := 0]
    thislist <- c("N")
    if (nchar(vartot) > 0) {
      processing[, var_perc := round(100 * var_N / get(vartot), digits = 1)]
      thislist <- c(thislist, "perc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", valuevar, "_", thislist, "_"))
  }
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}

desc_continuous <- function(processing, dataset, inputvar, outputvar, vartot = "",
                            condition = "!is.na(person_id)", compute_Nmissing = FALSE,
                            weightvar = NULL) {
  if (is.null(weightvar)) {
    temp <- dataset[eval(parse(text = condition)), .(
      var_mean   = round(mean(get(inputvar), na.rm = TRUE), 1),
      var_sd     = round(sd(get(inputvar), na.rm = TRUE), 1),
      var_median = round(median(get(inputvar), na.rm = TRUE), 1),
      var_q1     = round(quantile(get(inputvar), probs = 0.25, na.rm = TRUE), 1),
      var_q3     = round(quantile(get(inputvar), probs = 0.75, na.rm = TRUE), 1),
      var_min    = round(min(get(inputvar), na.rm = TRUE), 1),
      var_max    = round(max(get(inputvar), na.rm = TRUE), 1)
    ), by = "stratum"]
  } else {
    temp <- dataset[eval(parse(text = condition)), .(
      var_mean   = round(wtd_mean(get(inputvar), get(weightvar)), 1),
      var_sd     = round(sqrt(wtd_var(get(inputvar), get(weightvar))), 1),
      var_median = round(wtd_quantile(get(inputvar), get(weightvar), 0.5), 1),
      var_q1     = round(wtd_quantile(get(inputvar), get(weightvar), 0.25), 1),
      var_q3     = round(wtd_quantile(get(inputvar), get(weightvar), 0.75), 1),
      var_min    = round(min(get(inputvar), na.rm = TRUE), 1),
      var_max    = round(max(get(inputvar), na.rm = TRUE), 1)
    ), by = "stratum"]
  }
  processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
  thislist <- c("mean", "sd", "median", "q1", "q3", "min", "max")
  setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  if (compute_Nmissing) {
    if (is.null(weightvar)) {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)), .(var_missingN = .N), by = "stratum"]
    } else {
      temp <- dataset[eval(parse(text = condition)) & is.na(get(inputvar)),
                      .(var_missingN = round(sum(get(weightvar), na.rm = TRUE), 1)), by = "stratum"]
    }
    processing <- merge(processing, temp, by = "stratum", all.x = TRUE)
    thislist <- c("missingN")
    if (nchar(vartot) > 0) {
      processing[, var_missingperc := round(100 * var_missingN / get(vartot), digits = 1)]
      thislist <- c(thislist, "missingperc")
    }
    setnames(processing, paste0("var_", thislist), paste0(outputvar, "_", thislist, "_"))
  }
  return(processing)
}



subpop <- "PC_HOSP"

for (subpop in thissubpopulations) {
  suffix <- fifelse(subpop == "all","",paste0("_",subpop))
  print(suffix)
  
  #########################################
  # load input dataset
  
  allstrata <- readRDS(file.path(thisdirinput, paste0("D4_EXP_ACT_COV_WEIGHT",suffix,".rds")))
  
  weights <-  readRDS(file.path(thisdirinput, paste0("D4_EXP_ACT_COV_WEIGHT_TRUNCATED",suffix,".rds")))
  weights <- weights[,.(person_id, t0, exposure, truncated_weight)]
  
  or <- readRDS(file.path(thisdirexp, paste0("D5_PS_model_OR",suffix,".rds")))
  
  # setnames(base, names(base), gsub("_at_t0", "", names(base)))
  or[, variable := gsub("_at_t0", "", variable)]
  
  allstrata <- merge(allstrata, weights , by = c("person_id", "t0", "exposure"), all.x = T)
  allstrata[is.na(truncated_weight), truncated_weight := 0]
  allstrata[, stratum := fifelse(exposure == 1, "exposed", "comparators")]
  
  #########################################
  # UNWEIGHTED descriptive table
  
  processing <- allstrata[, .(N_ = .N), by = "stratum"]
  
  for (covar in thiscovariates_cat) {
    namevar <- paste0(covar, "_at_t0")
    listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter == paste0(covar,"value"), .(values)])))
    processing <- desc_cat(processing, dataset = allstrata, inputvar = namevar, listofvalues = listofvaluesthisvar,
                           outputvar = covar, vartot = "N_", compute_Nmissing = TRUE)
  }
  
  for (covar in thiscovariates_cont) {
    namevar <- paste0(covar, "_at_t0")
    allstrata[, (namevar) := as.numeric(get(namevar))]
    processing <- desc_continuous(processing, dataset = allstrata, inputvar = namevar, outputvar = covar,
                                  vartot = "N_", compute_Nmissing = TRUE)
  }
  
  for (covar in thiscovariates_bin) {
    namevar <- paste0(covar, "_at_t0")
    processing <- desc_binary(processing, dataset = allstrata,  inputvar = namevar, outputvar = covar,
                              vartot = "N_", compute_Nmissing = FALSE)
  }
  
  #########################################
  # WEIGHTED descriptive table
  
  processing_w <- allstrata[, .(Nw_ = round(sum(truncated_weight, na.rm = TRUE), 1)), by = "stratum"]
  
  for (covar in thiscovariates_cat) {
    namevar <- paste0(covar, "_at_t0")
    listofvaluesthisvar <- unname(unlist(unique(parameters_this_step[parameter == paste0(covar,"value"), .(values)])))
    processing_w <- desc_cat(processing_w, dataset = allstrata, inputvar = namevar, listofvalues = listofvaluesthisvar,
                             outputvar = covar, vartot = "Nw_", compute_Nmissing = TRUE,
                             weightvar = "truncated_weight")
  }
  
  for (covar in thiscovariates_cont) {
    namevar <- paste0(covar, "_at_t0")
    processing_w <- desc_continuous(processing_w, dataset = allstrata, inputvar = namevar, outputvar = covar,
                                    vartot = "Nw_", compute_Nmissing = TRUE,
                                    weightvar = "truncated_weight")
  }
  
  for (covar in thiscovariates_bin) {
    namevar <- paste0(covar, "_at_t0")
    processing_w <- desc_binary(processing_w, dataset = allstrata, inputvar = namevar, outputvar = covar,
                                vartot = "Nw_", compute_Nmissing = FALSE,
                                weightvar = "truncated_weight")
  }
  
  # tag every weighted column (except the join key) so it doesn't clash with the unweighted names
  
  setnames(processing, setdiff(names(processing), "stratum"),
           paste0(setdiff(names(processing), "stratum"), "BF"))
  
  setnames(processing_w, "Nw_", "N_")
  setnames(processing_w, setdiff(names(processing_w), "stratum"),
           paste0(setdiff(names(processing_w), "stratum"), "AF"))
  
  #########################################
  # merge unweighted + weighted descriptive tables
  
  processing <- merge(processing, processing_w, by = "stratum")
  
  #########################################
  # ASD, unweighted and weighted
  
  for (covar in thiscovariates_bin) {
    varname <- paste0(covar, "_at_t0")
    x1 <- allstrata[stratum == "exposed", get(varname)]
    x0 <- allstrata[stratum == "comparators", get(varname)]
    w1 <- allstrata[stratum == "exposed", truncated_weight]
    w0 <- allstrata[stratum == "comparators", truncated_weight]
    processing[, asd   := asd_bin(x1, x0)]
    processing[, asd_w := asd_bin(x1, x0, w1, w0)]
    setnames(processing, c("asd","asd_w"), c(paste0(covar,"_asd_BF"), paste0(covar,"_asd_AF")))
  }
  
  for (covar in thiscovariates_cont) {
    varname <- paste0(covar, "_at_t0")
    x1 <- allstrata[stratum == "exposed", get(varname)]
    x0 <- allstrata[stratum == "comparators", get(varname)]
    w1 <- allstrata[stratum == "exposed", truncated_weight]
    w0 <- allstrata[stratum == "comparators", truncated_weight]
    processing[, asd   := asd_cont(x1, x0)]
    processing[, asd_w := asd_cont(x1, x0, w1, w0)]
    setnames(processing, c("asd","asd_w"), c(paste0(covar,"_asd_BF"), paste0(covar,"_asd_AF")))
  }
  
  for (covar in thiscovariates_cat) {
    varname <- paste0(covar, "_at_t0")
    x1 <- allstrata[stratum == "exposed", get(varname)]
    x0 <- allstrata[stratum == "comparators", get(varname)]
    w1 <- allstrata[stratum == "exposed", truncated_weight]
    w0 <- allstrata[stratum == "comparators", truncated_weight]
    processing[, asd   := asd_cat(x1, x0)]
    processing[, asd_w := asd_cat(x1, x0, w1, w0)]
    setnames(processing, c("asd","asd_w"), c(paste0(covar,"_asd_BF"), paste0(covar,"_asd_AF")))
  }
  
  processing[, exposure := fifelse(stratum == "exposed", 1, 0) ]
  
  #############################
  # OR
  
  
  for (covar in c(thiscovariates_cat)) {
    orcovar <- copy(or)[variable == covar,]
    nrowcovar <- nrow(orcovar)
    if (nrowcovar > 0) {
      for (j in 1:nrowcovar ) {
        thisvalue <- orcovar[j]$level
        thisor <- orcovar[j]$OR
        thisll <- orcovar[j]$CI_lower
        thisul <- orcovar[j]$CI_upper
        print(covar)
        print(thisvalue)
        for (par in c("or_","ll_","ul_")) {
          namevar <- paste0(covar,"_",thisvalue,"_",par)
          thisval <- as.character(get(paste0("this",substr(par, 1, nchar(par) - 1))))
          processing[, (namevar) := fifelse(str_length(thisval) > 0, round(as.numeric(thisval),3),NA_real_)]
          
        }
      }
    }
  }
  
  for (covar in c(thiscovariates_cont, thiscovariates_bin)) {
    orcovar <- copy(or)[variable == covar,]
    nrowcovar <- nrow(orcovar)
    if (nrowcovar > 0) {
      for (j in 1) {
        # thisvalue <- orcovar[j]$level
        thisor <- orcovar[j]$OR
        thisll <- orcovar[j]$CI_lower
        thisul <- orcovar[j]$CI_upper
        # print(covar)
        # print(thisvalue)
        for (par in c("or_","ll_","ul_")) {
          namevar <- paste0(covar,"_",par)
          thisval <- as.character(get(paste0("this",substr(par, 1, nchar(par) - 1))))
          processing[, (namevar) := fifelse(str_length(thisval) > 0, round(as.numeric(thisval),3),NA_real_)]
          
        }
      }
    }
  }
  
  
  #########################################
  # SAVE
  
  ordercols <- c("stratum", "exposure", setdiff(names(processing), c("stratum", "exposure")))
  processing <- processing[, ..ordercols]
 
  nameoutput <- "D5_Table_3_asd_before_after_trimming"
  nameoutputext <- paste0(nameoutput,suffix,".rds")
  assign(nameoutput, processing)
  saveRDS(processing, file = file.path(thisdiroutput, nameoutputext))
  
}