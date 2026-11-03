############################################################
#                                                          #
#              CREATE D6_TABLE_11_SA_MICE                 #
#                                                          #
############################################################

# author: Rosa Gini
# v 1.0 09 Sep 2026

print("CREATE D6_Table_11_SA_MICE")

#########################################
# assign directories

if (TEST) {
  # testname <- "test_D6_Table_11_SA_MICE"
  # thisdirinput <- file.path(dirtest, testname)
  # thisdiroutput <- file.path(thisdirinput, "g_output")
  # thisdirsettings <- thisdirinput
  # dir.create(thisdiroutput, showWarnings = FALSE)
} else {
  if (localprocessing) {
    thisdirinput <- direxp
    thisdiroutput <- direxp
    thisdirsettings <- dirpargen
    settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
    thisdatasources_for_postprocessing <- settings$ds
    thisfolder_submission <- list()
    thisfolder_submission[[thisdatasource]] <- thisdirinput
  } else {
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- file.path(direxp, "Formatted tables")
    dir.create(thisdiroutput, showWarnings = FALSE, recursive = TRUE)
  }
}

#########################################
# create final table

output <- data.table()

for (thisds in thisdatasources_for_postprocessing) {

  files_ds <- list.files(
    path = thisfolder_submission[[thisds]],
    pattern = "^D5_ResultsIR_HR_MICE.*\\.csv$",
    full.names = TRUE
  )

  if (length(files_ds) == 0L) {
    message(paste0("No D5_ResultsIR_HR_MICE file found for ", thisds))
    next
  }

  for (filetoread in files_ds) {

    temp <- fread(filetoread)

    required_cols <- c("ds", "HR", "HR_low", "HR_high")
    missing_cols <- setdiff(required_cols, names(temp))

    if (length(missing_cols) > 0L) {
      stop(paste0(
        "Missing column(s) in ", basename(filetoread), ": ",
        paste(missing_cols, collapse = ", ")
      ))
    }

    temp[, `Adjusted pooled HR (95% CI)` := sprintf(
      "%.3f (%.3f–%.3f)",
      as.numeric(HR),
      as.numeric(HR_low),
      as.numeric(HR_high)
    )]

    temp <- temp[, .(
      ds,
      `Adjusted pooled HR (95% CI)`
    )]

    output <- rbind(output, temp, fill = TRUE)
  }
}

if (nrow(output) == 0L) {
  stop("No D5_ResultsIR_HR_MICE results were available to create D6_Table_11_SA_MICE.")
}

#########################################
# save

nameoutput <- "D6_Table_11_SA_MICE"
assign(nameoutput, output)

fwrite(
  output,
  file = file.path(thisdiroutput, paste0(nameoutput, ".csv"))
)

saveRDS(
  output,
  file = file.path(thisdiroutput, paste0(nameoutput, ".rds"))
)

print(output)

#########################################
