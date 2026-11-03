############################################################
#                                                          #
#########         08_T5_10_Create_D6_Table_1        ########
#                                                          #
############################################################

# v 2.0

# added support for running in DRE with multiple input datasources

# v 1.0

print("CREATE D6_Table_1")

#########################################
# assign directories

if (TEST) {
  testname <- "test_09_Flowchart"
  thisdirinput <- file.path(dirtest, testname)
  thisdiroutput <- file.path(thisdirinput, "g_output")
  thisdirsettings <- thisdirinput
  settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
  thisdatasources_for_postprocessing <- settings$ds
  thisdatasource <- settings$ds
  thisdirarchive <- thisdirinput
  thisfolder_submission <- list()
  thisfolder_submission[[thisdatasource]] <- thisdirinput
} else {
  if (localprocessing){
    thisdirinput <- direxp
    thisdiroutput <- direxp
    thisdirsettings <- dirpargen
    settings <- readRDS(file.path(thisdirsettings,"SETTINGS.rds"))
    thisdatasources_for_postprocessing <- settings$ds
    thisdirarchive <- dirarchive
    thisfolder_submission <- list()
    thisfolder_submission[[thisdatasource]] <- thisdirinput
  }else{
    thisdatasources_for_postprocessing <- datasources_for_postprocessing
    thisfolder_submission <- folders_submission
    thisdiroutput <- paste0(direxp,"Formatted tables/")
    dir.create(thisdiroutput, showWarnings = F)
    thisdirarchive <- dirarchive
    thisdirsettings <- dirpargen
  }
}

#########################################
# load parameters

if (localprocessing) {
  settings <- readRDS(file.path(thisdirsettings, "SETTINGS.rds"))
  thissubpopulations <- unique(settings[, .(subpop)])
  thissubpopulations <- unlist(strsplit(trimws(thissubpopulations), " +"))
}

# from parameter settings: create the list of subpopulations of all the datasources
PARSETTINGS <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SETTINGS.xlsx") ))
thissubpopulations_ds <- list()
for (thisds in thisdatasources_for_postprocessing) {
  thissubpopulations_ds[[thisds]] <- PARSETTINGS[ds == thisds,.(subpop)]
  thissubpopulations_ds[[thisds]] <- thissubpopulations <- unlist(strsplit(trimws(thissubpopulations_ds[[thisds]]), " +"))
}

PARPOP <-   as.data.table(readxl::read_excel(file.path(thisdirarchive, "PAR_SUBPOP.xlsx") ))


tab_nice_all <- list()
#tab_nice_all2 <- list()
list_ds_analysed <- c()
name_suffix <- list()

for (thisds in thisdatasources_for_postprocessing) {
  
  for (thissubpop in thissubpopulations_ds[[thisds]]) {
    
    thissuffix <- fifelse(thissubpop == "all","",paste0("_",thissubpop))
    
    filetoread <- file.path(
      thisfolder_submission[[thisds]],
      paste0("D5_Table_1_masked", thissuffix, ".csv")
    )
    filetoread2 <- file.path(
      thisfolder_submission[[thisds]],
      paste0("excluded_counts_masked", thissuffix, ".csv")
    )
    if (file.exists(filetoread) & file.exists(filetoread2) ) { 
      temp <- fread(filetoread, colClasses = "character")
      temp2 <- fread(filetoread2, colClasses = "character")
      suffix <- paste0("_",thisds,thissuffix)
      name_suffix[[suffix]] <- fifelse(unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]) == "", suffix, unlist(PARPOP[ds == thisds & subpop == thissubpop,.(name_in_tables)]))
      tab_nice_all[[suffix]] <- temp
      #tab_nice_all2[[suffix]] <- temp2
      list_ds_analysed <- c(list_ds_analysed, suffix)
    }else{
      errmess <- paste("File",filetoread,"does not exist in the export folder of", thisds)
      stop(errmess)
    }
  }
}


tab1 <- data.table()
for (suffix in list_ds_analysed) {
  temp <- copy(tab_nice_all[[suffix]])
  temp[, name_ds_subpop := name_suffix[[suffix]]]
  tab1  <- rbind(tab1, temp, fill = T)
}

# tab2 <- data.table()
# for (suffix in list_ds_analysed) {
#   temp <- copy(tab_nice_all2[[suffix]])
#   temp[, name_ds_subpop := name_suffix[[suffix]]]
#   tab2  <- rbind(tab2, temp, fill = T)
# }


#########################################
# identify N and percentage columns independently from exact names
n_cols <- grep("^N_", names(tab1), value = TRUE)
fixed_cols <- c(
  "N_entering_ds",
  "N_entering_flowchart",
  "N_entering_study",
  "N_eligible_dapagliflozin",
  "N_eligible_Dpp4i"
)
criteria_n_cols <- setdiff(n_cols, fixed_cols)
criteria_n_cols <- n_cols[n_cols %in% criteria_n_cols]
criteria_pct_cols <- sub("^N_", "PERCENTAGE_", criteria_n_cols)
get_col <- function(dt, col) {
  if (col %in% names(dt)) dt[[col]] else NA
}

#########################################
# create table content (generic over multiple data sources)

build_ds_columns <- function(tab1_row) {
  N <- c(
    get_col(tab1_row, "N_entering_ds"),
    unlist(lapply(criteria_n_cols, \(x) get_col(tab1_row, x))[1:3]),
    get_col(tab1_row, "N_entering_flowchart"),
    unlist(lapply(criteria_n_cols, \(x) get_col(tab1_row, x))[4:14]),
    get_col(tab1_row, "N_entering_study"),
    get_col(tab1_row, "N_eligible_dapagliflozin"),
    get_col(tab1_row, "N_eligible_Dpp4i")
  )
  
  Percent <- c(
    NA,
    unlist(lapply(criteria_pct_cols, \(x) get_col(tab1_row, x))[1:3]),
    get_col(tab1_row, "PERCENTAGE_entering_flowchart"),
    unlist(lapply(criteria_pct_cols, \(x) get_col(tab1_row, x))[4:14]),
    get_col(tab1_row, "PERCENTAGE_entering_study"),
    get_col(tab1_row, "PERCENTAGE_eligible_dapagliflozin"),
    get_col(tab1_row, "PERCENTAGE_eligible_Dpp4i")
  )
  
  list(N = N, Percent = Percent)
}

Criteria <- c(
  "All individuals registered in the data source",
  "AND have a sex non-missing",
  "AND have complete date of death",
  "AND have at least a spell",
  "All individuals registered in the primary care data source with complete basic variables",
  "AND have a valid year of birth",
  "AND have linkage with hospital data",
  "AND are registered during the study period (2012 to 2024)",
  "AND received dapagliflozin or DPP4i during the enrolment period 1 Jan 2013 to 31 December 2024",
  "AND have at least one year of recorded medical history in database prior to dapagliflozin or DPP4i",
  "AND DO NOT have a documented history or diagnosis of Type 1 diabetes at any time prior to index",
  "AND are aged 40 or older at index",
  "AND have a diagnosis of T2D prior to index",
  "AND have either established ACVD or high ACVD risk",
  "AND WERE NOT treated with an SGLT2-I or DPP4-I in previous year",
  "AND DO NOT have a documented history of acute cardiovascular event in the year prior to index",
  "Total Eligible Cohort",
  "Eligible dapagliflozin",
  "Eligible DPP4i"
)

ds_names <- tab1$ds
ds_cols_list <- lapply(seq_len(nrow(tab1)), function(i) build_ds_columns(tab1[i]))

D6_tab <- data.frame(Criteria = Criteria)

for (i in seq_along(ds_cols_list)) {
  D6_tab[[paste0("N_", ds_names[i])]] <- ds_cols_list[[i]]$N
  D6_tab[[paste0("Percent_", ds_names[i])]] <- ds_cols_list[[i]]$Percent
}

#########################################
# create flextable

ft <- flextable(D6_tab)

header_labels <- list(Criteria = "")
for (nm in ds_names) {
  header_labels[[paste0("N_", nm)]] <- "N"
  header_labels[[paste0("Percent_", nm)]] <- "%"
}
ft <- set_header_labels(ft, values = header_labels)

ft <- add_header_row(
  ft,
  values = c("", rep(ds_names, each = 2)),
  colwidths = c(1, rep(1, 2 * length(ds_names)))
)

ft <- width(ft, j = "Criteria", width = 5.8)
for (nm in ds_names) {
  ft <- width(ft, j = paste0("N_", nm), width = 2.6)
  ft <- width(ft, j = paste0("Percent_", nm), width = 1.2)
}

n_pct_cols <- unlist(lapply(ds_names, function(nm) c(paste0("N_", nm), paste0("Percent_", nm))))
ft <- align(ft, j = n_pct_cols, align = "center", part = "all")
ft <- align(ft, j = "Criteria", align = "left", part = "all")

ft <- fontsize(ft, size = 10, part = "all")
#ft <- font(ft, fontname = "Arial", part = "all")

ft <- border_remove(ft)
ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.75))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.75))

ft <- bold(ft, i = c(1,5,17,18,19), j = "Criteria", bold = TRUE)
ft <- bold(ft, part = "header")

ft <- padding(ft, padding = 4, part = "all")
ft <- padding(ft, i = c(18, 19), j = 1, padding.left = 20)

ft <- set_caption(
  ft,
  caption = paste0("Table 1. Attrition Table, ", paste(ds_names, collapse = " vs "))
)

#########################################
# save outputs

nameoutput <- paste0("D6_Table_1")

save_as_docx(
  ft,
  path = file.path(thisdiroutput, paste0(nameoutput, ".docx"))
)

save_as_html(
  ft,
  path = file.path(thisdiroutput, paste0(nameoutput, ".html"))
)

# fix: insert an empty favicon to prevent the 404 error that causes Chrome_Print to crash
htmlfile <- file.path(thisdiroutput, paste0(nameoutput, ".html"))
htmlcontent <- readLines(htmlfile, warn = FALSE)
htmlcontent <- sub(
  "<head>",
  '<head><link rel="icon" href="data:,">',
  htmlcontent
)
writeLines(htmlcontent, htmlfile)

chrome_print(
  input = htmlfile,
  output = file.path(thisdiroutput, paste0(nameoutput, ".pdf"))
)



# CREATE D6_Figure_1_masked

print("CREATE D6_Figure_1_masked")

#########################################
# Flowchart

flowchart <- glue("
  
  digraph flowchart {{
  
    graph [
      layout = dot,
      rankdir = TB,
      nodesep = 0.5,
      ranksep = 1
    ]
  
    node [
      shape = box,
      style = filled,
      fontname = Helvetica,
      fontsize = 11,
      width = 4
    ]
  
    edge [
      arrowsize = 0.8
    ]
    
    S [
      label = 'All individuals registered in {tab1$ds}\\ndata source\\nN = {format({temp2$N_entering_ds}, big.mark = \",\")}'
    ]
  
    E0 [
      label = 'Exclude\\l\\lN = {format({temp2$N_excluded_sex_missing}, big.mark = \",\")} sex non-missing\\lN = {format({temp2$N_excluded_partial_date_of_death}, big.mark = \",\")} did not have complete date of death\\lN = {format({temp2$N_excluded_no_spells}, big.mark = \",\")} did not have at least a spell\\l',
      width = 6
    ]
    
    A [
      label = 'All individuals registered in \\n{tab1$ds} with basic variables\\nN = {format({tab1$N_entering_flowchart}, big.mark = \",\")}'
    ]
  
    B [
      label = 'All individuals registered in {tab1$ds} primary\\ncare with hospital linkage with valid birth year\\nwho received dapa or dpp4-I between\\n1 Jan 2013 - 31 December 2024\\nN = {format({tab1$N_with_episode_in_eligibility_period}, big.mark = \",\")}'
    ]
  
    C [
      label = 'Adults with T2D and either established\\nASCVD or high risk of ASCVD, who initiated\\ndapagliflozin or a DPP-4 inhibitor between\\n2013 and 2024 who are part of the eligible\\npatient cohort\\nN = {format({tab1$N_entering_study}, big.mark = \",\")}'
    ]

    E1 [
      label = 'Exclude\\l\\lN = {format({temp2$N_excluded_not_valid_birth_date}, big.mark = \",\")} without valid year of birth\\lN = {format({temp2$N_excluded_no_HES_linkage}, big.mark = \",\")} did not have linkage to HES data\\lN = {format({temp2$N_excluded_no_spells_in_enrolment_period}, big.mark = \",\")} registration outside study period\\lN = {format({temp2$N_excluded_with_no_episode_in_eligibility_period
}, big.mark = \",\")} did not receive dapagliflozin or DPP-4i\\l',
      width = 6
    ]
  
    E2 [
      label = 'Exclude\\l\\lN = {format({temp2$N_excluded_not_enough_lookback}, big.mark = \",\")} less than one year recorded history\\lN = {format({temp2$N_excluded_T1D_diagnosis}, big.mark = \",\")} Type 1 diabetes prior to index\\lN = {format({temp2$N_excluded_no_too_young}, big.mark = \",\")} younger than 40 years at index\\lN = {format({temp2$N_excluded_no_T2D_diagnosis}, big.mark = \",\")} no T2D diagnosis prior to index\\lN = {format({temp2$N_excluded_no_ACVD_or_risk_of_ACVD}, big.mark = \",\")} no established/high-risk ASCVD\\lN = {format({temp2$N_excluded_prevalent_SGLT2I_or_DPP4}, big.mark = \",\")} SGLT2 or DPP-4 use in prior year\\lN = {format({temp2$N_excluded_acute_CV_history}, big.mark = \",\")} acute cardiovascular event in prior year\\l',
      width = 6
    ]
  
    S -> A
    A -> B
    B -> C
  
    S -> E0
    A -> E1
    B -> E2
  
    {{rank = same; S; E0}}
    {{rank = same; A; E1}}
    {{rank = same; B; E2}}
  
  }}
  
  ")

svg <- grViz(flowchart)

#########################################
# save Figure 1

outputfile <- export_svg(svg)

nameoutput <- paste0("D6_Figure_1_masked", suffix)

# writeLines(
#   outputfile,
#   file.path(thisdiroutput, paste0(nameoutput, ".svg"))
# )

rsvg_png(
  charToRaw(outputfile),
  file.path(thisdiroutput, paste0(nameoutput, ".png")),
  width = 3000,
  height = 2000
)
