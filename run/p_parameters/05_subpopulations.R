##%######################################################%##
#                                                          #
####      Definition of subpopulations parameters       ####
#                                                          #
##%######################################################%##

# in some subpopulations, some records must be discarded. these are the records whose meaning indicate that they are not collected for all individuals in the subpopulation. such meanings are stored in the parameter file 'SUBPOP'. in such file for each data source and each subpopulation,  the meanings to be discarded are a space-separated sequence of strings in the cell exclude_meaning.
# This function reads the SUBPOP parameter file, extracts exclude_meaning for each requested subpopulation, splits it on spaces, and returns a named 2-level list of selection rules whose names are the subpopulations (when this is run, we are already restricted to a single datasource therefore datasources are not handled in this function). 
# The function is to be used in all the steps where conceptset or itemset datasets are imported. Note that if the cell of SUBPOP is empty, the rule is '' and no record is discarded.

create_selection_rules_for_subpopulations <- function(filesubpoppar, thissubpopulations) {
  
  thisexcludemeaning <- list()
  
  if (file.exists(filesubpoppar)) {
    subpoppar <- readRDS(filesubpoppar)
    
    for (subp in thissubpopulations) {
      temp <- unname(unlist(subpoppar[subpop == subp, .(exclude_meaning)]))
      
      temp1 <- if (length(temp) == 0 || is.na(temp) || trimws(temp) == "") {
        character(0)
      } else {
        strsplit(trimws(temp), "\\s+")[[1]]
      }
      
      temp2 <- if (length(temp1) == 0) {
        character(0)
      } else {
        paste0('meaning_renamed != "', temp1, '"')
      }
      
      thisexcludemeaning[[subp]] <- temp2 # c("!is.na(person_id)", temp2)
      thisexcludemeaning[[subp]] <- paste(thisexcludemeaning[[subp]], collapse = " & ")
    }
    
  } else {
    for (subp in thissubpopulations) {
      thisexcludemeaning[[subp]] <- ""
    }
  }
  
  return(thisexcludemeaning)
}