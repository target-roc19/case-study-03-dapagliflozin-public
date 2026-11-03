
temp <- processing

baselinedate <- ymd(20161231)

datevarlist <- c("t0", "outcome_date", "exit_spell_category", "death_date", "discontinuation_date" ,"switch_date", "addition_date", "end_time_window_fup", "censoring_date", "censoring_date_E1", "date_intercurring_events_E1", "censoring_date_excluding_intercurring_E1", "censoring_date_E2", "censoring_date_E3b", "new_medicine_date")

# datevarlist <- c("start_look_back","exit_spell_category","start_d","end_d"   )

for (datevar in datevarlist) { 
  print(datevar)
  temp[, (datevar) := as.integer(get(datevar) - baselinedate)]
}

fwrite(temp, "C:/temp/temp.csv")

