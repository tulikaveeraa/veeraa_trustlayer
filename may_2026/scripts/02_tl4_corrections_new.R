# =============================================================================
# Title:    TL4 Data Recoding
# Author:   Tulika Jain
# Created:  2026-05-26
# Purpose:  Clean the TL4 pilot and main survey data, prep for analysis 
# Modified: 2026-05-28
# =============================================================================

library(tidyverse)
library(janitor)
combined <- read_rds("data/tl4_combined_clean.rds")

combined <- combined |> 
  mutate(unique_id = str_to_upper(unique_id))  
#converting unique IDs to all uppercase 

combined_recode <- combined

#RECORDING CORRECTIONS -----------------------------------

#LOAD CORRECTIONS 
corrections <- read_csv("data/corrections.csv")

#APPLY ID-BASED NUMERIC CORRECTIONS
apply_corrections <- function(df, corrections) {
  for (i in seq_len(nrow(corrections))) {
    id     <- corrections$unique_id[i]
    status <- corrections$survey_completion[i]
    var    <- corrections$variable[i]
    val    <- corrections$new_value[i]
    
    # Build a logical mask for which rows to modify
    rows_to_update <- df$unique_id == id & df$survey_completion == status
    
    # Update only those rows, leave the rest alone
    df[[var]][rows_to_update] <- val
  }
  df
}

combined_recode <- apply_corrections(combined, corrections)

#APPLY SPECIAL-CASE CORRECTIONS 

combined_recode <- combined_recode |> 
  mutate(smartphone = if_else(
    unique_id == "SBUP00003" & survey_completion == "completed",
    "no", smartphone
  ))


#DEBRIEF CORRECTIONS -----------------------------------------

#--CASE 1- Ajit 2 child HH

# Step 1: Grab the row you want to duplicate, modify it
new_row <- combined_recode |> 
  filter(unique_id == "AYUP00041", survey_completion == "completed") |> 
  mutate(
    unique_id      = "AYUP00044",
    community_member = "Khushbu",
    respondent_age = 9,
    educ_level = 2
  )

# Step 2: Add it to the dataset
combined_recode <- bind_rows(combined_recode, new_row)


#--CASE2- Remove Ummey calls
combined_recode |> filter(unique_id == "AYUP00001" | unique_id == "AYUP00005") |> view()

combined_recode <- combined_recode |> 
  filter(!(unique_id %in% c("AYUP00001", "AYUP00005") & surveyor == "ummey"))


#--CASE3- SHOBHA Doesnt Know Leader Recoding 

combined_recode <- combined_recode |> 
  mutate(survey_completion = if_else(
    unique_id %in% c("KPGJ00003", "KPGJ00081", "KPGJ00083") & dontknowvl == "no",
    "doesntknow", survey_completion
  ))


#HOW TO TREAT HALFWAYS-----------------------------

combined_recode |> 
  filter(survey_completion == "halfway") |> 
  mutate(across(everything(), as.character)) |> 
  rowwise() |> 
  mutate(first_99 = {
    vals <- c_across(everything())
    cols <- names(pick(everything()))
    first_match <- which(vals == "99")[1]
    if (is.na(first_match)) NA_character_ else cols[first_match]
  }) |> 
  ungroup() |> 
  count(first_99, sort = TRUE) |> 
  adorn_totals("row")

#We have 9 "halfways". One observation is <NA>. 
#Investigating further 
combined_recode |> 
  filter(survey_completion == "halfway") |> 
  mutate(across(everything(), as.character)) |> 
  rowwise() |> 
  mutate(has_99 = any(c_across(everything()) == "99", na.rm = TRUE)) |> 
  ungroup() |> 
  filter(!has_99) |> 
  select(unique_id, has_99, everything())

#KPGJ00029 is incorrect. It is actually a call_back. Correction: 

combined_recode <- combined_recode |> 
  mutate(survey_completion = if_else(unique_id == "KPGJ00029", "call_back", survey_completion))

#Another observation ended the survey before we could ask time_since_contact. Investigating further
combined_recode |> filter(survey_completion == "halfway", time_since_contact == 99) |> view()

#This is a key variable for active community metric. This survey should hence be recoded as "refused to talk"

#Correction:
combined_recode <- combined_recode |> 
  mutate(survey_completion = if_else(unique_id == "KPGJ00019", "refused", survey_completion))

#FROM ANALYSIS FILE: 
combined_recode |> 
  filter(time_since_contact == 99, survey_completion != "halfway") |>
  select(unique_id, survey_completion)
#time_since_contact should be counted as missing as this survey is "doesnt know". 
#Going back and changing it in the corrections file. 

combined_recode <- combined_recode|> 
  mutate(time_since_contact = if_else(unique_id == "ASJH00093", NA, time_since_contact))

#The remaining have some substantial information, and can be used as denominators in analysis


#SAVE----
write_rds(combined_recode, "data/tl4_corrected.rds")
