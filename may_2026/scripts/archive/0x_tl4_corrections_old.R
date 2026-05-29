# =============================================================================
# Title:    TL4 Data Recoding
# Author:   Tulika Jain
# Created:  2026-05-26
# Purpose:  Clean the TL4 pilot and main survey data, prep for analysis 
# Modified: 2026-05-26
# =============================================================================

library(tidyverse)
combined <- read_rds("data/tl4_combined_clean.rds")


#Recoding Data based on Recordings-----------------

#MALAY - KMWB00017


combined |> 
  filter(unique_id == "KMWB00017" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

#education, aware, confidence, environment - yes; harm- no 

combined_recode <- combined |> 
  filter(unique_id == "KMWB00017" & survey_completion == "completed") |> 
  mutate(impact_areas_harm = 0)
#Setting harm to 0 

combined |> 
  filter(unique_id == "KMWB00017" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_educ_", "impact_aware_", "impact_harm_", "impact_envir_", "impact_confidence_"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

combined_recode <- combined |> 
  filter(unique_id == "KMWB00017" & survey_completion == "completed") |> 
  mutate(impact_educ_1 = 0, impact_educ_2 = 0) |>
  mutate(impact_harm_1 = NA, impact_harm_2 = NA, impact_harm_3 = NA, impact_harm_4 = NA, impact_harm_10 = NA) |> 
  mutate(impact_confidence_2 = 0) |> 
  mutate(impact_envir_9 = 0)
#Correcting codes based on recording 



#MALAY - KMWB00003


combined |> 
  filter(unique_id == "KMWB00003" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)
#educ, aware, environment. Add health. 

combined_recode <- combined |> 
  filter(unique_id == "KMWB00003" & survey_completion == "completed") |> 
  mutate(impact_areas_health = 1)
#Setting health to 1

combined |> 
  filter(unique_id == "KMWB00003" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_educ_", "impact_aware_", "impact_health_", "impact_envir_"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined_recode <- combined |> 
  filter(unique_id == "KMWB00003" & survey_completion == "completed") |> 
  mutate(impact_educ_1 = 0, impact_educ_7 = 0, impact_educ_8 = 0) |>
  mutate(impact_aware_8 = 0) |> 
  mutate(impact_envir_7 = 0) |> 
  mutate(across(starts_with("impact_health_"), \(x) 0)) |> 
  mutate(impact_health_5 = 1) 
#Correcting codes based on recording 


#JAGRITI - 3 surveys miscoded (Love1)

combined |> 
  filter(survey_completion == "completed" &  source == "pilot", leader_name == "Sanjay Sahni") |> 
  select(love1, love2)

combined_recode <-combined |>
  filter(survey_completion == "completed" &  source == "pilot", leader_name == "Sanjay Sahni") |> 
  mutate(love1 = 5)


#SSBH00001 - miscoded NREGA wages for stable job
combined |> 
  filter(unique_id == "SSBH00001" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

combined |> 
  filter(unique_id == "SSBH00001" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_income_")) |> 
  pivot_longer(
    cols = starts_with("impact_income_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined_recode <- combined |> 
  filter(unique_id == "SSBH00001" & survey_completion == "completed") |> 
  mutate(impact_income_1 = 0, impact_income_5 = 1)


#Manoshi - KMWB00019

combined <- combined |> 
  mutate(unique_id = str_to_upper(unique_id))  

combined |> 
  filter(unique_id == "KMWB00019" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined |> 
  filter(unique_id == "KMWB00019" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_aware_", "impact_income_", "impact_govt_access", "impact_envir_"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

#Add education2, remove income, remove govt access, remove envir2, remove aware6

combined_recode <- combined |> 
  filter(unique_id == "KMWB00019" & survey_completion == "completed") |> 
  mutate(across(starts_with("impact_educ_"), \(x) 0)) |> 
  mutate(impact_areas_educ = 1, impact_educ_2 = 1) |> 
  mutate(impact_areas_income = 0, impact_income_98 = 0) |> 
  mutate(across(starts_with("impact_income_"), \(x) NA)) |> 
  mutate(impact_areas_govt_access = 0, impact_govt_access_3 = 0) |> 
  mutate(across(starts_with("impact_govt_access_"), \(x) NA)) |> 
  mutate(impact_envir_2 = 0) |> 
  mutate(impact_aware_6 = 0)


# Jagriti- SBUP00062


combined |> 
  filter(unique_id == "SBUP00062" & survey_completion == "completed") |>  
  select(frequency_engagement)

combined_recode <- combined |> 
  filter(unique_id == "SBUP00062" & survey_completion == "completed") |>  
  mutate(frequency_engagement = 9)



# Shobha - KPGJ00014
combined |> 
  filter(unique_id == "KPGJ00014" & survey_completion == "halfway") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

combined |> 
  filter(unique_id ==  "KPGJ00014" & survey_completion == "halfway") |>  
  select(unique_id, starts_with(c("impact_educ_"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1) 

#add  education2, confidence2
combined_recode <- combined |> 
  filter(unique_id == "KPGJ00014" & survey_completion == "halfway") |>  
  mutate(impact_educ_99 = 0, impact_educ_2 = 1) |> 
  mutate(impact_areas_confidence = 1, impact_confidence_2 = 1)


# Shobha - KPGJ00087
combined |> 
  filter(unique_id == "KPGJ00087" & survey_completion == "completed") |>  
  select(unique_id, frequency_engagement, love2, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined |> 
  filter(unique_id == "KPGJ00087" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_educ_", "impact_aware_", "impact_income", "impact_confidence_"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


#frequency9, educ2 (not 4), aware6 wrong, love2 5

combined_recode <- combined |> 
  filter(unique_id == "KPGJ00087" & survey_completion == "completed") |>    
  mutate(frequency_engagement = 9) |> 
  mutate(impact_educ_4 = 0, impact_educ_2 = 1) |> 
  mutate(impact_areas_aware = 0) |> 
  mutate(across(starts_with("impact_aware_"), \(x) NA)) |> 
  mutate(love2 = 5)


# Gunja - SSBH00162
combined |> 
  filter(unique_id == "SSBH00162" & survey_completion == "completed") |>  
  select(love1)

combined_recode <- combined |> 
  filter(unique_id == "SSBH00162" & survey_completion == "completed") |>  
  mutate(love1 = 98)


# Rani- ASJH00020

#NO PERSONAL IMPACT

combined |> 
  filter(unique_id == "ASJH00020" & survey_completion == "completed") |>  
  select(unique_id, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

combined_recode <- combined |> 
  filter(unique_id == "ASJH00020" & survey_completion == "completed") |>  
  mutate(depth1 = 2) |> 
  mutate(impact_areas_educ = 0) |> 
  mutate(impact_areas_aware = 0) |> 
  mutate(across(starts_with("impact_aware_"), \(x) NA)) |>
  mutate(across(starts_with("impact_educ_"), \(x) NA))

#KPGJ00001

combined |> 
  filter(unique_id == "KPGJ00001" & survey_completion == "completed") |>  
  select(unique_id, educ_level, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined |> 
  filter(unique_id == "KPGJ00001" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_aware_", "impact_govt_access"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined_recode <- combined |> 
  filter(unique_id == "KPGJ00001" & survey_completion == "completed") |>  
  mutate(educ_level = 4) |> 
  mutate(impact_aware_6 = 1) 


#SBUP00003

combined |> 
  filter(unique_id == "SBUP00003" & survey_completion == "completed") |>  
  select(unique_id, frequency_engagement, smartphone, starts_with("impact_areas_")) |> 
  pivot_longer(
    cols = starts_with("impact_areas_"),
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)


combined |> 
  filter(unique_id == "SBUP00003" & survey_completion == "completed") |>  
  select(unique_id, starts_with(c("impact_educ_", "impact_harm", "impact_water_land"))) |> 
  pivot_longer(
    cols = -unique_id,
    names_to = "area",
    values_to = "selected"
  ) |> 
  filter(selected == 1)

#frequency9, aware7, income1, govt3, belong6. no: waterlandother, harm4, 

combined_recode <- combined |> 
  filter(unique_id == "SBUP00003" & survey_completion == "completed") |>  
  mutate(frequency_engagement = 9) |> 
  mutate(impact_areas_aware = 1, impact_aware_7 = 1) |> 
  mutate(impact_areas_income = 1, impact_income_1 = 1) |> 
  mutate(impact_areas_govt_access = 1, impact_govt_access_3 = 1) |> 
  mutate(impact_areas_belong = 1, impact_belong_6 = 1) |> 
  mutate(impact_water_land_other = 0, impact_water_land_3= 0) |> 
  mutate(impact_harm_4 = 0) |> 
  mutate(smartphone = "no")


#KPGJ00036

combined |> 
  filter(unique_id == "KPGJ00036" & survey_completion == "completed") |>  
  select(unique_id, frequency_engagement, depth2, depth3)

#RECODING BASED ON NOTES ----------

#Case 1 - Ajit 2 child HH
combined |> filter(leader_name == "Ajit Yadav") |> view()
combined |> filter(leader_name == "Ajit Yadav", unique_id == "AYUP00044") |> select(survey_completion)
combined |> filter(uuid == "860a1529-1da7-40be-ac4f-c662a1dc5976") |> select(unique_id)
#AYUP00044 responses need to be the same as AYUP00041

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

combined_recode |> filter(unique_id == "AYUP00041" | unique_id == "AYUP00044") 



#Case 2 - Remove Ummey calls
combined_recode |> filter(unique_id == "AYUP00001" | unique_id == "AYUP00005") |> view()

combined_recode <- combined_recode |> 
  filter(!(unique_id %in% c("AYUP00001", "AYUP00005") & surveyor == "ummey"))



