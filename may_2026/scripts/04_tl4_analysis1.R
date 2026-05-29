# =============================================================================
# Title:    TL4 Analysis 1- for Metrics
# Author:   Tulika Jain
# Created:  2026-05-28
# Purpose:  Analysis Script 1 for website metrics 
# Modified: 2026-05-28
# =============================================================================

#change i am changing this


#LOAD DATASET ------------------------------------
library(tidyverse)
library(janitor)
#install.package("writexl)
library(writexl)

tl4_analysis <- read_rds("data/tl4_analysis.rds")
codebook <- read_csv("data/codebook_tl4.csv")
commsize <- read_csv("data/community_size.csv")

#ACTIVE COMMUNITY ------------------------------

#Looking at Distribution across VLs
tl4_analysis |> filter(survey_completion %in% c("completed", "halfway", "doesntknow")) |> 
  tabyl(leader, time_since_contact) |> 
  adorn_totals(c("row", "col")) |> 
  adorn_percentages("row") |> 
  adorn_pct_formatting(digits = 1) |> 
  adorn_ns()


#Active Community Percentage:
tl4_analysis <- tl4_analysis |> 
  mutate(active = if_else(time_since_contact_num%in% c(1, 2, 3, 4, 5), 1, NA)) |> 
  mutate(active = if_else(time_since_contact_num == 6, 0, active)) 

#Check
tl4_analysis |> 
  filter(survey_completion  %in% c("completed", "halfway", "doesntknow")) |> 
  tabyl(active, survey_completion, na.rm = TRUE) |> 
  adorn_ns()

tl4_analysis |> filter(survey_completion == "doesntknow", active == 0) |> view() |>  select(unique_id) 
#this survey should be recoded as "complete". Its not that they dont know the vl, its that they met them more than 2 years ago (as per surveyor comments)
tl4_analysis <- tl4_analysis |> mutate(survey_completion = if_else(unique_id == "ASJH00080", "completed", survey_completion))


#Generating the table
active_summary <- tl4_analysis |> 
  filter(survey_completion %in% c("completed", "halfway", "doesntknow")) |> 
  group_by(leader) |> 
  summarise(
    active_comm       = sum(active == 1, na.rm = TRUE),
    n_completed       = sum(survey_completion == "completed", survey_completion == "halfway", na.rm = TRUE),
    n_dkleader        = sum(survey_completion == "doesntknow", na.rm = TRUE),
    n_total           = n_completed + n_dkleader
  ) |> 
  mutate(pct_active = round(active_comm / n_total * 100, 2))

#Calculating actual community size by multiplying:
active_summary <- active_summary |> 
  left_join(commsize, by = "leader")

active_summary <- active_summary |> 
  mutate(total_active = pct_active/100 * comm_size)
  

#IMPACT SCORE------------------------------
tl4_analysis |> 
  filter(is.na(depth1), active == 1) |> 
  view()
#removing the pilot surveys as they did not ask the depth question

# Step 1: Crosstab of depth1 counts per leader (one column per category)
category_counts <- tl4_analysis |> 
  filter(active == 1, source == "main") |> 
  group_by(leader, leader_name) |> 
  count(depth1, .drop = FALSE) |> 
  pivot_wider(names_from = depth1, values_from = n, values_fill = 0) |> 
  ungroup()

# Step 2: Summary stats per leader
summary_stats <- tl4_analysis |> 
  filter(active == 1, source == "main") |> 
  group_by(leader) |> 
  summarise(
    total_positive = sum(depth1 %in% c("Some positive impact",
                                       "Significantly improved",
                                       "Saved life"), na.rm = TRUE),
    denom          = sum(depth1 != "Survey left incomplete", na.rm = TRUE),
    pct_impact     = round(total_positive / denom * 100, 2)
  )

# Step 3: Join them
impact_score <- category_counts |> 
  left_join(summary_stats, by = "leader") |> 
  arrange(desc(pct_impact))


#IMPACT AREAS----------------------------------------


#Crosstab of each impact area counts per leader (one row per category)
# Define the variable range (drop text fields)
impact_vars <- tl4_analysis |> 
  select(impact_educ_1:envir_other, 
         -impact_other, -educ_other, -aware_other, -income_other, -health_other,
         -finance_other, -harm_other, -confidence_other, -govt_access_other, -water_land_other, 
         -belong_other, -envir_other, -impact_confidenceyn, -ends_with("_99"), -ends_with("_98")) |> 
  names()

#last line extracts just the column names and discards the data

impact_vars
#check

# Responders row (n() - count of 99-selectors)
responders_row <- tl4_analysis |> 
  filter(active == 1) |> 
  group_by(leader) |> 
  summarise(n = n() - sum(impact_areas_99 == 1, na.rm = TRUE)) |> 
  pivot_wider(names_from = leader, values_from = n) |> 
  mutate(variable = "n_responders") |> 
  select(variable, everything())

# Counts per variable per leader
counts_by_leader <- tl4_analysis |> 
  filter(active == 1) |> 
  group_by(leader) |> 
  summarise(across(all_of(impact_vars), \(x) sum(x == 1, na.rm = TRUE))) |> 
  pivot_longer(-leader, names_to = "variable", values_to = "count") |> 
  pivot_wider(names_from = leader, values_from = count)

# Combine
impact_areas <- bind_rows(responders_row, counts_by_leader)
impact_areas <- impact_areas |> left_join(codebook, by = "variable" )


#EXPORT TO EXCEL ------------------------------------
write_xlsx(
  list(
    "Active Summary"    = active_summary,
    "Impact Score"      = impact_score,
    "Impact Areas"      = impact_areas
  ),
  "output/tl4_tables.xlsx"
)


