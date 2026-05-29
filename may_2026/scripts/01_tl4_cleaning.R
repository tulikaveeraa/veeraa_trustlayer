# =============================================================================
# Title:    TL4 Data Cleaning 
# Author:   Tulika Jain 
# Created:  2026-05-25
# Purpose:  Clean the TL4 pilot and main survey data, merge 
# Modified: 2026-05-26
# =============================================================================

# sonakshi test

# Setup -------------------------------------------------------------------
library(tidyverse)
#install.packages("janitor")
library(janitor)

# Load --------------------------------------------------------------------
tl4_main <- read_csv2("data/tl4_main.csv") |> clean_names()
tl4_pilot <- read_csv("data/tl4_pilot.csv") |> clean_names()
#Removing all backslashes from variable names

#INSTRUCTIONS: DOWNLOAD PILOT AND RAW DATA FROM GOOGLE DRIVE AND STORE ON PERSONAL DEVICE. 
#THIS HAS PII, SO PLEASE DELETE FROM YOUR DEVICE AFTER YOU RUN THIS CODE. 
#THE RAW DATA SHOULD ONLY EXIST ON DRIVE 

# Inspect -----------------------------------------------------------------
glimpse(tl4_main)
glimpse(tl4_pilot)
#This will show us all variable names

# Recoding Pilot Options -------------------------------------------------

#AWARENESS
tl4pilotclean <- tl4_pilot |> 
  mutate(iaware1 = impact_aware_1,
         iaware2 = impact_aware_3,
         iaware3 = impact_aware_5,
         iaware4 = impact_aware_2,
         iaware5 = impact_aware_7,
         iaware6 = impact_aware_8,
         iaware7 = impact_aware_10,
         iaware8 = impact_aware_11,
         iaware9 = impact_aware_12,
         iaware10 = impact_aware_13,
         iaware11 = impact_aware_14,
         iaware7 = if_else(impact_aware_4 == 1, 1, iaware7),
         iaware4 = if_else(impact_aware_6 == 1, 1, iaware4)
         )

#EDUCATION
tl4pilotclean <- tl4pilotclean |> 
  mutate(ieduc1 = impact_educ_1,
         ieduc1 = if_else(impact_educ_2 == 1, 1, ieduc1),
         ieduc2 = impact_educ_4,
         ieduc2 = if_else(impact_educ_6 == 1, 1, ieduc2),
         ieduc2 = if_else(impact_educ_7 == 1, 1, ieduc2),
         ieduc3 = impact_educ_5,
         ieduc4 = impact_educ_3,
         ieduc5 = impact_educ_8,
         ieduc5 = if_else(impact_aware_9 == 1, 1, ieduc5),
         ieduc6 = impact_educ_9,
         ieduc7 = impact_educ_10,
         ieduc7 = if_else(impact_aware_15 == 1, 1, ieduc7),
         ieduc8 = impact_educ_11,
         ieduc9 = NA 
         )

#HEALTH
tl4pilotclean <- tl4pilotclean |> 
  mutate(impact_health_5 = impact_health_7) |> 
  select(-c(impact_health_12, impact_health_13, impact_health_14, impact_health_15))
#Only recoding one option as this is the only response we received during the pilot 

#INCOME
tl4pilotclean <- tl4pilotclean |> 
  mutate(iincome1 = impact_income_1,
         iincome2 = impact_income_2,
         iincome3 = impact_income_3,
         iincome4 = impact_income_4,
         iincome5 = impact_income_6,
         iincome6 = impact_income_7,
         iincome7 = impact_income_8,
         iincome8 = impact_income_9,
         iincome9 = impact_income_11,
         iincome10 = impact_income_12,
         iincome11 = impact_income_13,
         iincome4 = if_else(impact_income_14 == 1, 1, iincome4) #because only 1 response had option14 and for that case specifically it should be recoded to 4
  )

#FINANCE
tl4pilotclean <- tl4pilotclean |> 
  select(-impact_finance_7)
#No responses from pilot 

#HARM
tl4pilotclean <- tl4pilotclean |> 
  mutate(iharm1 = impact_harm_1, 
         iharm2 = impact_harm_2, 
         iharm3 = impact_harm_3, 
         iharm4 = impact_harm_4, 
         iharm5 = impact_harm_5, 
         iharm6 = impact_harm_6,
         iharm7 = impact_harm_7, 
         iharm8 = impact_harm_8, 
         iharm10 = impact_harm_9, 
         iharm11 = impact_harm_10,
         iharm12 = impact_harm_12, 
         iharm12 = if_else(impact_harm_11 == 1, 1, iharm12),
         iharm13 = impact_harm_13,
         iharm14 = NA,
         iharm9 = NA
  )
#Assuming FIR was for a minor offence

#CONFIDENCE
tl4pilotclean <- tl4pilotclean |> 
  mutate(impact_confidence_8 = impact_confidence_10) |>
  select(-c(impact_confidence_9, impact_confidence_10))
#Only recoding one option as everything else is the same 


#GOVT SCHEMES
tl4pilotclean <- tl4pilotclean |> 
  mutate(igovt1 = impact_govt_access_1,
         igovt2 = NA,
         igovt3 = NA,
         igovt4 = impact_govt_access_3,
         igovt5 = NA,
         igovt6 = impact_govt_access_8, 
         igovt7 = impact_govt_access_9, 
         igovt8 = impact_govt_access_10,
  )


#FINANCE
tl4pilotclean <- tl4pilotclean |> 
  select(-impact_belong_7)
#No responses from pilot 

#ENVIRONMENT
tl4pilotclean <- tl4pilotclean |> 
  mutate(impact_envir_10 = NA)
#Adding new variable


#Deleting and renaming variables --------------
tl4pilotclean <- tl4pilotclean |> 
  select(-c(impact_aware_1:impact_aware_15)) |>
  rename_with(
    ~str_replace(.x, "^iaware", "impact_aware_"),
    starts_with("iaware")
  ) 

tl4pilotclean <- tl4pilotclean |> 
  select(-c(impact_educ_1:impact_educ_11)) |>
  rename_with(
    ~str_replace(.x, "^ieduc", "impact_educ_"),
    starts_with("ieduc")
  ) 

tl4pilotclean <- tl4pilotclean |> 
  select(-c(impact_income_1:impact_income_14)) |>
  rename_with(
    ~str_replace(.x, "^iincome", "impact_income_"),
    starts_with("iincome")
  ) 


tl4pilotclean <- tl4pilotclean |> 
  select(-c(impact_harm_1:impact_harm_13)) |>
  rename_with(
    ~str_replace(.x, "^iharm", "impact_harm_"),
    starts_with("iharm")
  ) 

tl4pilotclean <- tl4pilotclean |> 
  select(-c(impact_govt_access_1:impact_govt_access_11)) |>
  rename_with(
    ~str_replace(.x, "^igovt", "impact_govt_access_"),
    starts_with("igovt")
  ) 

#Remove Depth1 (as question has changed entirely) + Delete irrelevant variables

tl4pilotclean <- tl4pilotclean |>  
  mutate(depth1 = NA) |> 
  select(-c(literacy_read, literacy_write))


#Do both files have the same variable names?
setdiff(names(tl4pilotclean), names(tl4_main)) #in pilot but not main
setdiff(names(tl4_main), names(tl4pilotclean)) #in main but not pilot

tl4pilotclean <- tl4pilotclean |> 
  select(-c(village_locality, show_respondentname, show_org, show_contactname, 
            show_vlname, show_phoneno, show_activityname, show_urban_rural, 
            explain_dontknow, confirm_village, village_name, 
            family_other, child_age, both_child_parent, give_phone2, only_child, 
            speak_to_fam, share_relative_no, phonenumber_family, contactinfo1, contactinfo2, contactinfo3, 
            x327))


#Removing all PII-----------------------------------

tl4_main <- tl4_main |> 
  rm(id_check, community_member, phone_number, nameperson1:numberperson3, phonenumber)


tl4_pilotclean <- tl4_pilotclean |> 
  rm(id_check, community_member, phone_number, nameperson1:numberperson3, phonenumber)


#MERGING------------------------------------------------

combined <- bind_rows(
  main = tl4_main,
  pilot = tl4pilotclean,
  .id = "source"
)
#this assigns an ID which marks the data as "main" or "pilot" in the source variable


#Quality checks:
nrow(tl4_main) + nrow(tl4pilotclean)   
nrow(combined)                          

glimpse(combined)                      

combined |> count(source)    

combined <- combined |> rename (impact_confidenceyn = impact_confidence_yn)

# Save file: 
write_rds(combined, "data/tl4_combined_clean.rds")


