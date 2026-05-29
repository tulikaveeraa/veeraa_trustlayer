# =============================================================================
# Title:    TL4 Pre-Analysis
# Author:   Tulika Jain
# Created:  2026-05-27
# Purpose:  Transforming Data to Prep for Analysis 
# Modified: 2026-05-27
# =============================================================================

#LOAD DATASET ------------------------------------
library(tidyverse)
library(janitor)

tl4_corrected <- read_rds("data/tl4_corrected.rds")
codebook <- read_csv("data/codebook_tl4.csv")

#GENERATE NEW VARIABLES W/O 98 AND 99s--------------------

# Analysis dataset: 98/99 → NA, for stats and models  
tl4_preanalysis <- tl4_corrected |> 
  mutate(across(c(depth1, depth2, depth3, love1, love2, educ_level, time_since_contact, frequency_engagement), 
                \(x) if_else(x %in% c(98,99), NA_real_, x),
                .names = "{.col}_num"
                )
         )

tl4_preanalysis |> count(love1_num)

#CONVERTING CATEGORICALS INTO FACTORS ------------------------

tl4_preanalysis <-tl4_preanalysis |> 
  mutate(
    frequency_engagement = factor(frequency_engagement,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99),
                                  labels = c("Daily",
                                             "More than once a week, but not everyday",
                                             "Once a week",
                                             "Once a month",
                                             "2 - 3 times a month",
                                             "Once every 3 months or so",
                                             "A few times a year (more than 4 times)",
                                             "Once a year",
                                             "Ad hoc (I go when I need help/ I come when they call me)",
                                             "Don’t know/ don’t remember",
                                             "Survey left incomplete")
                                  ),
    depth1 =              factor(depth1,
                                 levels = c(1, 2, 3, 4, 5, 98, 99),
                                 labels = c("Made worse",
                                            "No impact",
                                            "Some positive impact",
                                            "Significantly improved",
                                            "Saved life", 
                                            "Don't know/Don't remember",
                                            "Survey left incomplete")
                                ),
    depth2 =              factor(depth2,
                                 levels = c(1, 2, 3, 4, 5, 98, 99),
                                 labels = c("Very bad", 
                                            "Somewhat bad",
                                            "Not good not bad", 
                                            "Good",
                                            "Very good",
                                            "Don't know/Don't remember",
                                            "Survey left incomplete")
                                ),
    depth3 =              factor(depth3,
                                 levels = c(1, 2, 3, 4, 5, 98, 99),
                                 labels = c("Very bad", 
                                            "Somewhat bad",
                                            "Not good not bad", 
                                            "Good",
                                            "Very good",
                                            "Don't know/Don't remember",
                                            "Survey left incomplete")
                                ),
    love1 =              factor(love1,
                                 levels = c(1, 2, 3, 4, 5, 98, 99),
                                 labels = c("Definitely not", 
                                            "Probably not",
                                            "Not sure (neutral)", 
                                            "Probably yes",
                                            "Definitely",
                                            "Don't know/Don't remember",
                                            "Survey left incomplete")
                                ),
    love2 =              factor(love2,
                                levels = c(1, 2, 3, 4, 5, 98, 99),
                                labels = c("Not at all", 
                                           "Not very much",
                                           "Somewhat", 
                                           "Mostly yes",
                                           "Yes completely",
                                           "Don't know/Don't remember",
                                           "Survey left incomplete")
                                  ),
    
    educ_level =              factor(educ_level,
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 98, 99),
                                labels = c("No schooling",
                                           "Between Class 1 - 8",
                                           "Completed class 8",
                                           "Completed Class 9",
                                           "Completed Class 10",
                                           "Completed Class 11",
                                           "Completed Class 12",
                                           "Completed Diploma",
                                           "Completed Bachelors degree",
                                           "Completed Masters degree",
                                           "Don't know/Don't remember",
                                           "Survey left incomplete")
                                  ),
    time_since_contact =             factor(time_since_contact,
                                     levels = c(1, 2, 3, 4, 5, 6, 98, 99),
                                     labels = c("Less than 1 month ago",
                                                "More than 1 month, but less than 3 months",
                                                "More than 3 months but less than 6 months",
                                                "More than 6 months but less than 12 months",
                                                "More than 12 months but less than 24 months",
                                                "More than 2 years ago",
                                                "Don't know/Don't remember",
                                                "Survey left incomplete")
                                  )
    

                                  
      )


#GENERATE LEADER VARIABLE ------------------
tl4_preanalysis  <- tl4_preanalysis |> 
  mutate(leader = str_sub(unique_id, 1, 4))

tl4_preanalysis |> 
  count(leader_name, leader)

#SAVE----
write_rds(tl4_preanalysis, "data/tl4_analysis.rds")



