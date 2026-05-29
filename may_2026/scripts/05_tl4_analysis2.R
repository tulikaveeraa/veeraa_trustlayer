# =============================================================================
# Title:    TL4 Analysis 2 - Detailed
# Author:   Tulika Jain
# Created:  2026-05-29
# Purpose:  Detailed Analysis of Phone Survey Data 
# Modified: 2026-05-29
# =============================================================================

#LOAD DATASET ------------------------------------
library(tidyverse)
library(janitor)
#install.package("writexl)
library(writexl)

tl4_analysis2 <- read_rds("data/tl4_analysis2.rds")
codebook <- read_csv("data/codebook_tl4.csv")


#DEMOGRAPHICS----------------------------------------------------------------

#GENDER & AGE
genderage <-tl4_analysis2 |>
  filter(active == 1) |> 
  group_by(leader) |> 
  summarise(
    female = sum(respondent_gender == "female", na.rm = TRUE),
    male = sum(respondent_gender == "male", na.rm = TRUE), 
    n = sum(respondent_gender %in% c("male", "female", "other"), na.rm = TRUE),
    fem_pct = round(female/n *100, 1),
    male_pct = round(male/n*100, 1),
    avg_age = round(mean(respondent_age, na.rm = TRUE), 1)
  )

#NOTE: We do not have 98 and 99 as options for this question. 
#I have also removed "other" in the table as we do not have any "others", but if we do, ADD IT TO THE TABLE 


#AGE DISTRIBUTION


tl4_analysis2 |> 
  filter(active == 1) |> 
  ggplot(aes(x = respondent_age)) +
  geom_histogram(fill = "lightgreen", color = "white", binwidth = 5, na.rm = TRUE) +
  facet_wrap(~ leader_name) +
  labs(title = "Age distribution by leader", x = "Age", y = "Count")
  
  ggsave(filename = "output/age_byvl.png")


tl4_analysis2 |> 
  filter(active == 1) |> 
  ggplot(aes(x = respondent_age)) +
  geom_histogram(fill = "lightblue", color = "white", binwidth = 8, na.rm = TRUE) +
  labs(title = "Age distribution", x = "Age", y = "Count")

  ggsave(filename = "output/age.png")


tl4_analysis2 |> filter(active ==1, respondent_age > 55) |> count(leader)
#Sanjay has the highest count of respondents over the age of 55 (11). Second is Khairun (7)



#EDUCATION

# Build the tabyl as percentages
educ_table <- tl4_analysis2 |>
  filter(active == 1, educ_level != "Survey left incomplete") |> 
  tabyl(leader, educ_level, show_missing_levels = FALSE) |> 
  adorn_percentages("row") |> 
  adorn_pct_formatting(digits = 1)

# Build the n column separately
n_table <- tl4_analysis2 |>
  filter(active == 1, educ_level != "Survey left incomplete") |> 
  group_by(leader) |> 
  summarise(n = n())

# Join
educ_table |> left_join(n_table, by = "leader")




#VISUALISING EDUCATION LEVEL
educ_long <- tl4_analysis2 |>
  filter(active == 1, educ_level != "Survey left incomplete", educ_level != "Don't know/Don't remember") |> 
  mutate(educ_level = fct_drop(educ_level)) |> 
  group_by(leader_name, educ_level) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(leader_name) |> 
  mutate(pct = n / sum(n) * 100) |> 
  ungroup()

educ_long |> 
  ggplot(aes(x = pct, y = leader_name, fill = educ_level)) +
  geom_col() +
  scale_fill_viridis_d(option = "D", direction = -1) +
  labs(
    title = "Education distribution by leader",
    x = "% of respondents",
    y = NULL,
    fill = "Education level"
  ) +
  theme_minimal()


ggsave(filename = "output/education.png")








#EXPORT TO EXCEL ------------------------------------
write_xlsx(
  list(
    "GenderAge"            = genderage,
    "Education"            = educ_table
  ),
  "output/tl4_analysis.xlsx"
)



