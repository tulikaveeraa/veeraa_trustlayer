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


#DEMOGRAPHICS----------------------------------------------------------------

demographics <- tl4_analysis2 |> 
  filter(active == 1) |> 
  
  
