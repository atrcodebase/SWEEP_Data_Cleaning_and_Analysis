# Organization:   ATR
# Date:           02/04/2022
# Script:         SWEEP Data Cleaning and Analysis
# Author:         ATR Data Management Department
# Script Status:  Completed

# required packages -------------------------------------------------------

#### Check if All packages are installed
packages <- c("plyr", "tidyverse", "patchwork", "googlesheets4", "readxl", "writexl", "glue")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#### Load Packages
library(plyr)
library(tidyverse)
library(patchwork)
library(googlesheets4)
library(readxl)
library(writexl)
library(glue)

# Load analysis Functions
source("R/functions/Analysis_double_disagg.R")

##### global variables --------------------------------------------
week <- "week1"
date <- Sys.Date()
data_path <- glue::glue("input/raw_data/SWEEP_CBSG _Monitoring_Actual_Form_WIDE_{week}.xlsx")
backcheck_path <- glue::glue("input/raw_data/SWEEP Callback-Backchecks_WIDE_{week}.xlsx")
output_path_data <- glue::glue("output/{week}/cleaned_dt/")
output_path_graphs <- glue::glue("output/{week}/graphs/")
output_path_tables <- glue::glue("output/{week}/tables/")
output_path_backcheck <- glue::glue("output/{week}/backcheck/")

##### cleaning log -------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/1Bklm01Ld2sTQZ50FhAukEJbuH07WIfU-6eaqTLY8_rU/edit?usp=sharing"
gs4_deauth()
correction_log <- googlesheets4::read_sheet(url)
correction_log %>% count(changed)
correction_log <- correction_log %>% filter(changed == "Yes")


###### 1 - Data processing -----------------------------------------
source("R/1- data cleaning.R")

##### 2 - Plots Generated in output/graphs folder ------------------
theme_set(theme_bw())
source("R/2- plots.R")

##### 3 - analysis and other required tables -----------------------
source("R/3- tables.R")

##### 4 - callback & backcheck -------------------------------------
source("R/4- backcheck.R") # preparing data
source("R/5- backcheck tables.R") # anlaysis and comparison
source("R/6 - backcheck plots.R") # data visualization

#### export tables -------------------------------------------------

write_xlsx(df, glue::glue("{output_path_data}sweep_cleaned_data_{date}.xlsx")) # cleaned data (all cases)
write_xlsx(df_complete_approved, glue::glue("{output_path_data}sweep_cleaned_data_completed & approved cases_{date}.xlsx")) # cleaned data (compeleted & approved cases)
write_xlsx(analysis, glue::glue("{output_path_tables}analysis.xlsx")) # analysis
write_xlsx(decison_making, glue::glue("{output_path_tables}decison_making.xlsx")) # decision making score
write_xlsx(relocated, glue::glue("{output_path_tables}respondents_who_moved_from_their_location.xlsx")) # respondents who have been relocated
write_xlsx(attempts_vs_complete_interviews, glue::glue("{output_path_tables}attempt_vs_complete_interviews.xlsx")) # number of attempts vs. completed surveys
write_xlsx(backcheck_complete, glue::glue("{output_path_backcheck}sweep_cleaned_backcheck_and_callback_data_completed cases_{date}.xlsx")) # backcheck data (completed cases)
write_xlsx(backcheck_analysis, glue::glue("{output_path_backcheck}backcheck_analysis.xlsx")) # backcheck analysis
write_xlsx(comparison_long, glue::glue("{output_path_backcheck}comparison_long.xlsx")) # backcheck comparison (long format)
write_xlsx(comparison_wide, glue::glue("{output_path_backcheck}comparison_wide.xlsx")) # backcheck comparison (wide format)


