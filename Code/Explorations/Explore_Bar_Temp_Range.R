# Header -----------------------------------------------------------------------
# Find the max and minimum temperatures from kriging 
# (look at raw, and maybe avg by month?)

# Written 14 Jan 2019 by Kathryn Doering
# Load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# Load data ------------------------------------------------------
#Watertemp
temp_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/cruise_date_lookup.csv")
temp_pred_all <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/Predicted_S_WTEMP_all.csv")
# Manipulate data -------------------------- -----------------------------------
#  make min and max dates into dat format (not character)
temp_cruise_date$min_date <- as.Date(temp_cruise_date$min_date, format = "%m/%d/%Y")
temp_cruise_date$max_date <- as.Date(temp_cruise_date$max_date, format = "%m/%d/%Y")
# Bind datasets
temp_pred_all <- left_join(temp_pred_all, temp_cruise_date, by = "cruise")
#get month for each dataset by using th earliest sampled date.
temp_pred_all <- mutate(temp_pred_all, min_month = lubridate::month(min_date))
#check if min and max date ever give different months (yes)
#any(lubridate::month(sal_pred_all$min_date) == lubridate::month(sal_pred_all$max_date))
# remove any observations before 1991
temp_pred_all <- filter(temp_pred_all, year >= 1991 & year <= 2017)
# summarize raw data -----------------------------------------------------------
# to get min and max temp observed that oysters experienced. 
summary(temp_pred_all$PredVal)
min(temp_pred_all$PredVal, na.rm = T)
max(temp_pred_all$PredVal, na.rm = T)
