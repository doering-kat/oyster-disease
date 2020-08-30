# Header -----------------------------------------------------------------------
# 
# Look at the Salinity/Temperature Window - does averaging differently change
# The values by a lot? Use paired t tests to see if the difference among groups is
# significantly different from 0. 
# 
# Written 29 Nov 2018 by Kathryn Doering 
# 
# Load packages and set options ------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)

# Read in data -----------------------------------------------------------------
# Salinity
sal_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv") # cruise date lookup for salinity.
# salinity (with cruises with too few obs removed.)
sal_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_SAL_subset_2018_10_10.csv")

#Wtemp
#cruise date lookup for water temperature
wtemp_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/cruise_date_lookup.csv")
#wtemp (with cruises with too few obs removed.)
wtemp_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_WTEMP_subset_2018_10_10.csv")
# change script here -----------------------------------------------------------
# These are the min and max months for each of the filtering variables. 

# # summer temperature min and max months
min_max_mo_summer_temp <- c(4,10)

#winter temp min and max months
min_max_mo_win_temp <- c(1,3)
# summer salinity min and max months
min_max_mo_summer_sal <- c(4,10)
# These values were used in the model.

# Test window
# Try a narrower, summer window
test_window_narrow <- c(6,9)
#Try a broader, window
test_window_broad <- c(3,11)

# Bind cruise dates to salinity ------------------------------------------------
#  make min and max dates into dat format (not character)
sal_cruise_date$min_date <- as.Date(sal_cruise_date$min_date, format = "%m/%d/%Y" )
sal_cruise_date$max_date <- as.Date(sal_cruise_date$max_date, format = "%m/%d/%Y")
wtemp_cruise_date$min_date <- as.Date(wtemp_cruise_date$min_date, format = "%m/%d/%Y")
wtemp_cruise_date$max_date <- as.Date(wtemp_cruise_date$max_date, format = "%m/%d/%Y")

sal_dat <- left_join(sal_pred,  sal_cruise_date, by = "cruise")%>% 
              mutate(min_month = lubridate::month(min_date))

temp_dat <- left_join(wtemp_pred, wtemp_cruise_date, by = "cruise") %>% 
                mutate(min_month = lubridate::month(min_date))

sal_dat <- sal_dat %>% 
    mutate(PredVal = ifelse(PredVal < 0, NA, PredVal)) %>% 
    mutate(PredVal = ifelse(PredVal > 35, NA, PredVal)) # get rid of one really high observation. (maybe need to deal with it some other way?)

# Examine salinity -------------------------------------------------------------
# salinity Summarize salinity data by month
sal_avg_month <- sal_dat %>% 
    na.omit() %>% 
    select(ID, year, min_month, PredVal) %>% 
    group_by(ID, year, min_month) %>% 
    summarize(Month_avg = mean(PredVal)) %>% 
    ungroup() #remove groups
#Now, average over the "summer" months

sal_avg_summer <- sal_avg_month %>% 
    filter(min_month >= min_max_mo_summer_sal[1]) %>% 
    filter(min_month <= min_max_mo_summer_sal[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(Summer_avg = mean(Month_avg)) # average for each month.

sal_avg_narrow <- sal_avg_month %>% 
    filter(min_month >= test_window_narrow[1]) %>% 
    filter(min_month <= test_window_narrow[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(narrow_avg = mean(Month_avg)) # average for each month.

sal_avg_broad <- sal_avg_month %>% 
    filter(min_month >= test_window_broad[1]) %>% 
    filter(min_month <= test_window_broad[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(broad_avg = mean(Month_avg)) # average for each month.

#bind together the observations.
windows <- full_join(sal_avg_summer, sal_avg_narrow, by = c("ID", "year")) %>% 
            full_join(sal_avg_broad, b = c("ID", "year")) %>% 
             na.omit()
# look at histogram:
hist(windows$Summer_avg)
hist(windows$narrow_avg)
hist(windows$broad_avg)

# t test indicates the differences between these values are not different from
# 0 
t.test(windows$Summer_avg, windows$narrow_avg)
t.test(windows$Summer_avg, windows$broad_avg)
t.test(windows$broad_avg, windows$narrow_avg)

#  Examine for temperature -----------------------------------------------------


temp_avg_month <- temp_dat %>% 
    na.omit() %>% 
    select(ID, year, min_month, PredVal) %>% 
    group_by(ID, year, min_month) %>% 
    summarize(Month_avg = mean(PredVal)) %>% 
    ungroup() #remove groups
#Now, average over the "summer" months
#
temp_avg_summer <- temp_avg_month %>% 
    filter(min_month >= min_max_mo_summer_sal[1]) %>% 
    filter(min_month <= min_max_mo_summer_sal[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(Summer_avg = mean(Month_avg)) # average for each month.

temp_avg_narrow <- temp_avg_month %>% 
    filter(min_month >= test_window_narrow[1]) %>% 
    filter(min_month <= test_window_narrow[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(narrow_avg = mean(Month_avg)) # average for each month.

temp_avg_broad <- temp_avg_month %>% 
    filter(min_month >= test_window_broad[1]) %>% 
    filter(min_month <= test_window_broad[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(broad_avg = mean(Month_avg)) # average for each month.

#bind together the observations.
temp_windows <- full_join(temp_avg_summer, temp_avg_narrow, by = c("ID", "year")) %>% 
    full_join(temp_avg_broad, b = c("ID", "year")) %>% 
    na.omit()
# look at histogram:
hist(temp_windows$Summer_avg)
hist(temp_windows$narrow_avg)
hist(temp_windows$broad_avg)

# #standardize
# means <- temp_windows %>% 
#             ungroup() %>% 
#             select(Summer_avg, narrow_avg, broad_avg) %>% 
#             summarize_all(funs(mean))
# 
# sds <- temp_windows %>%             
#             ungroup() %>% 
#             select(Summer_avg, narrow_avg, broad_avg) %>% 
#             summarize_all(funs(sd))


cor(temp_windows$Summer_avg, temp_windows$narrow_avg)
cor.test(temp_windows$Summer_avg, temp_windows$narrow_avg)

cor(temp_windows$Summer_avg, temp_windows$broad_avg)
cor.test(temp_windows$Summer_avg, temp_windows$broad_avg)
