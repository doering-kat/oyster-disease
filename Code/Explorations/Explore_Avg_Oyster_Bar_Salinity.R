# Header -----------------------------------------------------------------------
# Explore salinity at oyster bars
# 
# This is in order to calculate an average salinity across oyster bars 
# Load packages, set options ---------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)

# Load data --------------------------------------------------------------------
# salinity at Maryland oyster bars
sal_dat <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_SAL_subset_2018_10_10.csv")
# Salinity
sal_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv")

# Manipulate data -------------------------- -----------------------------------
#  make min and max dates into dat format (not character)
sal_cruise_date$min_date <- as.Date(sal_cruise_date$min_date, format = "%m/%d/%Y" )
sal_cruise_date$max_date <- as.Date(sal_cruise_date$max_date, format = "%m/%d/%Y")

# Bind datasets and calculate the month for each cruise (put in new col)
sal_dat <- left_join(sal_dat, sal_cruise_date, by = "cruise") %>% 
                   mutate(min_month = lubridate::month(min_date))

# filter so only using data between 1991 and 2017
sal_dat <- filter(sal_dat, year >= 1991 & year <= 2017)
summary(sal_dat$year)

# Avg Sal by bar ---------------------------------------------------------------

# get rid of obs below 0 and above 32
sal_dat <- sal_dat %>% 
             mutate(PredVal = ifelse(PredVal < 0, NA, ifelse(PredVal >32, NA, PredVal)))


summary(sal_dat$PredVal) # some predictions are clearly wrong (note: summary stats do not change except for
# min and max when excluding the neg and higher then 32 values.)

# calculate monthly salinity
avg_sal_month <- sal_dat %>% 
                    filter(!is.na(PredVal)) %>% 
                    group_by(ID, year, min_month ) %>% 
                    summarize(mean_sal_mo = mean(PredVal)) %>% 
                    ungroup()

# now, average by bar 
avg_sal_bar <- avg_sal_month %>% 
                 group_by(ID) %>% 
                 summarize(mean_sal_bar = mean(mean_sal_mo))

# Report the min and max here (approximate) in the study site description of 
# Ch. 2 of thesis to quantify the salinity gradient in Chesapeake Bay.
summary(avg_sal_bar$mean_sal_bar)
                    

pdf("./Figures/Sal_of_MD_Dis_Bars.pdf")
hist(avg_sal_bar$mean_sal_bar, main = "Average salinity on\ndisease bars in Maryland")
hist(sal_dat$PredVal, main = "Distribution of salinity observations\non disease bars in Maryland")
dev.off()
