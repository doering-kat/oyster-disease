# Summarize Kriging output into summer salinity, summer temperatuer, and winter
# Temperature so that there is one value at each bar for each year

# This version just eliminates all cruises that had negative observations.
# Created in September.
# packages and options ---------------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# read in data -----------------------------------------------------------------
# Salinity
sal_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv")
sal_pred_all <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/Predicted_S_SAL_all.csv")
#Wtemp
wtemp_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/cruise_date_lookup.csv")
wtemp_pred_all <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/Predicted_S_WTEMP_all.csv")

# Manipulate data -------------------------- -----------------------------------
#  make min and max dates into dat format (not character)
sal_cruise_date$min_date <- as.Date(sal_cruise_date$min_date, format = "%m/%d/%Y" )
sal_cruise_date$max_date <- as.Date(sal_cruise_date$max_date, format = "%m/%d/%Y")
wtemp_cruise_date$min_date <- as.Date(wtemp_cruise_date$min_date, format = "%m/%d/%Y")
wtemp_cruise_date$max_date <- as.Date(wtemp_cruise_date$max_date, format = "%m/%d/%Y")
# Bind datasets
sal_pred_all <- left_join(sal_pred_all, sal_cruise_date, by = "cruise")
wtemp_pred_all <- left_join(wtemp_pred_all, wtemp_cruise_date, by = "cruise")
#get month for each dataset by using th earliest sampled date.
sal_pred_all <- mutate(sal_pred_all, min_month = lubridate::month(min_date))
wtemp_pred_all <- mutate(wtemp_pred_all, min_month = lubridate::month(min_date))
#check if min and max date ever give different months (yes)
#any(lubridate::month(sal_pred_all$min_date) == lubridate::month(sal_pred_all$max_date))

# Look at neg obs---------------------------------------------------------------
# List of cruises with negative observations of salinity.
# Examine these and decide what to do with it - for now, exclude these cruises.
problem_cruises <- sal_pred_all %>% 
                        filter(PredVal < 0) %>% 
                        select(cruise) %>% 
                        unique()
problem_cruises <- problem_cruises$cruise #make into a vector.

sal_pred_no_neg <- sal_pred_all %>% 
                    filter(!(cruise %in% problem_cruises))

# Summarize data ---------------------------------------------------------------

# for now, do June - Oct Salinity and Temperature avgs (avg monthly, then avg for
# all months), and jan - April Temperature ("winter")

# salinity
sal_avg_month <- sal_pred_no_neg %>% 
                    na.omit() %>% 
                    select(ID, year, min_month, PredVal) %>% 
                    group_by(ID, year, min_month) %>% 
                    summarize(Month_avg = mean(PredVal)) %>% 
                    ungroup() #remove groups
#Now, average over the "summer" june (6) - 10 (oct)

sal_avg_summer <- sal_avg_month %>% 
                    filter(min_month >= 6) %>% 
                    filter(min_month <= 10) %>% 
                    group_by(ID, year) %>% 
                    summarize(Summer_avg = mean(Month_avg)) # average for each month.
#plots
dir.create("./Figures/Summarize_Kriging_Output")
ggplot(sal_avg_summer, aes(x = year, y = Summer_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()
ggsave("./Figures/Summarize_Kriging_Output/sal_avg_summer.png", device = "png", width = 13, height = 8.5, units = "in")

# summarize temperature --------------------------------------------------------
# Will ultimately not use temperature, beyond just for model development, so will
# not scrutinize as much (also, a neg value for temperature does have meaning)

#get monthly avg temp
wtemp_avg_month <- wtemp_pred_all %>% 
                     na.omit() %>% 
                     select(ID, year, min_month, PredVal) %>% 
                     group_by(ID, year, min_month)  %>% 
                     summarize(Month_avg = mean(PredVal)) %>% 
                     ungroup()
#now take "summer" average. 
wtemp_avg_summer <- wtemp_avg_month %>% 
                        filter(min_month >= 6) %>% 
                        filter(min_month <= 10) %>% 
                        group_by(ID, year) %>% 
                        summarize(Summer_avg = mean(Month_avg))
#take "winter" average (do from jan - april for simplicity now.)
wtemp_avg_winter <- wtemp_avg_month %>% 
                      filter(min_month <= 4) %>% 
                      group_by(ID, year) %>% 
                      summarize(Winter_avg = mean(Month_avg))

# plots
ggplot(wtemp_avg_summer, aes(x = year, y = Summer_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()+
    ggtitle("Summer Water Temp")
ggsave("./Figures/Summarize_Kriging_Output/wtemp_avg_summer.png", device = "png", width = 13, height = 8.5, units = "in")

ggplot(wtemp_avg_winter, aes(x = year, y = Winter_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()+
    ggtitle("Winter Water Temp")
ggsave("./Figures/Summarize_Kriging_Output/wtemp_avg_winter.png", device = "png", width = 13, height = 8.5, units = "in")
# Summarize by NOAA code -------------------------------------------------------
# Maybe?
                    
# Save summarized data ---------------------------------------------------------
write.csv(sal_avg_summer, "./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/sal_avg_summer.csv", row.names = F)

write.csv(problem_cruises,  "./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/problem_cruises.csv", row.names = F)

write.csv(wtemp_avg_summer, "./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/wtemp_avg_summer.csv", row.names = F)

write.csv(wtemp_avg_winter, "./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/wtemp_avg_winter.csv", row.names = F)

# TODO
# All bars/years have an observation - however, should probably look at which 
# months are missing data also, to make sure all of these avgs are a fair summary
# maybe should not pretend a value exists if it doesn't?
