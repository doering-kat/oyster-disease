# Header -----------------------------------------------------------------------
#
# Summarize Kriging output into "summer" salinity, "summer" temperature, and "winter"
# Temperature so that there is one value at each bar for each year

# This version uses Kriging output that has been filtered to eliminate cruises
# where there was not good sample coverage (at least 5 sites sampled each in the
# mainstemp, eastern, and western shore.)
# packages and options ---------------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# change script here -----------------------------------------------------------
# These are the min and max months for each of the filtering variables. 

# # summer temperature min and max months
min_max_mo_summer_temp <- c(4,10)

#winter temp min and max months
min_max_mo_win_temp <- c(1,3)
# summer salinity min and max months
min_max_mo_summer_sal <- c(4,10)

# read in data -----------------------------------------------------------------
# Salinity
sal_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv") # cruise date lookup for salinity.
# salinity (with cruises with too few obs removed.)
#TODO: this is the data set to use in the window change.
sal_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_SAL_subset_2018_10_10.csv")

#Wtemp
#cruise date lookup for water temperature
wtemp_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/cruise_date_lookup.csv")
#wtemp (with cruises with too few obs removed.)
wtemp_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_WTEMP_subset_2018_10_10.csv")

# Manipulate data -------------------------- -----------------------------------
#  make min and max dates into dat format (not character)
sal_cruise_date$min_date <- as.Date(sal_cruise_date$min_date, format = "%m/%d/%Y" )
sal_cruise_date$max_date <- as.Date(sal_cruise_date$max_date, format = "%m/%d/%Y")
wtemp_cruise_date$min_date <- as.Date(wtemp_cruise_date$min_date, format = "%m/%d/%Y")
wtemp_cruise_date$max_date <- as.Date(wtemp_cruise_date$max_date, format = "%m/%d/%Y")
# Bind datasets
sal_pred <- left_join(sal_pred, sal_cruise_date, by = "cruise")
wtemp_pred <- left_join(wtemp_pred, wtemp_cruise_date, by = "cruise")
#get month for each dataset by using th earliest sampled date.
sal_pred <- mutate(sal_pred, min_month = lubridate::month(min_date))
wtemp_pred <- mutate(wtemp_pred, min_month = lubridate::month(min_date))
#check if min and max date ever give different months (yes)
#any(lubridate::month(sal_pred$min_date) == lubridate::month(sal_pred$max_date))

# Change neg sal obs to 0-------------------------------------------------------
# Previous examination of the data set showed only a few negatives that were only 
# slightly below zero.
# 
# List of cruises with negative observations of salinity.
# Examine these and decide what to do with it - for now, exclude these cruises.
# sal_pred %>% 
#     filter(PredVal <0 | PredVal > 35)

sal_pred <- sal_pred %>% 
            mutate(PredVal = ifelse(PredVal < 0, NA, PredVal)) %>% 
            mutate(PredVal = ifelse(PredVal > 35, NA, PredVal)) # get rid of one really high observation. (maybe need to deal with it some other way?)

# Summarize data ---------------------------------------------------------------

# for now, do June - Oct Salinity and Temperature avgs (avg monthly, then avg for
# all months), and jan - April Temperature ("winter")

# salinity
sal_avg_month <- sal_pred %>% 
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
#plots
## make date for saving plots
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_",
                substr(date, start = 6, stop = 7), "_",
                substr(date, start = 9, stop = 10))

fig_folder_name <- "./Figures/SEM"
fig_folder_name_code <- paste0(fig_folder_name,"/3_Summarize_Filtered_Kriging_Output")
dir.create(fig_folder_name)
dir.create(fig_folder_name_code)

ggplot(sal_avg_summer, aes(x = year, y = Summer_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()
ggsave(paste0(fig_folder_name_code, "/", "sal_avg_months_",min_max_mo_summer_sal[1], "_to_", min_max_mo_summer_sal[2],"_", date,".png"), device = "png", width = 13, height = 8.5, units = "in")

# summarize temperature --------------------------------------------------------
# Will ultimately not use temperature, beyond just for model development, so will
# not scrutinize as much (also, a neg value for temperature does have meaning)

#get monthly avg temp
wtemp_avg_month <- wtemp_pred %>% 
    na.omit() %>% 
    select(ID, year, min_month, PredVal) %>% 
    group_by(ID, year, min_month)  %>% 
    summarize(Month_avg = mean(PredVal)) %>% 
    ungroup()
#now take "summer" average. 
wtemp_avg_summer <- wtemp_avg_month %>% 
    filter(min_month >= min_max_mo_summer_temp[1]) %>% 
    filter(min_month <= min_max_mo_summer_temp[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(Summer_avg = mean(Month_avg))
#take "winter" average (do from jan - april for simplicity now.)
wtemp_avg_winter <- wtemp_avg_month %>% 
    filter(min_month >= min_max_mo_win_temp[1]) %>% 
    filter(min_month <= min_max_mo_win_temp[2]) %>% 
    filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
    group_by(ID, year) %>% 
    summarize(Winter_avg = mean(Month_avg))
# plots


ggplot(wtemp_avg_summer, aes(x = year, y = Summer_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()+
    ggtitle(paste0("Avg Water Temp, Months ", min_max_mo_summer_temp[1],"-", min_max_mo_summer_temp[2]))
ggsave(paste0(fig_folder_name_code, "/", "wtemp_avg_months_", min_max_mo_summer_temp[1],"_to_", min_max_mo_summer_temp[2],"_", date, ".png"), device = "png", width = 13, height = 8.5, units = "in")

ggplot(wtemp_avg_winter, aes(x = year, y = Winter_avg)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~ID) +
    theme_classic()+
    ggtitle(paste0("Avg Water Temp, Months",min_max_mo_win_temp[1], "-", min_max_mo_win_temp[2]))
    ggsave(paste0(fig_folder_name_code, "/", "wtemp_avg_months_", min_max_mo_win_temp[1], "_to_", min_max_mo_win_temp[2],"_", date,".png"), device = "png", width = 13, height = 8.5, units = "in")

# Save summarized data ---------------------------------------------------------
#create directory
folder_name <- "./Derived_Data/SEM"
folder_name_code <- paste0(folder_name, "/3_Summarize_Filtered_Kriging_Output")
dir.create(folder_name)
dir.create(folder_name_code)

write.csv(sal_avg_summer, paste0(folder_name_code, "/sal_avg_months_", min_max_mo_summer_sal[1], "_to_", min_max_mo_summer_sal[2],"_", date, ".csv"), row.names = F)

write.csv(wtemp_avg_summer, paste0(folder_name_code, "/wtemp_avg_months_", min_max_mo_summer_temp[1], "_to_", min_max_mo_summer_temp[2],"_", date, ".csv"), row.names = F)

write.csv(wtemp_avg_winter, paste0(folder_name_code, "/wtemp_avg_months_", min_max_mo_win_temp[1], "_to_", min_max_mo_win_temp[2],"_",date, ".csv"), row.names = F)

# calc and save number of samples -------------------------------------

#number of samples used to calculate the avg salinity for "summer".
n_sal_avg_summer <- sal_pred %>% 
                        na.omit() %>% 
                        filter(min_month >= min_max_mo_summer_sal[1]) %>% 
                        filter(min_month <= min_max_mo_summer_sal[2]) %>% 
                        select(ID, year, PredVal) %>% 
                        group_by(ID, year) %>% 
                        count()
 # number of samples used to calculate the avg temp for "summer"
 n_wtemp_avg_summer <- wtemp_pred %>% 
                         na.omit() %>% 
                         filter(min_month >= min_max_mo_summer_temp[1]) %>% 
                         filter(min_month <= min_max_mo_summer_temp[2]) %>% 
                         select(ID, year, PredVal) %>% 
                         group_by(ID, year) %>% 
                         count()
 # number of samples used to calculate the avg temp for "winter"
 n_wtemp_avg_winter <- wtemp_pred %>% 
                         na.omit() %>% 
                         filter(min_month <= min_max_mo_win_temp[1]) %>% 
                         filter(min_month <= min_max_mo_win_temp[2]) %>% 
                         select(ID, year, PredVal) %>% 
                         group_by(ID, year) %>% 
                         count()

 # write to .csv.

 write.csv(n_sal_avg_summer, paste0(folder_name_code, "/n_sal_avg_months_",min_max_mo_summer_sal[1], "_to_", min_max_mo_summer_sal[2],"_", date, ".csv"), row.names = F)
 
 write.csv(n_wtemp_avg_summer, paste0(folder_name_code, "/n_wtemp_avg_months_", min_max_mo_summer_temp[1], "_to_", min_max_mo_summer_temp[2],"_", date, ".csv"), row.names = F)
 
 write.csv(n_wtemp_avg_winter, paste0(folder_name_code, "/n_wtemp_avg_months_",min_max_mo_win_temp[1], "_to_", min_max_mo_win_temp[2],"_", date, ".csv"), row.names = F)
                    