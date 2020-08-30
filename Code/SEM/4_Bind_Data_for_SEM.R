# HEADER -----------------------------------------------------------------------
# TODO: add freshet dataset to the SEM data.
# Bind Datasets for SEM
# 
# (remove extreme freshet events before running?)
# Created Oct 11 2018 by Kathryn Doering 
# Load Packages and set options ------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------
# Read in M values (on the NOAA code level)
M_df <- read.csv("./Data/median_M_inst_df_ordered.csv")

# Read in bar-level prevalences.
dermo_prev_df <-  read.csv("./Derived_Data/Summarize_Disease/dermo_prevalance_ID_year_2018_09_26.csv")
MSX_prev_df <-  read.csv("./Derived_Data/Summarize_Disease/MSX_Prevalance_ID_year_2018_09_26.csv")
# read in bar -level avg intensity and weighted orevalence for dermo.
dermo_intensity_df <- read.csv("./Derived_Data/Summarize_Disease/dermo_mean_intensity_percent_lethal_ID_year_2018_10_12.csv")


# read in bar-level environmental data
date <- "2018_10_17" # date that these derived data were saved (part of file name.)

sal_avg_sum <- read.csv(paste0("./Derived_Data/SEM/3_Summarize_Filtered_Kriging_Output/sal_avg_months_4_to_10_", date, ".csv"))
wtemp_avg_sum <- read.csv(paste0("./Derived_Data/SEM/3_Summarize_Filtered_Kriging_Output/wtemp_avg_months_4_to_10_", date, ".csv"))
wtemp_avg_win <- read.csv(paste0("./Derived_Data/SEM/3_Summarize_Filtered_Kriging_Output/wtemp_avg_months_1_to_3_", date, ".csv"))

# read in spatial categories (Salinity Zones)
# Categorized by MDNR in their 5 yr rpt (plus KD added potomac cats based on cats
# of the adjacent NOAA codes)
# 
NOAA_sal_zones <-  read.csv("./Data/Sal_Zones_Five_Yr_Report_2010_15_Table_4_5.csv")

# read in data on freshet occurences
# need to finish compiling these data.
# (probably will only be used to pull out) - bar or NOAA level?
# 
freshet_NOAA <- read.csv("./Data/Freshets_Fall_Survey_Reports_by_Yr_NOAA.csv")

# 
# #list of disease Bars and their associated NOAA code.
# from mod_all_run_4 (so the bars will be grouped with NOAA codes as they were 
# in the model)
ID_NOAA_key <- read.csv("./Data/bar_reg_key.csv")

# maybe want to also include dermo intensity, or dermo weighted prevlance?
# ------------------------------------------------------------------------------
# 
# 
# Start with a model including summer temp and salinity, dermo prevalence, MSX
# prevalance, and natural mortality

#manipulate disease.
MSX_prev_df <- MSX_prev_df %>% 
    select(YearSamp, ID, dis_Prev) %>% 
    rename(MSX_Prev = dis_Prev)
dermo_prev_df <- dermo_prev_df %>% 
    select(YearSamp, ID, dis_Prev) %>% 
    rename(Dermo_Prev = dis_Prev)

dz_df <- full_join(dermo_prev_df, MSX_prev_df, by = c("YearSamp", "ID")) %>% 
            full_join(dermo_intensity_df, by = c("YearSamp", "ID")) %>% 
            rename(year = YearSamp) %>% 
            select(ID,year, MSX_Prev, Dermo_Prev, PercentLethal, Mean_Intensity)

# manipulate environmental data.
sal_avg_sum <- rename(sal_avg_sum, sal = Summer_avg)
wtemp_avg_sum <- rename(wtemp_avg_sum, temp_s = Summer_avg)
wtemp_avg_win <- rename(wtemp_avg_win, temp_w = Winter_avg)

#join data sets.
envir_df <- full_join(wtemp_avg_sum, sal_avg_sum, by = c("year", "ID")) %>% 
    full_join(wtemp_avg_win, by = c("year", "ID"))


#Manipulate natural mortality.
ID_NOAA_key <- select(ID_NOAA_key, ID, NOAACode)
M_df <- M_df %>% 
    mutate(M_Med_Annual = 1-exp(-M_Med_Inst)) %>% 
    select(year, NOAA_code, M_Med_Annual) %>% 
    rename(NOAACode = NOAA_code)

#bind by NOAA code to associate an ID with each NOAA code
#(for now, treating NOAA code level M as if it applies individually to each bar
#in the NOAA code.)
M_ID_df <- full_join(ID_NOAA_key, M_df, by = "NOAACode") %>% 
    select(-NOAACode)

# Bind all data sets together - the limiting factor should be the disease
# data sets, so left join with disease.

#unique bars and years for now
disease_bars <- unique(dz_df$ID) #should have length of 43.
years <- 1991:2017 #years of interest

#May consider other ways to do this, eventually.

SEM_dat <- full_join(dz_df, M_ID_df, by = c("year", "ID") ) %>% 
    full_join(envir_df, by = c("year", "ID"))  %>% 
    filter(year >= min(years)) %>% 
    filter(year <= max(years)) %>% 
    filter(ID %in% disease_bars)

#fill in SEM data with missing years and Bars. (in case any are missing)
SEM_dat_complete <- SEM_dat %>% 
    complete(year = years , ID = disease_bars)
anyNA(SEM_dat_complete) #there are NAS

#eliminate NAs
SEM_dat_no_NA <- na.omit(SEM_dat_complete) #use this data set for now.

# data characteristics.
length(unique(SEM_dat_no_NA$ID)) # 41 of 43 bars were included. 
length(unique(SEM_dat_no_NA$ID))*length(years) - nrow(SEM_dat_no_NA) #missing 33 rows of data from from these 43 bars (indicates there are some missing years in the data)

#Use SEM_dat_no_NA for analysis.
SEM_dat_no_NA <-  SEM_dat_no_NA %>% 
    mutate(MSX_Prev_Per = MSX_Prev * 100) %>%  # percent
    mutate(Dermo_Prev_Per = Dermo_Prev * 100) %>%  #percent
    mutate(M_Med_Annual_Per = M_Med_Annual * 100) #percent.
# Group the data in time -------------------------------------------------------
SEM_dat_no_NA <- SEM_dat_no_NA %>% 
    #make a group of 2 time series
    mutate(ts_2 = ifelse(year <=2002, "91to02", "02to17")) %>% 
    #make a group of 3 ts
    # mutate(ts_3 = ifelse(year <= 1999, 1, 
    #     ifelse(year >= 2000 & year <=2008, 2,
    #         ifelse(year >=2009 & year <= 2017, 3,
    #             NA)))) %>%
    #make a group of 4
    mutate(ts_4 = ifelse(year <= 1997, "91to97",
        ifelse(year >= 1998 & year <=2004, "98to04",
            ifelse(year >= 2005 & year <= 2011, "05to11",
                ifelse(year >= 2012 & year <= 2017, "12to17",
                    NA)))))
# Group the data in space ------------------------------------------------------
#still need to add spatial groups. (two-level analysis is also possible using
# cluster in lavaan - this is another potential spatial way of grouping data
# (perhaps estimate on bar level, but also regional level clusters? or maybe 
# 2 clusters?))

#add NOAA code to the SEM_dat_no_NA.
SEM_dat_no_NA <- left_join(SEM_dat_no_NA, ID_NOAA_key, by = "ID")
# add spatial cluster to the data.
NOAA_sal_zones <- rename(NOAA_sal_zones, NOAACode = NOAA_Code)
SEM_dat_no_NA <- left_join(SEM_dat_no_NA, NOAA_sal_zones, by = "NOAACode" )
# make into categorical levels
SEM_dat_no_NA <- SEM_dat_no_NA %>% 
                    #2 spatial groups - 0 = low, med/high = 1
                    mutate(spat_2 = ifelse(Sal_Zone == "Low", "Low", "MedHigh")) %>% 
                    # 3 spatial groups - 0 = low, med = 1, high = 2.
                    mutate(spat_3 = Sal_Zone)

# Group in space and time ------------------------------------------------------
SEM_dat_no_NA <- SEM_dat_no_NA %>% 
                    #get code for 2 temporal, 2 spatial groups
                    mutate(ts_2_spat_2 = paste0(ts_2, spat_2)) %>% 
                    mutate(ts_2_spat_3 = paste0(ts_2, spat_3)) %>% 
                    mutate(ts_4_spat_2 = paste0(ts_4, spat_2)) %>% 
                    mutate(ts_4_spat_3 = paste0(ts_4, spat_3))

# Remove freshets -------------------------------------------------------------
# Remove noaa codes and years of data where/when freshets occured.
freshet_NOAA <- freshet_NOAA %>%  #rename columns.
                rename(NOAACode = NOAACODE) %>% 
                rename(year = Year)
#make a yearNOAACode combo column in both data sets.
freshet_NOAA$yearNOAACode <- paste0(freshet_NOAA$year,"_", freshet_NOAA$NOAACode)
SEM_dat_no_NA$yearNOAACode <- paste0(SEM_dat_no_NA$year,"_", SEM_dat_no_NA$NOAACode)

# antijoin so only the rows in SEM_dat_no_NA that do not have a match in the freshet
# NOAA are included in the dataset.
SEM_dat_no_NA <- anti_join(SEM_dat_no_NA, freshet_NOAA, by = "yearNOAACode") %>% 
                    select(-yearNOAACode) #get rid of the column.

# save the dataframe -----------------------------------------------------------
# This one can then be used in SEM. 
# get today's date to append to file.
save_file_dir <- "./Derived_Data/SEM/4_Bind_Data_for_SEM"
dir.create(save_file_dir)
#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))
#write the data that has no missing values.
write.csv(SEM_dat_no_NA, paste0(save_file_dir, "/SEM_dat_no_NA_",save_date, ".csv" ), 
          row.names = F)

