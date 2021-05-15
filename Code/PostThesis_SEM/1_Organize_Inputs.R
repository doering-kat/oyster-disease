# Header -----------------------------------------------------------------------
# Organize SEM inputs data inputs

# Make several changes to SEM, based on suggestions from thesis.
# Trying to make analysis more sophisticated for publication
# Changes are bar level M estimates. Will also run additional sensitivity
# Analyses. 
# 
# Written 2 Jan 2019 by KD
# 
# Load packages, set options ---------------------------------------------------
library(dplyr)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
# Bar level M obs (instead of the NOAA code level ones used previously)
M_summary <- read.csv("./Data/summary_M_by_bar_model_d_4_1.csv")

# Read in previous SEM data set 
# Note: updated 5/8/2021 due to duplicate rows
file_date <- "2021_05_08"
dat_SEM_prev <- read.csv(paste0("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_", file_date, ".csv"))

# Set up folders to save  ------------------------------------------------------
der_save_path <- "./Derived_Data/PostThesis_SEM" # for all PostThesis_SEM results
der_save_subfolder_path <- paste0(der_save_path, "/1_Organize_Inputs") # for results from this code.

dir.create(der_save_path)
dir.create(der_save_subfolder_path)
    
# Add Bar level M to data ------------------------------------------------------
# get rid of noaa code level M estimates in the old data set.
dat_SEM_new <- dat_SEM_prev %>% 
                select(-M_Med_Annual) %>% 
                select(-M_Med_Annual_Per)
# get the essentials from the new M by bar data.
M_med_by_bar <- M_summary %>% 
                    select(Yr, BarName, X50.) %>% 
                    rename(ID = BarName) %>% 
                    rename(M_Med_A_Bar = X50.) %>% 
                    rename(year = Yr)
#add M by bar to the new dataset
dat_SEM_new <- left_join(dat_SEM_new, M_med_by_bar, by = c("year", "ID")) 
anyNA(dat_SEM_new) #Check for NA's (there shouldn't be any; if there are, may have
#joined incorrectly)    

# add a percent column
dat_SEM_new <- mutate(dat_SEM_new, M_Med_A_Bar_Per = 100*M_Med_A_Bar)

# Center temperature and salinity data -----------------------------------------
mean_sal <- mean(dat_SEM_new$sal)
mean_temp_s <- mean(dat_SEM_new$temp_s)
mean_temp_w <- mean(dat_SEM_new$temp_w)

# make new columns with the centered enviro data.
dat_SEM_new <- dat_SEM_new %>% 
                mutate(sal_cen = sal - mean_sal) %>% 
                mutate(temp_s_cen = temp_s - mean_temp_s) %>% 
                mutate(temp_w_cen = temp_w - mean_temp_w)


# Write to .csv ----------------------------------------------------------------

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))

#write the data that has no missing values.
write.csv(dat_SEM_new, paste0(der_save_subfolder_path, "/dat_SEM_post_thesis_",save_date, ".csv" ), 
    row.names = F)
