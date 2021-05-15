# Header -----------------------------------------------------------------------
# Change temperature/salinity window
# 
# Try alternative models by summarizing temperature and salinity differntly, to 
# see what effect this has on the results for the best fit model.
# 
# One scenario is using winter temperature with summer salinity (vary salinity window) 
# Another is modifying summer and temperature salinity windows together
# Other feasibile scenarios may also exist?
# 
# Written 7 Jan 2019 by Kathryn Doering
# 
# Difference between base model script and the sensitivities are in how the 
# centering was being done. Now centering after binding model obs.

#Still need to rerun this script
# Load packages, set options ---------------------------------------------------
library(dplyr)
library(lavaan)
options(stringsAsFactors = F)
write_files <- T #set as true if want to save the data files. 
# Load data --------------------------------------------------------------------
# List of scenarios
scen <- read.csv("./Data/scenarios_temp_sal_window_2019_01_07.csv")
# 
# Original model dataset
file_date <- "2021_05_08"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date, ".csv"))

# Unsummarized temperature, salinity from kriging
# # Salinity
sal_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv")
#TODO: try to change this to using the count filtered kriging set instead.
sal_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_SAL_subset_2018_10_10.csv")
#sal_pred <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/Predicted_S_SAL_all.csv")
#Wtemp
wtemp_cruise_date <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/cruise_date_lookup.csv")
wtemp_pred <- read.csv("./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count/Predicted_S_WTEMP_subset_2018_10_10.csv")
#wtemp_pred <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/Predicted_S_WTEMP_all.csv")

# Create folders to save data, figures -----------------------------------------
save_der_dat_path <- "./Derived_Data/PostThesis_SEM/10_Run_temp_sal_window_change_redo"
save_fig_path <- "./Figures/PostThesis_SEM/10_Run_temp_sal_window_change_redo"

dir.create(save_der_dat_path)
dir.create(save_fig_path)

#create a folder to save all datasets (each dataset specific for a scenario)
save_datasets_path <- paste0(save_der_dat_path, "/model_input_data" )
dir.create(save_datasets_path)

# Write Lavaan model -----------------------------------------------------------
# used centered salinity and temp, and M on the bar level

mod_lavaan<-'
#regressions
M_Med_A_Bar_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
Dermo_Prev_Per ~ b3*wtemp_cen + b4*sal_cen
MSX_Prev_Per ~ b5*wtemp_cen + b6*sal_cen
#intercepts
M_Med_A_Bar_Per~1
Dermo_Prev_Per~1
MSX_Prev_Per~1
'

# par down the original model dataset ------------------------------------------
#get rid of the unessential details
#Note: may want to change which grouping variables are included. 
dat <- dat  %>% 
        select(year, ID, Sal_Zone, MSX_Prev_Per, Dermo_Prev_Per, M_Med_A_Bar_Per, ts_2_spat_2, ts_2_spat_3)

# manipulate temp/sal data ---------------------------------------------------
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
# Chage out of range sal estimates to NA
## remove high/low sal values ---------------------------------------------------
# save obs that will be changed
changed_obs <- sal_pred %>%
    filter(PredVal <0 | PredVal > 35) 
# save
if(write_files == T){
    write.csv(changed_obs, paste0(save_der_dat_path, "/sal_krig_obs_changed.csv"), row.names = F)
    print(paste0(save_der_dat_path, "/sal_krig_obs_changed.csv was written"))
} else (
    print(paste0(save_der_dat_path, "/sal_krig_obs_changed.csv was not written"))
)
#convert sal obs that are too high or too low to NA's. 
sal_pred <- sal_pred %>% 
    mutate(PredVal = ifelse(PredVal < 0, NA, PredVal)) %>% 
    mutate(PredVal = ifelse(PredVal > 35, NA, PredVal)) # get rid of one really high observation. (maybe need to deal with it some other way?)

# Summarize temp and sal data by monthly avgs for each bar ---------------------
# salinity
sal_avg_month <- sal_pred %>% 
    na.omit() %>% 
    select(ID, year, min_month, PredVal) %>% 
    group_by(ID, year, min_month) %>% 
    summarize(Month_avg = mean(PredVal)) %>% 
    ungroup() #remove groups

#get monthly avg temp
wtemp_avg_month <- wtemp_pred %>% 
    na.omit() %>% 
    select(ID, year, min_month, PredVal) %>% 
    group_by(ID, year, min_month)  %>% 
    summarize(Month_avg = mean(PredVal)) %>% 
    ungroup()

# Save the these summaries
if(write_files == T){
    write.csv(sal_avg_month, paste0(save_der_dat_path, "/sal_avg_month.csv"), row.names = F)
    print(paste0(save_der_dat_path, "/sal_avg_month.csv was written"))
    write.csv(wtemp_avg_month, paste0(save_der_dat_path, "/wtemp_avg_month.csv"), row.names = F)
    print(paste0(save_der_dat_path, "/wtemp_avg_month.csv was written"))
    
} else {
    print(paste0(save_der_dat_path, "/sal_avg_month.csv was not written"))
    print(paste0(save_der_dat_path, "/wtemp_avg_month.csv was not written"))
}

# Loop through all sections below based on scenarios: --------------------------
# 1. Winter temp, summer salinity (modify salinity window)
# 2. Summer temp, summer salinity (modify salinity window)

# Loop through to create and save datasets -------------------------------------
datasets <- list()
for (i in 1:nrow(scen)){

    # Summarize data for model input -----------------------------------------------
    tmp_wtemp_min <- scen[scen$scen_number == i,"temp_start"]
    tmp_wtemp_max <- scen[scen$scen_number == i, "temp_end"]
    tmp_sal_min <- scen[scen$scen_number == i, "sal_start"]
    tmp_sal_max <- scen[scen$scen_number == i, "sal_end"]
    
    #get overall avg water temp (model input)
    wtemp_avg <- wtemp_avg_month %>% 
        filter(min_month >= tmp_wtemp_min) %>% 
        filter(min_month <= tmp_wtemp_max) %>% 
        filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
        group_by(ID, year) %>% 
        summarize(wtemp = mean(Month_avg))
    # ##center the data
    # wtemp_overall_mean <- mean(wtemp_avg$wtemp) #mean
    # #subtract the mean to center
    # wtemp_avg <- mutate(wtemp_avg, wtemp_cen = wtemp - wtemp_overall_mean)
    
    # get overall avg salinity (model input)
    sal_avg <- sal_avg_month %>% 
        filter(min_month >= tmp_sal_min) %>% 
        filter(min_month <= tmp_sal_max) %>% 
        filter(year < 2018) %>%  # 2018 data incomplete (and also not necessary)
        group_by(ID, year) %>% 
        summarize(sal = mean(Month_avg)) # average for each month.
    # #center the data
    # sal_overall_mean <- mean(sal_avg$sal) #mean
    # #subtract the mean to center
    # sal_avg <- mutate(sal_avg, sal_cen = sal - sal_overall_mean)
    
    # save datasets (in a subfolder) (0r maybe just save the bound datasets?)
    if(write_files == T){
        write.csv(wtemp_avg, paste0(save_datasets_path, "/temp_", tmp_wtemp_min, "_", tmp_wtemp_max, ".csv"))
        write.csv(sal_avg, paste0(save_datasets_path, "/sal_", tmp_sal_min, "_", tmp_sal_max, ".csv"))
    }
    
    # Bind data (sal, temp, Prevalences, M) ----------------------------------------
    tmp_dat <- dat %>%
                left_join(wtemp_avg, by = c("ID", "year")) %>% #add water temp
                left_join(sal_avg, by = c("ID", "year")) # add salinity 
    if(anyNA(tmp_dat)) {
        print(paste0("Data for scenario ", i, " has NAs and may not have joined
            correctly"))
    }
    # center the data
    mean_sal <- mean(tmp_dat$sal, na.rm = TRUE)
    mean_temp <- mean(tmp_dat$wtemp, na.rm = TRUE)
    
    # make new columns with the centered enviro data.
    tmp_dat <- tmp_dat %>% 
        mutate(sal_cen = sal - mean_sal) %>% 
        mutate(wtemp_cen = wtemp - mean_temp)
    #save the dataset and add to list of datasests
    datasets[[i]] <- tmp_dat
    if(write_files == T){
        write.csv(tmp_dat, paste0(save_datasets_path, "/mod_dat_scen_", i, ".csv"), row.names = F)
    }
}

# remove NA values
# for some reason this is stripping out all values.
# really just want to remove rows with nas in sal or wtemp
# pick up here, fix this.
for(i in 1:length(datasets)) {
    datasets[[i]] <- na.omit(datasets[[i]])
}
lapply(datasets, function(x) anyNA(x)) # check all say false

# Loop to Run the models -------------------------------------------------------
# May want to try different grouping variables, but do 4 groups total for now. 
# had to remove some NA's....but now lavann having errors. Will need to look at
# the data sets and see where data are missing.

fits <- vector(mode = "list", length = nrow(scen))
for(i in 1:nrow(scen)){
    fits[[i]] <- sem(mod_lavaan, data = datasets[[i]], group = "ts_2_spat_2")
}
names(fits) <- paste0("Scenario_", 1:length(fits))
# Diagnostics ------------------------------------------------------------------
# Make sure all models converged
for(i in 1:length(fits)){
    print(fits[[i]]@Fit@converged)
}

# Store model data -------------------------------------------------------------
if(write_files == T){
    #get date to use in file names.
    save_date <- as.character(Sys.Date()) #for making a folder, add the date
    #add underscores instead of dashes.
    save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
        substr(save_date, start = 6, stop = 7), "_",
        substr(save_date, start = 9, stop = 10))
    
    #save models
    saveRDS(fits, paste0(save_der_dat_path, "/lavaan_fits_", save_date, ".rds"))
    # save model data in an RDS object
    saveRDS(datasets, paste0(save_der_dat_path, "/lavaan_data_", save_date, ".rds"))
    
    #save text output from summaries as a text document.
    sink(paste0(save_der_dat_path, "/lavaan_fits_summaries_", save_date, ".txt"))
    for(i in 1:length(fits)){
        print(names(fits)[i])
        print(summary(fits[[i]]))
    }
    sink()
    
}
