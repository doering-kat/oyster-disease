# Header
# Filter Kriging results by the number of unique points sampled for the imput data
# (implies that there is decent spatial coverage)
# Created 9 Oct 2018 by Kathryn Doering
# 
# Load packages and set options ------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
file_dir <- "./Derived_Data/Filter_Kriging/2_Count_Data_in_CB_Polygons"

# get information about number of points sampled in the main, east, and western
#  parts of the bay
n_sal_df <-  read.csv(paste0(file_dir, "/S_SAL_Count_Points.csv"))
n_temp_df <- read.csv(paste0(file_dir, "/S_WTEMP_Count_Points.csv"))

#load the .csv with all kriging values at oyster bars.
pred_sal_df <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/Predicted_S_SAL_all.csv")
pred_temp_df <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/Predicted_S_WTEMP_all.csv")

# Filter data by point criteria ------------------------------------------------
#set some minimum sampling criteria to determine if kriging should not be done.
## Get cruise names that should be included for the set of data
# to krige
# Chose 5 in each, because it is a fairly small number to krige, but still got
# rid of unreasonable cruise values (i.e., cruises where it was obvious that 
# kriging failed.)

sal_cruise_include <- n_sal_df %>% 
                          filter(count_main >= 5) %>% 
                          filter(count_west >= 5) %>% 
                          filter(count_east >= 5) %>% 
                          select(Cruise) #%>% #most important to know which cruises to include
                          #nrow()
                          #nrow()

temp_cruise_include <- n_temp_df %>% 
                        filter(count_main >= 5) %>% 
                        filter(count_west >= 5) %>% 
                        filter(count_east >= 5) %>%
                        select(Cruise) 
                        #nrow()

#filter the kriging predictions for only cruises that met the criteria.
pred_sal_filtered_df <- pred_sal_df %>% 
                filter(cruise %in% sal_cruise_include$Cruise)
pred_temp_filtered_df <- pred_temp_df %>%
                 filter(cruise %in% temp_cruise_include$Cruise)

# Save files -------------------------------------------------------------------
# Save the filtered sets of kriged data. 
save_file_dir <- "./Derived_Data/Filter_Kriging/3_Filter_Kriging_By_Count"
dir.create(save_file_dir)

#create directory
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_",
    substr(date, start = 6, stop = 7), "_",
    substr(date, start = 9, stop = 10))

write.csv(pred_sal_filtered_df, paste0(save_file_dir, "/Predicted_S_SAL_subset_", date,".csv"))
write.csv(pred_temp_filtered_df, paste0(save_file_dir, "/Predicted_S_WTEMP_subset_", date,".csv"))

# Test obs----------------------------------------------------------------------
# Test different min number of obs  and see how it changes the number of cruises.
# test different number for each region.
sal_sample_size <- rep(NA, length.out = 20)
for (i in 1:20){
    tmp_sal_cruise_include <- n_sal_df %>% 
        filter(count_main >= i) %>% 
        filter(count_west >= i) %>% 
        filter(count_east >= i) %>% 
        select(Cruise) %>% #most important to know which cruises to include
        nrow()
    sal_sample_size[i] <- tmp_sal_cruise_include
}
plot(sal_sample_size)  

temp_sample_size <- rep(NA, length.out = 20)
for (i in 1:20){
    tmp_temp_cruise_include <- n_temp_df %>% 
        filter(count_main >= i) %>% 
        filter(count_west >= i) %>% 
        filter(count_east >= i) %>% 
        select(Cruise) %>% #most important to know which cruises to include
        nrow()
    temp_sample_size[i] <- tmp_temp_cruise_include
}
plot(temp_sample_size)
