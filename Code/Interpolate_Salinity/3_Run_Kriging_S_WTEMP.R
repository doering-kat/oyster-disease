# Header -----------------------------------------------------------------------
# Driver to Run Kriging functions only on data sets of water temp for each cruise
# for the surface layer.
# Based on 2_Run_Kriging_S_SAL.R
# Created 20 Sept 2018 by Kathryn Doering

# Load packages and set options ------------------------------------------------
library(tidyverse)
library(automap)
library(sp)
#Load custom kriging functions: 
source("./Code/Interpolate_Salinity/Functions/InterpolateWQData.R")
options(stringsAsFactors = F)
# load the data ----------------------------------------------------------------
#named chesapeake, CBpolygons (confusing name)
load("./Data/chesapeake_salinity_data.rda")

# disease sites
dzbar <- read.csv("./Derived_Data/Get_Lon_Lat_DZ_and_mod_bars/dzbar_lonlat_2018-09-05.csv")

#folder where the water quality monitoring derived data is located.
dir_name_dat <- "./Derived_Data/CBP_WQ_Grouped_Parameter_Cruise_Layer_2018_09_18"

# Load nested dataframe of cruise data
nested_dat <- readRDS(paste0(dir_name_dat, "/WQ_Nested_all.rda"))

# Manipulate dzbar -------------------------------------------------------------
# Need to get this into a form so that it can be used by the written functions.
dzbar <- dzbar %>% 
    select(ID, cpntLongitude, cpntLatitude) %>% 
    rename(Lon = cpntLongitude) %>% 
    rename(Lat = cpntLatitude)

# Krige (map)-------------------------------------------------------------------
#subset the nested dataset so it only contains the surface layer and Water temp
#datasets.
nested_dat_S_WTEMP <- nested_dat %>%
    filter(Parameter == "WTEMP") %>%
    filter(Layer == "S ")

# use map function to krige. NOTE: Takes ~ 2-3 sec per data set kriged.
nested_dat_S_WTEMP <- nested_dat_S_WTEMP %>%
    #Krige
    mutate(Interpolated = map(data, InterpolateWQData, CBPolygons = chesapeake, res = 1)) %>% 
    #get predicted values
    mutate(Predicted = map(Interpolated, GetPredictions, locations = dzbar))

# Save kriging results ---------------------------------------------------------
#create directory
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_",
    substr(date, start = 6, stop = 7), "_",
    substr(date, start = 9, stop = 10))
dir_name_save <- paste0("./Derived_Data/Run_Kriging_S_WTEMP_Output_",date)
dir.create(dir_name_save)

# save Nested WTEMPinity at Surface kriging output as rda object
saveRDS(nested_dat_S_WTEMP, paste0(dir_name_save, "/WQ_Nested_S_WTEMP_Kriged_Predicted.rds"))


# Save the predicted data sets as .csv's. 
# Write date in nested df to individual .CSV files.
dir_name_csv <- paste0(dir_name_save, "/Predicted_S_WTEMP_CSVs") #CSV DIR
dir.create(dir_name_csv)
# Make csv names
csv_names <- paste0(dir_name_csv,"/WQ_", 
    nested_dat_S_WTEMP$Parameter,"_",
    nested_dat_S_WTEMP$Cruise, "_",
    nested_dat_S_WTEMP$Layer, ".csv")

csv_names <- gsub(" ", "", csv_names, fixed =T) #get rid of all white space

#write each nested data set to a .csv
map2(nested_dat_S_WTEMP$Predicted, csv_names, write.csv)


