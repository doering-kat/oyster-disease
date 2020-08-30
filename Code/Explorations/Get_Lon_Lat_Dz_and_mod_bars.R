# Header -----------------------------------------------------------------------
# Get the lon and lat points for disease and modeled bars. Need these to send to
# Andrew Elmore to pull out observations of temperature. 
# 
# Written 9/5/2018 by Kathryn Doering
# 
# Load packages and specify session options-------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# Load datasets-----------------------------------------------------------------
bar_info <- read.csv("./Data/BarInfo.csv")
mod_bar_list <- read.csv("./Data/bar_reg_key.csv") #List of bars modeled
# Get disease bar sites --------------------------------------------------------
dzbar_lonlat <- bar_info %>% 
            filter(DzKeyBar == "Yes") %>% 
            select(ID, cpntLongitude, cpntLatitude)
# Get modeled bar sites --------------------------------------------------------
modbar_lonlat <- mod_bar %>% 
                   left_join(bar_info, by = "ID") %>% 
                   select(ID, cpntLongitude, cpntLatitude)
# Combine modeled and disease bar sites ----------------------------------------
# All potental sites that could be used, so do not include duplicates.
allbar_lonlat <- bind_rows(dzbar_lonlat,modbar_lonlat) %>% #bind together
                    distinct() #remove duplicate rows
# test that all ID's are unique (only 1 lat/lon obs per Bar ID)
allbar_lonlat %>% 
    select(ID) %>% 
    distinct() %>% 
    nrow()
# Export to csv ----------------------------------------------------------------
results_dir <- "./Derived_Data/Get_Lon_Lat_Dz_and_mod_bars"
date_info <- as.character(Sys.Date())  #add creation date to append to file names
dir.create(results_dir) #create folder
# #finish exporting (add date info to files )
write.csv(dzbar_lonlat, (paste0(results_dir, "/dzbar_lonlat_", date_info, ".csv")))
write.csv(allbar_lonlat, (paste0(results_dir, "/allbar_lonlat_", date_info, ".csv")))
write.csv(modbar_lonlat, (paste0(results_dir, "/modbar_lonlat_", date_info, ".csv")))

