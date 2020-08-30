# Header -----------------------------------------------------------------------
# Driver to Run Kriging functions only on data sets of salinity for each cruise
# for the surface layer.
# Created 18 Sept 2018 by Kathryn Doering

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
# # Krige in for loop (commented out) ------------------------------------------
# Written when there were NA's not being omitted from the kriged observations.
# This could be nice if we want individual csvs to save as the code goes, otherwise,
# It is not necessary.
# 
# # # set up folder for results
# # #create directory
# date <- as.character(Sys.Date()) #for making a folder, add the date
# #add underscores instead of dashes.
# date <- paste0(substr(date,start = 1,stop =4), "_", 
#     substr(date, start = 6, stop = 7), "_", 
#     substr(date, start = 9, stop = 10))
# dir_name_save <- paste0("./Derived_Data/Run_Kriging_S_SAL_Output_",date)
# dir.create(dir_name_save)
# 
# # Make kriging folder
# dir_name_krige <- paste0(dir_name_save, "/Interp_RDS")
# dir.create(dir_name_krige)
# #make kriged results at bar folder
# dir_name_predictions <- paste0(dir_name_save, "/Predictions_csv")
# dir.create(dir_name_predictions)
# 
# # Krige and predict, and write objects to a file.
# for (i in 1:nrow(nested_dat_S_SAL)){
#     #get the temporary variables for the loop
#     tmp_data <- nested_dat_S_SAL$data[[i]]
#     tmp_Parameter <- nested_dat_S_SAL$Parameter[[i]]
#     tmp_Cruise <- nested_dat_S_SAL$Cruise[[i]]
#     tmp_Layer <- nested_dat_S_SAL$Layer[[i]]
#     # Krige
#     tmp_interpolated <- InterpolateWQData(WQData = tmp_data, CBPolygons = chesapeake, res = 1)
#     # save kriged data.
#     saveRDS(tmp_interpolated, paste0(dir_name_krige, "/", tmp_Parameter, "_", 
#                                      tmp_Cruise, "_", tmp_Layer, "_", i, ".rds"))
#     # get predictions
#     tmp_predicted <- GetPredictions(interp_return = tmp_interpolated, 
#                                     locations = dzbar)
#     #save predictions.
#     write.csv(tmp_predicted, paste0(dir_name_predictions, "/", tmp_Parameter,
#                                     "_", tmp_Cruise, "_", tmp_Layer, "_", i, 
#                                     ".csv"))
# }
# 
# 
# # Krige and predict, and write objects to a file.
# for (i in 340:nrow(nested_dat_S_SAL)){
#     #get the temporary variables for the loop
#     tmp_data <- nested_dat_S_SAL$data[[i]]
#     tmp_Parameter <- nested_dat_S_SAL$Parameter[[i]]
#     tmp_Cruise <- nested_dat_S_SAL$Cruise[[i]]
#     tmp_Layer <- nested_dat_S_SAL$Layer[[i]]
#     # Krige
#     tmp_interpolated <- InterpolateWQData(WQData = tmp_data, CBPolygons = chesapeake, res = 1)
#     # save kriged data.
#     saveRDS(tmp_interpolated, paste0(dir_name_krige, "/", tmp_Parameter, "_", 
#         tmp_Cruise, "_", tmp_Layer, "_", i, ".rds"))
#     # get predictions
#     tmp_predicted <- GetPredictions(interp_return = tmp_interpolated, 
#         locations = dzbar)
#     #save predictions.
#     write.csv(tmp_predicted, paste0(dir_name_predictions, "/", tmp_Parameter,
#         "_", tmp_Cruise, "_", tmp_Layer, "_", i, 
#         ".csv"))
# }
# 
# Krige (map)-------------------------------------------------------------------
#subset the nested dataset so it only contains the surface layer and SALINITY
#datasets.
#
nested_dat_S_SAL <- nested_dat %>%
                        filter(Parameter == "SALINITY") %>%
                        filter(Layer == "S ")

# use map function to krige.
nested_dat_S_SAL <- nested_dat_S_SAL%>%
                         #Krige
                         mutate(Interpolated = map(data, InterpolateWQData, CBPolygons = chesapeake, res = 1))
# use map adn mutate to get predicted values 
nested_dat_S_SAL <- nested_dat_S_SAL%>%
                         #get predicted values
                         mutate(Predicted = map(Interpolated, GetPredictions, locations = dzbar))

# Save kriging results ---------------------------------------------------------
#create directory
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_",
    substr(date, start = 6, stop = 7), "_",
    substr(date, start = 9, stop = 10))
dir_name_save <- paste0("./Derived_Data/Run_Kriging_S_SAL_Output_",date)
dir.create(dir_name_save)

# save Nested Salinity at Surface kriging output as rda object
saveRDS(nested_dat_S_SAL, paste0(dir_name_save, "/WQ_Nested_S_SAL_Kriged.rds"))

# Save the predicted data sets as .csv's. 
# Write date in nested df to individual .CSV files.
dir_name_csv <- paste0(dir_name_save, "/Predicted_S_SAL_CSVs") #csv dir
dir.create(dir_name_csv)
# Make csv names
csv_names <- paste0(dir_name_csv,"/WQ_", 
    nested_dat_S_SAL$Parameter,"_",
    nested_dat_S_SAL$Cruise, "_",
    nested_dat_S_SAL$Layer, ".csv")

csv_names <- gsub(" ", "", csv_names, fixed =T) #get rid of all white space

#write each nested data set to a .csv
map2(nested_dat_S_SAL$Predicted, csv_names, write.csv)
