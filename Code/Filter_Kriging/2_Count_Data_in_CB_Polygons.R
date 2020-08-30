# Header -----------------------------------------------------------------------
# Use the Spatial polygons created, find how many distinct locations from each data sets #are within the polygons
# 
# Created 9 Oct 2018 by Kathryn Doering
# Load Packages and set options ------------------------------------------------
library(tidyverse)
library(sp)
source("./Code/Filter_Kriging/Filter_Kriging_Functions.R")
# Load Data --------------------------------------------------------------------

# read in the polygon regions
subfolder_name <- "./Derived_Data/Filter_Kriging/1_Create_CB_Polygons"
reg_vec <- c("main", "west", "east")

NOAA_1_poly <- map(reg_vec, function(.x) readRDS(paste0(subfolder_name,"/NOAA_1_polygon_", .x, ".rds")))
names(NOAA_1_poly) <- reg_vec # add names

# load the kriging data frames for salinity and temperature

sal_df <- readRDS("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/WQ_Nested_S_SAL_Kriged.rds")
temp_df <- readRDS("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/WQ_Nested_S_WTEMP_Kriged_Predicted.rds")

#subset the kriging dfs with only the essential columns -----------------------
sal_df <- sal_df %>% 
            select(Parameter, Cruise, Layer, data)
temp_df <- temp_df %>% 
            select(Parameter, Cruise, Layer, data)

# count number of points -------------------------------------------------------
# count the number of points per data set in each of the NOAA polygons
#salinity
sal_df <- sal_df %>% 
            mutate(count_main = map(data, n_in_Poly, polygon = NOAA_1_poly[["main"]])) %>%
            mutate(count_west = map(data, n_in_Poly, polygon = NOAA_1_poly[["west"]])) %>%
            mutate(count_east = map(data, n_in_Poly, polygon = NOAA_1_poly[["east"]])) %>% 
            #flatten lists into numbers.
            mutate(count_main = flatten_int(count_main)) %>%
            mutate(count_west = flatten_int(count_west)) %>%
            mutate(count_east = flatten_int(count_east)) %>% 
            #select columns of interest.
            select(Parameter, Cruise, Layer, count_main, count_west, count_east)
    
    
# count the number of points per data set in each of the NOAA polygons
#temperature
temp_df <- temp_df %>% 
            mutate(count_main = map(data, n_in_Poly, polygon = NOAA_1_poly[["main"]])) %>% 
            mutate(count_west = map(data, n_in_Poly, polygon = NOAA_1_poly[["west"]])) %>% 
            mutate(count_east = map(data, n_in_Poly, polygon = NOAA_1_poly[["east"]])) %>% 
            #flatten lists into numbers.
            mutate(count_main = flatten_int(count_main)) %>%
            mutate(count_west = flatten_int(count_west)) %>%
            mutate(count_east = flatten_int(count_east)) %>% 
                #select columns of interest.
            select(Parameter, Cruise, Layer, count_main, count_west, count_east)
# Save objects -----------------------------------------------------------------
#create the file directory to save the objects.
file_dir <- "./Derived_Data/Filter_Kriging/2_Count_Data_in_CB_Polygons"
dir.create(file_dir)

# Save the data frames as .csv files. 
write.csv(sal_df, paste0(file_dir, "/S_SAL_Count_Points.csv"), row.names = F)
write.csv(temp_df, paste0(file_dir, "/S_WTEMP_Count_Points.csv"), row.names = F)
