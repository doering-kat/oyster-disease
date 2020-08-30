# Header -----------------------------------------------------------------------
# Make kriging plots for BAY284 cruise to add to thesis appendix.
# Add also the points. 

# load packages ----------------------------------------------------------------
library(automap)
library(tidyverse)
library(sp)
options(stringsAsFactors = F)
# load data --------------------------------------------------------------------

# for plotting
#folder where the kriging derived data is located.for salinity
save_date <- "2018_09_20" #fill in this string with the date, format 2018_09_19
dir_name_dat <- paste0("./Derived_Data/Run_Kriging_S_SAL_Output_",save_date)
# load the nesting data containing all kriging results, as well as predictions 
# of interest fr temp and salinity.
nested_df_sal <- readRDS(paste0(dir_name_dat, "/WQ_Nested_S_SAL_Kriged.rds"))
nested_df_temp <- readRDS("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/WQ_Nested_S_WTEMP_Kriged_Predicted.rds")

# #date lookup table
# date_lookup <- read.csv("Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv")

# get the data sets
data_dir <- "./Derived_Data/CBP_WQ_Grouped_Parameter_Cruise_Layer_2018_09_18"
# for temp and salinity for a specific cruise.
sample_loc_data_sal <- read.csv(paste0(data_dir, "/WQ_SALINITY_BAY284_S.csv"))
sample_loc_data_temp <- read.csv(paste0(data_dir, "/WQ_WTEMP_BAY284_S.csv"))


# get the kriging predictions for salinity
sal_pred <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/Predicted_S_SAL_CSVs/WQ_SALINITY_BAY284_S.csv")

#get the kriging predictions for temperature
wtemp_pred <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/Predicted_S_WTEMP_CSVs/WQ_WTEMP_BAY284_S.csv")

# Create the plots -------------------------------------------------------------
# make plot directory
plot_dir_name <- "./Figures/Create_Figures/4_Create_Kriging_Example_Plots"
dir.create(plot_dir_name)

#Plot salinity for BAY284
png(paste0(plot_dir_name, "/KrigePlot_SAL_BAY284.png"), res = 300, width = 8.5, height = 8.5, units = "in")
tmp_row <- which(nested_df_sal$Cruise == "BAY284") #get row of the cruise
tmp_kriging <- nested_df_sal$Interpolated[[tmp_row]]$kriging_results
#find the min and max dates for the cruise
#tmp_min <- date_lookup[date_lookup$cruise == "BAY284", "min_date"]
#tmp_max <- date_lookup[date_lookup$cruise == "BAY284", "max_date"]
tmp_data <- sample_loc_data_sal
#create the points spatial object.
tmp_pts_obs <- SpatialPointsDataFrame(coords = data.frame(lon = tmp_data$Longitude, lat = tmp_data$Latitude),              
    data = data.frame(Val = tmp_data$MeasureValue),
    proj4string = CRS('+proj=longlat'))
tmp_pts_obs <- spTransform(tmp_pts_obs,
    CRSobj = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
#tmp_title <- paste0("BAY284", ", from ", tmp_min, " to ", tmp_max)
# plot.
#print(plot(1,1,main = tmp_title, axes = F, col = "white")) #header sheet
sp::plot(tmp_kriging, sp.layout = list(pts = list("sp.points", tmp_pts_obs, pch = 1, col = "black")))
dev.off()

# Plot Water temp for BAY284
png(paste0(plot_dir_name, "/KrigePlot_WTEMP_BAY284.png"), res = 300,width = 8.5, height = 8.5, units = "in")
tmp_row <- which(nested_df_temp$Cruise == "BAY284") #get row of the cruise
tmp_kriging <- nested_df_temp$Interpolated[[tmp_row]]$kriging_results
#find the min and max dates for the cruise
#tmp_min <- date_lookup[date_lookup$cruise == "BAY284", "min_date"]
#tmp_max <- date_lookup[date_lookup$cruise == "BAY284", "max_date"]
tmp_data <- sample_loc_data_temp
#create the points spatial object.
tmp_pts_obs <- SpatialPointsDataFrame(coords = data.frame(lon = tmp_data$Longitude, lat = tmp_data$Latitude),              
    data = data.frame(Val = tmp_data$MeasureValue),
    proj4string = CRS('+proj=longlat'))
tmp_pts_obs <- spTransform(tmp_pts_obs,
    CRSobj = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
#tmp_title <- paste0("BAY284", ", from ", tmp_min, " to ", tmp_max)
# plot.
#print(plot(1,1,main = tmp_title, axes = F, col = "white")) #header sheet
plot(tmp_kriging, sp.layout = list(pts = list("sp.points", tmp_pts_obs,pch = 1, col = "black")))
dev.off()
