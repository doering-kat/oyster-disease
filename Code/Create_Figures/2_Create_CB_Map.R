# Create a map of Chesapeake Bay, including VA.
# # Header -----------------------------------------------------------------------
# Make a map of the study sites, with different symbols for each groups
# For chapter 2 of thesis
# 
# Written 25 Oct 2018 by Kathryn Doering
# 
# Load packages and set options ------------------------------------------------
library(tidyverse)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(maps)
options(stringsAsFactors = F)
# Load data --------------------------------------------------------------------
# NOAA code object
load("./Data/chesapeake_salinity_data.rda")

CB_1_poly <- readOGR("./Data/cbdata", "CBnet_Shoreline_simplify")
# NOAA code object
NOAA <- readRDS("./Data/Spatial_Objects_2018_09_21/NOAA_CB.rds")
# #data used in models
# dat <- read.csv("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_2018_10_17.csv")
# # lat/lon for bar centriods
# bar_info <- read.csv("./Data/BarInfo.csv")

# Make both lon/lat ------------------------------------------------------------
NOAA_longlat <- sp::spTransform(NOAA,sp::CRS('+proj=longlat'))
chesapeake_longlat <- sp::spTransform(chesapeake,sp::CRS('+proj=longlat'))
CB_1_poly_longlat <- sp::spTransform(CB_1_poly,sp::CRS('+proj=longlat'))
# Unite polygons ---------------------------------------------------------------
NOAA_longlat_union <- rgeos::gUnaryUnion(NOAA_longlat)
chesapeake_longlat_union <- rgeos::gUnaryUnion(chesapeake_longlat)

#make plot ---------------------------------------------------------------------
#plot(chesapeake_longlat_union, axes = T)
plot(CB_1_poly_longlat, axes = T)
plot(NOAA_longlat_union, add = T, col = "gray", border = "gray")

#add states
#us <- map(database = "usa", regions = "main")
#Need to still add DC
figure_folder_name <- "./Figures/Create_Figures"
dir.create(figure_folder_name)
png(paste0(figure_folder_name, "/map_CB.png"), height = 7.5, width = 5,units = "in", res = 300)
map(database = "state", 
              regions =  c("Maryland", "Washington, D.C.", "Virginia", "Delaware"),
              xlim = c(-78,-75),
              ylim = c(36.5, 39.5), 
              fill = T, col = "gray")
# map(database = "state", 
#     regions =  c("Maryland", "Virginia", "Delaware"),
#     xlim = c(-78,-75),
#     ylim = c(36.5, 39.5), 
#     fill = F, col = "black")
plot(CB_1_poly_longlat, add = T, col = "lightblue", border = "lightblue", axes = T)
plot(NOAA_longlat_union, add = T, col = "blue", border = "blue")
legend("topright", legend = c("Maryland", "Virgina"), fill = c("blue", "lightblue"), title = "Chesapeake Bay")
# Try to use NOAA to divide chesapeake into MD and virginia.
text(x = -77.0369, 39, labels = "Washington, D.C.", font = 3, col = "black")
dev.off()
# also add text labels for  the states: VA, MD, DE.

# An OK start, but maybe look into getting state objects and CBobjects from USGS
# instead so there are not weird spaces where the objects don't line up perfectly.
# Need to add lat/lon axes, also. 
# 
# 