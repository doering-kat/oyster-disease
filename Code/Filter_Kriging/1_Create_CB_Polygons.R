# Header -----------------------------------------------------------------------
# Create objects that can be used to set minimum spatial patterns
# Objects have NOAA Codes for the mainstem, western, and eastern tribs.
# 
# Written 10/9/2018 by Kathryn Doering
# Load packages and set options ------------------------------------------------
library(maps)
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(RColorBrewer)
library(tidyverse)
options(stringsAsFactors = F)

# Load data --------------------------------------------------------------------
# Chesapeake Bay by NOAA code
NOAA <- readRDS("./Data/Spatial_Objects_2018_09_21/NOAA_CB.rds")

# Specify NOAA codes in regions-------------------------------------------------
# make a vector for each region with all the NOAA codes in each region.
NOAA_vec<- list()
# Mainstem NOAA codes vector (did not include 014 because no oysters in this area)
NOAA_vec[[1]] <-  c("025", "127", "027", "229", "129")
# Western NOAA codes vector
NOAA_vec[[2]]<- c("055", "082", "088", "094",         #Western shore
                  "168","268","368",                   # Patuxet
                  "177","277","377","078","174","274") # Potomac
# Eastern NOAA codes vector
NOAA_vec[[3]] <-  c("131","231","331", # Chester
                    "039","099","060", # Eastern Bay
                    "137","237","337","437","537","637","053", # Choptank
                    "047","043","062","192","292","096","057","005","072") # TS
#set names
names(NOAA_vec) <- c("main", "west", "east")
# subset NOAA by regions -------------------------------------------------------
NOAA_reg <- map(NOAA_vec, function(.x) NOAA[(NOAA$NOAACODE %in% .x),] )
names(NOAA_reg) <- names(NOAA_vec) #set names
# Make into 1 polygon ----------------------------------------------------------
NOAA_1_poly <- map(NOAA_reg, function(.x) gUnaryUnion(.x))
names(NOAA_1_poly) <- names(NOAA_reg) #set names
# Save the spatial objects -----------------------------------------------------
# Make folders
folder_name <- "./Derived_Data/Filter_Kriging"
subfolder_name <- "./Derived_Data/Filter_Kriging/1_Create_CB_Polygons"
dir.create(folder_name)
dir.create(subfolder_name)
# create file names
file_names <- paste0(subfolder_name,"/NOAA_1_polygon_",names(NOAA_1_poly), ".rds")
#save the objects
map2(NOAA_1_poly,file_names,function(.x,.y) saveRDS(object = .x, file = .y))
