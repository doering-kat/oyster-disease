# Header -----------------------------------------------------------------------
# Make a map of the study sites, with different symbols for each groups
# For chapter 2 of thesis
# 
# Written 25 Oct 2018 by Kathryn Doering
# 
# Load packages and set options ------------------------------------------------
library(tidyverse)
library(sp)
library(RColorBrewer)
options(stringsAsFactors = F)
# Load data --------------------------------------------------------------------
# NOAA code object
NOAA <- readRDS("./Data/Spatial_Objects_2018_09_21/NOAA_CB.rds")
#data used in models
dat <- read.csv("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_2018_10_17.csv")
# lat/lon for bar centriods
bar_info <- read.csv("./Data/BarInfo.csv")
# Create spatial objects by salinity zone --------------------------------------

# get only necessary bar info
bar_info <- bar_info %>% 
                select(ID, cpntLongitude,cpntLatitude)

#get unique bars (with their associated salinity zone)
unique_bars <- dat %>%
                select(ID, Sal_Zone) %>% 
                distinct()
length(unique(unique_bars$ID)) #make sure a bar is not assigned different salinity zones by making sure length is the same for unique_bars and unique(unique_bars$id)

# bind data sets
bar_locations <- left_join(unique_bars, bar_info, by = "ID")

# change map to lat lon
NOAA_longlat <- sp::spTransform(NOAA,sp::CRS('+proj=longlat'))

# make points into high, medium, and low salinity groups.
bar_locations_by_Sal <- bar_locations %>% 
                          group_by(Sal_Zone) %>% 
                          nest() 
bar_locations_by_Sal$Sal_Zone <- factor(bar_locations_by_Sal$Sal_Zone, 
                                        levels = c("Low", "Med", "High"),
                                        labels = c("Low", "Medium", "High"))
#sort by salinity zone:
bar_locations_by_Sal <- bar_locations_by_Sal %>% 
                          arrange(Sal_Zone)
pch_vec <- 21:23
col_vec <- RColorBrewer::brewer.pal(n = 3, name = "Greys")
# create map -------------------------------------------------------------------
# Make figure folder
figure_folder_name <- "./Figures/Create_Figures"
dir.create(figure_folder_name)
png(paste0(figure_folder_name, "/map_bars_salinity_zones_gray.png"), height = 7.5, width = 5,units = "in", res = 300)
#par(bty = "n")
plot(NOAA_longlat, axes = T, border = "black", col = "paleturquoise", xlab = "Longitude", ylab = "Latitude")

map2(bar_locations_by_Sal$data, col_vec, function(.x,.y) points(.x$cpntLongitude, .x$cpntLatitude, pch = 21, col = "black", bg = .y, cex = 1.5))
points(x = -77.0369, 38.9072, pch = 8, col = "gray70")
text(x = -77.0369, 39, labels = "Washington, D.C.", font = 3, col = "gray70")
#add the legend
    legend(title = "Salinity Zones",
           x = "topleft", 
           legend = levels(bar_locations_by_Sal$Sal_Zone), 
           col = rep("black", times = 3), 
           pt.bg = col_vec,
           pt.cex = rep(1.5, times = 3),
           pch = rep(21, times = 3))
# add scale bar
# add north arrow. 
arrows(x0 =  -77, y0 = 38, y1 =  38.1, length = 0.1)
text(x = -76.99, y = 37.96, labels = "N" )
# add a scale bar. (maybe? )
dev.off()
    
