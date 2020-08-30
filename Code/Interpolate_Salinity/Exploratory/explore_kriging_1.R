# Header -----------------------------------------------------------------------
# Explore different ideas for kriging salinity values in Chesapeake Bay.
# Based in pare
# 
# Created 17 Sept 2018 by Kathryn Doering
# Last edited 18 Sept 2018
# load packages ----------------------------------------------------------------
library(sp)  # need to make and manipulate spatial objects (although sf is a newer spatial system that is becoming more common)
library(geoR) #first tried using functions from this
library(gstat) # the automap package uses gstat, but is more automated
library(kriging) #explored using this package, but did not work out
library(automap) # ended up using the automap package
library(tidyverse)
options(stringsAsFactors = F)
# load data --------------------------------------------------------------------
load("./Data/chesapeake_salinity_data.rda") #spatial CB data (whole bay)
#read in some data
water_qual_dat <- read.csv("./Data/CBF_water_qual/WaterQuality_1990.csv")
sta_info <- read.csv("./Data/CBF_water_qual/WaterQualityStation_Info.csv")
#disease stations (lat/lon points of interest)
dz_sta <- read.csv("./Derived_Data/Get_Lon_Lat_DZ_and_mod_bars/dzbar_lonlat_2018-09-05.csv")
# manipulate  data --------------------------------------------------------------
# Manipulate water quaility data
water_qual_dat$SampleDate <- as.Date(water_qual_dat$SampleDate, format = "%m/%d/%Y")
# just look at 1 cruise of data.
sal_cruise_1 <- water_qual_dat %>% 
                    filter(Cruise == "BAY112") %>% 
                    filter(Layer == "S ") %>% 
                    filter(Parameter == "SALINITY") %>%  #Just in case
                    select(Station, MeasureValue) %>% 
                    group_by(Station) #%>% 
                    #summarize(Salinity = mean(MeasureValue))  #may be better to summarize later
# Manipulate the station data 
sta_info_simple <- sta_info %>% 
                    select(Station, Latitude, Longitude, UTMX, UTMY, LLDatum)

# Bind samples with station data 
sal_dat <- left_join(sal_cruise_1, sta_info_simple, by = "Station")

#set up in form for analysis.
coordinates <- data.frame(UTMX = sal_dat$UTMX/1000, UTMY = sal_dat$UTMY/1000) #kUTM
# coordinates <- data.frame(lon = sal_dat$Longitude, lat = sal_dat$Latitude)
sal <- data.frame(sal = sal_dat$MeasureValue)
res <- 1 #resolution in km
# Make Spatial objects ---------------------------------------------------------
# water_qual <- sp::SpatialPointsDataFrame(     coords = coordinates, 
#                                                 data = sal,
#                                          proj4string = sp::CRS('+proj=longlat'))
# #transform to northing/easting
# water_qual <- sp::spTransform(water_qual,   
#     sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))

water_qual <- sp::SpatialPointsDataFrame(     coords = coordinates,
                                                data = sal,
                                         proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))

# Change the northing and easting values to correspond with the grid in the data.
water_qual@data <- water_qual@data %>%
                     #round northing and easting values according to the resolution of the data.
                     dplyr::mutate(reasting = res * round(water_qual@coords[, 1] / res),
                                   rnorthing = res * round(water_qual@coords[, 2] / res)) %>%
                     dplyr::group_by(reasting, rnorthing) %>% #group by rounded values
                     dplyr::summarize(mean = mean(sal)) %>% #take avg of data in each grid.
                     as.data.frame()

# change the coordinates to the rounded easting and northing values (currently only in the data section)
#water_qual@coords <- as.matrix(water_qual@data[, c('reasting','rnorthing')])

#Make a new spatial object to clarify
water_qual_krig_res <- sp::SpatialPointsDataFrame(
                         coords = water_qual@data[, c('reasting','rnorthing')],
                           data = water_qual@data,
                    proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))

# Make grid to krige over ------------------------------------------------------
grid <- expand.grid(
    seq(min(water_qual_krig_res@data$reasting), max(water_qual_krig_res@data$reasting), res),
    seq(min(water_qual_krig_res@data$rnorthing), max(water_qual_krig_res@data$rnorthing), res))
grid <- SpatialPoints(coords = grid[,1:2], proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km') )
# grid <- sp::SpatialPixelsDataFrame(points = grid[, 1:2], data = grid,
#     tolerance = 0.99,
#     proj4string =
#         sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
grid_test <- sp::SpatialPixels(points  = grid,
                tolerance = 0.99,
                proj4string =
                    sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km')
                )

# Select points that lie within water and label them as such.
grid_water <- grid_test[chesapeake,]
#grid_water$water <- 'YES'


#want to krig over grid_water

# Fit a variogram in geoR-------------------------------------------------------
# Empirical variaogram
empirical_variogram <- variog(coords = as.matrix(water_qual_krig_res@coords),
                              data = water_qual_krig_res@data$mean,
                              estimator.type = "modulus"
                              )
mod_variogram <-  variofit(vario = empirical_variogram,
                           nugget = 0,
                           limits = c(0, 35)
                            
    
    )
# Kriging in geoR---------------------------------------------------------------

krig <- krige.conv(coords = as.matrix(water_qual_krig_res@coords), 
                    data = water_qual_krig_res@data$mean,
                   locations = as.matrix(grid_water@coords),
                    krige.control = list(),
                    type.krige = "OK"
                   )


# #try kriging package instead to do ordinary kriging.
# test <- kriging::kriging(x = water_qual_krig_res@coords[,1], 
#                  y = water_qual_krig_res@coords[,2],
#                  response = water_qual_krig_res@data$mean
#                  #model = "spherical", #variogram model, may need to try several
#                  )
# plot(test$map)
# image(test$map)
# Kriging in automap -----------------------------------------------------------
# try automap package instead to do ordinary kriging.
ok_test <- automap::autoKrige(mean~1,
                            input_data = water_qual_krig_res,
                            new_data = grid_water,
                            model = c("Sph", "Mat"), #can add matern later
                            kappa = c(0.5, 1, 2.5), #smoothness for matern
                            verbose = T,
                            start_vals = c(2, 60, 100)
                            )

#KD
plot(ok_test$krige_output, axes = T)
plot(chesapeake, add = T)
image(ok_test$krige_output)
#
# Check out automap package and kriging package as perhaps simpler options 
# than gstat and geoR (the top line ones are a more streamlined approach to do
# Ordinary Kriging, at least)

# Calculate euclidean distance from the atlantic ocean -------------------------
# (or maybe could do water distance, but perhaps not necessary)

#Euclidean distance from a point and many spatial points(In SpatialPoints or 
# SpatialPointsDataframe object (or spatial pixels)
Eu_Dist <- function(point,spatial_points){
    sq_x <- (spatial_points@coords[,1]-point[1])^2
    sq_y <- (spatial_points@coords[,2]-point[2])^2
    eu_dist <- sqrt(sq_x+sq_y)
    eu_dist
}
# approximate km northing/easting for atlantic ocean opening.
atlantic <- c(420, 4100)

#calculate euclidean distance for the data.
water_qual_krig_res@data$eu_dist<- Eu_Dist(point = atlantic, 
                                           spatial_points = water_qual_krig_res)
#calculate eculidean distance for the predictions.
grid_eu_dist <- Eu_Dist(point = atlantic,
                              spatial_points = grid)
# create a spatial pixels data frame this time
grid_test_2 <- sp::SpatialPixelsDataFrame(points  = grid,
    data = data.frame(eu_dist = grid_eu_dist),
    tolerance = 0.99,
    proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km')
)

# Select points that lie within water and label them as such.
grid_water_2 <- grid_test_2[chesapeake,]
#grid_water$water <- 'YES'

#try universal kriging in automap ----------------------------------------------
#using eu_dist as a covariate

UK_test <- automap::autoKrige(mean~eu_dist,
                                input_data = water_qual_krig_res,
                                new_data = grid_water_2,
                                model = c("Sph", "Mat"), #can add matern later
                                kappa = c(0.5, 1, 2.5), #smoothness for matern
                                verbose = T,
                                start_vals = c(2, 60, 100)
                              )

UK_test_Krige <- UK_test$krige_output
#Essentially, there are 2 different ways that seem to work (although estimating
#variance seems to have problems, perhaps due to the small size of the dataset?)

# Get prediciton locations -----------------------------------------------------
# Use lat/lon from disease bars: convert to northing and easting, and use to 
# subset the kriging output data.
dz_sta_sp <- SpatialPointsDataFrame( 
                coords = dz_sta[,c("cpntLongitude","cpntLatitude")],
                proj4string = CRS('+proj=longlat'),
                data = dz_sta)
#convert projection.
dz_sta_sp_Kutm <- spTransform(dz_sta_sp, 
                CRSobj = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
#add northing and easting to the dataframe
dz_sta_sp_Kutm@data$easting <- dz_sta_sp_Kutm@coords[,1]
dz_sta_sp_Kutm@data$northing <- dz_sta_sp_Kutm@coords[,2]

# look at some plots
plot(chesapeake)
plot(UK_test$krige_output, axes = T)
plot(dz_sta_sp_Kutm, add = T, col = "black")

#At 2 res, may have some issues with points not matching up to a cell
#at reasting and rnorthing points to dz_sta_sp
dz_sta_sp_Kutm@data <- dz_sta_sp_Kutm@data %>% 
                   mutate(reasting = res * round(dz_sta_sp_Kutm@coords[,1]/res)) %>% 
                   mutate(rnorthing = res *round(dz_sta_sp_Kutm@coords[,2]/res))
dz_sta_rounded <- SpatialPointsDataFrame(
    coords = data.frame( x = dz_sta_sp_Kutm@data$reasting, 
                         y = dz_sta_sp_Kutm@data$rnorthing),
    proj4string = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'),
    data = dz_sta_sp_Kutm@data[,c("ID", "rnorthing", "reasting")])

plot(UK_test$krige_output, axes = T)
plot(dz_sta_rounded, add = T)

#subset the spatial object
test <- UK_test_Krige[dz_sta_rounded,]

#however, it would probably be best to do this just to match the coordinates
#With the bar ID's (because we want them associated)
subset_data <- dz_sta_rounded@data
UK_output<- data.frame(reasting = UK_test_Krige@coords[,1],
                      rnorthing = UK_test_Krige@coords[,2],
                      sal = UK_test_Krige@data$var1.pred,
                      sal_var = UK_test_Krige@data$var1.stdev,
                      sal_sd = UK_test_Krige@data$var1.stdev)
subset_data_all <- left_join(subset_data, UK_output,
                                by = c("reasting", "rnorthing"))
# add the cruise and the layer
subset_data_all$Cruise <- "BAY112"
subset_data_all$Layer <- "S "
subset_data_all$year <- 1990 #knowing the year is helpful
#include somthing to better understand the date range of the cruise...unless
#cruises ALWAYS have the same sample date (KD is not sure about this)
# subset_data_all$date_start 
# subset_data_all$date_end
# need 
# will want to write to CSV
# Compare UK and OK  -----------------------------------------------------------
UK_test$krige_output@data$var1.pred
ok_test$krige_output@data$var1.pred

compare_OK_UK <- UK_test$krige_output
compare_OK_UK@data$OK_pred <-  ok_test$krige_output@data$var1.pred
#take the difference
compare_OK_UK@data <- mutate(compare_OK_UK@data, diff = abs(var1.pred - OK_pred))
compare_OK_UK@data <- data.frame(diff = compare_OK_UK@data$diff)
plot(compare_OK_UK)
plot(dz_sta_rounded, add = T, col = "red", pch = 1, cex = 2)
plot(water_qual, add = T, col = "orange")
#Looks like the biggest difference is in the and all of the others are not
#very different. Te locations where salinity predicitons are really different
#between the 2 methods do not have disease bars
#hard to say which technique is better?
#

plot(ok_test$krige_output)
plot(UK_test$krige_output)

#comparing these, it looks like the UK gives stange results in the patapsco,
#and doesn't change the results elsewhere - it doesn't seem to help anything
# with the Kriging process, so I think it is best to stick with the 
# simplest approach.
# 
# Conclusions ------------------------------------------------------------------
# Based on this exploratory analysis, using ordinary kriging will probably do an
# acceptable job (and Murphy et al. 2010 supports this). While it would be nice
# to use water distance, there is no clear way to incorporate this into the 
# analysis (convergence problems may be an issue in incorporating the water
# distance), and although Murphy et al. 2015 shows that water distance performs
# better for salinity (lower RSME), both are highly correlated with salinity,
# (R2 = 0.97 for Euclidean, compared to R2 = 0.98 for Water distance kriging.)
# 
# Incorporating euclidean distance from the Atlantic ocean created little
# difference, except for some strange results is the Paptasco, which suggests 
# that maybe it is not a useful covariate to include in UK. Perhaps using water
# Distance instead could be better, but since there is not anything written about
# this (the idea came from a discussion section in Murphy 2015, so there is not
# peer reviewed support of this idea).
# 
# Based on this, I plan to use OK for each cruise of data. There were some NA's 
# produced for the CI, but I cannot find any way to check convergence or deal 
# with these. It may be necessary to explore multiple data sets first before 
# deciding if there is an issue.
# 
# If there is an issue, using inverse distance weighting may be acceptable, as
# it does not perform that much worse according to Murphy et al. 2010. Therefore
# I will proceed with developing functions to do OK by cruise, and fall back on
# IDW if necessary. 

#Still need to decide if 2 km res is reasonable?