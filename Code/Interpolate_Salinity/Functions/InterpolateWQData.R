# Kriging functions ------------------------------------------------------------
# 
# Function 1: Interpolate Water Quality Data
# Use ordinary kriging.
# WQData data from chesapeake bay program data to be kriged
# Parameter the parameter that is kriged over (assume only 1 parameter is inclued in WQData)
# res: resolution of kriging in km
# Grid: pixels to krige over.
InterpolateWQData <- function(WQData, CBPolygons, res = 1) {
    #Requred packages ----------------------------------------------------------
    require(sp)
    require(tidyverse)
    require(automap)
    # Manipulate water quality data --------------------------------------------
    #Get rid of NAs.
    WQData <- WQData %>% 
                select(Longitude, Latitude, MeasureValue) %>% 
                na.omit()
    # specify lat/long coordinates
    WQ_coords <- data.frame(lon = WQData$Longitude, lat = WQData$Latitude)
    #Make water quality data into a spatial object.
    WQ_sp <- sp::SpatialPointsDataFrame(coords = WQ_coords, data = data.frame(Param = WQData$MeasureValue), proj4string = sp::CRS('+proj=longlat'))
    #convert projection to UTM (km)
    WQ_sp <- sp::spTransform(WQ_sp,sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
    # Change the northing and easting values to correspond with the grid in the data.
    WQ_sp_new_data <- WQ_sp@data %>% 
                        dplyr::mutate(reasting = res*round(WQ_sp@coords[,1]/res)) %>% 
                        dplyr::mutate(rnorthing = res*round(WQ_sp@coords[,2]/res)) %>% 
                        dplyr::group_by(reasting, rnorthing) %>%                     
                        dplyr::summarize(MeanValue = mean(Param)) #%>% 
                    #as.data.frame()
    # Making Kriging input data -----------------------------------------------
    # Make a new spatial object to use as input for kriging.
    WQ_krige_sp <- sp::SpatialPointsDataFrame(coords = WQ_sp_new_data[, c("reasting", "rnorthing")], data = WQ_sp_new_data, proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
    
    # Make Grid to krige over --------------------------------------------------
    # grid coordinates
    grid_coords <- expand.grid(
                        seq(min(WQ_krige_sp@data$reasting), 
                            max(WQ_krige_sp@data$reasting), res),
                        seq(min(WQ_krige_sp@data$rnorthing), 
                            max(WQ_krige_sp@data$rnorthing), res))
    # Make the grid into spatial pixels. It is a rectangular object.
    grid_rect <- sp::SpatialPixelsDataFrame(points = grid_coords[, 1:2], 
                                            data   = grid_coords,
                                            tolerance = 0.99,
                                            proj4string = sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km')
                              )
    # select only the Pixels that lie within or touch the polygons. This will be
    # the grid used for kriging.
    grid_water <- grid_rect[CBPolygons,]
    
    # Perform Ordinary kriging -------------------------------------------------
    #  OK was performed as close to what was done by Murphy et al. 2010. They 
    #  Chose the model and kappa options for the variograms. 
    ok <- automap::autoKrige(MeanValue ~ 1, #formula for ordinary kriging
                             input_data = WQ_krige_sp,
                             new_data = grid_water,
                             model = c("Sph", "Mat"),
                             kappa = c(0.5, 1, 2.5)#, #smoothness for matern
                             #start_vals = c(2,60,100) #could set starting vals
                             )
    #make a list of return values: want the results of kriging, as well as the 
    #resolution
    interp_return <- list(kriging_results = ok,
                   resolution = res)
    return(interp_return) #return the results of ordinary kriging
}

# Function 2: Get desired point predictions from kriging output.
# interp_return: output from InterpolateWQData function (ordinary kriging output
#     and resolution)
# locations: a data.frame with columns ID, Lon, and Lat (these names and order)
# res should be the resolution used to generate the ok object.
GetPredictions <- function(interp_return, locations){
    # required packages --------------------------------------------------------
    require(sp)
    require(tidyverse)
    require(automap)
    # Break up interp_return ---------------------------------------------------
    #put the interp_return into separate objects
    res <- interp_return$resolution
    #just the krigings spatial output, not any of the other autoKrige output.
    ok  <- interp_return$kriging_results$krige_output 
    # Manipulate locations -----------------------------------------------------
    # Put lon/lat into a spatial object, and project to utkm.
    loc_sp_latlon <- SpatialPointsDataFrame(
                                     coords = locations[,c("Lon", "Lat")],
                                     proj4string = CRS('+proj=longlat'),
                                     data = locations)
    loc_sp_utm <- spTransform(loc_sp_latlon,
                              CRSobj = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
    #add northing and esating to the data frame (currently are the coords)
    loc_sp_utm@data$easting <- loc_sp_utm@coords[,1]
    loc_sp_utm@data$northing <- loc_sp_utm@coords[,2]
    # calculate rounded easting and northing values.
    loc_sp_utm@data <- loc_sp_utm@data %>% 
        dplyr::mutate(reasting = res * round(loc_sp_utm@coords[,1]/res)) %>% 
        dplyr::mutate(rnorthing = res *round(loc_sp_utm@coords[,2]/res))
    # Make a new spatial object with the rounded easting and northing values
    loc_sp_utm_rounded <- SpatialPointsDataFrame(
                            coords = data.frame(x = loc_sp_utm@data$reasting, 
                                                y = loc_sp_utm@data$rnorthing),
                            proj4string = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'),
                            data = loc_sp_utm@data[,c("ID", "reasting", "rnorthing")])
    # Get Kriging obs at locations ---------------------------------------------
    # Match the coordinates in the kriging data with those in loc_sp_utm so that
    # the bar ID's can be associated with these locations.
    loc_data <- loc_sp_utm_rounded@data #rounded location data.
    # get necessary kriging data (locations, predicted values/uncertainty)
    ok_output<- data.frame(reasting = ok@coords[,1],
                            rnorthing = ok@coords[,2],
                            PredVal = ok@data$var1.pred,
                            PredValVar = ok@data$var1.stdev,
                            PredValSD = ok@data$var1.stdev)
    #match kriging output with locations.
    kriged_obs <- dplyr::left_join(loc_data, ok_output, 
                                   by = c("reasting", "rnorthing")) #%>% 
                  #dplyr::select(ID, PredVal, PredValVar, PredValSD)
    # return -------------------------------------------------------------------
    #ret <- list(kriged_obs = kriged_obs, loc_sp_utm_rounded = loc_sp_utm_rounded)
    return(kriged_obs) #return this dataframe.
}