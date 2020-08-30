# Look at the distribution of temperature and salinity data available through the
# Chesapeake Bay Program. (May need to make a list of lat/lon of stations to add to 
# arc GIS so that it can be determined which NOAA codes contain at least 1 station.)

#-------------------------------------------------------------------------------
# Load packages.
library(dplyr)
library(tidyr)
library(ggplot2)
options(stringsAsFactors = F)
#-------------------------------------------------------------------------------
dat_path <- "./Data/CBF_water_qual/CBSeg_TWQM/CBSeg2003_" # #start of file names are the same, too.
#Read in the data.
Main_90_94 <- read.csv(paste0(dat_path, "Main_Jan1990toDec1994.csv"))
Trib_90_17 <- read.csv(paste0(dat_path, "Trib_Jan1990toJun2017.csv"))
#will add other data sets later, but work with this one for now.
#-------------------------------------------------------------------------------
dat_2 <- Main_90_94 %>%
            filter(Parameter %in% c("SALINITY", "WTEMP")) %>% 
            filter(Layer == "S ") %>% #Only care about surface layer
            select(CBSeg2003, EventId, Station, SampleDate, SampleTime, Layer, Longitude, Latitude)
#use this funcction to get the last n characters of a character string in r:
substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}
#make a separate variable for the Year: 
dat_2 <- mutate(dat_2, Year = substrRight(SampleDate, 4))

#Only c

lon_lat_main <- dat_2 %>% 
             select(Station,Longitude, Latitude) %>% 
             unique() #Only look at unique combinations of station, lon and lat.

# Export the lon_lat combos: 
write.csv(lon_lat_main, "./Derived_Data/Main_90_94_Stations.csv", row.names = F)

#-------------------------------------------------------------------------------
#do the same for the tributaries.
dat_trib <- Trib_90_17 %>%
    filter(Parameter %in% c("SALINITY", "WTEMP")) %>% 
    filter(Layer == "S ") %>% #Only care about surface layer
    select(CBSeg2003, EventId, Station, SampleDate, SampleTime, Layer, Longitude, Latitude)

#make a separate variable for the Year: 
dat_trib <- mutate(dat_trib, Year = substrRight(SampleDate, 4))

#Only c

lon_lat_trib <- dat_trib %>% 
    select(Station,Longitude, Latitude) %>% 
    unique() #Only look at unique combinations of station, lon and lat.

# Export the lon_lat combos: 
write.csv(lon_lat_trib, "./Derived_Data/Trib_90_17_Stations.csv", row.names = F)

#Note that 1 station does have 2 different lat lon points in the above data set; likely a mistake,
#but I won't worry too much about it for now. 
