# HEADER -----------------------------------------------------------------------
# Explore salinity observations at surface and at 4 m (approximate avg depth of 
# oyster bars)
# look at about 1 and 4 m depth
# 
# created 26 Nov 2018 by Kathryn Doering
#
# Load packages and set options ------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# Read in data -----------------------------------------------------------------
# save the flat data frame as an rds
dat <- readRDS("./Derived_Data/CBP_WQ_Main_Trib_2018_09_18.rda")
#
# extract 1 and 4 m obs for sal and temp ---------------------------------------
# 
sal_1 <- dat %>% 
            select(MonitoringStation, Cruise, Station, Depth, Parameter, MeasureValue) %>% 
            filter(Parameter == "SALINITY") %>% 
            filter(Depth <= 1) %>% 
            rename(Value_1 = MeasureValue)
sal_4 <- dat %>% 
        select(MonitoringStation, Cruise, Station, Depth, Parameter, MeasureValue) %>% 
        filter(Parameter == "SALINITY") %>% 
        filter(Depth >= 3.5 & Depth <= 4.5) %>% 
        rename(Value_4 = MeasureValue)

# join together and get rid of nas.
sal_join <- full_join(sal_1, sal_4, by = c("MonitoringStation", "Cruise", "Station")) %>% 
            na.omit()
plot(sal_join$Value_4~sal_join$Value_1)
summary(lm(sal_join$Value_4~sal_join$Value_1)) # slope is ~ 1, and intercept ~ 0.5  

ggplot(sal_join, aes(x = Value_1, y = Value_4))+
    geom_point()+
    geom_smooth(method = lm)+
    theme_classic()
