#####Header: Investigate Difference in Surface and Bottom Temp/ Salinity#####
# I want to know if the water over oyster bars is generally "well mixed" (it 
# should be, but it would be nice to support this with data). Use the fall 
# survey data set to assess the difference in temperature and salinity from 
# bottom and surface waters during the survey.

# To investigate further, I may also use other data sets.

#####Load Packages and Options#####

library(tidyverse)
options(stringsAsFactors = F) # to prevent read in data as being loaded as a factor.

#####Add Data sets#####

Fall_Survey <- read.csv("./Data/FallSurveyHalfBushel_KD.csv")

#####Use Fall Survey Data#####

#only interested in temperature and salinity data that have both bottom and top
#observations, so extract that information
Temp_Dat <- Fall_Survey %>%
              select(TempC, BTempC) %>% #temp vars only 
              na.omit() #get rid of missing obs.
Sal_Dat <- Fall_Survey %>%  
             select(Salinity, BSalinity) %>% #sal vars only
             na.omit() #get rid of missing obs.

# Look for relationships
Find_Association <- function(dat, var1, var2){
   with(dat, plot(get(var1)~get(var2), ylab = var1, xlab = var2))
   tmp_correlation <- with(dat, cor(get(var1), get(var2)))
   tmp_linear_mod <- with(dat, lm(get(var1)~get(var2)))
   list_data <- list(
                      correlation = tmp_correlation,
                      linear_mod = tmp_linear_mod
                      )
}

temp_output <- Find_Association(Temp_Dat, "BTempC","TempC")
summary(temp_output$linear_mod)
temp_output$corr
sal_output <- Find_Association(Sal_Dat, "BSalinity", "Salinity")
sal_output$correlation #not as good of a correlation.
summary(sal_output$linear_mod)

#####Summary from fall survey Data#####
# The linear models suggest that the there is a strong relationship between 
# bottom and surface salinity on average; however, the r2 value for temperature
# is high for temperature, but fairly low for salinity (only 0.57 for salinity,
# 0.98 for temperature). Likewise, bottom and surface temperature were correlated,
# but bottom and surface salinity were not as correlated

# Given this analysis, it seems like surface temp is a good proxy for bottom 
# temperature at least during the fall survey. Surface Salinity may be less good
# as a proxy for bottom temperature (maybe? are these differences just do to 
# reliability of the measurements? Or do they mean something about differences in
# bottom temp and surf temp (does it depend on tides, for instance?))

#####Use alternative data sources#####
# To do if interested in investigating further.






