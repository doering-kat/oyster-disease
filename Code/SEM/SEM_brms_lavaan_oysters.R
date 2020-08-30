# Header -----------------------------------------------------------------------
# Use the brms packaage to construct an SEM for oysters.
# 
# For now, maybe just use temp/salinity at the time of the survey as dummy 
# variables. Note that these values are likely not ecologically meaningful.
# 
# Created 10 Sept 2018 by Kathryn Doering
# Load packages and data -------------------------------------------------------
library(brms)
library(lavaan)
fall_survey <- read.csv("./Data/FallSurveyHalfBushel_KD.csv") # dummy values of salinity and temp
#M_mod <- read.csv("./Data/summary_statistics_M_Model_All_run_3.csv") # dummy values for now
DZ_prev <-  read.csv("./Derived_Data/5_Disease_nested_LM/MSXDermoPrevbyNOAAYr.csv") #dz and M rates
# Bind Datasets-----------------------------------------------------------------
# Warning: these are dummy value for now!!! The models will NOT tell anything.
# Get salinity and temp dummy v
fall_survey <- fall_survey %>% 
                    select(NOAACode, SampleYr, Salinity, TempC) %>% 
                    na.omit() %>% 
                    group_by(NOAACode, SampleYr) %>% 
                    sample_n(1) %>%  # just get 1 temp and salinity observation
                    rename(year = SampleYr) %>% 
                    rename(NOAA_code = NOAACode)
DZ_prev <- DZ_prev %>% 
                select(year, NOAA_code, M_Med_Annual, dermo_Prev, MSX_Prev)
#left join disease data with fall survey
SEM_dat <- left_join(DZ_prev, fall_survey, by = c("year", "NOAA_code")) %>% 
            filter(year > 1991) #get rid of 1991 data for now b/c missing temp/sal vals in many NOAA codes.
anyNA(SEM_dat) #should be false


# Specify lavaan model----------------------------------------------------------


# To add: temporal components
# Specify error. 
# consider interaction between dermo and MSX?
# 
#lavaan model
mod_lavaan<-'
M_Med_Annual ~ MSX_Prev + dermo_Prev
dermo_Prev ~ TempC + Salinity
MSX_Prev ~ TempC + Salinity
'
# could add some interatction between MSX and dermo:
# MSX_Prev ~~ dermo_Prev (some correlation between the two)
# 
fit <- sem(mod_lavaan, data = SEM_dat) #run lavaan model

summary(fit)

# fix variances of M - fix M variances to 1
#M ~~ 1*M
# Still not sure how to incorporate measurement uncertainty into lavaan when it
# is known.... Perhaps could use some sort of approach where values for the 
# variables are selected based on a distribution, the model is run many times, 
# and averaged? Not sure...


# Fit model in brms ------------------------------------------------------------

# specify brms models.

M_mod <- bf(M_Med_Annual ~ MSX_Prev + dermo_Prev)
Dermo_mod <- bf(dermo_Prev ~ TempC + Salinity)
MSX_mod <- bf(MSX_Prev ~ TempC + Salinity)

#run model
fit_brms <- brm(M_mod + Dermo_mod + MSX_mod + set_rescor(FALSE),
                data = SEM_dat, 
                cores = 4, 
                chains = 2) #just to keep it from running so long.
#uses stan, so can control algorithm the same way as with the full software. 
summary(fit_brms)
plot(fit_brms)

# blavaan----------------------------------------------------------------------
# Also does SEM in a Bayesian framework. 
# 
