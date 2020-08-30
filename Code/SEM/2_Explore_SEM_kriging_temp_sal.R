# Header -----------------------------------------------------------------------
# Use the lavaan packaage to construct an SEM for oysters.
# Can probably use either sem() or lavaan() functions to specify this model, but
# sem will make decisions for you in order to make the model identifiable, whereas
# lavaan will not, and requires the full specification of the model.
#
# Created 26 Sept 2018 by Kathryn Doering
# 
# Load packages and set options-------------------------------------------------

library(lavaan)
library(tidyverse)
library(semPlot)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------

# Read in M values (on the NOAA code level)
M_df <- read.csv("./Data/median_M_inst_df_ordered.csv")

# Read in bar-level prevalences.
dermo_prev_df <-  read.csv("./Derived_Data/Summarize_Disease/dermo_prevalance_ID_year_2018_09_26.csv")
MSX_prev_df <-  read.csv("./Derived_Data/Summarize_Disease/MSX_Prevalance_ID_year_2018_09_26.csv")

# read in bar-level environmental data
sal_summer_df <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/sal_avg_summer.csv")

wtemp_summer_df <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/wtemp_avg_summer.csv")

wtemp_winter_df <- read.csv("./Derived_Data/Run_Kriging_S_WTEMP_Output_2018_09_20/wtemp_avg_winter.csv")

#list of disease Bars and their associated NOAA code.
# from mod_all_run_4 (so the bars will be grouped with NOAA codes as they were 
# in the model)
ID_NOAA_key <- read.csv("./Data/bar_reg_key.csv")

# Clean and bind data-----------------------------------------------------------

# Start with a model including summer temp and salinity, dermo prevalence, MSX
# prevalance, and natural mortality

#manipulate disease.
MSX_prev_df <- MSX_prev_df %>% 
                select(YearSamp, ID, dis_Prev) %>% 
                rename(MSX_Prev = dis_Prev)
dermo_prev_df <- dermo_prev_df %>% 
                   select(YearSamp, ID, dis_Prev) %>% 
                   rename(Dermo_Prev = dis_Prev)

dz_df <- full_join(dermo_prev_df, MSX_prev_df, by = c("YearSamp", "ID")) %>% 
            rename(year = YearSamp) %>% 
            select(ID,year, MSX_Prev, Dermo_Prev)

# manipulate environmental data.
sal_summer_df <- rename(sal_summer_df, sal = Summer_avg)
wtemp_summer_df <- rename(wtemp_summer_df, temp_s = Summer_avg)
wtemp_winter_df <- rename(wtemp_winter_df, temp_w = Winter_avg)

#join data sets.
envir_df <- full_join(wtemp_summer_df, sal_summer_df, by = c("year", "ID")) %>% 
                full_join(wtemp_winter_df, by = c("year", "ID"))


#Manipulate natural mortality.
ID_NOAA_key <- select(ID_NOAA_key, ID, NOAACode)
M_df <- M_df %>% 
          mutate(M_Med_Annual = 1-exp(-M_Med_Inst)) %>% 
          select(year, NOAA_code, M_Med_Annual) %>% 
          rename(NOAACode = NOAA_code)

#bind by NOAA code to associate an ID with each NOAA code
#(for now, treating NOAA code level M as if it applies individually to each bar
#in the NOAA code.)
M_ID_df <- full_join(ID_NOAA_key, M_df, by = "NOAACode") %>% 
            select(-NOAACode)

# Bind all data sets together - the limiting factor should be the disease
# data sets, so left join with disease.

#unique bars and years for now
disease_bars <- unique(dz_df$ID) #should have length of 43.
years <- 1991:2017 #years of interest

#May consider other ways to do this, eventually.

SEM_dat <- full_join(dz_df, M_ID_df, by = c("year", "ID") ) %>% 
                 full_join(envir_df, by = c("year", "ID"))  %>% 
                 filter(year >= min(years)) %>% 
                 filter(year <= max(years)) %>% 
                 filter(ID %in% disease_bars)

#fill in SEM data with missing years and Bars. (in case any are missing)
SEM_dat_complete <- SEM_dat %>% 
                      complete(year = years , ID = disease_bars)
anyNA(SEM_dat_complete) #there are NAS

#eliminate NAs
SEM_dat_no_NA <- na.omit(SEM_dat_complete) #use this data set for now.

# data characteristics.
length(unique(SEM_dat_no_NA$ID)) # 41 of 43 bars were included. 
length(unique(SEM_dat_no_NA$ID))*length(years) - nrow(SEM_dat_no_NA) #missing 33 rows of data from from these 43 bars (indicates there are some missing years in the data)

#Use SEM_dat_no_NA for analysis.
SEM_dat_no_NA <-  SEM_dat_no_NA %>% 
                    mutate(MSX_Prev_Per = MSX_Prev * 100) %>%  # percent
                    mutate(Dermo_Prev_Per = Dermo_Prev * 100) %>%  #percent
                    mutate(M_Med_Annual_Per = M_Med_Annual * 100) #percent.
# Group the data ---------------------------------------------------------------
SEM_dat_no_NA <- SEM_dat_no_NA %>% 
                    #make a group of 2 time series
                    mutate(ts_grp_2 = ifelse(year <=2002, 1, 2)) %>% 
                    #make a group of 3 ts
                    mutate(ts_grp_3 = ifelse(year <= 1999, 1, 
                                      ifelse(year >= 2000 & year <=2008, 2,
                                      ifelse(year >=2009 & year <= 2017, 3,
                                              NA)))) %>%
                    #make a group of 4
                    mutate(ts_grp_4 = ifelse(year <= 1997, 1,
                                      ifelse(year >= 1998 & year <=2004, 2,
                                      ifelse(year >= 2005 & year <= 2011, 3,
                                      ifelse(year >= 2012 & year <= 2017, 4,
                                                NA)))))
#still need to add spatial groups. (two-level analysis is also possible using
# cluster in lavaan - this is another potential spatial way of grouping data
# (perhaps estimate on bar level, but also regional level clusters? or maybe 
# 2 clusters?))

# Specify lavaan model----------------------------------------------------------

#lavaan model
mod_lavaan<-'
M_Med_Annual ~ MSX_Prev + Dermo_Prev
Dermo_Prev ~ temp_s + sal
MSX_Prev ~ temp_s + sal
#interaction that could be added:
#MSX_Prev ~~ dermo_Prev
'
#add custom labels which can be useful sometimes. (for ex, if want 2 params to 
#be equal, simply give them the same name in the formulas.)
mod_lavaan_lab<-'
M_Med_Annual ~ b1*MSX_Prev + b2*Dermo_Prev
Dermo_Prev ~ b3*temp_s + b4*sal
MSX_Prev ~ b5*temp_s + b6*sal
'

# Fit the null model -----------------------------------------------------------
fit_null <- sem(mod_lavaan, data = SEM_dat_no_NA) #run lavaan model
fit_null_lab <- sem(mod_lavaan_lab, data = SEM_dat_no_NA)
fit_null@Fit@converged
summary(fit_null)
# to see more information:
summary(fit_null, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
AIC(fit_null) 
parTable(fit_null) # to see a table of the parameter estimates, with their relationships
# and if the parameters are free or not. 
Est <- parameterEstimates(fit_null, standardized = F) # to just get estimates of the parameters in a df.(CI's are 95%, but can change)
subset(Est, op == "~") # to see just the regression coefficients.
coef(fit_null) # to just get the point estimates of the free parameters.
fitMeasures(fit_null)#, c("AIC", "BIC")) # to just get the fit measures.
#Plot with parameter estimates.
semPaths(fit_null, "path", whatLabels = "par")

# group = can be used to do multiple groups analysis for SEM. Can impose equality
# constraints on the group if desired.

# Fit the model, 2 time periods ------------------------------------------------
# fit separate models to 2002 and before, and after 2002.
fit_2_start <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year <= 2002))
fit_2_end <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year > 2002))
fit_2_start@Fit@converged #check convergence.
fit_2_end@Fit@converged #check convergence.

summary(fit_2_start)
summary(fit_2_end)

# Fit the model, 4 time periods ------------------------------------------------
fit_4_1 <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year <= 1997))
fit_4_2 <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year >= 1998 & year <= 2004))
fit_4_3 <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year >= 2005 & year <= 2011))
fit_4_4 <- sem(mod_lavaan, data = filter(SEM_dat_no_NA, year >= 2012 & year <= 2017))

#get fits.
fits <- data.frame(tmp_fit = rep(0, length.out = 12))
for (i in 1:4){
    tmp_mod <- get(paste0("fit_4_",i))
    tmp_fit <- tmp_mod@ParTable$est
    tmp_fit <- data.frame(tmp_fit)
    fits <- bind_cols(fits, tmp_fit)
}
fits <- fits[,2:5]

#fits are the parameter estimates in the same order as in the summary table

# linear regresssion (out of curiousity) ---------------------------------------
lm_MSX_env <- lm(MSX_Prev_Per ~ sal + temp_s,data = SEM_dat_no_NA)
lm_dermo_env <- lm(Dermo_Prev_Per ~ sal + temp_s, data = SEM_dat_no_NA)

graphics::pairs(MSX_Prev_Per ~ Dermo_Prev_Per + sal + temp_w + temp_s+M_Med_Annual, SEM_dat_no_NA)

graphics::pairs(MSX_Prev_Per ~ Dermo_Prev_Per + sal + temp_w + temp_s , 
                filter(SEM_dat_no_NA, year > 2002))
graphics::pairs(MSX_Prev_Per ~ Dermo_Prev_Per + sal + temp_w + temp_s, 
                filter(SEM_dat_no_NA, year < 2002))
summary(lm_MSX_env )
summary(lm_dermo_env)


# Notes ------------------------------------------------------------------------
# could add some interatction between MSX and dermo:
# MSX_Prev ~~ dermo_Prev (some correlation between the two)

# fix variances of M - fix M variances to 1
#M ~~ 1*M
# Still not sure how to incorporate measurement uncertainty into lavaan when it
# is known.... Perhaps could use some sort of approach where values for the 
# variables are selected based on a distribution, the model is run many times, 
# and averaged? Not sure...


# # To add: temporal components
# Specify error. 
# consider interaction between dermo and MSX?

# TODO: add grouping variables for year, location so that group =  argument in
# lavaan can be used to run models simultaneously for different location and year
# groups.