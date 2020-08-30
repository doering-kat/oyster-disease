# Use correlations and simple linear regressions to try to relate disease
# (MSX and Dermo) to model natural mortality estimates.

#Written 31 July 2018 by Kathryn Doering
#-------------------------------------------------------------------------------
# Load packages and insert options
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
options(stringsAsFactors = F)
#-------------------------------------------------------------------------------
#read in the data sets
DZ_dat <- read.csv("./Derived_Data/DZDataSampleInfoJoin.csv") #joined disease data (done in Explore_Disease_Data)
# natural mortality point estimates by NOAA code
M_inst_dat <- read.csv("./Data/median_M_inst_df_ordered.csv")
# for grouping NOAA codes into regions
Regions <- read.csv("./Data/Doering_Thesis_Regions_3.csv")
# for grouping regions into North and south
N_S <- read.csv("./Data/North_South_1_Aug_18.csv")
#-------------------------------------------------------------------------------
# Calculate prevalance by NOAA code in the regular disease samples. Do this because
# the natural mortality values are on the NOAA code and not bar level .

dermo_In <-  DZ_dat %>% 
    select(YearSamp, NOAACode, PMAR_Intensity, SiteType.x) %>% 
    mutate(PMAR_Presence = ifelse(PMAR_Intensity %in% 1:7, 1, PMAR_Intensity))

#Calculate number without (no) or with (yes) PMAR present by NOAACode and YearSamp
no_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, NOAACode, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 0) %>% 
    group_by(YearSamp, NOAACode) %>% 
    count()%>% 
    mutate(no_n = n) %>% 
    select(-n) %>% 
    ungroup()
yes_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, NOAACode, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 1) %>%  #there was a bug here (said ==0 instead ==1)
    group_by(YearSamp, NOAACode) %>% 
    count()%>% 
    mutate(yes_n = n) %>% 
    select(-n) %>% 
    ungroup()
#calculate the number sampled (yes or no)
total_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, NOAACode, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 1 |PMAR_Intensity == 0) %>% 
    group_by(YearSamp, NOAACode) %>% 
    count() %>% 
    mutate(dermo_n = n) %>% 
    select(-n) %>% 
    ungroup()

#join no and yes dermo to total dermo
dermo_df <- left_join(total_dermo, yes_dermo, by = c("YearSamp", "NOAACode")) %>% #join the positive dermo
    left_join(no_dermo, by = c("YearSamp", "NOAACode")) %>% #join the negative dermos also
    replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
#calculate prevalance: should just be yes_n/tot_n *100
dermo_df <- mutate(dermo_df, dermo_Prev = yes_n/dermo_n)
ggplot(dermo_df, aes(x = YearSamp, y = Prev))+
    geom_point()+
    geom_line()+
    facet_wrap(~NOAACode)
#Note that the sample size for some NOAA codes is really small (several years/ NOAACodes
# were only 1 - maybe need a cutoff sample size????)

#--------------------------------------------------------------------------------
# calculate MSX prevalance
#Calculate number with PMAR present by NOAACode and YearSamp
no_MSX <- DZ_dat %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, NOAACode, Hnel_Presence) %>% 
    filter(Hnel_Presence == 0) %>% 
    group_by(YearSamp, NOAACode) %>% 
    count()%>% 
    mutate(no_n = n) %>% 
    select(-n) %>% 
    ungroup()
yes_MSX <- DZ_dat %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
    select(YearSamp, NOAACode, Hnel_Presence) %>% 
    filter(Hnel_Presence == 1) %>% 
    group_by(YearSamp, NOAACode) %>% 
    count()%>% 
    mutate(yes_n = n) %>% 
    select(-n) %>% 
    ungroup()
total_MSX <- DZ_dat %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
    select(YearSamp, NOAACode, Hnel_Presence) %>% 
    filter(Hnel_Presence == 1 |Hnel_Presence == 0) %>% 
    group_by(YearSamp, NOAACode) %>% 
    count() %>% 
    mutate(MSX_n = n) %>% 
    select(-n) %>% 
    ungroup()
#join no and yes MSX to total MSX
MSX_df <- left_join(total_MSX, yes_MSX, by = c("YearSamp", "NOAACode")) %>% #join the positive MSX
    left_join(no_MSX, by = c("YearSamp", "NOAACode")) %>% #join the negative MSXs also
    replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
#calculate prevalance: should just be yes_n/tot_n *100
MSX_df <- mutate(MSX_df, MSX_Prev = yes_n/MSX_n)

#plot prevalance by year and location

ggplot(MSX_df, aes(x = YearSamp, y = Prev))+
    geom_point()+
    geom_line()+
    facet_wrap(~NOAACode)
#-------------------------------------------------------------------------------
# bind Mortality, MSX, and dermo prevalance together by NOAA code and year.
tmp_dermo_df <- select(dermo_df, YearSamp, NOAACode, dermo_n, dermo_Prev)
tmp_MSX_df <-   select(MSX_df, YearSamp, NOAACode, MSX_n, MSX_Prev)
dis_df <- full_join(tmp_dermo_df, tmp_MSX_df, by = c("YearSamp", "NOAACode"))
colnames(dis_df) <- c("year","NOAA_code",colnames(dis_df)[3:6]) #to match with mortality df

#modify the mortality data
M_inst_dat <- M_inst_dat %>%
                select(-X) %>%  #get rid of x, an indexing rvariable
                mutate(M_Med_Annual = 1-exp(-M_Med_Inst)) #convert to annual rates
#join disease with the mortality data. (full join for now)
all_dat <- full_join(M_inst_dat, dis_df, by = c("year", "NOAA_code"))

all_dat_no_NA <- na.omit(all_dat) #remove all NA's for the analysis.

#--------------------------------------------------------------------------------
#find correlation between msx and dermo
dis_cor <- cor(all_dat_no_NA$dermo_Prev, all_dat_no_NA$MSX_Prev) #fairly low.
plot(all_dat_no_NA$dermo_Prev, all_dat_no_NA$MSX_Prev) #no relationship.

#see plots of the independent and dependent vars
old.par <- par()
par(mfrow = c(2,2))
plot(all_dat_no_NA$dermo_Prev, all_dat_no_NA$M_Med_Annual) #no relationship.
plot(all_dat_no_NA$MSX_Prev, all_dat_no_NA$M_Med_Annual) #no relationship.
plot(all_dat_no_NA$dermo_Prev, all_dat_no_NA$M_Med_Inst) #no relationship.
plot(all_dat_no_NA$MSX_Prev, all_dat_no_NA$M_Med_Inst) #no relationship.

#Since there is low correlation, try a lm with MSX and derrmo prevalaence as indep.
# variables and mortality as the response var
lm1 <- with(all_dat_no_NA, lm(M_Med_Annual~dermo_Prev+MSX_Prev))
summary(lm1)
plot(lm1)

#try with instantaneous?
lm2 <- with(all_dat_no_NA, lm(M_Med_Inst~dermo_Prev+MSX_Prev))
summary(lm2)
plot(lm2)

#there is a relationship with both of these. BOth terms are significant overall.

#transforming 
hist(all_dat_no_NA$M_Med_Annual)
hist(all_dat_no_NA$dermo_Prev)
hist(all_dat_no_NA$MSX_Prev)

#maybe should tranform vvalues, because are 0 inflated. at a small # 0.02 to all the 0's and try
#tranforming

all_dat_no_NA <- all_dat_no_NA %>% 
                   mutate(log_dermo_Prev = log(dermo_Prev +0.02)) %>% #add a small value so that 0s can be added (will be some over 1 value now, though.)
                   mutate(log_MSX_Prev = log(MSX_Prev+0.02)) %>% 
                   mutate(logit_dermo_Prev = log((dermo_Prev +0.02)/(1-dermo_Prev+0.02))) %>% #calculat logit
                   mutate(logit_MSX_Prev = log((MSX_Prev+0.02)/(1-MSX_Prev+0.02)))            #calc logit

par(mfrow = c(1,2))
hist(all_dat_no_NA$log_dermo_Prev) #worked ok for dermo, but still 0 inflated
hist(all_dat_no_NA$log_MSX_Prev)   #works if you ignore the 0 inflation

hist(all_dat_no_NA$logit_dermo_Prev) #worked good for dermo
hist(all_dat_no_NA$logit_MSX_Prev)   #works if you ignore the 0 inflation

#maybe try using log transformed dermo and not MSX (since it does improve the normality of 
# the dermo data.)
lm3 <- with(all_dat_no_NA, lm(M_Med_Inst~log_dermo_Prev+MSX_Prev))
summary(lm3)
plot(lm3)
# I think lm1 makes the most sense.

#-------------------------------------------------------------------------------
# Has there been a change before and after 2002?  (Include 2002 in the first half?)

dat_pre_03 <- filter(all_dat_no_NA, year <= 2002)
dat_post_02 <- filter(all_dat_no_NA, year > 2002) 

par(mfrow = c(2,3))
with(dat_pre_03, hist(dermo_Prev))
with(dat_pre_03, hist(MSX_Prev))
with(dat_pre_03, hist(M_Med_Annual))
with(dat_pre_03, hist(log_dermo_Prev))
with(dat_pre_03, hist(log_MSX_Prev))

par(mfrow = c(2,3))
with(dat_post_02, hist(dermo_Prev))
with(dat_post_02, hist(MSX_Prev))
with(dat_post_02, hist(M_Med_Annual))
with(dat_post_02, hist(log_dermo_Prev))
with(dat_post_02, hist(log_MSX_Prev))


#try lm's with both data sets
lm1_pre_03 <- with(dat_pre_03, lm(M_Med_Annual~dermo_Prev+MSX_Prev))
lm1_post_02 <- with(dat_post_02, lm(M_Med_Annual~dermo_Prev+MSX_Prev))

summary(lm1_pre_03)
summary(lm1_post_02)

par(old.par)
plot(lm1_pre_03)
plot(lm1_post_02) #qq plot indicates non-normaldata.may need to account for the 0 inflation

#the coefficient on MSX is smaller after 2002, but dermo is really similar. It may not be
# valid to compare these values, though, because the residuals are not normal for the second
# model.

# maybe use GLM to account for zero inflation?


#-------------------------------------------------------------------------------
#Try lagging the data. perhaps dermo from x year predicts mortality in x+1 years?
all_dat_lag_1 <- all_dat %>% 
                    mutate(year = year + 1) %>% #add a year so that the dermo and MSX data is from the previous year
                    mutate(lag_1_dermo_Prev = dermo_Prev) %>%
                    mutate(lag_1_MSX_Prev = MSX_Prev) %>% 
                    dplyr::select(year, NOAA_code, lag_1_dermo_Prev, lag_1_MSX_Prev)
#bind all_dat_lag_1 with all_dat

all_dat_lag_1 <- full_join(all_dat_lag_1, all_dat, by = c("year", "NOAA_code")) %>%  #join lagged data with mort
    dplyr::select(year, NOAA_code, M_Med_Inst, M_Med_Annual, lag_1_dermo_Prev, lag_1_MSX_Prev) %>% #select only relevant cols
    na.omit() #remove any rows with NA's

#already looked at the histograms of this data. try a lm with both lagged to start
lm1_lag <- with(all_dat_lag_1, lm(M_Med_Annual~lag_1_dermo_Prev+lag_1_MSX_Prev))
summary(lm1_lag) #explains less variability and coefficients are smaller, but still significant
plot(lm1_lag)

#-------------------
#Try 2 year lag
#Try lagging the data. perhaps dermo from x year predicts mortality in x+2 years?
all_dat_lag_2 <- all_dat %>% 
    mutate(year = year + 2) %>% #add a year so that the dermo and MSX data is from the previous year
    mutate(lag_2_dermo_Prev = dermo_Prev) %>%
    mutate(lag_2_MSX_Prev = MSX_Prev) %>% 
    dplyr::select(year, NOAA_code, lag_2_dermo_Prev, lag_2_MSX_Prev)

# bind all_dat_lag_2 with all_dat

all_dat_lag_2 <- full_join(all_dat_lag_2, all_dat, by = c("year", "NOAA_code")) %>%  #join lagged data with mort
    dplyr::select(year, NOAA_code, M_Med_Inst, M_Med_Annual, lag_2_dermo_Prev, lag_2_MSX_Prev) %>% #select only relevant cols
    na.omit() #remove any rows with NA's
# hist(all_dat_lag_2$lag_2_dermo_Prev)
# hist(all_dat_lag_2$lag_2_MSX_Prev)
# already looked at the histograms of this data. try a lm with both lagged to start
lm1_lag_2 <- with(all_dat_lag_2, lm(M_Med_Annual~lag_2_dermo_Prev+lag_2_MSX_Prev))
summary(lm1_lag_2) #explains less variability and coefficients are smaller, but still significant
plot(lm1_lag_2)
# explains even less variance, so lagging the data does not seem to explain mortality better.

#-------------------------------------------------------------------------------------------------
# Try calculating Mortality/Dermo prev and Mortality/MSX prev and plot over time.

all_dat_no_NA <- all_dat_no_NA %>% 
                    mutate(M_per_dermo = M_Med_Annual/(dermo_Prev+0.02)) %>% #add a small constant to prevent the metric from being undef
                    mutate(M_per_MSX = M_Med_Annual/(MSX_Prev+0.02)) #add a small constant to prevent the metric from being undef

yr_sum <- all_dat_no_NA %>%
            dplyr::select(year, M_per_dermo, M_per_MSX) %>% 
            group_by(year) %>% 
            summarise_all(funs(mean))

par(mfrow = c(1,2))
with(all_dat_no_NA, plot(M_per_dermo~year))
with(all_dat_no_NA, plot(M_per_MSX~year))

#plot the summarized means over time.
par(mfrow = c(1,2))
plot(yr_sum$M_per_dermo~yr_sum$year, type = "o")
abline(v = 2002, col = "red") #lines to show where 2002 is.
plot(yr_sum$M_per_MSX~yr_sum$year, type = "o")
abline(v = 2002, col = "red") #lines to show where 2002 is.

#-------------------------------------------------------------------------------

#Is there a spatial component to how MSX and dermo correlate with mortality?
# - to address this, maybe divide the data into broad regions and examine the relationship
# maybe also find avg Mortality/dermo or MSX by NOAA code and plot (then, avg  pre and post
# 2002?)

NOAA_code_sum <- all_dat_no_NA %>%
                    dplyr::select(NOAA_code, M_per_dermo, M_per_MSX) %>% 
                    group_by(NOAA_code) %>% 
                    summarise_all(funs(mean))

#make an r map here (would be ideal.) for now, use regions to create a factor to order
# roughly from N to S.
NOAA_code_sum$NOAA_code_fac <- factor(NOAA_code_sum$NOAA_code, levels = Regions$NOAA_code,
                                      ordered = T)
NOAA_code_sum <- arrange(NOAA_code_sum, NOAA_code_fac)

#Plot the data to get a rough Idea of N to S gradient over the TS
plot(NOAA_code_sum$M_per_dermo, type = "o")
plot(NOAA_code_sum$M_per_MSX, type = "o")

#--------------------------------------------------------------------------------
# maybe somewhat higher M/dermo in the south compared to north, and lower M/MSX in south compared to
# north, but it's a little noisy. May be worth plotting to understand better. 

#try dividing into pre and post 2002, also? 

# Try a regression for each region.
dat_by_reg <- left_join(all_dat_no_NA, Regions, by = c("NOAA_code"))

#Create a nested data frame for each region.

#linar model with no data dtransformation.
lin_mod_1 <- function(x){ #x is the data frame.
    mod <- with(x, lm(M_Med_Annual~dermo_Prev+MSX_Prev))
    return(mod)
}

lm_by_reg_1 <- dat_by_reg %>% 
            group_by(Region_name) %>% 
            nest() %>%  # make a nested data frame with data for each reason in a separate entry
            mutate(model = map(data, lin_mod_1)) # run a linar model for each region.

names(lm_by_reg_1$model) <- lm_by_reg_1$Region_name #add region names
map(lm_by_reg_1$model, summary) # see basic model output
map(lm_by_reg_1$model,coef)
map(lm_by_reg_1$model, function(x){summary(x)$adj.r.squared}) #r squared only

#-------------------------------------------------------------------------------
# To potentially increase power of the analysis, try broader groupings of north
# and south. But also divide into 2002 and before, and afer 2002.

dat_NS_time <- left_join(dat_by_reg, N_S, by = "Region_name")
dat_NS_time$yearcat <- ifelse(dat_NS_time$year <= 2002, "Start", "End") #Make a year categories
dat_NS_time$yearcatfac <- factor(dat_NS_time$yearcat, levels = c("Start", "End"), ordered = T)
dat_NS_time$N_Sfac <- factor(dat_NS_time$N_S, levels = c("North", "South"), ordered = T)
lm_by_reg_2 <- dat_NS_time %>% 
    group_by(yearcat, N_S) %>% 
    nest() %>%  # make a nested data frame with data for each reason in a separate entry
    mutate(model = map(data, lin_mod_1)) # run a linar model for each region.
names(lm_by_reg_2$model) <- paste(lm_by_reg_2$yearcat, lm_by_reg_2$N_S) #add names to the modle

map(lm_by_reg_2$model, summary) # see basic model output
map(lm_by_reg_2$model,coef)
map(lm_by_reg_2$model, function(x){summary(x)$adj.r.squared}) #r squared only

plot(lm_by_reg_2$model[[1]])

#plot the data 
# plot dermo
ggplot(data = dat_NS_time, aes(x = dermo_Prev, y = M_Med_Annual))+
    geom_point(aes(color = N_Sfac), position = position_jitter(w = 0.01, h = 0))+
    facet_grid(N_Sfac~yearcatfac)+
    xlab("Dermo Prevalance")+
    ylab("Natural Mortality")+
    theme_classic()
#ggsave("./Figures/Dermo_Prev_temp_spatial_xjitter.png", device = "png")
#plot msx
ggplot(data = dat_NS_time, aes(x = MSX_Prev, y = M_Med_Annual))+
    geom_point(aes(color = N_Sfac),position = position_jitter(w = 0.01, h = 0))+
    facet_grid(N_Sfac~yearcatfac)+
    xlab("MSX Prevalance")+
    ylab("Natural Mortality")+
    theme_classic()
#ggsave("./Figures/MSX_Prev_temp_spatial_xjitter.png", device = "png")

lm_by_reg_2 <- lm_by_reg_2 %>% 
                    mutate(dermo_hist = map(data,function(x)(hist(x$dermo_Prev)))) %>% 
                    mutate(MSX_hist = map(data,function(x)(hist(x$MSX_Prev))))
par(old.par)

# look at histograms;
# Most aren't normal, mostly due to inflated zeros - can try to use transformations to 
# make more normal
map(lm_by_reg_2$data, function(x)(hist(x$dermo_Prev)))

hist(lm_by_reg_2$data[[4]]$dermo_Prev)
hist(log(lm_by_reg_2$data[[4]]$dermo_Prev+0.02))

hist(lm_by_reg_2$data[[4]]$MSX_Prev)
hist(log(lm_by_reg_2$data[[4]]$MSX_Prev+0.02))


#------------------------------

#plot M per disease using the spatial and temporal variables.
#plot the data 

# plot dermo
ggplot(data = dat_NS_time, aes(y = M_per_dermo, x = year))+
    geom_smooth(aes(color = N_Sfac))+
    geom_point(aes(color = N_Sfac), position = position_jitter(w = 0.01, h = 0))+
    facet_grid(N_Sfac~.)+
    xlab("year ")+
    ylab("M per dermo")+
    theme_classic()
#ggsave("./Figures/M_per_Dermo_spatial.png", device = "png")
#plot msx
ggplot(data = dat_NS_time, aes(y = M_per_MSX, x = year))+
    geom_smooth(aes(color = N_Sfac))+
    geom_point(aes(color = N_Sfac), position = position_jitter(w = 0.01, h = 0))+
    facet_grid(N_Sfac~.)+
    xlab("year ")+
    ylab("M per MSX")+
    theme_classic()
#ggsave("./Figures/M_per_MSX_spatial.png", device = "png")
