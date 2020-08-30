# After exploring script 4, want to compare linear regression models with and
# without separating the data into north and south or before or after 2002.

#Written 2 Aug 2018 by Kathryn Doering
#-------------------------------------------------------------------------------
# Load packages and insert options
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
options(stringsAsFactors = F)
write_files <- TRUE #if want to regenerate figures and data, make TRUE 
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
    dplyr::select(YearSamp, NOAACode, PMAR_Intensity, SiteType.x) %>% 
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
ggplot(dermo_df, aes(x = YearSamp, y = dermo_Prev))+
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

ggplot(MSX_df, aes(x = YearSamp, y = MSX_Prev))+
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
#-------------------------------------------------------------------------------

#Add factors for before and after 2002, as well as north and south.
# To potentially increase power of the analysis, try broader groupings of north
# and south. But also divide into 2002 and before, and afer 2002.
dat_NS_time <- left_join(all_dat_no_NA, Regions, by = c("NOAA_code")) %>% 
                    left_join(N_S, by = "Region_name")
dat_NS_time$yr_cat <- ifelse(dat_NS_time$year <= 2002, "Start", "End") #Make a year categories
dat_NS_time$yr_cat_fac <- factor(dat_NS_time$yr_cat, levels = c("Start", "End"), ordered = F)
dat_NS_time$N_S_fac <- factor(dat_NS_time$N_S, levels = c("North", "South"), ordered = F)

#--------------------------------------------------------------------------------
#find correlation between msx and dermo
dis_cor <- cor(dat_NS_time$dermo_Prev, dat_NS_time$MSX_Prev) #fairly low.
plot(dat_NS_time$dermo_Prev, dat_NS_time$MSX_Prev) #no relationship.

#Since correlation is fairly low, can include both in a linear regression.

#-------------------------------------------------------------------------------
# Nested regressions - try 4 different versions, calculate AIC and compare.
# 1. M = intercept + a*dermo + b*MSX + error
# 2. M = intercept + a*dermo*yr_cat_fac + b*MSX*yr_cat_fac + errorsame with yr_cat_fac 
# 3. same as 2, but with N_S_fac
# 4. Same with 2, but with yr_cat_fac and N_S_fac

#1
lm1 <- lm(M_Med_Annual~dermo_Prev+MSX_Prev, data = dat_NS_time)
summary(lm1)
#plot(lm1)   #maybe okay?
#2
lm2 <- lm(M_Med_Annual~dermo_Prev*yr_cat_fac+MSX_Prev*yr_cat_fac, data = dat_NS_time)
summary(lm2)
#3
lm3 <- lm(M_Med_Annual~dermo_Prev*N_S_fac+MSX_Prev*N_S_fac, data = dat_NS_time)
summary(lm3)
#4
lm4 <- lm(M_Med_Annual~dermo_Prev*N_S_fac*yr_cat_fac+MSX_Prev*N_S_fac*yr_cat_fac, data = dat_NS_time)
summary(lm4)


aic_comp <- AIC(lm1, lm2, lm3, lm4)
aic_comp$deltaAIC <- aic_comp$AIC - min(aic_comp$AIC) #compare difference in AIC

# according to AIC the model with both year and north south categories is the "best" of the 
# models
#plot(lm4) #maybe looks okay?

#interpretation
#the summary is in terms of 0 for North, 1 for South and 0 for Start and 1 for end.


coef4 <- coef(lm4) #save coeficients in here.
val_lm4 <- list(StartNorth = NA, EndNorth = NA, StartSouth = NA, EndSouth =NA)
#the first line is for Start North:
# M = intercept + dermo_Prev + MSX_Prev
val_lm4[["StartNorth"]] <- list("Intercept" = coef4["(Intercept)"], 
                                "m_Dermo"   = coef4["dermo_Prev"],
                                 "m_MSX"    = coef4["MSX_Prev"]
                                )
# 2. the next, for End North:
val_lm4[["EndNorth"]] <- list("Intercept" = coef4["(Intercept)"] + coef4["yr_cat_facEnd"], 
                                "m_Dermo"   = coef4["dermo_Prev"]+ coef4["dermo_Prev:yr_cat_facEnd"],
                                "m_MSX"    = coef4["MSX_Prev"]   + coef4["yr_cat_facEnd:MSX_Prev"]
                              )
# M = intercept + dermo_Prev + yr_cat_facEnd + MSX_Prev + dermo_Prev:yr_cat_facEnd + MSX_Prev:yr_cat_fac_End
# 3, for Start South
val_lm4[["StartSouth"]] <- list("Intercept" = coef4["(Intercept)"] + coef4["N_S_facSouth"], 
                              "m_Dermo"   = coef4["dermo_Prev"]+ coef4["dermo_Prev:N_S_facSouth"],
                              "m_MSX"    = coef4["MSX_Prev"]   + coef4["N_S_facSouth:MSX_Prev"]
)

#4, for End South
val_lm4[["EndSouth"]] <- list("Intercept" = coef4["(Intercept)"] + coef4["N_S_facSouth"]+ coef4["yr_cat_facEnd"] + coef4["N_S_facSouth:yr_cat_facEnd"], 
                                "m_Dermo"   = coef4["dermo_Prev"]+ coef4["dermo_Prev:N_S_facSouth"] + coef4["dermo_Prev:yr_cat_facEnd"] + coef4["dermo_Prev:N_S_facSouth:yr_cat_facEnd"],
                                "m_MSX"    = coef4["MSX_Prev"]   + coef4["N_S_facSouth:MSX_Prev"] + coef4["yr_cat_facEnd:MSX_Prev"] + coef4["N_S_facSouth:yr_cat_facEnd:MSX_Prev"]
                              )
# make these into a dataframe so that they are easier to compare.
coef_lm4 <- data.frame(N_S_fac = c("North", "North", "South", "South"),
                       yr_cat_fac   = c("Start", "End", "Start", "End"),
                       Intercept = c(val_lm4[["StartNorth"]][["Intercept"]], val_lm4[["EndNorth"]][["Intercept"]],val_lm4[["StartSouth"]][["Intercept"]],val_lm4[["EndSouth"]][["Intercept"]]),
                       m_Dermo = c(val_lm4[["StartNorth"]][["m_Dermo"]], val_lm4[["EndNorth"]][["m_Dermo"]],val_lm4[["StartSouth"]][["m_Dermo"]],val_lm4[["EndSouth"]][["m_Dermo"]]),
                       m_MSX = c(val_lm4[["StartNorth"]][["m_MSX"]], val_lm4[["EndNorth"]][["m_MSX"]],val_lm4[["StartSouth"]][["m_MSX"]],val_lm4[["EndSouth"]][["m_MSX"]])
                       )
#make into factors
coef_lm4$yr_cat_fac <- factor(coef_lm4$yr_cat_fac, levels = c("Start", "End"), ordered = F)
coef_lm4$N_S_fac <- factor(coef_lm4$N_S_fac, levels = c("North", "South"), ordered = F)
#get mean values of dermo and msx prevalance for each subset of data.
mean_dis <- dat_NS_time %>% 
                select(N_S_fac, yr_cat_fac, dermo_Prev, MSX_Prev) %>% 
                group_by(N_S_fac, yr_cat_fac) %>% 
                summarize_all(funs(mean))

pred <-  full_join(coef_lm4, mean_dis, by = c("N_S_fac", "yr_cat_fac")) %>% 
            mutate(mort_dermo_pred = m_Dermo*dermo_Prev) %>% #dermo predicted mortality
            mutate(mort_MSX_pred = m_MSX*MSX_Prev)           # msx predicted mortality.


#---------------------------------------------------------------------------------------------

# try a model that drops all ns terms from lm4
lm5 <- lm(M_Med_Annual~dermo_Prev*N_S_fac+MSX_Prev, data = dat_NS_time)

lm6 <- lm(M_Med_Annual~dermo_Prev+dermo_Prev:N_S_fac+MSX_Prev, data = dat_NS_time)
summary(lm6)
AIC(lm4, lm5, lm6) #Much lower AIC, but all terms are now significant....

#-------------------------------------------------------------------------------
# #save the summary from model 4
if(write_files ==T){
    write.csv(pred, "./Derived_Data/5_Disease_nested_LM/lm4_coef_pred.csv", row.names = F)
}
#save the linear models
if(write_files == T){
    for (i in 1:6){
        tmp_mod <- get(paste0("lm",i)) #get the model
        saveRDS(tmp_mod, paste0("./Derived_Data/5_Disease_nested_LM/lm", i, ".rda"))
    }
}



#-------------------------------------------------------------------------------
#plot pred data
pred_dis_mort <- pred %>%
                    mutate(Dermo = 100*mort_dermo_pred) %>% #add a new column so it can have a diff name * 100 so it is in percent 
                    mutate(MSX = 100*mort_MSX_pred) %>%     #add a new column so it can have a diff name *100 so it is in percent 
                    select(N_S_fac, yr_cat_fac, Dermo, MSX) %>% 
                    gather("Disease", "Mortality", 3:4)

ggplot(pred_dis_mort, aes(x = yr_cat_fac , y = Mortality)) +
    geom_col(aes(fill = N_S_fac), position = "dodge", color = "black")+
    labs(x = NULL, y = "Mortality (percent)", fill = NULL)+
    scale_y_continuous(expand = c(0,0), limits = c(0, 13))+
    scale_fill_brewer(palette = "Dark2" )+
    facet_wrap(~Disease) +
    theme_classic(base_size = 20)+
    theme(legend.position = c(0.5, 0.85))
if(write_files == T){                  
    ggsave("./Figures/lm4_Pred_mortality_barplot.png", device = "png", width = 6, height = 4, units = "in")
}

#create a region and north south joined csv (used to plot n/s regions on a map)
Regions_N_S_join <- left_join(Regions, N_S, by = "Region_name")
if(write_files == T){     
    write.csv(Regions_N_S_join, "./Derived_Data/Regions_North_South_Join_6_Aug_18.csv", row.names = F)
}
# #comare these summaries to make sure MSX and dermo are both not correlated and improve the 
# # r2 values of the model. 
# summary(lm(M_Med_Annual ~ dermo_Prev, data = dat_NS_time))
# summary(lm(M_Med_Annual ~ MSX_Prev, data = dat_NS_time))
# plot(dat_NS_time$dermo_Prev~dat_NS_time$MSX_Prev) #no relation
# summary(lm1) #with both predictors
# #including both dermo and MSX makes sense, as they have very little correlation
# plot(lm1)
# plot(lm(M_Med_Annual ~ MSX_Prev, data = dat_NS_time))

# #explore the data more: (look for transformations to make each one linear wrt mortality)
# dat_NS_time$MSX_Prev_sq <- dat_NS_time$MSX_Prev*dat_NS_time$MSX_Prev
# dat_NS_time$dermo_Prev_sq <- dat_NS_time$dermo_Prev*dat_NS_time$dermo_Prev
# par(mfrow = c(2,2))
# plot(M_Med_Annual ~ MSX_Prev*MSX_Prev, data = dat_NS_time)
# plot(M_Med_Annual ~ dermo_Prev, data = dat_NS_time)
# plot(M_Med_Annual ~ dermo_Prev, data = dat_NS_time)
# plot(M_Med_Annual ~ MSX_Prev, data = dat_NS_time)
# pairs(~M_Med_Annual+sqrt(M_Med_Annual)+MSX_Prev+(MSX_Prev^2)+dermo_Prev+(dermo_Prev^2),data=dat_NS_time)
# pairs(~M_Med_Annual + sqrt(M_Med_Annual)+ MSX_Prev_sq + MSX_Prev+ log(dermo_Prev+0.02) +dermo_Prev, data = dat_NS_time)
# 
# plot()
# 
# summary(lm(M_Med_Annual ~ MSX_Prev_sq, data = dat_NS_time))
# plot(lm(M_Med_Annual ~ MSX_Prev_sq +dermo_Prev, data = dat_NS_time)) #does not make resids more normal
# #conclusion: there does not appear to be a better solution than just going with untransformed
# # data. 

#-------------------------------------------------------------------------------------------------

summary(lm4)
summary(lm( M_Med_Annual ~ -1+MSX_Prev *yr_cat_fac* N_S_fac+
                dermo_Prev * yr_cat_fac*N_S_fac, data = dat_NS_time))
#tried changing orde,r and the same things remain significant.Not sure if it matters if there
# is an intercept or not? AIC is the same, so I don't think it changes the conclusions at all?
#It's almost the same model (same AIC) only thing that changes a lot is that/south
# intercept becomes significant

#-------------------------------------------------------------------------------------------------
# I think for now, I'm going to stick with  lm4 as the best model based on the AIC comparisons.
# the aic is MUCH lower 
AIC(lm1,lm2,lm3,lm4, lm5, lm6)$AIC - AIC(lm4)
summary(lm2)
 
#------------------------------
#mortality per prevalance
#plot pred data
pred_M_per_prev <- pred %>% 
    mutate(Dermo = m_Dermo) %>% #change names
    mutate(MSX = m_MSX) %>%  
    select(N_S_fac, yr_cat_fac, Dermo, MSX) %>% 
    gather("Disease", "Mortality_per_Prevalance", 3:4)
ggplot(pred_M_per_prev, aes(x = yr_cat_fac , y = Mortality_per_Prevalance)) +
    geom_col(aes(fill = N_S_fac), position = "dodge", color = "black")+
    labs(x = NULL, y = "Mortality/Prevalence", fill = NULL)+
    scale_y_continuous(expand = c(0,0), limits = c(0, 1.3))+
    scale_fill_brewer(palette = "Dark2" )+
    facet_wrap(~Disease) +
    theme_classic(base_size = 20)+
    theme(legend.position = c(0.2, 0.85))
if(write_files == T){
    ggsave("./Figures/lm4_M_per_Prev_barplot.png", device = "png", width = 6, height = 4, units = "in")
}

#background mortality:
ggplot(pred, aes(x = yr_cat_fac , y = Intercept*100)) +
    geom_col(aes(fill = N_S_fac), position = "dodge", color = "black")+
    labs(x = NULL, y = "Background Mortality (%)", fill = NULL)+
    scale_y_continuous(expand = c(0,0), limits = c(0, 20))+
    scale_fill_brewer(palette = "Dark2" )+
    theme_classic(base_size = 20)+
    theme(legend.position = c(0.8, 0.85))
if(write_files == T){
    ggsave("./Figures/lm4_M_intercept_bkgd_barplot.png", device = "png", width = 6, height = 4, units = "in")
}
    
    
# use the prediction function to get predictions from the model with uncertainty.
predict(lm4,newdata = pred,interval="confidence")
 
 pred %>% 
     mutate(fit_manual = Intercept + mort_dermo_pred+mort_MSX_pred)
 #matches with the predictions using predict, so was done correcty(compare fit
 # manual and fit columns)
 
 #find percent decreases.
    
# 
# dat_NS_time %>% 
#     select(N_S_fac, yr_cat_fac, dermo_Prev, MSX_Prev) %>% 
#     group_by(N_S_fac, yr_cat_fac) %>% 
#     summarize_all(funs(quantile(., probs = 0.25)))
if(write_files == T){
    write.csv(all_dat_no_NA, "./Derived_Data/5_Disease_nested_LM/MSXDermoPrevbyNOAAYr.csv",
                row.names = F)
}
 