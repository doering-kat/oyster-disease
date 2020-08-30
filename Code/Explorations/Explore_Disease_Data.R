# Explore the disease database data
# DNR_disease_data_june_29_2018

# Written 7/26/2018 by Kathryn Doering
#-------------------------------------------------------------------------------
# Load packages and insert options
library(dplyr)
library(tidyr)
library(ggplot2)
options(stringsAsFactors = F)
#-------------------------------------------------------------------------------
# Load Datasets
DZ_tbl <- read.csv("./Data/DZData.csv") # disease data
SampleInfo_tbl <- read.csv("./Data/SampleInfo.csv") # sample infor for disease data
BarInfo_tbl <- read.csv("./Data/BarInfo.csv") # bar info from the fall dredge survey
Mod_ID_df <- read.csv("./Data/bar_reg_key.csv") # contains the bars that were included in model All_run_4.
#-------------------------------------------------------------------------------
# Combine the two tables - there are many entries in DZ_tbl for each entry in SampleInfo_tbl

DZ_df <- left_join(DZ_tbl, SampleInfo_tbl, by = "SampleEvent" ) # size should be the same as DZ_tbl
DZ_df <-left_join(DZ_df, BarInfo_tbl, by = "ID")
# #save this table in derived data. (Uncomment to resave.)
write.csv(DZ_df, "./Derived_Data/DZDataSampleInfoJoin.csv")

#-------------------------------------------------------------------------------
# look at the Dz data. 
#simplify into just columns of interest (for now)
unique(DZ_df$SampleType)# only oysters included
DZ_df_2 <- select(DZ_df, 
                  SampleEvent, AnimalNum, PMARPresent, ShellHeight, 
                  Hnel_Presence, SiteType.x, ID, YearSamp, NOAACode, SampleComments)
#Calculate number with PMAR present by ID and YearSamp
no_dermo <- DZ_df_2 %>% 
                filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
                select(YearSamp, ID, PMARPresent) %>% 
                filter(PMARPresent == "-") %>% 
                group_by(YearSamp, ID) %>% 
                count()%>% 
                mutate(no_n = n) %>% 
                select(-n) %>% 
                ungroup()
yes_dermo <- DZ_df_2 %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
                select(YearSamp, ID, PMARPresent) %>% 
                filter(PMARPresent == "+") %>% 
                group_by(YearSamp, ID) %>% 
                count()%>% 
                mutate(yes_n = n) %>% 
                select(-n) %>% 
                ungroup()
total_dermo <- DZ_df_2 %>% 
                filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
                select(YearSamp, ID, PMARPresent) %>% 
                filter(PMARPresent == "+" |PMARPresent == "-") %>% 
                group_by(YearSamp, ID) %>% 
                count() %>% 
                mutate(tot_n = n) %>% 
                select(-n) %>% 
                ungroup()
#join no and yes dermo to total dermo
dermo_df <- left_join(total_dermo, yes_dermo, by = c("YearSamp", "ID")) %>% #join the positive dermo
                left_join(no_dermo, by = c("YearSamp", "ID")) %>% #join the negative dermos also
                replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
#calculate prevalance: should just be yes_n/tot_n *100
dermo_df <- mutate(dermo_df, Prev = yes_n/tot_n)

#plot prevalance by year and location

ggplot(dermo_df, aes(x = YearSamp, y = Prev))+
    geom_point()+
    geom_line()+
    facet_wrap(~ID)
#prevalance by bar, but it seems like the presence/ absence PMAR variable has only been used
# since 2010. Will need to use a different column to see the full time series.
# maybe should group these bars by NOAA code instead? Also, would be nice to order them geographically

#Maybe the PMAR_Intensity field could be used?

dermo_In <-  DZ_df %>% 
                select(YearSamp, ID, PMAR_Intensity, SiteType.x) %>% 
                mutate(PMAR_Presence = ifelse(PMAR_Intensity %in% 1:7, 1, PMAR_Intensity))
                    
#Calculate number with PMAR present by ID and YearSamp
no_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, ID, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 0) %>% 
    group_by(YearSamp, ID) %>% 
    count()%>% 
    mutate(no_n = n) %>% 
    select(-n) %>% 
    ungroup()
yes_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, ID, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 1) %>%  #there was a bug here (said ==0 instead ==1)
    group_by(YearSamp, ID) %>% 
    count()%>% 
    mutate(yes_n = n) %>% 
    select(-n) %>% 
    ungroup()
total_dermo <- dermo_In %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, ID, PMAR_Intensity) %>% 
    filter(PMAR_Intensity == 1 |PMAR_Intensity == 0) %>% 
    group_by(YearSamp, ID) %>% 
    count() %>% 
    mutate(tot_n = n) %>% 
    select(-n) %>% 
    ungroup()

#join no and yes dermo to total dermo
dermo_df <- left_join(total_dermo, yes_dermo, by = c("YearSamp", "ID")) %>% #join the positive dermo
    left_join(no_dermo, by = c("YearSamp", "ID")) %>% #join the negative dermos also
    replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
#calculate prevalance: should just be yes_n/tot_n *100
dermo_df <- mutate(dermo_df, Prev = yes_n/tot_n)

#plot prevalance by year and location

ggplot(dermo_df, aes(x = YearSamp, y = Prev))+
    geom_point()+
    geom_line()+
    facet_wrap(~ID)
ggsave("./Figures/Dermo_Prevalance.jpeg", device = "jpeg", width = 14, height = 8)
#-------------------------------------------------------------------------------
# Use the MSX prevalance metric (use presence/ absence column, but may not be filled in for all samples)

#Calculate number with PMAR present by ID and YearSamp
no_MSX <- DZ_df_2 %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% #can take this out to look at all additional samples
    select(YearSamp, ID, Hnel_Presence) %>% 
    filter(Hnel_Presence == 0) %>% 
    group_by(YearSamp, ID) %>% 
    count()%>% 
    mutate(no_n = n) %>% 
    select(-n) %>% 
    ungroup()
yes_MSX <- DZ_df_2 %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
    select(YearSamp, ID, Hnel_Presence) %>% 
    filter(Hnel_Presence == 1) %>% 
    group_by(YearSamp, ID) %>% 
    count()%>% 
    mutate(yes_n = n) %>% 
    select(-n) %>% 
    ungroup()
total_MSX <- DZ_df_2 %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
    select(YearSamp, ID, Hnel_Presence) %>% 
    filter(Hnel_Presence == 1 |Hnel_Presence == 0) %>% 
    group_by(YearSamp, ID) %>% 
    count() %>% 
    mutate(tot_n = n) %>% 
    select(-n) %>% 
    ungroup()
#join no and yes MSX to total MSX
MSX_df <- left_join(total_MSX, yes_MSX, by = c("YearSamp", "ID")) %>% #join the positive MSX
    left_join(no_MSX, by = c("YearSamp", "ID")) %>% #join the negative MSXs also
    replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
#calculate prevalance: should just be yes_n/tot_n *100
MSX_df <- mutate(MSX_df, Prev = yes_n/tot_n)

#plot prevalance by year and location

ggplot(MSX_df, aes(x = YearSamp, y = Prev))+
    geom_point()+
    geom_line()+
    facet_wrap(~ID)
ggsave("./Figures/MSX_Prevalance.jpeg", device = "jpeg", width = 14, height = 8)
#looks like MSX data has been classified this way throughout the time series, so 
# I think this should be an acceptable way to calculate prevalance.


#-------------------------------------------------------------------------------
#Look at the NOAA codes that each ID is in (use the fall dredge survey db to figure
# this out. - bar info table?)

#We will only have disease values for these NOAA codes (will need to deal with missing
# data for the other ones, or potentiall use some other samples in the database.)

DZ_df_2 %>% 
filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
select(ID,NOAACode, YearSamp) %>% 
group_by(ID,NOAACode) %>% 
count()

n_by_NOAA <- DZ_df_2 %>% 
                filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
                select(ID,NOAACode) %>% 
                unique() %>% 
                group_by(NOAACode) %>% 
                count()
# 19 NOAA codes have disease sample sites, and most only have 1 bar (some have 3)

#How many of these NOAA codes were include in the model? 

#

ID_NOAA_dis <- DZ_df_2 %>% 
    filter(SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
    select(ID,NOAACode) %>% 
    unique()
ID_NOAA_mod <- Mod_ID_df %>% 
                select(ID, NOAACode)

nrow(intersect(ID_NOAA_dis, ID_NOAA_mod)) #all except 1of the disease bars are included in the model.
nrow(ID_NOAA_dis)

setdiff(ID_NOAA_dis, ID_NOAA_mod) #only HLSHS0 from NOAACode292 is not included.

#NOAA codes in the model and the disease data:
NOAA_dis_mod <- intersect(ID_NOAA_dis, ID_NOAA_mod) %>% 
    select(NOAACode) %>% 
    unique()   
#NOAA codes in the model
NOAA_mod <- Mod_ID_df %>% 
              select(NOAACode) %>% 
              unique()
#missing NOAA codes from the disease data:
NOAA_mod_no_dis <- setdiff(NOAA_mod, NOAA_dis_mod ) #number of noaa codes in the model that dont have disease data.
nrow(intersect(NOAA_mod, NOAA_dis_mod )) #numbre that do have data 
#only 3 are missing disease data: 96, 168, 174 (1 TS, 1 Pax, 1 Potomac). i.e., 29 have data.
