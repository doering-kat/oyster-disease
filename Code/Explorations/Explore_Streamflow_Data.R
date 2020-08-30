# Explore annual and monthtly streamflow data 

# This is of interest because streamflow can be a proxy for salinity.

# It would at least be useful to plot above and below average flow years when 
# ploting natural mortality

# Using annual averages would be one way, but perhaps another way that is more
# relevant for oysters would be using monthly averages and only averaging them
# over the relavent months (could weight these equally or by Number of days)

# Created 7/3/2018 by Kathryn Doering
#-------------------------------------------------------------------------------
# load packages
library(dplyr)
library(ggplot2)
library(lubridate)
# change default options
options(stringsAsFactors = F)
#-------------------------------------------------------------------------------
# load the data

annual_dat <- read.delim("./Data/USGS_annual_mean_streamflow.txt", header = T, skip = 9)
monthly_dat <- read.delim("./Data/USGS_monthly_mean_streamflow.txt", header = T, skip = 24)
class_dat <- read.delim("./Data/USGS_annual_mean_all_CB_streamflow_classifications.txt", header = T, skip = 9)
M_dat <- read.csv("./Data/summary_statistics_M_model_All_run_3.csv")
# Note that a "water year" as in annual dat starts October 1st and ends Sept. 31. 
# It is designated by the calandar year in which it ENDS (the year that includes
# 9/12 months). So 2013 water year should correspond with mortality that happened in 
#2013 that is calculated with the 2013 fall survey data.
# https://water.usgs.gov/nwc/explain_data.html

# I'm pretty sure that the Month-Year in monthly dat is calendar years and months.


#-------------------------------------------------------------------------------
# Annual data

# Mitch uses Section C in the Fall survey report and states that it is the inflow to
# the MD portion of Chesapeake Bay, so maybe just use section C? Could not find
# the definitions of the sections online.

annual_dat_MD <- annual_dat %>% 
                    select(Water.Year, Section.C) %>% 
                    filter(Water.Year >= 1991) #estimate mortality starting in 1991
with(annual_dat_MD, barplot(height = Section.C, names.arg = Water.Year, ylab = "cfs", xlab = "Water year"))

#-------------------------------------------------------------------------------
#add classifications to plots of natural mortality. 
# visually, the flows follow the CB wide classification, except perhaps 2014/2015, 
#which have similar flows in MD, but one is classifide as "below avg" and another
# was classified as "above average". Still, this is a starting point.
colnames(class_dat) <- c("Year", "Streamflow", "Streamflow_Class")
#bind the classifications by year. 
M_dat <- left_join(M_dat, class_dat, by = "Year")

#Just an example with 1 noaa code for now
labs <- c("1991", "", "", "", "", "1996", "", "", "", "", "2001", "", "", "", "", "2006",
          "", "", "", "", "2011", "", "", "", "", "2016", "")
labs <- as.character(labs)
plot <- ggplot(data = subset(M_dat, NOAA_code == 537), aes(x = Year, y = Percentile_50.))+
            geom_rect(aes(xmin = Year - 0.5, xmax = Year +0.5, ymin = 0, ymax = 1, fill = Streamflow_Class))+
           #geom_ribbon(aes(x = SA_Year, ymax = MaxM_SA, ymin = MinM_SA), fill = "gray")+ #uncertainty SA model
           #geom_line(aes(x = SA_Year, y = M_SA), color = "black")+ #SA model
           geom_linerange(aes(ymin = Percentile_2.5., ymax = Percentile_97.5.), color = "black")+ #whiskers for mort. model results
           geom_crossbar(aes(ymin = Percentile_25., ymax = Percentile_75.), color = "black", fill = "#9ad0f3", width =0.5)+ #box for mort. model results
           #geom_point(aes(x = SA_Year, y = M_bx), shape = 21, fill = "red", color = "black", size =1)+ # box count mortality.
           #Need to modify below here to be specific for this plot:
           scale_fill_manual(values = c("aquamarine", "coral1","gray"))+
           scale_y_continuous(expand = c(0.01,0), limits = c(0,1))+
           scale_x_continuous(expand = c(0.01,0), #to get rid of the x axis space before the first year.
                              breaks = 1991:2017,#c(2000,2005,2010,2015), #specify where labels go
                              labels =  labs#c("2000-\n2001", "2005-\n2006", "2010-\n2011", "2015-\n2016")#labely by fishing season.
           )+
           theme_classic(base_size = 10)+
           theme(
               aspect.ratio = 4/6,
               plot.title = element_text(hjust = 0.5, size = 10), #center
               axis.title.x = element_blank(),         #No x axis lab
               axis.title.y = element_blank())+        #no y axis lab
           ggtitle("537")

#Just looking at this, it looks like there were more years classified as above or
#below normal before 2003, and most years have had "NORmal" flow. HOwever, mortality
# was not as high in the two years that were classified as "Below normal" 
# (would be more bulletproof to look at flow JUST for MD, maybe, instead of for the
# whole chesapeake bay. )


# Plot class dat by itself----------------------------------------

ggplot(class_dat,aes(y = Annual.Mean.Streamflow..cfs., x = Water.Year)) +
    geom_col(aes(fill = Classification.of.Streamflow))
