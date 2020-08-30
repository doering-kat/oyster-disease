# Look at stats from the database about the surface layer observations.
# 
# Wanted to know for Ch. 2 writeup
# Created 22 Octo 2018 by Kathryn Doering
# 
# Add packages and set options -------------------------------------------------
library(tidyverse)
options(stringsAsFactors = F)
# read in data -----------------------------------------------------------------
dat <- readRDS("./Derived_Data/CBP_WQ_Main_Trib_2018_09_18.rda")
# manipulate data --------------------------------------------------------------
# 
# Get the surface data and find the number by depth.
sum_dat <- dat %>% 
            filter(Layer == "S ") %>% 
            select(Depth) %>% 
            group_by(Depth) %>% 
            count()
# create a histogram for number of samples by depth (this is for both wtemp and
# salinity)
dat %>% 
    filter(Layer == "S ") %>% 
    select(Depth) %>% 
    hist()

# find the percentage of samples collected at each depth    
tot_obs <- sum(sum_dat$n)
sum_dat <- sum_dat %>% 
    mutate(per_n = 100*n/tot_obs) #percentage.

# look at the percent of samples that are collected at depths of less than 1 m
# (should be 99.9)
n_less_1m <- sum_dat %>% 
                filter(Depth <= 1) %>% 
                ungroup(Depth) %>% 
                select(per_n) %>% 
                sum()
