# Header------------------------------------------------------------------------
# Explore available salinity data collected from the MD portion of Chesapeake Bay 
# during 1991 - 2017. 
# Available from Chesapeake Bay Program.
# Originially, we planned on using salinity from the ROMS model, but apparently
# it is not "good" in the tributaties, so exploring other options
# 
# Written 6 Sept 2018 by Kathryn Doering
# 
# Load Packages And set options-------------------------------------------------
library(tidyverse)
# library(data.table) # to read in a large .csv file
options(stringsAsFactors = F)
# Load Datasets ----------------------------------------------------------------
file_names <- list.files("./Data/CBF_water_qual/CBSeg_TWQM", pattern = "CBSeg2003")
dat <- list()
#read.delim()
#nrecords <- c(117133,90542,93905,97892,120285,619955)
for (f in 1:length(file_names)){
    #NOTE: not reading full file for tribs; will break up data into smaller files,
    # because I can't find a better solution for now.(tried read.delim and fread)
 dat[[f]] <- read.csv(paste0("./Data/CBF_water_qual/CBSeg_TWQM/",file_names[f]),
     nrows = 620000)
}
#add names based on part of the file names.
names(dat) <- substring(file_names, first = 11, last = 31)

dat <- map(dat, function(x) x[-nrow(x),]) #remove last row

#Work with Tribs data-----------------------------------------------------------
nrow(dat[[6]])
unique(dat[[6]]$Layer)
test <- dat[[6]] 

test <-  test %>% 
    filter(Parameter == "SALINITY" ) %>% 
    #filter(Layer == "S " ) %>% 
    group_by(Station) %>% 
    sample_n(1)
    #count()
    
#Bind together and work with  mainstem data-------------------------------------
dat_main <- data.frame()
for(i in 1:5){
    tmp_dat <- dat[[i]] %>% filter(Parameter == "SALINITY")
    dat_main <- bind_rows(dat_main, tmp_dat)
}


