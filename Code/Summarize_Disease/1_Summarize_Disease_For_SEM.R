# Header -----------------------------------------------------------------------

# Summarize Disease for first SEM analysis.
# Calculate Prevalances of MSX and Dermo on the bar level and save data/
# Written 26 Sept 2016 by Kathryn Doering
# ------------------------------------------------------------------------------

# Load Packages and set options
library(tidyverse)
options(stringsAsFactors = F)
source("./Code/Summarize_Disease/Functions_Summarize_Disease.R")

# Load the Data ----------------------------------------------------------------

DZ_df <- read.csv("./Derived_Data/DZDataSampleInfoJoin.csv") #previously created joined table in Explore_Disease_Data.R.

# Calculate Prevalances --------------------------------------------------------
# 
# Calculate prevalance by Bar in the regular disease samples.
dermo_prev <- CalcPrevalence(dis_df = DZ_df, disease = "Dermo")
MSX_prev <- CalcPrevalence(dis_df = DZ_df, disease = "MSX")

# calculate dermo weighted prevalance (avg. intensity) and percent lethal infections.
dermo_intensity <- CalcDermoIntensty(dis_df = DZ_df, disease = "Dermo" )
# Save Prevalances -------------------------------------------------------------
# Create directory
dir_name <- "./Derived_Data/Summarize_Disease"
# create directory
dir.create(dir_name)

#get date to use in file names.
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_",
    substr(date, start = 6, stop = 7), "_",
    substr(date, start = 9, stop = 10))

# save data as .csv files
write.csv(dermo_prev, paste0(dir_name, "/dermo_prevalance_ID_year_", date, ".csv"), row.names = F)
write.csv(MSX_prev, paste0(dir_name, "/MSX_prevalance_ID_year_", date, ".csv"), row.names = F)
write.csv(dermo_intensity,paste0(dir_name, "/dermo_mean_intensity_percent_lethal_ID_year_", date, ".csv"), row.names = F)


