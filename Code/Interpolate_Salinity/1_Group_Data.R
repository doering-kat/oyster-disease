# Header -----------------------------------------------------------------------
# Make datasets of CBP data
# Separate by Variable and cruise and Layer.
# Created 18 Sept 2018 by Kathryn Doering

# Load packages, set options ---------------------------------------------------
library(data.table) #for importing trib data, which has some issue
library(tidyverse)
options(stringsAsFactors = F)
# Load data --------------------------------------------------------------------
# Use fread to discard single-line footer, as well as mend some improper quoting,
# which read.csv is not able to fix. ()
main_WQ_dat <- fread("./Data/CBP_WQ_Data_18_Sept_2018/WaterQualityData_Tidal_Main_AllStations_1_1_1988_to_6_1_2018.csv", data.table = F)

# There appears to be a bug , where the comment SAVS PRESENT AT STATION has an 
# extra quote, therefore messing up the reading. fread is able to repair it.
trib_WQ_dat <- fread("./Data/CBP_WQ_Data_18_Sept_2018/WaterQualityData_Tidal_Trib_AllStations_1_1_1988_to_6_1_2018.csv",  data.table = F)

# Probably won't do anything with the special data for now, but may want in the
# future
spec_WQ_dat <- fread("./Data/CBP_WQ_Data_18_Sept_2018/WaterQualityData_Tidal_Special_AllStations_1_1_1988_to_6_1_2018.csv", data.table = F)

# Manipulate data --------------------------------------------------------------

main_trib_WQ_dat <- bind_rows(main_WQ_dat, trib_WQ_dat)

# Get nested data frames grouped by Parmeter, Cruise, and Layer
nested_WQ_df <- main_trib_WQ_dat %>% 
                group_by(Parameter, Cruise, Layer) %>% 
                nest()
# get unique attributes
attributes <- c("Parameter", "Cruise", "Layer")
unique_attr <- map(attributes, function(.x) unique(nested_WQ_df[,.x]))

# Write to files ---------------------------------------------------------------
#create directory
date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
date <- paste0(substr(date,start = 1,stop =4), "_", 
               substr(date, start = 6, stop = 7), "_", 
               substr(date, start = 9, stop = 10))
dir_name <- paste0("./Derived_Data/CBP_WQ_Grouped_Parameter_Cruise_Layer_",date)
dir.create(dir_name)

# Write date in nested df to individual .CSV files.
# Make csv names
csv_names <- paste0(dir_name,"/WQ_", 
                    nested_WQ_df$Parameter,"_",
                    nested_WQ_df$Cruise, "_",
                    nested_WQ_df$Layer, ".csv")

csv_names <- gsub(" ", "", csv_names, fixed =T) #get rid of all white space
#write each nested data set to a .csv (WARNING: takes ~ 5 min)
map2(nested_WQ_df$data, csv_names, write.csv)

#save the nested dataframe as an rds file.
saveRDS(nested_WQ_df, file = paste0(dir_name,"/WQ_Nested_all.rda"))
# save the flat data frame as an rds
saveRDS(main_trib_WQ_dat, file = paste0("./Derived_Data/CBP_WQ_Main_Trib_", date,".rda"))
nested_WQ_df<- readRDS(paste0(dir_name,"/WQ_Nested_all.rda"))


#save unique attributes to files
att_names <-  paste0(dir_name,"/WQ_unique_", attributes, ".csv")
future_map2(unique_attr, att_names, write.csv)
