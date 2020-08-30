#Explore kriging results

# Load packages and set options ------------------------------------------------
library(tidyverse)
library(automap)
library(sp)
library(maptools) # too read in polygon
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
#folder where the kriging derived data is located.
save_date <- "2018_09_20" #fill in this string with the date, format 2018_09_19
dir_name_dat <- paste0("./Derived_Data/Run_Kriging_S_WTEMP_Output_",save_date)

# load the nesting data containing all kriging results, as well as predictions 
# of interest
nested_df <- readRDS(paste0(dir_name_dat, "/WQ_Nested_S_WTEMP_Kriged_Predicted.rds"))

# #named chesapeake, CBpolygons (confusing name)
# load("./Data/chesapeake_salinity_data.rda")
# #simplified CB shoreline (from stats class)
# shoreline <- readShapePoly(fn="./Data/cbdata/CBnet_Shoreline_simplify.shp")
# Make plotting directory ------------------------------------------------------
dir_name_figures <- paste0("./Figures/Run_Kriging_S_WTEMP_Output_",save_date)
dir.create(dir_name_figures)

# Make Cruise/date lookup table ------------------------------------------------
# Get min and max date, as well as year for each data set.
SummarizeDates <- function(x, Type = "min"){
    dates <- as.Date(x$SampleDate, format = "%m/%d/%Y")
    if(Type == "min"){
        return_date <- min(dates)
        return_date <- format(return_date, "%m/%d/%Y")
    }
    if(Type == "max"){
        return_date <- max(dates)
        return_date <- format(return_date, "%m/%d/%Y")
    }
    if(Type == "year"){ #should not be more than 1 yr per cruise?
        return_date <- lubridate::year(dates[1]) # year of the first date
    }
    return(return_date)
}
# Get dates in order of the nested data frame. Can then bind to the df.
min_date <- map(nested_df$data, SummarizeDates, Type = "min") %>% 
    flatten_chr()
max_date <- map(nested_df$data, SummarizeDates, Type = "max") %>% 
    flatten_chr()
year <- map(nested_df$data, SummarizeDates, Type = "year") %>% 
    flatten_dbl()
# make into a data frame.
date_lookup <- data.frame(cruise = nested_df$Cruise, 
                          min_date = min_date, 
                          max_date = max_date,
                          year = year
                          )
#write to a .csv file.
write.csv(date_lookup, paste0(dir_name_dat, "/cruise_date_lookup.csv"), row.names = F)

#TODO: check that the month is the same of min and max date for each cruise (may 
#Be easier to deal with?)


# Create kriging plots ---------------------------------------------------------
# #Make the directory.
dir_name_kriging <- paste0(dir_name_figures, "/Kriging_Plots")
dir.create(dir_name_kriging)

#create the kriging figures. 
#add kriging plots to the data frame.
# nested_df <- nested_df %>% 
#                 mutate(KrigePlots = map(Interpolated, Cruise, 
#                                     ~plot(.x$kriging_results, main = .y)))
#                                     
#  Try this to plot: if doesn't work, use a for loop.
for(i in 1:nrow(nested_df)){
    tmp_cruise <- nested_df$Cruise[i]
    tmp_kriging <- nested_df$Interpolated[[i]]$kriging_results
    pdf(paste0(dir_name_kriging, "/KrigePlot_", tmp_cruise, ".pdf"))
    print(plot(tmp_kriging))
    dev.off()
}

# same plots,but put all in 1 file with a header sheet.
nested_df_ordered <- arrange(nested_df, Cruise) #
pdf(paste0(dir_name_figures, "/KrigePlotsAll_ordered.pdf"))
for(i in 1:nrow(nested_df_ordered)){
    tmp_cruise <- nested_df_ordered$Cruise[i]
    tmp_kriging <- nested_df_ordered$Interpolated[[i]]$kriging_results
    #find the min and max dates for the cruise
    tmp_min <- date_lookup[date_lookup$cruise == tmp_cruise, "min_date"]
    tmp_max <- date_lookup[date_lookup$cruise == tmp_cruise, "max_date"]
    tmp_title <- paste0(tmp_cruise, ", from ", tmp_min, " to ", tmp_max)
    print(plot(1,1,main = tmp_title, axes = F, col = "white")) #header sheet
    print(plot(tmp_kriging))
}
dev.off()


# Make 1 CSV file of all the kriged results. Necessary information is the Bar, Cruise,
# Predicted values (point estimates and uncertainty.)
pred <- data.frame()
for(i in 1:nrow(nested_df)){
    tmp_pred <- nested_df$Predicted[[i]] %>%  # the predictions data
                    select(ID, PredVal, PredValVar, PredValSD)
    tmp_pred$cruise <- nested_df$Cruise[i] # add a column for cruise
    pred<- bind_rows(pred, tmp_pred) # bind the new obs to the previous ones.
}
# save
write.csv(pred, paste0(dir_name_dat, "/Predicted_S_WTEMP_all.csv"), row.names = F)
