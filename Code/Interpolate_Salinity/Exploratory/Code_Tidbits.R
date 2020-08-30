# Header: Code tidbits ----------------------------------------------------------
# Pieces of code written while developing kriging analysis that may be useful,
# but do not have a purpose yet.
# Created 20 Sept 2018 by Kathryn Doering

#get all the unique Layer, Cruise, and parameter names
attributes <- c("Parameter", "Cruise", "Layer")
unique_vals <- map(attributes, ~read.csv(paste0(dir_name_dat, "/WQ_unique_", .x, ".csv")))
names(unique_vals) <- attributes



# Add min and max date, as well as year, to the df------------------------------
#get it as lists:
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
min_dates <- map(test2$data, SummarizeDates, Type = "min") %>% 
    flatten_chr()
max_dates <- map(test2$data, SummarizeDates, Type = "max") %>% 
    flatten_chr()
year <- map(test2$data, SummarizeDates, Type = "year") %>% 
    flatten_dbl()

# bind these as col's to the nested dataframe.

# Look at nested kriging results -----------------------------------------------

# # some subsetting of the results (save code for now)
# #make plots.(could save these to pdf.)
# map(test$Kriging, function(x) plot(x$kriging_results$krige_output, axes = T))
# 
# plot(test$Kriging[[3]]$kriging_results$krige_output, axes = T)
# #save predictions
# tmp_loc_map <- test2$Predictions[[1]]$loc_sp_utm_rounded
# plot(tmp_loc_map, add = T)
# 
# tmp_pred <- test2$Predictions[[3]]$kriged_obs
# 
## Load data for 1 cruise -------------------------------------------------------
# start with just one cruise for now
tmp_layer <- "B " #note that did include white space as in the dataset
tmp_parameter <- "SALINITY"
tmp_cruise <- "BAY072"

tmp_dat_filename <- paste0(dir_name_dat, "/WQ_", tmp_parameter, "_", tmp_cruise, "_", tmp_layer, ".csv")
# remove whitespace from file name
tmp_dat_filename  <- gsub(" ", "", tmp_dat_filename, fixed =T)

tmp_dat <- read.csv(tmp_dat_filename)

#snippet to get nice plots of kriging results
plot(nested_dat_S_SAL$Interpolated[[1]]$kriging_results)
# may be nice to also plot locations of observations and desired output locations?
