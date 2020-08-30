# Header -----------------------------------------------------------------------
# problem cruises for salinity
# There were 11 kriged cruises of Surface salinity that had negative values. want
# to look at the datasets more carefully to try to understand why the kriging went
# poorly.
# created 25 Sept 2018 by KD.
# load packages ----------------------------------------------------------------
library(automap)
library(tidyverse)
options(stringsAsFactors = F)
# load data --------------------------------------------------------------------
problem_cruises <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/problem_cruises.csv")

# for plotting
#folder where the kriging derived data is located.
save_date <- "2018_09_20" #fill in this string with the date, format 2018_09_19
dir_name_dat <- paste0("./Derived_Data/Run_Kriging_S_SAL_Output_",save_date)
# load the nesting data containing all kriging results, as well as predictions 
# of interest
nested_df <- readRDS(paste0(dir_name_dat, "/WQ_Nested_S_SAL_Kriged.rds"))

#date lookup table
date_lookup <- read.csv("Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/cruise_date_lookup.csv")
    
# get problem cruise data sets (inputs to kriging)
problem_cruises <- problem_cruises$x
data_dir <- "./Derived_Data/CBP_WQ_Grouped_Parameter_Cruise_Layer_2018_09_18"
# read in the problem cruise data sets for surfaces
problem_data <- list()
for (i in 1:length(problem_cruises)){
    tmp_dat <- read.csv(paste0(data_dir, "/WQ_SALINITY_", problem_cruises[i], "_S.csv"))
    problem_data[[i]] <- tmp_dat #add to the list.
}
names(problem_data) <- problem_cruises

# get the kriging predictions for salinity
sal_pred_all <- read.csv("./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/Predicted_S_SAL_all.csv")

# see the summarized values ----------------------------------------------------
for (i in 1:length(problem_cruises)){
    print(summary(problem_data[[i]]$MeasureValue))
}
for (i in 1:length(problem_cruises)){
    print(nrow(problem_data[[i]]))
}

#nothing is explicitly wrong with the data, but the highly negative values likely 
#occur when predictions are made somewhat outside the range of the data set. the 
#range of the data is chosen by drawing a box around the min and max values for
#lon lat, without regard to the fact that there may be no samples for ex in the
#mainstem. this seems like something that will be hard to eal with, but maybe
#these 11 datasets should just be eliminated?
#
# plot problem cruise obs and kriging ------------------------------------------
# Add the data points that were used for kriging.
# same plots,but put all in 1 file with a header sheet.
dir.create("./Figures/ProblemCruises")
nested_df <- arrange(nested_df, Cruise) #
pdf("./Figures/ProblemCruises/KrigePlotsProblem.pdf")
for(i in problem_cruises){
    tmp_row <- which(nested_df$Cruise == i) #get row of the cruise
    tmp_kriging <- nested_df$Interpolated[[tmp_row]]$kriging_results
    #find the min and max dates for the cruise
    tmp_min <- date_lookup[date_lookup$cruise == i, "min_date"]
    tmp_max <- date_lookup[date_lookup$cruise == i, "max_date"]
    tmp_data <- nested_df$data[[tmp_row]]
    #create the points spatial object.
    tmp_pts_obs <- SpatialPointsDataFrame(coords = data.frame(lon = tmp_data$Longitude, lat = tmp_data$Latitude),              
                                          data = data.frame(Val = tmp_data$MeasureValue),
                                          proj4string = CRS('+proj=longlat'))
    tmp_pts_obs <- spTransform(tmp_pts_obs,
                     CRSobj = CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
    tmp_title <- paste0(i, ", from ", tmp_min, " to ", tmp_max)
    # plot.
    print(plot(1,1,main = tmp_title, axes = F, col = "white")) #header sheet
    print(plot(tmp_kriging, sp.layout = list(pts = list("sp.points", tmp_pts_obs))))
}
dev.off()

#get the number of negative observations ---------------------------------------
total_neg <- 0
n_problem <- rep(NA, length.out = length(problem_cruises))
for(i in problem_cruises){
    index <- which(problem_cruises == i)
    tmp_n <- sal_pred_all %>% 
        filter(cruise == i) %>% 
        filter(PredVal < 0) %>% 
        select(PredVal) %>% 
        na.omit() %>% 
        nrow()
    # print(i)
    # print(tmp_n)
    total_neg <- total_neg +tmp_n
    n_problem[index] <- tmp_n
}
#names(n_problem) <- problem_cruises #add names
n_problem_df <- data.frame(cruise = problem_cruises, n_negative = n_problem)
# save the number of problem obs.
write.csv(n_problem_df, "./Derived_Data/Run_Kriging_S_SAL_Output_2018_09_20/problem_cruises_n_negative_predicted.csv", row.names = F)
