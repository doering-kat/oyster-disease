# Header -----------------------------------------------------------------------
# 
# Select the optimal model from 5_Run_M_by_Bar_dermo_intensity.R
# 
# Also, plot results of the chosen model.
# 
# Created 4 Jan 2019 by Kathryn Doering
# 
# Load Packages-----------------------------------------------------------------
library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
options(stringsAsFactors = F)
write_files <- T #set to false if do not want to save new data.

# Load data and results --------------------------------------------------------
# Results from SEM.
save_dir_name_fits<- "./Derived_Data/PostThesis_SEM/5_Run_M_by_Bar_dermo_intensity"
file_date_fits <- "2019_01_04"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))

# list of group names for each of the 9 models (in same order as fits)
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# load the data used to make these models
file_date_dat <- "2019_01_02"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date_dat, ".csv"))

# Create folder for Derived data -----------------------------------------------
if(write_files == T){
    write_derived_dat_path <- "./Derived_Data/PostThesis_SEM/6_Model_Comparisons_M_by_Bar_dermo_intensity"
    dir.create(write_derived_dat_path)
}

# Compare AICs,BICs-------------------------------------------------------------
# AIC can be used for model selection, although a corrected AIC may be better (it
# is corrected for the sample size)
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    AICval[i] <- AIC(fits[[i]])
}
#names(AICval) <- grp_names
dAICval <- AICval - min(AICval)

AICdf <- data.frame(model = grp_names, AIC = AICval, dAIC = dAICval)

if(write_files == T){
    write.csv(AICdf, paste0(write_derived_dat_path, "/AICdf_model_run_",file_date_fits, ".csv"), row.names = F)
}

# Is the same found using BIC? 
BICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    BICval[i] <- BIC(fits[[i]])
}
#names(BICval) <- grp_names

dBICval <- BICval - min(BICval)
mod_sel_df <- AICdf #create a new df with all mod comparison measures
mod_sel_df$BIC <- BICval
mod_sel_df$dBIC <- dBICval

# try the corrected BIC val also.
BIC2val <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    BIC2val[i] <- fitMeasures(fits[[i]], fit.measures = "bic2")
}
#names(BIC2val) <- grp_names
dBIC2 <- BIC2val - min(BIC2val) #calculate change from min.
# add to model comparison 
mod_sel_df$BIC2 <- BIC2val
mod_sel_df$dBIC2 <- dBIC2

#add corrected AIC
moreFitIndices(fits[[1]], fit.measures = "all")
AICcval <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    AICcval[i] <- moreFitIndices(fits[[i]], fit.measures = "aic.smallN")
}
#names(BIC2val) <- grp_names
dAICc <- AICcval - min(AICcval) #calculate change from min.
# add to model comparison 
mod_sel_df$AICc <- AICcval
mod_sel_df$dAICc <- dAICc

#add hqc, another model selection measure for SEM
hqcval <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    hqcval[i] <- moreFitIndices(fits[[i]], fit.measures = "hqc")
}
#names(BIC2val) <- grp_names
dhqc <- hqcval - min(hqcval) #calculate change from min.
# add to model comparison 
mod_sel_df$hqc <- hqcval
mod_sel_df$dhqc <- dhqc


if(write_files == T){
    write.csv(mod_sel_df, paste0(write_derived_dat_path, "/model_selection_model_run_",file_date_fits, ".csv"), row.names = F)
}
# Explore model fit diagnostics ------------------------------------------------

# 
# Look at the fit measures for the largest model
fitMeasures(fits[[9]])
grp_names
names(fits) <- grp_names
fitMeasures(fits[["ts_2_spat_2"]])

# Fit measures -----------------------------------------------------------------
allfit <- data.frame()
for (i in 1:9){
    tmp_fit <- fitMeasures(fits[[i]], fit.measures = c("rmsea", "srmr", "cfi"))
    allfit <- bind_rows(allfit, tmp_fit)
}
# add model names
allfit$model <- grp_names

# output as a .csv.
if(write_files==T){
    write.csv(allfit, paste0(write_derived_dat_path, "/fit_measures.csv"), row.names = F)
}

# make Partables ---------------------------------------------------------------
# use parTable to get coef estimates for each of the models.
parTable_list <- list()
for (i in 1:length(fits)){
    tmp_name <- grp_names[i] #model name
    tmp_grp_labels <- lavInspect(fits[[i]], "group.label")
    if(is.na(tmp_name)){
        tmp_name <- "No_Grps"
        tmp_grp_labels <- "No_Grps"
    }
    tmp_grp_labels_df <- data.frame(group = 1:length(tmp_grp_labels),
        group_name = tmp_grp_labels)
    tmp_table <- parTable(fits[[i]]) #model results par est.
    #add group names to the tmp_table
    tmp_table <- left_join(tmp_table, tmp_grp_labels_df, by = "group")
    #add to list
    parTable_list[[i]] <- tmp_table
    #save to .csv.
    if(write_files == T){
        write.csv(tmp_table, paste0(write_derived_dat_path,"/par_table_", tmp_name ,"_", file_date_fits, ".csv"))
        #     write.csv(tmp_grp_labels, paste0(write_derived_dat_name,"/grp_key_", tmp_name ,"_", file_date_fits, ".csv"))
    }
}

# R2 ---------------------------------------------------------------------------
# R2 values for 2 t, 2 sp groups, and 2 t, 3 sp groups ONLY
#get r2 values 2t2s.

# get the r2 values for a fitted lavaan model with multiple groups, and format
# the information as a dataframe.
get_r2_df <- function(fits) {
    require(lavaan)
    require(tidyverse)
    r2 <- lavInspect(fits, what = "r2")
    #reformat output to a dataframe
    if(is.list(r2)){ #  f
        r2_df <- data.frame(group = names(r2)) #group names
        for( i in 1:3) { # loop through the columns to make a dataframe
            tmp_col <- purrr::map(r2,~.x[i]) %>%  #get first element of each list(group)
                purrr::flatten_dbl()
            tmp_col_name <- names(tmp_col)[1] #names should be all the same so take the first
            r2_df[,(1+i)] <- tmp_col #add elements to a new column
            names(r2_df)[1+i] <- tmp_col_name #add name of the column
        }
    } else { #convert to a dataframe with 1 row, and add a group column as the first col.
        tmp_r2_df <- r2 %>% 
            matrix() %>% 
            t() %>% 
            data.frame()
        colnames(tmp_r2_df) <- names(r2)
        tmp_grp <- data.frame(group = "all") #make the group col
        r2_df <- dplyr::bind_cols(tmp_grp, tmp_r2_df) # bind group col with others
    }
    return(r2_df)
}

# Get r2 values for all models.
# 
all_r2 <- map(fits, get_r2_df) #get for all models

# add a new column that has the model name
add_new_col <- function(df,col_dat){
    if(is.na(col_dat)){
        col_dat <- "no_groups"
    }
    df$model <- as.character(col_dat)
    return(df)
}

#make into a single dataframe
all_r2_df <- map2(all_r2, as.character(grp_names), add_new_col)
all_r2_df <- do.call("rbind", all_r2_df)

#save 
if(write_files == T){
    write.csv(all_r2_df, paste0(write_derived_dat_path, "/all_r2_df.csv"), row.names = F)
    print(paste0(write_derived_dat_path, "/all_r2_df.csv rewritten"))
} else{
    print(paste0(write_derived_dat_path, "/all_r2_df.csv not written"))
}
