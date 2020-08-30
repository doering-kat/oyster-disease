# Header -----------------------------------------------------------------------
# 
# Select the optimal model from 10_Run_temp_sal_window_change
# 
# Created 7 Jan 2019 by Kathryn Doering
# 
# TODO: clean up this code; think about how best to represent the differences
# among models and how to visualize it.
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

save_dir_name_fits<- "./Derived_Data/PostThesis_SEM/10_Run_temp_sal_window_change"
file_date_fits <- "2019_01_07"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))
datasets <- readRDS(paste0(save_dir_name_fits, "/lavaan_data_", file_date_fits, ".rds"))

#scenarios
scen <- read.csv("./Data/scenarios_temp_sal_window_2019_01_07.csv")

# Create folder for Derived data -----------------------------------------------
if(write_files == T){
    write_derived_dat_path <- "./Derived_Data/PostThesis_SEM/11_Model_Comparisons_temp_sal_window_change"
    dir.create(write_derived_dat_path)
}
# Get group names --------------------------------------------------------------
grp_names <- names(fits)

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

# Fit measures -----------------------------------------------------------------
allfit <- data.frame()
for (i in 1:length(fits)){
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
    print(paste0(write_derived_dat_path, "/all_r2_df.csv written"))
} else{
    print(paste0(write_derived_dat_path, "/all_r2_df.csv not written"))
}

# Compare all models to the base model (scenario 17) ----------------------------
# Functions 

#calc change (end - beginning) within a salinity group
#input: 1 parTable
Calc_Change <- function(parTable){
      change_parTable <-  parTable %>%
                          select(lhs,op,rhs,group, est, group_name) %>%  #just the essentials
                          filter(op == "~") %>% #get just the coefficients
                          mutate(relationship = paste0(lhs, op, rhs)) %>% 
                          select(est, group_name, relationship) %>% 
                          #break up year and salinity zon groups into separate columns
                          # mutate(years = substr(group_name, start = 1, stop = 6)) %>%
                          # mutate(sal_zone = substring(group_name, first = 7)) %>% 
                          select(relationship, group_name, est) %>% 
                          spread(group_name, est) %>% 
                          mutate(Change_Low = `02to17Low` - `91to02Low`) %>% 
                          #mutate(Change_Low_Per =100*(`02to17Low` - `91to02Low`) / `91to02Low`) %>% 
                          mutate(Change_MedHigh = `02to17MedHigh` - `91to02MedHigh`) # %>% 
                          #mutate(Change_MedHigh_Per =100*(`02to17MedHigh` - `91to02MedHigh`) / `91to02MedHigh`) 
  return(change_parTable)
}

# Combind output from runs of Calc_Change (all put in a list) into 1 dataframe
Combine_Dfs <- function(dflist){
    #check inputs
    if(!is.list(dflist)) stop("Input is not a list. Make input a list of dataframes.")
    for (i in 1:length(dflist)){
        tmp_df <- dflist[[i]]
        if(!is.data.frame(tmp_df)) stop("Not all list elements are dataframes.")
    }
    # combine outputs from Calc_Change
    df_all <- data.frame()
    for(i in 1:length(dflist)){
        tmp_df <- dflist[[i]]
        tmp_name <- names(dflist)[i]
        # keep only the relationship and change columns
        tmp_df <-  tmp_df %>% 
            mutate(Scenario = tmp_name) %>% # add scenario name
            # choose and order rows
            select(relationship, Scenario, starts_with("Change"))
        # put change numbers in 1 column
        tmp_df <- gather(tmp_df, "SalZone", "Change", 3:ncol(tmp_df))
        # add to dataframe
        df_all <- bind_rows(df_all, tmp_df)
    }
    return(df_all)
}
# calculate the diference between the change in coefficients between 2 parameter
# tables.  (NOTE: not used in this script, but may be useful in the future)
Calc_Mod_Diff <- function(change_parTable_new, change_parTable_base){
    #Make sure relationships are in the same order in both dataframes")
    #calculate difference
    diff_Low <- change_parTable_new$Change_Low - change_parTable_base$Change_Low
    diff_MedHigh<- change_parTable_new$Change_MedHigh - change_parTable_base$Change_MedHigh
    # create a new dataframe to store. 
    diff_df <- data.frame(relationship = change_parTable_base$relationship,
        coef_mod_diff_Low = diff_Low,
        coef_mod_diff_MedHigh = diff_MedHigh)
    return(diff_df)
}

# calculate change: 
change_list <- map(parTable_list, Calc_Change)# list()
# make descriptive names based on temp and sal
scen_names <-  scen %>% 
                  mutate(scen_names = paste0("temp_", temp_start, "-" ,temp_end, "; sal_", sal_start,"-", sal_end)) %>% 
                  select(scen_names)
scen_names <- scen_names$scen_names
names(change_list) <- scen_names #set names

# put change in 1 dataframe
change_df <- Combine_Dfs(change_list)


# Try a graphic
ggplot(change_df, aes(x = Scenario, y = Change))+
    geom_col(aes(fill = SalZone), position = "dodge")+
    facet_wrap(~relationship)+
    theme_classic()
# not that informative; may need to break up more. 

#need to clean up visualization, but save dataframe for now.
if(write_files == T){
    saveRDS(change_list, paste0(write_derived_dat_path, "/coef_change_scen_list.rda"))
    write.csv(change_df, paste0(write_derived_dat_path, "/coef_change_df_all_scen.csv"), row.names = F)
}
