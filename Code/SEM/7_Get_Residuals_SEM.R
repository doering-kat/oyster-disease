# Calculate the model residuals given the model results. Do this for the first 
# model to start.
# 
# There appears to be no easy to use function to do this in lavaan, so need to 
# write a separate function for it.
#
# Written 10/30/2018 by KD
# 
# Load packages and set options ------------------------------------------------

library(lavaan)
library(tidyverse)
library(lazyeval) # to use column name as arguement in function.
library(semPlot)
options(stringsAsFactors = F)
write_files <- T #set to false if do not want to save new data.
# Load data and results --------------------------------------------------------
# Results from SEM.
save_dir_name_fits<- "./Derived_Data/SEM/5_SEM_spatial_temporal_models"
file_date_fits <- "2018_10_17"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))
# list of group names for each of the 9 models (in same order as fits)
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# load the data used to make these models
file_date_dat <- "2018_10_17"
dat <- read.csv(paste0("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_", file_date_dat, ".csv"))

# Get residuals, model 1 -------------------------------------------------------
# This is the model that estimates with no space/time separate models

# sample_dat <- dat %>%
#                 slice(1:10) %>% 
#                 select(sal, temp_s, MSX_Prev_Per, Dermo_Prev_Per, M_Med_Annual_Per)
sample_dat <- dat

mod_1_par <- parTable(fits[[1]])



# # predict temp
# temp <- mod_1_par %>% 
#             filter(lhs == "temp_s") %>% 
#             filter(op == "~" | op == "~1") %>% 
#             select(lhs, op, rhs, est)

# predict Dermo
dermo_coef <- mod_1_par %>% 
            filter(lhs == "Dermo_Prev_Per") %>% 
            filter(op == "~" | op == "~1")
sample_dat <- sample_dat %>% 
                  mutate(pred_dermo = 
                          dermo_coef[dermo_coef$op == "~1","est"] + #intercept
                          dermo_coef[dermo_coef$rhs == "temp_s","est"]*temp_s +
                          dermo_coef[dermo_coef$rhs == "sal","est"]*sal)
# predict MSX (using observed temp and salinity)
MSX_coef <-  mod_1_par %>% 
                filter(lhs == "MSX_Prev_Per") %>% 
                filter(op == "~" | op == "~1")
sample_dat <- sample_dat %>% 
                mutate(pred_MSX = 
                        MSX_coef[MSX_coef$op == "~1","est"] + #intercept
                        MSX_coef[MSX_coef$rhs == "temp_s","est"]*temp_s +
                        MSX_coef[MSX_coef$rhs == "sal","est"]*sal)

# dermo_inter <- mod_1_par %>% 
#                 filter(lhs == "temp_s"| lhs == "sal") %>% 
#                 filter(op == "~1") %>% 
#                 select(lhs, op, rhs, est)

# predict M (using observed MSX and dermo)
M_coef <- mod_1_par %>% 
            filter(lhs == "M_Med_Annual_Per") %>% 
            filter(op == "~" | op == "~1")

sample_dat <- sample_dat %>% 
                mutate(pred_M = 
                        M_coef[M_coef$op == "~1","est"] + #intercept
                        M_coef[M_coef$rhs == "MSX_Prev_Per","est"]*MSX_Prev_Per +
                        M_coef[M_coef$rhs == "Dermo_Prev_Per","est"]*Dermo_Prev_Per)

# predict M (using predicted MSX and dermo) - is this useful? 
sample_dat <- sample_dat %>% 
                mutate(pred_M_use_pred = 
                            M_coef[M_coef$op == "~1","est"] + #intercept
                            M_coef[M_coef$rhs == "MSX_Prev_Per","est"]*pred_MSX +
                            M_coef[M_coef$rhs == "Dermo_Prev_Per","est"]*pred_dermo)

# calculate residuals 
sample_dat <- sample_dat %>% 
                mutate(MSX_resid = MSX_Prev_Per - pred_MSX) %>% 
                mutate(Dermo_resid = Dermo_Prev_Per- pred_dermo) %>% 
                mutate(M_resid = M_Med_Annual_Per - pred_M) %>% 
                mutate(M_resid_pred_M_use_pred = M_Med_Annual_Per - pred_M_use_pred)

# plot residuals
# make folder
resid_fig_folder_name <-  "./Figures/SEM/7_Get_Residuals_SEM"
dir.create(resid_fig_folder_name)

pdf(paste0(resid_fig_folder_name, "/NoGroupsResid.pdf"))
par(mfrow = c(2,2))
plot(sample_dat$MSX_resid ~ sample_dat$MSX_Prev_Per, main ="MSX")
plot(sample_dat$Dermo_resid ~ sample_dat$Dermo_Prev_Per, main = "Dermo")
plot(sample_dat$M_resid ~ sample_dat$M_Med_Annual_Per, main = "M")
plot(sample_dat$M_resid_pred_M_use_pred ~ sample_dat$M_Med_Annual_Per, main = "M, pred. MSX and Dermo")

plot(sample_dat$MSX_resid, main ="MSX")
plot(sample_dat$Dermo_resid, main = "Dermo")
plot(sample_dat$M_resid, main = "M")
plot(sample_dat$M_resid_pred_M_use_pred, main = "M (pred MSX/Dermo)")

hist(sample_dat$MSX_resid)
hist(sample_dat$Dermo_resid)
hist(sample_dat$M_resid)
hist(sample_dat$M_resid_pred_M_use_pred)
dev.off()


# predict MSX, dermo, using the mean temp and salinity estimates (is this useful?) not sure.
# mean temp, sal from data set is just included as temp_s ~ 1 and sal ~ 1 in the 
# 
# 
# Predicted values functions for grouped models --------------------------------
# 
# Get table of predictions from lavaan fitted model, including the names of the
# groups. 
# lavaan_fit: a grouped model.
# returns: a dataframe that can be used as input to GetPreds.
GetParTable <- function(lavaan_fit){
    modpar <- parTable(lavaan_fit) # get the model parameters
    #get the group labels and put numbers and names  in a dataframe.
    grp_labels <- lavInspect(lavaan_fit, "group.label")
    grp_labels_df <- data.frame(group = 1:length(grp_labels),
        group_name = grp_labels)
    #add group names to the model parameter table
    modpar  <- left_join(modpar , grp_labels_df, by = "group")
    modpar 
}
# GetPreds: get predicted values for each of MSX, dermo, M and M (assuming the 
# predicted values of dermo/MSX)
# dat: dataframe with columns MSX_Prev_Per, Dermo_Prev_Per, M_Med_Annual_Per, sal, temp_s,
# and an additional column col_name (specify name in col_name argument) with the
# model grouping variables
# col_name: column including grouping variables. Must be the same groups in dat as in
# the partable.
# partable: output from GetParTable.
# which_preds: unused right now, but could eventually be used to specify which 
# returns are desired.
# Returns: a dataframe with new columns pred_Dermo, pred_MSX, pred_M, pred_M_use_pred
GetPreds <- function(dat, col_name, partable, which_preds = c("MSX", "dermo", "M", "M_MSX_dermo_pred")){
    require(tidyverse)
    require(lazyeval)
    #get the group names
    ngrps <- max(partable$group) #find total number of groups.
    grp_names <- unique(partable$group_name) #get the group names.
    if (ngrps != length(grp_names)){
        stop("check number of groups")
    }
    # loop through groups to calculate predicted values for each.
    pred_df <- data.frame() # df to hold results
    for(g in grp_names){
        #need to us
        filter_criteria <-lazyeval::interp(~y == x, .values=list(y = as.name(col_name), x = g))
        tmp_dat <- dplyr::filter_(dat,filter_criteria) #get the data set for the group. 
        #calculate MSX pred
        # predict MSX (using observed temp and salinity)
        MSX_coef <-  partable %>% 
            filter(group_name == g) %>%  #get coefficients for the group
            filter(lhs == "MSX_Prev_Per") %>% 
            filter(op == "~" | op == "~1")
        tmp_dat <- tmp_dat %>% 
            mutate(pred_MSX = 
                    MSX_coef[MSX_coef$op == "~1","est"] + #intercept
                    MSX_coef[MSX_coef$rhs == "temp_s","est"]*temp_s +
                    MSX_coef[MSX_coef$rhs == "sal","est"]*sal)
        
        #calculate dermo pred
        Dermo_coef <-  partable %>% 
            filter(group_name == g) %>%  #get coefficients for the group
            filter(lhs == "Dermo_Prev_Per") %>% 
            filter(op == "~" | op == "~1")
        tmp_dat <- tmp_dat %>% 
            mutate(pred_Dermo = 
                    Dermo_coef[Dermo_coef$op == "~1","est"] + #intercept
                    Dermo_coef[Dermo_coef$rhs == "temp_s","est"]*temp_s +
                    Dermo_coef[Dermo_coef$rhs == "sal","est"]*sal)
        #calculate M pred
        M_coef <-  partable %>% 
            filter(group_name == g) %>%  #get coefficients for the group
            filter(lhs == "M_Med_Annual_Per") %>% 
            filter(op == "~" | op == "~1")
        tmp_dat <- tmp_dat %>% 
            mutate(pred_M = 
                    M_coef[M_coef$op == "~1","est"] + #intercept
                    M_coef[M_coef$rhs == "MSX_Prev_Per","est"]* MSX_Prev_Per +
                    M_coef[M_coef$rhs == "Dermo_Prev_Per","est"]*Dermo_Prev_Per)
        #calculate M pred (use predicted values for MSX, dermo.)
        tmp_dat <- tmp_dat %>% 
            mutate(pred_M_use_pred = 
                    M_coef[M_coef$op == "~1","est"] + #intercept
                    M_coef[M_coef$rhs == "MSX_Prev_Per","est"]*pred_MSX +
                    M_coef[M_coef$rhs == "Dermo_Prev_Per","est"]*pred_Dermo)
        # as a check, as g as another column
        tmp_dat$group_name_check <- g
        #add the results to the full group model data frame
        pred_df <- bind_rows(pred_df, tmp_dat)
    }
    return(pred_df) 
}
# Residuals for model 9 -------------------------------------------------

mod_9_par <- GetParTable(fits[[9]])

#Get predicted for model 9 Ithe full model)
mod_9_pred <- GetPreds(dat = dat, col_name = "ts_4_spat_3", partable = mod_9_par)

# calculate residuals
mod_9_resid <- mod_9_pred %>% 
                mutate(MSX_resid = MSX_Prev_Per - pred_MSX) %>% 
                mutate(Dermo_resid = Dermo_Prev_Per- pred_Dermo) %>% 
                mutate(M_resid = M_Med_Annual_Per - pred_M) %>% 
                mutate(M_resid_use_pred = M_Med_Annual_Per - pred_M_use_pred)

#make plots
pdf(paste0(resid_fig_folder_name, "/TS_4_spat_3_resids.pdf"))

# commented out: residual v observed, not useful plots. 
# print(ggplot(mod_9_resid, aes(y = MSX_resid, x = MSX_Prev_Per))+
#     geom_smooth()+
#     geom_point(aes(color = ts_4_spat_2))+
#     theme_classic()+
#     ggtitle("MSX residuals"))
# 
# print(ggplot(mod_9_resid, aes(y = Dermo_resid, x = Dermo_Prev_Per))+
#         geom_smooth()+
#         geom_point(aes(color = ts_4_spat_2))+
#         theme_classic()+
#         ggtitle("Dermo residuals"))
# 
# print(ggplot(mod_9_resid, aes(y = M_resid, x = M_Med_Annual_Per))+
#         geom_smooth()+
#         geom_point(aes(color = ts_4_spat_2))+
#         theme_classic()+
#         ggtitle("M residuals"))
# 
# print(ggplot(mod_9_resid, aes(y = M_resid_use_pred, x = M_Med_Annual_Per))+
#         geom_smooth()+
#         geom_point(aes(color = ts_4_spat_2))+
#         theme_classic()+
#         ggtitle("M residuals, using predicted MSX and Dermo"))

par(mfrow = c(2,2))
hist(mod_9_resid$MSX_resid)
hist(mod_9_resid$Dermo_resid)
hist(mod_9_resid$M_resid)
hist(mod_9_resid$M_resid_use_pred)

dev.off()

pdf(paste0(resid_fig_folder_name, "/TS_4_spat_3_resids_by_group.pdf"))

print(ggplot(mod_9_resid, aes(y = MSX_resid, x = MSX_Prev_Per))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_4_spat_3)+
        ggtitle("MSX residuals"))

print(ggplot(mod_9_resid, aes(y = Dermo_resid, x = Dermo_Prev_Per))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_4_spat_3)+
        ggtitle("Dermo residuals"))

print(ggplot(mod_9_resid, aes(y = M_resid, x = M_Med_Annual_Per))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_4_spat_3)+
        ggtitle("M residuals"))

print(ggplot(mod_9_resid, aes(y = M_resid_use_pred, x = M_Med_Annual_Per))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_4_spat_3)+
        ggtitle("M residuals, using predicted MSX and Dermo"))
dev.off()


pdf(paste0(resid_fig_folder_name, "/TS_4_spat_3_resids_v_pred.pdf"))

print(ggplot(mod_9_resid, aes(y = MSX_resid, x = pred_MSX))+
        geom_smooth()+
        geom_point(aes(color = ts_4_spat_3))+
        theme_classic()+
        ggtitle("MSX residuals"))

print(ggplot(mod_9_resid, aes(y = Dermo_resid, x = pred_Dermo))+
        geom_smooth()+
        geom_point(aes(color = ts_4_spat_3))+
        theme_classic()+
        ggtitle("Dermo residuals"))

print(ggplot(mod_9_resid, aes(y = M_resid, x = pred_M))+
        geom_smooth()+
        geom_point(aes(color = ts_4_spat_3))+
        theme_classic()+
        ggtitle("M residuals"))

print(ggplot(mod_9_resid, aes(y = M_resid_use_pred, x = pred_M_use_pred))+
        geom_smooth()+
        geom_point(aes(color = ts_4_spat_3))+
        theme_classic()+
        ggtitle("M residuals, using predicted MSX and Dermo"))

dev.off()

# Residuals for 2 time groups, 3 spatial groups --------------------------------
# 
mod_7_par <- GetParTable(fits[[7]]) # for 3 spatial, 2 temporal groups.

#Get predicted for model 9 Ithe full model)
mod_7_pred <- GetPreds(dat = dat, col_name = "ts_2_spat_3", partable = mod_7_par)

# calculate residuals
mod_7_resid <- mod_7_pred %>% 
                mutate(MSX_resid = MSX_Prev_Per - pred_MSX) %>% 
                mutate(Dermo_resid = Dermo_Prev_Per- pred_Dermo) %>% 
                mutate(M_resid = M_Med_Annual_Per - pred_M) %>% 
                mutate(M_resid_use_pred = M_Med_Annual_Per - pred_M_use_pred)

#make plots
pdf(paste0(resid_fig_folder_name, "/TS_2_spat_3_hist_resids.pdf"))

par(mfrow = c(2,2))
hist(mod_7_resid$MSX_resid)
hist(mod_7_resid$Dermo_resid)
hist(mod_7_resid$M_resid)
hist(mod_7_resid$M_resid_use_pred)

dev.off()

pdf(paste0(resid_fig_folder_name, "/TS_2_spat_3_resids_by_group.pdf"))

print(ggplot(mod_7_resid, aes(y = MSX_resid, x = pred_MSX))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_3)+
        ggtitle("MSX residuals"))

print(ggplot(mod_7_resid, aes(y = Dermo_resid, x = pred_Dermo))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_3)+
        ggtitle("Dermo residuals"))

print(ggplot(mod_7_resid, aes(y = M_resid, x = pred_M))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_3)+
        ggtitle("M residuals"))

print(ggplot(mod_7_resid, aes(y = M_resid_use_pred, x = pred_M_use_pred))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_3)+
        ggtitle("M residuals, using predicted MSX and Dermo"))
dev.off()


pdf(paste0(resid_fig_folder_name, "/TS_2_spat_3_resids_v_pred.pdf"))

print(ggplot(mod_7_resid, aes(y = MSX_resid, x = pred_MSX))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_3))+
        theme_classic()+
        ggtitle("MSX residuals"))

print(ggplot(mod_7_resid, aes(y = Dermo_resid, x = pred_Dermo))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_3))+
        theme_classic()+
        ggtitle("Dermo residuals"))

print(ggplot(mod_7_resid, aes(y = M_resid, x = pred_M))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_3))+
        theme_classic()+
        ggtitle("M residuals"))

print(ggplot(mod_7_resid, aes(y = M_resid_use_pred, x = pred_M_use_pred))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_3))+
        theme_classic()+
        ggtitle("M residuals, using predicted MSX and Dermo"))

dev.off()

# Get and plot resid for 2 spatial and 2 temporal groups------------------------
#model 6
# 
mod_6_par <- GetParTable(fits[[6]]) # for 2 spatial, 2 temporal groups.

#Get predicted for model 9 Ithe full model)
mod_6_pred <- GetPreds(dat = dat, col_name = "ts_2_spat_2", partable = mod_6_par)

# calculate residuals
mod_6_resid <- mod_6_pred %>% 
    mutate(MSX_resid = MSX_Prev_Per - pred_MSX) %>% 
    mutate(Dermo_resid = Dermo_Prev_Per- pred_Dermo) %>% 
    mutate(M_resid = M_Med_Annual_Per - pred_M) %>% 
    mutate(M_resid_use_pred = M_Med_Annual_Per - pred_M_use_pred)

#make plots
pdf(paste0(resid_fig_folder_name, "/ts_2_spat_2_hist_resids.pdf"))

par(mfrow = c(2,2))
hist(mod_6_resid$MSX_resid)
hist(mod_6_resid$Dermo_resid)
hist(mod_6_resid$M_resid)
hist(mod_6_resid$M_resid_use_pred)

dev.off()

pdf(paste0(resid_fig_folder_name, "/ts_2_spat_2_resids_by_group.pdf"))

print(ggplot(mod_6_resid, aes(y = MSX_resid, x = pred_MSX))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_2)+
        ggtitle("MSX residuals"))

print(ggplot(mod_6_resid, aes(y = Dermo_resid, x = pred_Dermo))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_2)+
        ggtitle("Dermo residuals"))

print(ggplot(mod_6_resid, aes(y = M_resid, x = pred_M))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_2)+
        ggtitle("M residuals"))

print(ggplot(mod_6_resid, aes(y = M_resid_use_pred, x = pred_M_use_pred))+
        geom_smooth(method = lm)+
        geom_point()+
        theme_classic()+
        facet_wrap(~ts_2_spat_2)+
        ggtitle("M residuals, using predicted MSX and Dermo"))
dev.off()


pdf(paste0(resid_fig_folder_name, "/ts_2_spat_2_resids_v_pred.pdf"))

print(ggplot(mod_6_resid, aes(y = MSX_resid, x = pred_MSX))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_2))+
        theme_classic()+
        ggtitle("MSX residuals"))

print(ggplot(mod_6_resid, aes(y = Dermo_resid, x = pred_Dermo))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_2))+
        theme_classic()+
        ggtitle("Dermo residuals"))

print(ggplot(mod_6_resid, aes(y = M_resid, x = pred_M))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_2))+
        theme_classic()+
        ggtitle("M residuals"))

print(ggplot(mod_6_resid, aes(y = M_resid_use_pred, x = pred_M_use_pred))+
        geom_smooth()+
        geom_point(aes(color = ts_2_spat_2))+
        theme_classic()+
        ggtitle("M residuals, using predicted MSX and Dermo"))

dev.off()

