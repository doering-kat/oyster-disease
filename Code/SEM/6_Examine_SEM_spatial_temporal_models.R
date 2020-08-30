# HEADER -----------------------------------------------------------------------
# Examine results from SEM done in 5.
# 
# Written 18 Oct 2018 by Kathryn Doering
# Load Packages-----------------------------------------------------------------
library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
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
# Create folder for Derived data -----------------------------------------------
if(write_files == T){
    write_derived_dat_name <- "./Derived_Data/SEM/6_Examine_SEM_spatial_temporal_models"
    dir.create(write_derived_dat_name)
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
    write.csv(AICdf, paste0(write_derived_dat_name, "/AICdf_model_run_",file_date_fits, ".csv"), row.names = F)
}
# # best model is with 4 time periods and 3 spatial ones, second best is 2 time
# # periods and 3 spatial one. (space is important, but so is time - 20 AIC units
# # is a big difference!)
# # 
# Is the same found using BIC? (NO - ughhh which metric makes more sense to use
# for the model selection we want to do?)
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
    write.csv(mod_sel_df, paste0(write_derived_dat_name, "/model_selection_model_run_",file_date_fits, ".csv"), row.names = F)
}
# Explore model fit diagnostics ------------------------------------------------
#fitMeasures(fits[[1]]) # to see all the fitMeasures
# For model selection, either AIC or BIC can be used.  There is a bic2, which I'm
# not sure how this differs from normal aic, but there does not appear to be a 
# corrected AIC.
# 
# There is also the option to use a different baseline model to calculate some 
# relative fit test statistics? Perhaps this is an alternate way of assuring
# that the fit makes sense?
# residual, 
# rmsea (<0.06 is good), SRMR (standardized root mean squared, < 0.09 is good). CFI (scaled between 0 and 1, a measure of the amount of variance accounted for in the covariance matrix, should be 0.95 or higher), 
# TLI can maybe be used , > 0.90 is acceptable
# 
# Perhaps a combo of these should be good? Using too many may eliminate "good" models, but probably should use at least more than 1?

# may need to read more about these fit measures and talk to mike to see what he thinks are the best ones to use.
# 
# Look at the fit measures for the largest model
fitMeasures(fits[[9]])
grp_names
names(fits) <- grp_names
fitMeasures(fits[["ts_2_spat_2"]])

# scatter plots of the data (by groups) ----------------------------------------
# Make plots of data by thier groups

fig_dir_name <- "./Figures/SEM/6_Examine_SEM_spatial_temporal_models"
dir.create(fig_dir_name)

# Assume 4 time chunks and 3 spatial chunks.
pdf(paste0(fig_dir_name, "/scatterplots_ts_4_spat_3.pdf"),width = 11, height = 8.5)
ggplot(data = dat, aes(x = Dermo_Prev_Per, y = M_Med_Annual))+
     geom_point()+
     geom_smooth()+
     facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = MSX_Prev_Per, y = M_Med_Annual))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = (Dermo_Prev_Per + MSX_Prev_Per), y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_4_spat_3)

# salinity plots
ggplot(data = dat, aes(x = sal, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = sal, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = sal, y = MSX_Prev_Per))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_4_spat_3)

# temperature plots
ggplot(data = dat, aes(x = temp_s, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = temp_s, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_4_spat_3)

ggplot(data = dat, aes(x = temp_s, y = MSX_Prev_Per))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_4_spat_3)

dev.off()

# ts_2_spat_3 plots --------------------------------------------------
pdf(paste0(fig_dir_name, "/scatterplots_ts_2_spat_3.pdf"), width = 11, height = 8.5)

ggplot(data = dat, aes(x = Dermo_Prev_Per, y = M_Med_Annual))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = MSX_Prev_Per, y = M_Med_Annual))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = (Dermo_Prev_Per + MSX_Prev_Per), y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

# salinity plots
ggplot(data = dat, aes(x = sal, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = sal, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = sal, y = MSX_Prev_Per))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_2_spat_3)

# temperature plots
ggplot(data = dat, aes(x = temp_s, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = temp_s, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = temp_s, y = MSX_Prev_Per))+
    geom_jitter()+
    #geom_smooth()+
    facet_wrap(~ts_2_spat_3)

dev.off()

# plots for no ts or spatial groups --------------------------------------------
pdf(paste0(fig_dir_name, "/scatterplots_ts_0_spat_0.pdf"), width = 11, height = 8.5)

ggplot(data = dat, aes(x = Dermo_Prev_Per, y = M_Med_Annual))+
    geom_point()+
    geom_smooth()

ggplot(data = dat, aes(x = MSX_Prev_Per, y = M_Med_Annual))+
    geom_jitter()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)


# ggplot(data = dat, aes(x = (Dermo_Prev_Per + MSX_Prev_Per), y = M_Med_Annual_Per))+
#     geom_point()+
#     geom_smooth()#+
#     #facet_wrap(~ts_2_spat_3)

# salinity plots
ggplot(data = dat, aes(x = sal, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = sal, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()#+
    #(~ts_2_spat_3)

ggplot(data = dat, aes(x = sal, y = MSX_Prev_Per))+
    geom_jitter()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)

# temperature plots
ggplot(data = dat, aes(x = temp_s, y = M_Med_Annual_Per))+
    geom_point()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = temp_s, y = Dermo_Prev_Per))+
    geom_point()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)

ggplot(data = dat, aes(x = temp_s, y = MSX_Prev_Per))+
    geom_jitter()+
    geom_smooth()#+
    #facet_wrap(~ts_2_spat_3)

dev.off()
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
        write.csv(tmp_table, paste0(write_derived_dat_name,"/par_table_", tmp_name ,"_", file_date_fits, ".csv"))
    #     write.csv(tmp_grp_labels, paste0(write_derived_dat_name,"/grp_key_", tmp_name ,"_", file_date_fits, ".csv"))
     }
}


# Summary for full model -------------------------------------------------------
# Look at output for the full model to try to understand what is going on with
# The results (and try to connect it back to the scatterplots)
# 
# summary(fits[[9]]) # model with 4 time chunks and 3 spatial groups.
# 
# coef(fits[[9]])
# 
# write.csv(parTable(fits[[9]]), paste0(write_derived_dat_name,"/par_table_4_ts_3_sp_", file_date_fits, ".csv"))
# 
# parTable(fits[[9]])
# look into temp and salinity correlations by cruise ? Or by avg even?
# Sometimes, there appears to be a relatively large neg relationshp between 
# temperature and disease, which isn't intuitive - is there some correlation
# betwen temperature and percipitation (lower temp leads to more percip? this 
# should show up as correlations between temp and salinity)
# are there potential other reasons that a lower avg temp could lead to a higher
# prevalance (perhaps the oysters die earlier in the year, thus by the time the
# fall survey rolls around, most of the infected oysters have already died? 
# I would expect this to be the case more often for MSX than for dermo, since there
# is sometimes an early MSX peak )
# 
# Another thought: synergy between MSX and dermo. if an oyster is infected with
# both, does this increase its chance of dying? But what does this means in terms
# of prevalance? If we see MSX and dermo separately, will this same amount of 
# prevalance kill more oysters than if they co-occur (some prevalance from dermo
# for ex is "wasted" if the animal dies of msx)? Or would this actually mean that
# more oysters die than expected because the stress of having 2 diseases is multi
# plicative and therefore kills more oysters. (or maybe the antagonism v. synergy
# depends on the amount of prevalence/intensity) - definely a tricky question,
# but perhaps there is a way to understand this using the disease data?
# (It may be too buried in the data to extract :/) - are there any studies on the
# synergism between the diseases? 
# 
# Why in the high salinity group at the end of the time series is the salinity
# such that there is a negative relationship with dermo prevalance? Is there 
# any logical reason why this would be expected? need to think more about this,
# it seems strange? (something with intercepts?)
# 
# Think more about intercepts and what they mean in the grand scheme of the
# results...
# 
# Residuals --------------------------------------------------------------------
# predict(fits[[1]])
# new_data <- predict(fits[[1]], 
# )
# dat_new <- dat %>% 
#             select(sal, temp_s, MSX_Prev_Per, Dermo_Prev_Per, M_Med_Annual_Per) %>% 
#             slice(1:50)
#             
# #fit <- cfa(model, data = dataset1)
# test <- predict(fits[[1]], newdata = dat_new)
# predict or lavPredict() functions can be used to predict new values of latent
# variables- I'm not sure if there any way to get residuals for lavan? we can 
# see residual variation in the variables, though! Maybe just using fit diagnostics
# and looking at the residual variation are sufficient checks, as well as looking
# at scatter plots 
# 
# Look at parameter table/plot for "ffull" model ----------------------------------

ts_grp <- parTable_list[[9]] %>% 
             select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
             filter(op == "~") %>% #get just the coefficients
             mutate(relationship = paste(lhs, op, rhs)) %>% 
             #break up year and salinity zon groups into separate columns
             mutate(years = substr(group_name, start = 1, stop = 6)) %>% 
             mutate(sal_zone = substring(group_name, first = 7)) %>% 
             #calculate error
             mutate(upper_CI = est + 2*se) %>% #approx 95% CI
             mutate(lower_CI = est - 2*se)        #approx 95% CI.

# order groups
factor_levels_sal <- c("Low", "Med", "High")
factor_levels_yr <- c("91to97", "98to04","05to11", "12to17")
factor_levels_grp <- paste0(rep(factor_levels_yr, each = 3),rep(factor_levels_sal, times = 4))
ts_grp$group_name <- factor(ts_grp$group_name, levels = factor_levels_grp)
ts_grp$sal_zone <- factor(ts_grp$sal_zone, levels = factor_levels_sal)
ts_grp$years <- factor(ts_grp$years, levels = factor_levels_yr)
# plot the coefficients
ggplot(ts_grp, aes(x = group_name, y = est)) +
    geom_col(aes(fill = sal_zone, col = years)) +
    geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    scale_fill_grey(start = 0.8, end = 0.2)+
    facet_wrap(~relationship, scales = "free_y")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(fig_dir_name, "/coefficients_", grp_names[9], ".png"), device = "png",
    width = 14, height = 8)

# plot coef. for 2 ts, 3 spatial -------------------------------------------------

ts_grp <- parTable_list[[7]] %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~") %>% #get just the coefficients
    mutate(relationship = paste(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    #calculate error
    mutate(upper_CI = est + 2*se) %>% #approx 95% CI
    mutate(lower_CI = est - 2*se)        #approx 95% CI.

# order groups
factor_levels_sal <- c("Low", "Med", "High")
factor_levels_yr <- c("91to02", "02to17") # these groups were misnamed, but actually represent 91to02 and 03t017.
factor_levels_grp <- paste0(rep(factor_levels_yr, each = length(factor_levels_sal)),rep(factor_levels_sal, times = length(factor_levels_yr)))
ts_grp$group_name <- factor(ts_grp$group_name, levels = factor_levels_grp)
ts_grp$sal_zone <- factor(ts_grp$sal_zone, levels = factor_levels_sal)
ts_grp$years <- factor(ts_grp$years, levels = factor_levels_yr)
# plot the coefficients
ggplot(ts_grp, aes(x = group_name, y = est)) +
    geom_col(aes(fill = sal_zone, col = years)) +
    geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    scale_fill_grey(start = 0.8, end = 0.2)+
    facet_wrap(~relationship, scales = "free_y")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(fig_dir_name, "/coefficients_", grp_names[7], ".png"), device = "png",
    width = 14, height = 8)
# plot coef. for 2 ts, 2 spatial -------------------------------------------------

ts_grp <- parTable_list[[6]] %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~") %>% #get just the coefficients
    mutate(relationship = paste(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    #calculate error
    mutate(upper_CI = est + 2*se) %>% #approx 95% CI
    mutate(lower_CI = est - 2*se)        #approx 95% CI.

# order groups
factor_levels_sal <- c("Low", "MedHigh")
factor_levels_yr <- c("91to02", "02to17") # these groups were misnamed, but actually represent 91to02 and 03t017.
factor_levels_grp <- paste0(rep(factor_levels_yr, each = length(factor_levels_sal)),rep(factor_levels_sal, times = length(factor_levels_yr)))
ts_grp$group_name <- factor(ts_grp$group_name, levels = factor_levels_grp)
ts_grp$sal_zone <- factor(ts_grp$sal_zone, levels = factor_levels_sal)
ts_grp$years <- factor(ts_grp$years, levels = factor_levels_yr)
# plot the coefficients
ggplot(ts_grp, aes(x = group_name, y = est)) +
    geom_col(aes(fill = sal_zone, col = years)) +
    geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    scale_fill_grey(start = 0.8, end = 0.2)+
    facet_wrap(~relationship, scales = "free_y")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(fig_dir_name, "/coefficients_", grp_names[6], ".png"), device = "png",
    width = 14, height = 8)


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
    write.csv(allfit, paste0(write_derived_dat_name, "/fit_measures.csv"), row.names = F)
}
# get the sample sizes of all the groups (either from the model output, or from
# the data inputs)

#sample sizes for the full model

n_ts_4_spat_3 <- dat %>% 
                    select(ts_4_spat_3) %>% 
                    group_by(ts_4_spat_3) %>% 
                    count()
write.csv(n_ts_4_spat_3, paste0(write_derived_dat_name, "/ts_4_spat_3_sample_size.csv"), row.names = F)

#get r2 values for the full model.
r2_ts_4_spat_3 <- lavInspect(fits[[9]], what = "r2")

# unlist_test <- unlist(r2_ts_4_spat_3)
# flatten_dfc(r2_ts_4_spat_3)

#reformat ouput to a dataframe.
r2_ts_4_spat_3_df <- data.frame(group = names(r2_ts_4_spat_3)) #group names.
for(i in 1:3){ #loop through to add columns to make a dataframe
   tmp_col <- map(r2_ts_4_spat_3,~.x[i]) %>%  #get first element of each list(group)
        flatten_dbl()
   tmp_col_name <- names(tmp_col)[1] #names should be all the same so take the first
   r2_ts_4_spat_3_df[,(1+i)] <- tmp_col #add elements to a new column
   names(r2_ts_4_spat_3_df)[1+i] <- tmp_col_name #add name of the column
}
#organize by factors
factor_levels_sal <- c("Low", "Med", "High")
factor_levels_yr <- c("91to97", "98to04","05to11", "12to17")
factor_levels_grp <- paste0(rep(factor_levels_yr, each = 3),rep(factor_levels_sal, times = 4))

r2_ts_4_spat_3_df$group <- factor(r2_ts_4_spat_3_df$group, levels = factor_levels_grp)

r2_ts_4_spat_3_df <- arrange(r2_ts_4_spat_3_df, group) %>% 
                        mutate(years = substr(group, start = 1, stop = 6)) %>% 
                        mutate(sal_zone = substring(group, first = 7)) %>% 
                        select(group, years, sal_zone,M_Med_Annual_Per, Dermo_Prev_Per, MSX_Prev_Per)

if(write_files==T){
    write.csv(r2_ts_4_spat_3_df, paste0(write_derived_dat_name, "/ts_4_spat_3_r2_df.csv"), row.names = F)
}

# r2 values for the 2/2 group model ----------------------
# model 6

#get r2 values for the full model.
r2_ts_2_spat_2 <- lavInspect(fits[[6]], what = "r2")

# unlist_test <- unlist(r2_ts_4_spat_3)
# flatten_dfc(r2_ts_4_spat_3)

#reformat ouput to a dataframe.
r2_ts_2_spat_2_df <- data.frame(group = names(r2_ts_2_spat_2)) #group names.
for(i in 1:3){ #loop through to add columns to make a dataframe
    tmp_col <- map(r2_ts_2_spat_2,~.x[i]) %>%  #get first element of each list(group)
        flatten_dbl()
    tmp_col_name <- names(tmp_col)[1] #names should be all the same so take the first
    r2_ts_2_spat_2_df[,(1+i)] <- tmp_col #add elements to a new column
    names(r2_ts_2_spat_2_df)[1+i] <- tmp_col_name #add name of the column
}
#organize by factors
factor_levels_sal <- c("Low", "MedHigh")
factor_levels_yr <- c("91to02", "02to17")
factor_levels_grp <- paste0(rep(factor_levels_yr, times = length(factor_levels_sal)),rep(factor_levels_sal, each = length(factor_levels_yr)))

r2_ts_2_spat_2_df$group <- factor(r2_ts_2_spat_2_df$group, levels = factor_levels_grp)

r2_ts_2_spat_2_df <- arrange(r2_ts_2_spat_2_df, group) %>% 
    mutate(years = substr(group, start = 1, stop = 6)) %>% 
    mutate(sal_zone = substring(group, first = 7)) %>% 
    select(group, years, sal_zone,M_Med_Annual_Per, Dermo_Prev_Per, MSX_Prev_Per)

if(write_files==T){
    write.csv(r2_ts_2_spat_2_df, paste0(write_derived_dat_name, "/ts_2_spat_2_r2_df.csv"), row.names = F)
}

# Plot data --------------------------------------------------------------------
# plot disease data
ggplot(data = dat, aes(x = year, y = MSX_Prev_Per)) +
    geom_point() + 
    geom_line() + 
    geom_point(aes(x = year, y = Dermo_Prev_Per), color = "blue") + 
    geom_line(aes(x = year, y =Dermo_Prev_Per), color = "blue") + 
    ylab("Value (Percent)")+
    xlab("Year")+
    geom_point(aes(x = year, y = M_Med_Annual_Per), color = "orange") + 
    geom_line(aes(x = year, y = M_Med_Annual_Per), color = "orange") + 
    facet_wrap(~ID+Sal_Zone) + 
    theme_classic()+
ggtitle("MSX (black) and Dermo (blue) prevalences, M (orange) by bar")
ggsave(paste0(fig_dir_name, "/Disease_Data_by_Bar.png"), device = "png",
    width = 14, height = 8)

# plot temp/sal data.
ggplot(data = dat, aes(x = year, y = temp_s)) +
    geom_point(color = "red") + 
    geom_line(color = "red") + 
    ylab("Value (psu or degrees C)")+
    xlab("Year")+
    geom_point(aes(x = year, y = sal), color = "blue") + 
    geom_line(aes(x = year, y = sal), color = "blue") + 
    facet_wrap(~ID+Sal_Zone) + 
    theme_classic()+
ggtitle("Salinity (blue) and Temperature (red) by bar")
ggsave(paste0(fig_dir_name, "/Enviro_Data_by_Bar.png"), device = "png",
    width = 14, height = 8)

# Predict values ---------------------------------------------------------------
newdata <- data.frame(y1=rnorm(10), y2=rnorm(10), 
    x1=rnorm(10), x2=rnorm(10), x3=rnorm(10)) 
newdata <- dat %>% 
            slice(1:10) %>% 
            select(sal, temp_s, MSX_Prev_Per, Dermo_Prev_Per, M_Med_Annual_Per)
lavaan:::lavPredict(fits[[1]], type="lv", newdata = newdata) 


