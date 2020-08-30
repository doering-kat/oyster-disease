#Header ------------------------------------------------------------------------
#
#A check on model results using winter temperature instead of summer temperature
#Winter temp the same year (in jan - march?) should effects prevalence later that
#year in the fall, according to this model.
#
#This run was performed as a pre-defense check on using winter temp instead of
#summer.
#
#Code modeified from 5_SEM_spatial_temporal_models.
#
# Written 29 NOv 2018 by Kathryn Doering 
# 
# Load Packages and set options ------------------------------------------------

library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
options(stringsAsFactors = F)
write_files <- TRUE # if want to save csvs.
# Load Data --------------------------------------------------------------------
file_date <- "2018_10_17"
dat <- read.csv(paste0("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_", file_date, ".csv"))
# Saving locations -------------------------------------------------------------
# to dave derived data
save_dir_name <- "./Derived_Data/SEM/9_SEM_run_winter_temp"
dir.create(save_dir_name)

# to save figures
save_fig_dir_name <- "./Figures/SEM/9_SEM_run_winter_temp"
dir.create(save_fig_dir_name)
# Lavaan model -----------------------------------------------------------------

mod_lavaan<-'
#regressions
M_Med_Annual_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
Dermo_Prev_Per ~ b3*temp_w + b4*sal
MSX_Prev_Per ~ b5*temp_w + b6*sal
#intercepts
M_Med_Annual_Per~1
Dermo_Prev_Per~1
MSX_Prev_Per~1
#interaction that could be added:
#MSX_Prev_Per ~~ dermo_Prev_Per
'


# Run Lavaan models ------------------------------------------------------------

# list of group names for each of the 9 models.
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# run the 9 lavaan models.
lavaan_fits <- list()
# no gropus.
lavaan_fits[[1]] <- sem(mod_lavaan, data = dat)
for (i in 2:length(grp_names)) {
    lavaan_fits[[i]] <- sem(mod_lavaan, data = dat, group = grp_names[i])
}
# Diagnostics ------------------------------------------------------------------
# Make sure all models converged
for(i in 1:length(lavaan_fits)){
    print(lavaan_fits[[i]]@Fit@converged)
}

# Compare AIC ------------------------------------------------------------------
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    AICval[i] <- AIC(lavaan_fits[[i]])
}
names(AICval) <- grp_names
AICval - min(AICval)
# Results ----------------------------------------------------------------------

# Save models and results ------------------------------------------------------
save_dir_name <- "./Derived_Data/SEM/9_SEM_run_winter_temp"
dir.create(save_dir_name)

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))
if(write_files = T){
    #save models
    saveRDS(lavaan_fits, paste0(save_dir_name, "/lavaan_fits_", save_date, ".rds"))
    
    #save text output from summaries as a text document.
    sink(paste0(save_dir_name, "/lavaan_fits_summaries_", save_date, ".txt"))
    for(i in 1:length(lavaan_fits)){
        print(grp_names[i])
        print(summary(lavaan_fits[[i]]))
    }
    sink()
}

# Examine results --------------------------------------------------------------
# Look at model fit indices (would we still select the same model?)
# Look at plot of the "best" model and of the 2 spatial/ temporal group model (if not the best)

# Compare model selection criteria----------------------------------------------
# AIC can be used for model selection, although a corrected AIC may be better (it
# is corrected for the sample size)
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    AICval[i] <- AIC(lavaan_fits[[i]])
}
#names(AICval) <- grp_names
dAICval <- AICval - min(AICval)

AICdf <- data.frame(model = grp_names, AIC = AICval, dAIC = dAICval)


# # best model is with 4 time periods and 3 spatial ones, second best is 2 time
# # periods and 3 spatial one. (space is important, but so is time - 20 AIC units
# # is a big difference!)
# # 
# Is the same found using BIC? (NO - ughhh which metric makes more sense to use
# for the model selection we want to do?)
BICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    BICval[i] <- BIC(lavaan_fits[[i]])
}
#names(BICval) <- grp_names

dBICval <- BICval - min(BICval)
mod_sel_df <- AICdf #create a new df with all mod comparison measures
mod_sel_df$BIC <- BICval
mod_sel_df$dBIC <- dBICval

# try the corrected BIC val also.
BIC2val <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    BIC2val[i] <- fitMeasures(lavaan_fits[[i]], fit.measures = "bic2")
}
#names(BIC2val) <- grp_names
dBIC2 <- BIC2val - min(BIC2val) #calculate change from min.
# add to model comparison 
mod_sel_df$BIC2 <- BIC2val
mod_sel_df$dBIC2 <- dBIC2

#add corrected AIC
moreFitIndices(lavaan_fits[[1]], fit.measures = "all")
AICcval <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    AICcval[i] <- moreFitIndices(lavaan_fits[[i]], fit.measures = "aic.smallN")
}
#names(BIC2val) <- grp_names
dAICc <- AICcval - min(AICcval) #calculate change from min.
# add to model comparison 
mod_sel_df$AICc <- AICcval
mod_sel_df$dAICc <- dAICc

#add hqc, another model selection measure for SEM
hqcval <- rep(NA, length.out = length(grp_names))
for(i in 1:length(grp_names)){
    hqcval[i] <- moreFitIndices(lavaan_fits[[i]], fit.measures = "hqc")
}
#names(BIC2val) <- grp_names
dhqc <- hqcval - min(hqcval) #calculate change from min.
# add to model comparison 
mod_sel_df$hqc <- hqcval
mod_sel_df$dhqc <- dhqc


if(write_files == T){
    write.csv(mod_sel_df, paste0(save_dir_name, "/model_selection_model_run_", save_date, ".csv"), row.names = F)
}

# Take home: would likely still select the same model. Still the most parsimonious
# model, with most criteria suggesting it is the best (or close to the best) model.

# Absolute Fit measures --------------------------------------------------------
allfit <- data.frame()
for (i in 1:9){
    tmp_fit <- fitMeasures(lavaan_fits[[i]], fit.measures = c("rmsea", "srmr", "cfi"))
    allfit <- bind_rows(allfit, tmp_fit)
}
# add model names
allfit$model <- grp_names

# output as a .csv.
if(write_files==T){
    write.csv(allfit, paste0(save_dir_name, "/absolute_fit_measures.csv"), row.names = F)
}

#ts_2_spat_2 still has the best absolute fit measures of possible models to select (i.e., ones chosesn
#by at least one of the model selection criteria)
#
# Examine Results for the 2 spatial/2 temporal group model ----------------------
lavaan_fits[[6]]@Fit@converged # check model convergence.
# use parTable to get coef estimates for each of the models.
parTable_list <- list()
for (i in 1:length(lavaan_fits)){
    tmp_name <- grp_names[i] #model name
    tmp_grp_labels <- lavInspect(lavaan_fits[[i]], "group.label")
    if(is.na(tmp_name)){
        tmp_name <- "No_Grps"
        tmp_grp_labels <- "No_Grps"
    }
    tmp_grp_labels_df <- data.frame(group = 1:length(tmp_grp_labels),
        group_name = tmp_grp_labels)
    tmp_table <- parTable(lavaan_fits[[i]]) #model results par est.
    #add group names to the tmp_table
    tmp_table <- left_join(tmp_table, tmp_grp_labels_df, by = "group")
    #add to list
    parTable_list[[i]] <- tmp_table
    #save to .csv.
    if(write_files == T){
        write.csv(tmp_table, paste0(save_dir_name,"/par_table_", tmp_name ,"_", save_date, ".csv"))
        #     write.csv(tmp_grp_labels, paste0(write_derived_dat_name,"/grp_key_", tmp_name ,"_", file_date_fits, ".csv"))
    }
}

# plot coefficients for 2 spatial, 2 temporal (mod 6) --------------------------
# plot coef. for 2 ts, 2 spatial -------------------------------------------------

## Get table of predictions from lavaan fitted model, including the names of the
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
# Plot coef for model ----------------------------------------------------------
# Make a bar plot. 


# process data:
ts_grp <- GetParTable(lavaan_fits[[6]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~") %>% #get just the coefficients
    mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)        #approx 95% CI.

# order groups
factor_levels_sal <- c("Low", "MedHigh")
factor_levels_yr <- c("91to02", "02to17") # these groups were misnamed, but actually represent 91to02 and 03t017.
factor_levels_grp <- paste0(rep(factor_levels_yr, times = length(factor_levels_sal)),rep(factor_levels_sal, each = length(factor_levels_yr)))

ts_grp$group_name <- factor(ts_grp$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
ts_grp$sal_zone <- factor(ts_grp$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
ts_grp$years <- factor(ts_grp$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
#order relationships
fac_lev_rel <- paste0("MSX_Prev_Per~", c("temp_w", "sal"))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~MSX_Prev_Per")
fac_lev_rel <- c(fac_lev_rel, paste0("Dermo_Prev_Per~", c("temp_w", "sal")))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~Dermo_Prev_Per")

# # to label facets:
# rel_labels <- list(
#     'MSX_Prev_Per~temp_w' = "Temperature -> % MSX Prevalence",
#     'MSX_Prev_Per~sal' = "Salinity -> % MSX Prevalence",
#     'M_Med_Annual_Per~MSX_Prev_Per' = "% MSX Prevalence -> % Mortality",
#     'Dermo_Prev_Per~temp_w' = "Temperature -> % Dermo Prevalence",
#     'Dermo_Prev_Per~sal' = "Salinity -> % Dermo Prevalence",
#     'Dermo_Med_Annual_Per~MSX_Prev_Per'="% Dermo Prevalence -> % Mortality"
# )

# to label facets:
fac_rel_labels <- c("Temperature -> % MSX Prevalence",
    "Salinity -> % MSX Prevalence",
    "% MSX Prevalence -> % Mortality",
    "Temperature -> % Dermo Prevalence",
    "Salinity -> % Dermo Prevalence",
    "% Dermo Prevalence -> % Mortality"
)

# rel_labeller <- function(variable,value){
#     return(rel_labels[value])
# }

ts_grp$relationship <- factor(ts_grp$relationship, 
    levels = fac_lev_rel,
    labels = fac_rel_labels)

# plot the coefficients
ggplot(ts_grp, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    facet_wrap(~relationship, scales ="free",ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Salinity Zone Group")+
    ylab("Estimate")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())
#theme(legend.position="top")+
#guides(color = F)
#guides( fill = guide_legend(order = 1), guide_legend(order =2))

#theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(save_fig_dir_name, "/coefficients_winter_temp_", grp_names[6], ".png"), device = "png",
    width = 14, height = 8)
