# HEADER -----------------------------------------------------------------------
# 
# Run the spatial and temporal models and compare them using AIC.
# 
# (remove extreme freshet events before running?)
# Created Oct 11 2018 by Kathryn Doering 

# Load Packages and set options ------------------------------------------------

library(lavaan)
library(tidyverse)
library(semPlot)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
file_date <- "2018_10_17"
dat <- read.csv(paste0("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_", file_date, ".csv"))

# Write Lavaan model -----------------------------------------------------------

#add custom labels which can be useful sometimes. (for ex, if want 2 params to 
#be equal, simply give them the same name in the formulas.)
#the things before * are labels for the coefficients.
# mod_lavaan<-'
# #regressions
# M_Med_Annual_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
# Dermo_Prev_Per ~ b3*temp_s + b4*sal
# MSX_Prev_Per ~ b5*temp_s + b6*sal
# #intercepts
# M_Med_Annual_Per~1
# Dermo_Prev_Per~1
# MSX_Prev_Per~1
# #interaction that could be added:
# #MSX_Prev_Per ~~ dermo_Prev_Per
# '
mod_lavaan<-'
#regressions
M_Med_Annual_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
Dermo_Prev_Per ~ b3*temp_s + b4*sal
MSX_Prev_Per ~ b5*temp_s + b6*sal
#intercepts
M_Med_Annual_Per~1
Dermo_Prev_Per~1
MSX_Prev_Per~1
#interaction that could be added:
#MSX_Prev_Per ~~ dermo_Prev_Per
'
# mod_lavaan_2<-'
# #regressions
# M_Med_Annual_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
# Dermo_Prev_Per ~ b3*temp_s + b4*sal
# MSX_Prev_Per ~ b5*temp_s + b6*sal
# #intercepts
# # M_Med_Annual_Per~1
# # Dermo_Prev_Per~1
# # MSX_Prev_Per~1
# #interaction that could be added:
# #MSX_Prev_Per ~~ dermo_Prev_Per
# '

# Run Lavaan models ------------------------------------------------------------
# # test code: can delete. (commented out for now)
# lavaan_fit_test_1 <- sem(mod_lavaan, data = dat)
# lavaan_fit_test_2 <- sem(mod_lavaan_2, data = dat, group = "spat_2")
# lavaan_fit_test_2 <- sem(mod_lavaan_2, data = dat, group = "spat_2", group.equal=(c("loadings", "intercepts")))
# summary(lavaan_fit_test_2)
# 
# parTable(lavaan_fit_test_1)
# parTable(lavaan_fit_test_2)
# summary(lavaan_fit_test_1, fit.measures = T)
# summary(lavaan_fit_test_2, fit.measures = T)
# inspect(lavaan_fit_test_2)
# resid_test_2 <- resid(lavaan_fit_test_2)
# resid_test_2$Low

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

#some options for looking at the parameter values. 
# parTable(lavaan_fits[[2]])
# summary(lavaan_fits[[9]])
# semPaths(lavaan_fits[[1]],"path", whatLabels = "par")
# semPaths(lavaan_fits[[2]],"path", whatLabels = "par")

# Compare AIC ------------------------------------------------------------------
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    AICval[i] <- AIC(lavaan_fits[[i]])
}
names(AICval) <- grp_names
AICval
# Results ----------------------------------------------------------------------

# Save models and results ------------------------------------------------------
save_dir_name <- "./Derived_Data/SEM/5_SEM_spatial_temporal_models"
dir.create(save_dir_name)

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))

#save models
saveRDS(lavaan_fits, paste0(save_dir_name, "/lavaan_fits_", save_date, ".rds"))

#save text output from summaries as a text document.
sink(paste0(save_dir_name, "/lavaan_fits_summaries_", save_date, ".txt"))
for(i in 1:length(lavaan_fits)){
    print(grp_names[i])
    print(summary(lavaan_fits[[i]]))
}
sink()

