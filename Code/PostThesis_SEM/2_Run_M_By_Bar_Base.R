# Header -----------------------------------------------------------------------
# 
# Run the base models, which are the same as the SEM models for the thesis, but
# with M by bar instead.
# 
# This is the first set of models to run; however, there will be more that can
# be used to understand sensitivity of these models to different assumptions.
# Specifically, these model sets differ in their spatial and temporal groups
# assuming a break at 
# 
# TODO: may need to modify the inputs based on information from Mitch on Bars 
# That should be low salinity  (did not get to this)
# 
# Created 2 Jan 2019 by Kathryn Doering 
# 
# Load packages, set options ---------------------------------------------------

library(lavaan)
# library(tidyverse)
# library(semPlot)
options(stringsAsFactors = F)
# Load Data --------------------------------------------------------------------
file_date <- "2021_05_08"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date, ".csv"))
# Write Lavaan model -----------------------------------------------------------
# used centered salinity and temp, and M on the bar level

mod_lavaan<-'
#regressions
M_Med_A_Bar_Per ~ b1*MSX_Prev_Per + b2*Dermo_Prev_Per
Dermo_Prev_Per ~ b3*temp_s_cen + b4*sal_cen
MSX_Prev_Per ~ b5*temp_s_cen + b6*sal_cen
#intercepts
M_Med_A_Bar_Per~1
Dermo_Prev_Per~1
MSX_Prev_Per~1
'
# Run Lavaan models ------------------------------------------------------------

# list of group names for each of the 9 models.
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# run the 9 lavaan models.
fits <- list()
# no gropus.
fits[[1]] <- sem(mod_lavaan, data = dat)

for (i in 2:length(grp_names)) {
    fits[[i]] <- sem(mod_lavaan, data = dat, group = grp_names[i])
}
# Diagnostics ------------------------------------------------------------------
# Make sure all models converged
for(i in 1:length(fits)){
    print(fits[[i]]@Fit@converged)
}

# Compare AIC ------------------------------------------------------------------
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
    AICval[i] <- AIC(fits[[i]])
}
names(AICval) <- grp_names

AICval - min(AICval)

# Save models and results ------------------------------------------------------
save_dir_name <- "./Derived_Data/PostThesis_SEM/2_Run_M_By_Bar_Base"
dir.create(save_dir_name)

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))

#save models
saveRDS(fits, paste0(save_dir_name, "/lavaan_fits_", save_date, ".rds"))

#save text output from summaries as a text document.
sink(paste0(save_dir_name, "/lavaan_fits_summaries_", save_date, ".txt"))
for(i in 1:length(fits)){
    print(grp_names[i])
    print(summary(fits[[i]]))
}
sink()



