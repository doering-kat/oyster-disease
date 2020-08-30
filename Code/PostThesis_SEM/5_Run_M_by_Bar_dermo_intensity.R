# Header -----------------------------------------------------------------------

# used dermo avg intensity instead of prevalence as the important variable
# This is because  Mitch thinks dermo intensity is a better predictor of mortality
# than prevalence.
# 
# Written 4 Jan 2019 by Kathryn Doering
# 
# Load packages, set options ---------------------------------------------------

library(lavaan)
library(tidyverse)
library(semPlot)
options(stringsAsFactors = F)

# Load Data --------------------------------------------------------------------
file_date <- "2019_01_02"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date, ".csv"))

# set folder names to write to -------------------------------------------------

save_dir_name <- "./Derived_Data/PostThesis_SEM/5_Run_M_by_Bar_dermo_intensity"
dir.create(save_dir_name)
# for now: no figures created in this script.

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))

# Write Lavaan model -----------------------------------------------------------
# used centered salinity and temp, and M on the bar level
# Also

mod_lavaan<-'
#regressions
M_Med_A_Bar_Per ~ b1*MSX_Prev_Per + b2*Mean_Intensity
Mean_Intensity ~ b3*temp_s_cen + b4*sal_cen
MSX_Prev_Per ~ b5*temp_s_cen + b6*sal_cen
#intercepts
M_Med_A_Bar_Per~1
Mean_Intensity~1
MSX_Prev_Per~1
'
# Run Lavaan models ------------------------------------------------------------

# list of group names for each of the 9 models.
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# run the 9 lavaan models.
fits <- list()
# no groups.
fits[[1]] <- sem(mod_lavaan, data = dat)
# all models with groups.
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

#save models
saveRDS(fits, paste0(save_dir_name, "/lavaan_fits_", save_date, ".rds"))

#save text output from summaries as a text document.
sink(paste0(save_dir_name, "/lavaan_fits_summaries_", save_date, ".txt"))
for(i in 1:length(fits)){
    print(grp_names[i])
    print(summary(fits[[i]]))
}
sink()


