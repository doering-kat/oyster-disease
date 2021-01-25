# Saw a talk on how not accounting for exponential relationships but 
# treating as linear can lead to spurious results. Testing out for this
# data set.
# will try log transforming M to account for this.

# load pkgs, options ------------

library(lavaan)
options(stringsAsFactors = F)

# Load Data -----
file_date <- "2019_01_02"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date, ".csv"))
dat[["M_Med_A_Bar_Per_ln"]] <- log(dat[["M_Med_A_Bar_Per"]])
# run model ----
#note: used lavaan v 0.6-7 (with R 4.00.3), which required slightly different syntax to 
# get different estmates among groups. 
mod_lavaan<-'
#regressions
M_Med_A_Bar_Per_ln ~ c(b1.1, b1.2, b1.3, b1.4)*MSX_Prev_Per + c(b2.1, b2.2, b2.3, b2.4)*Dermo_Prev_Per
Dermo_Prev_Per ~ c(b3.1,b3.2,b3.3,b3.4)*temp_s_cen + c(b4.1,b4.2,b4.3,b4.4)*sal_cen
MSX_Prev_Per ~ c(b5.1, b5.2, b5.3, b5.4)*temp_s_cen + c(b6.1, b6.2, b6.3, b6.4)*sal_cen
#intercepts
M_Med_A_Bar_Per~1
Dermo_Prev_Per~1
MSX_Prev_Per~1
'


# Run Lavaan models ------------------------------------------------------------


# list of group names for each of the 9 models.
grp_names <- "ts_2_spat_2"

# run the 9 lavaan models.
fits <- list()
# no gropus.
for (i in 1:length(grp_names)) {
  fits[[i]] <- sem(mod_lavaan, data = dat, group = grp_names[i])
}
# Diagnostics -----

# Compare AIC ------------------------------------------------------------------
AICval <- rep(NA, length.out = length(grp_names))
for (i in 1:length(grp_names)){
  AICval[i] <- AIC(fits[[i]])
}
names(AICval) <- grp_names

AICval 

# Save models and results ------------------------------------------------------
save_dir_name <- "./Derived_Data/PostThesis_SEM/16_test_log_transform"
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
