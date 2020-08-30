# Header -----------------------------------------------------------------------
# Run the base models, but try with modifying the temporal breakpoint for a 2 temporal
# 2 spatial group model. (may need to switch to 3 spatial groups also)
# 
# 
# TODO: may need to modify the inputs based on information from Mitch on Bars 
# That should be low salinity 
# 
# Created 4 Jan 2019 by Kathryn Doering 
# 
# Load packages, set options ---------------------------------------------------
library(lavaan)
library(tidyverse)
library(semPlot)
options(stringsAsFactors = F)
write_files <- T 
# Load Data --------------------------------------------------------------------
file_date <- "2019_01_02"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date, ".csv"))

# Specify save paths -----------------------------------------------------------
save_dir_name <- "./Derived_Data/PostThesis_SEM/8_Run_TS_breakpoint_change"
dir.create(save_dir_name)

#get date to use in file names.
save_date <- as.character(Sys.Date()) #for making a folder, add the date
#add underscores instead of dashes.
save_date <- paste0(substr(save_date,start = 1,stop =4), "_",
    substr(save_date, start = 6, stop = 7), "_",
    substr(save_date, start = 9, stop = 10))

# Simplify the dataset ---------------------------------------------------------
# For clarity, remove variables that aren't necessary. 
dat <- dat %>% 
        select(year, ID, temp_s_cen, sal_cen,MSX_Prev_Per, Dermo_Prev_Per, M_Med_A_Bar_Per, spat_2)

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

# Make grouping variables ------------------------------------------------------
# Modify the factors to speciy the groups. (last year in beginning of ts.)
# change_col_name <- function(col_yr){ #add a new column in dplyr and vary the name
#     colname <- paste0(col_yr, "_spat_2")
#     !!col_name := 
# }

# specify the break years
breakyrs <- seq(1996,2014, by = 2)

colnames <- NULL # make a list of the column names that the grouping variable is
# stored in.
for(yr in breakyrs){
    tmp_colname <- paste0("grp_", yr, "_spat_2") # the temporary column name to add
    colnames <- c(colnames, tmp_colname) # add the column name to the list.
    #make the grouping variable based on the break year.
    #add the new grouping variables to the list 
    dat <- dat %>% 
                mutate(grp_yr = ifelse(year<=yr, "Beg", "End")) %>% 
                mutate(!!tmp_colname := paste0(grp_yr,spat_2)) # make a new column.
}
#delete the grp_yr column, which is now meaningless.
dat <- select(dat, -grp_yr)
# Run Lavaan models ------------------------------------------------------------
fits <- list() # to hold the lavaan models
for(n in 1:length(breakyrs)){
    tmp_fit <- sem(mod_lavaan, data = dat, group = colnames[n])
    
    #add to list of models
    fits[[n]] <- tmp_fit
}
#add names to the models
names(fits) <- as.character(breakyrs)

# Diagnostics ------------------------------------------------------------------
# Make sure all models converged
for(i in 1:length(fits)){
    print(fits[[i]]@Fit@converged)
}

# Compare AIC ------------------------------------------------------------------
AICval <- rep(NA, length.out = length(fits))
for (i in 1:length(fits)){
    AICval[i] <- AIC(fits[[i]])
}
names(AICval) <- as.character(breakyrs)

AICval - min(AICval)

# Save data, models and results ------------------------------------------------
if(write_files == T){
    # save the data
    write.csv(dat, paste0(save_dir_name, "/lavaan_fits_data_", save_date, ".csv"), row.names = F)

    #save models
    saveRDS(fits, paste0(save_dir_name, "/lavaan_fits_", save_date, ".rds"))
    
    #save text output from summaries as a text document.
    sink(paste0(save_dir_name, "/lavaan_fits_summaries_", save_date, ".txt"))
    for(i in 1:length(fits)){
        print(breakyrs[i])
        print(summary(fits[[i]]))
    }
    sink()
}

