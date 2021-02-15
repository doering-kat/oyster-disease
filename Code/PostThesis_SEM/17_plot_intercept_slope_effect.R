# description ----


# load pkgs, set options

# using R 4.00.3, so don't need to set stringsAsFactors = FALSE
# get the right lavaan version ----

# devtools::install_github("cran/lavaan@0.6-3") # as used in thesis

library(ggplot2)
library(lavaan)
# Load data --------------------------------------------------------------------

# see the residuals 
# Results from SEM.
save_dir_name_fits<- "./Derived_Data/PostThesis_SEM/2_Run_M_By_Bar_Base"
file_date_fits <- "2019_01_02"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))

# list of group names for each of the 9 models (in same order as fits)
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
               "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")
names(fits) <- grp_names
# load the data used to make these models
file_date_dat <- "2019_01_02"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date_dat, ".csv"))

par_tbl <- read.csv(paste0(
  "./Derived_Data/PostThesis_SEM/3_Model_Comparisons_M_By_Bar_Base/par_table_ts_2_spat_2_", 
  file_date_dat, ".csv"))
pred_vals <- read.csv(file.path("Data", "Pred_vals_2_2_model.csv"))

# make folders ----
plot_dir <- file.path("Figures", "PostThesis_SEM", "17_plot_intercept_slope_effects")
dir.create(plot_dir)

# see the residuals for selected mod ----
residuals <- lavResiduals(fits[["ts_2_spat_2"]])
# SRMR, which is an absolute fit metric, seems lke an indicator that don;t need to 
# worry any more about the residuals. The fit measure is relatively good.



# make the plot ----

# plot M given certain values of Salinity, Temp, MSX, dermo prevalence
# use the avgs for each group high/low. Then, use the same values over time.
# the summmary output should give 

# see the relationship - maybe these didn't load correctly (b/c using a newer version of lavaan?
# still need to make this plo
summary(fits[["ts_2_spat_2"]])
pred_vals$group_fac <- factor(pred_vals$group, 
                              levels = c("91to02Low", "02to17Low", "91to02MedHigh","02to17MedHigh"))
ggplot(pred_vals, aes(x = sal, y = temp))+
  geom_raster(aes(fill = mort), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  scale_fill_gradient(low = "yellow", high = "red")+
  facet_grid(group_fac ~ disease) +
  theme_classic()
ggsave(file.path(plot_dir, "heatmap_2_2_mod_temp_sal_M.png"))
