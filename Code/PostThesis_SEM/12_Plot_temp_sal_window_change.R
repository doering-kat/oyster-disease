# Header -----------------------------------------------------------------------
# 
# Plot change over time using winter and summer temperature tests. 
# 
# Written 8 Jan 2019 by Kathryn Doering
# TODO: Make other plots?
# 
# Load packages and set options ------------------------------------------------

library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
library(plotly)
library(cowplot) # added 
options(stringsAsFactors = F)
write_files <- T #set to false if do not want to save new data.

# Load data --------------------------------------------------------------------

# Results from SEM.
save_dir_name_fits<- "./Derived_Data/PostThesis_SEM/10_Run_temp_sal_window_change_redo"
file_date_fits <- "2021_05_11"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))
# load the data used to make these models
dat <- readRDS(paste0(save_dir_name_fits, "/lavaan_data_", file_date_fits, ".rds"))

# Change in coefficients (calculated in 11)
change_df <- read.csv("./Derived_Data/PostThesis_SEM/11_Model_Comparisons_temp_sal_window_change/coef_change_df_all_scen.csv")

# List of scenarios
#scenarios
scen <- read.csv("./Data/scenarios_temp_sal_window_2019_01_07.csv")

# Create folder for Derived data, figures --------------------------------------
if(write_files == T){
    # set paths
    write_derived_dat_path <- "./Derived_Data/PostThesis_SEM/12_Plot_temp_sal_window_change"
    fig_gen_path <- "./Figures/PostThesis_SEM"
    write_fig_path <- paste0(fig_gen_path, "/12_Plot_temp_sal_window_change")
    # create folders
    dir.create(write_derived_dat_path)
    dir.create(write_fig_path)
}

# Plot change over time --------------------------------------------------------
# base scenario


# This should match the Scenario column in change_df
scen <- mutate(scen, Scenario = paste0("temp_", temp_start, "-" ,temp_end, "; sal_", sal_start,"-", sal_end))
scen_num_key <- select(scen, scen_number, Scenario, scenario.type)

# Add scen_number to the change_df
change_df <- left_join(change_df, scen_num_key, by = "Scenario")

base_scen <- change_df[change_df$scen_number == 17, ]

# Make a scenario factor
change_df$Scenario_fac <- factor(change_df$scen_number, levels = scen_num_key$scen_number, labels = scen_num_key$Scenario )
# change_df$relationship_fac <- factor(change_df$relationship)
# change_df$scenario.type.fac <- factor(change_df$scenario.type)

# make plots for each relationship
change_nest <- change_df %>% 
                group_by(relationship) %>% 
                nest() %>% 
                mutate(plot = map2(data, relationship,  ~ggplot(.x, aes(x = Scenario_fac, y = Change ))+
                                           geom_col(aes(fill = SalZone), position = "dodge")+
                                           ggtitle(.y)+
                                           theme_classic()+
                                           theme(axis.text.x = element_text(angle = 90, hjust = 1)))
                       )
# save the plots in their own folder
change_nest_path <- paste0(write_fig_path, "/coef_change_temporal_plots")
dir.create(change_nest_path)
save_names <- paste0(change_nest_path, "/change_rel_", change_nest$relationship, ".png")#, "_scen_", change_nest$scenario.type, ".png")
#save_names <- paste0(write_fig_path, "/", as.character(1:12), ".png")
map2(save_names, change_nest$plot, ggplot2::ggsave,width = 11, height = 8.5)

# TODO: other plots?
# 
# 

tmp_ggplot_list <- change_nest$plot
cowplot::plot_grid(plotlist = tmp_ggplot_list, ncol = 2)
ggsave( paste0(write_fig_path, "/coef_change_temporal_plots_all.png"), device = "png", width = 14, height = 8.5)
    
