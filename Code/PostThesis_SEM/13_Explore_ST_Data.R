# Header -----------------------------------------------------------------------
# Explore ST Data for the VAST/Spatiotemporal readings
# Spring/summer 2019, NWFSC/AFSC

# Load packages, set options ---------------------------------------------------
library(tidyverse)
library(cowplot)
options(stringsAsFactors = FALSE)

# Load data --------------------------------------------------------------------
dat <- read.csv("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_2019_01_02.csv")
dat_latlon <- read.csv("./Data/BarInfo.csv")
# save location ----------------------------------------------------------------
save_fold <- "./Figures/PostThesis_SEM/13_Explore_ST_Data"
dir.create(save_fold)

# add lat long to dat ----------------------------------------------------------
dat_latlon <-  dat_latlon %>% 
                 select(ID, cpntLongitude, cpntLatitude)
# Add to data
dat <- left_join(dat, dat_latlon, by = "ID")

# Explore Hovmoller plots ------------------------------------------------------
# plot_gg <- function(dat, x_var, y_var, fill_var){
#     plot <- ggplot(data = dat, aes(x = x_var, y = y_var) )+
#         geom_tile(aes(fill =  fill_var, height = 0.005))+
#         scale_fill_gradient(low = "yellow",high = "red") +
#         theme_classic()
# }

plots <- list()

plots[[1]] <- ggplot(data = dat, aes(x = year, y = cpntLatitude) )+
               geom_tile(aes(fill = Dermo_Prev_Per), height = 0.005)+
               scale_fill_gradient(low = "yellow",high = "red") +
               theme_classic()

plots[[2]] <-ggplot(data = dat, aes(x = year, y = cpntLatitude) )+
                geom_tile(aes(fill = MSX_Prev_Per), height = .005)+
                scale_fill_gradient(low = "yellow",high = "red") +
                theme_classic()

plots[[3]] <-ggplot(data = dat, aes(x = year, y = cpntLatitude) )+
                geom_tile(aes(fill = sal), height = .005)+
                scale_fill_gradient(low = "yellow",high = "red") +
                theme_classic()

plots[[4]] <-ggplot(data = dat, aes(x = year, y = cpntLatitude) )+
                geom_tile(aes(fill = temp_s), height = .005)+
                scale_fill_gradient(low = "yellow",high = "red") +
                theme_classic()

plots[[5]] <-ggplot(data = dat, aes(x = year, y = cpntLatitude) )+
                geom_tile(aes(fill = temp_w), height = .005)+
                scale_fill_gradient(low = "yellow",high = "red") +
                theme_classic()

plot_grid(plotlist = plots)

ggsave(paste0(save_fold, "/Hovmoller_lat.png"), width = 18, height = 10, units = "in" )


