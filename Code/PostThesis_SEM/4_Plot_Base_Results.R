# Header -----------------------------------------------------------------------
# 
# Plot base results
# 
# Plot the results of the best model(s) for interpretation.
# Likely the 2 temporal, 3 spatial group model should be selected, but there is
# some evidence that the 2 temporal/2 spatial group also does well. Make plots
# For both.
# 
# Plot both the coefficient and the intercepts.
# 
# Written 3 Jan 2019 by Kathryn Doering
# 
# Load packages and set options ------------------------------------------------
 
library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
library(plotly)
options(stringsAsFactors = F)
write_files <- T #set to false if do not want to save new data.

# Load data --------------------------------------------------------------------

# Results from SEM.
save_dir_name_fits<- "./Derived_Data/PostThesis_SEM/2_Run_M_By_Bar_Base"
file_date_fits <- "2021_05_08"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))

# list of group names for each of the 9 models (in same order as fits)
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# load the data used to make these models
file_date_dat <- "2021_05_08"
dat <- read.csv(paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_", file_date_dat, ".csv"))


# Create folder for Derived data, figures --------------------------------------
if(write_files == T){
    # set paths
    write_derived_dat_path <- "./Derived_Data/PostThesis_SEM/4_Plot_Base_Results"
    fig_gen_path <- "./Figures/PostThesis_SEM"
    write_fig_path <- paste0(fig_gen_path, "/4_Plot_Base_Results")
    # create folders
    dir.create(write_derived_dat_path)
    dir.create(fig_gen_path)
    dir.create(write_fig_path)
}


# make Partables ---------------------------------------------------------------
# 
# will use parTable in plotting intercepts and coefficients.
# 
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

# Get and Plot coefficients for 2ts/2sp model ----------------------------------

# process data:
ts_grp <- GetParTable(fits[[6]]) %>%
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
fac_lev_rel <- paste0("MSX_Prev_Per~", c("temp_s_cen", "sal_cen"))
fac_lev_rel <- c(fac_lev_rel, "M_Med_A_Bar_Per~MSX_Prev_Per")
fac_lev_rel <- c(fac_lev_rel, paste0("Dermo_Prev_Per~", c("temp_s_cen", "sal_cen")))
fac_lev_rel <- c(fac_lev_rel, "M_Med_A_Bar_Per~Dermo_Prev_Per")

# to label facets:
rel_labels <- list(
    'MSX_Prev_Per~temp_s_cen' = "Temperature -> % MSX Prevalence",
    'MSX_Prev_Per~sal_cen' = "salinity -> % MSX Prevalence",
    'M_Med_A_Bar_Per~MSX_Prev_Per' = "% MSX Prevalence -> % Mortality",
    'Dermo_Prev_Per~temp_s_cen' = "Temperature -> % Dermo Prevalence",
    'Dermo_Prev_Per~sal' = "salinity -> % Dermo Prevalence",
    'Dermo_Med_Annual_Per~MSX_Prev_Per'="% Dermo Prevalence -> % Mortality"
)

# to label facets:
fac_rel_labels <- c("Temperature -> % MSX Prevalence",
    "salinity -> % MSX Prevalence",
    "% MSX Prevalence -> % Mortality",
    "Temperature -> % Dermo Prevalence",
    "salinity -> % Dermo Prevalence",
    "% Dermo Prevalence -> % Mortality"
)

# rel_labeller <- function(variable,value){
#     return(rel_labels[value])
# }

ts_grp$fac_relationship <- factor(ts_grp$relationship, 
    levels = fac_lev_rel,
    labels = fac_rel_labels)

# plot the coefficients
ggplot(ts_grp, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    facet_wrap(~fac_relationship, scales ="free",ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("salinity Zone Group")+
    ylab("Estimate")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())
#theme(legend.position="top")+
#guides(color = F)
#guides( fill = guide_legend(order = 1), guide_legend(order =2))

#theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(write_fig_path, "/coefficients_", grp_names[6], ".png"), device = "png",
    width = 14, height = 8)

table_coef <- ts_grp
table_coef <- table_coef  %>% 
    select(years, sal_zone, fac_relationship, est, se, lower_CI, upper_CI, group_name) %>% 
    arrange(sal_zone, years,fac_relationship)
write.csv(table_coef, file.path(write_derived_dat_path, "table_coefficients.csv"))
# Plot intercepts for 2ts/2sp model --------------------------------------------

intercepts_organized <- GetParTable(fits[[6]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~1") %>% #get just the coefficients
    filter(lhs != "sal_cen") %>% # just taken from the data.
    filter(lhs != "temp_s_cen") %>% # just taken from the data
    #mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)        #approx 95% CI.

lhs_fac <- c("MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_A_Bar_Per")
lhs_fac_name <- c("MSX Prevalence", "Dermo Prevalence", "Mortality")
# make factors
intercepts_organized$group_name <- factor(intercepts_organized$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
intercepts_organized$sal_zone <- factor(intercepts_organized$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
intercepts_organized$years <- factor(intercepts_organized$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
intercepts_organized$lhs <- factor(intercepts_organized$lhs, 
    levels = lhs_fac, 
    labels = lhs_fac_name)
# 
# intercepts_organized <- intercepts_organized %>%
#     arrange(lhs) %>% 
#     arrange(group_name) %>% 
#     select(years, sal_zone, lhs, est, se, lower_CI, upper_CI)

# plot the intercepts
ggplot(intercepts_organized, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    facet_wrap(~lhs, scales ="free") + #,ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Salinity Zone Group")+
    ylab("Estimate")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())

ggsave(paste0(write_fig_path, "/intercepts_", grp_names[6], ".png"), device = "png",
    width = 14, height = 0.5 + 8/2) #height: make approx half of the 2 row plots, but add extra 0.5 in for labels.
# intercept table ----
inter_tbl <- intercepts_organized %>% 
               select(years, sal_zone, lhs, est, se, lower_CI, upper_CI) %>% 
               arrange(sal_zone, years,lhs)
write.csv(inter_tbl, file = file.path(write_derived_dat_path, "table_intercepts.csv"))


# # Plot coefficients for 2ts/3sp model ----------------------------------------
# process data:
ts_grp_2_3 <- GetParTable(fits[[7]]) %>%
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
factor_levels_sal <- c("Low", "Med","High")
factor_levels_yr <- c("91to02", "02to17") # these groups were misnamed, but actually represent 91to02 and 03t017.
factor_levels_grp <- paste0(rep(factor_levels_yr, times = length(factor_levels_sal)),rep(factor_levels_sal, each = length(factor_levels_yr)))

ts_grp_2_3$group_name <- factor(ts_grp_2_3$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
ts_grp_2_3$sal_zone <- factor(ts_grp_2_3$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium", "High"))
ts_grp_2_3$years <- factor(ts_grp_2_3$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
#order relationships
fac_lev_rel <- paste0("MSX_Prev_Per~", c("temp_s_cen", "sal_cen"))
fac_lev_rel <- c(fac_lev_rel, "M_Med_A_Bar_Per~MSX_Prev_Per")
fac_lev_rel <- c(fac_lev_rel, paste0("Dermo_Prev_Per~", c("temp_s_cen", "sal_cen")))
fac_lev_rel <- c(fac_lev_rel, "M_Med_A_Bar_Per~Dermo_Prev_Per")

# to label facets:
# rel_labels <- list(
#     'MSX_Prev_Per~temp_s_cen' = "Temperature -> % MSX Prevalence",
#     'MSX_Prev_Per~sal_cen' = "salinity -> % MSX Prevalence",
#     'M_Med_A_Bar_Per~MSX_Prev_Per' = "% MSX Prevalence -> % Mortality",
#     'Dermo_Prev_Per~temp_s_cen' = "Temperature -> % Dermo Prevalence",
#     'Dermo_Prev_Per~sal' = "salinity -> % Dermo Prevalence",
#     'Dermo_Med_Annual_Per~MSX_Prev_Per'="% Dermo Prevalence -> % Mortality"
# )

# to label facets:
fac_rel_labels <- c("Temperature -> % MSX Prevalence",
    "salinity -> % MSX Prevalence",
    "% MSX Prevalence -> % Mortality",
    "Temperature -> % Dermo Prevalence",
    "salinity -> % Dermo Prevalence",
    "% Dermo Prevalence -> % Mortality"
)

# rel_labeller <- function(variable,value){
#     return(rel_labels[value])
# }

ts_grp_2_3$fac_relationship <- factor(ts_grp_2_3$relationship, 
    levels = fac_lev_rel,
    labels = fac_rel_labels)

# plot the coefficients
ggplot(ts_grp_2_3, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    facet_wrap(~fac_relationship, scales ="free",ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Salinity Zone Group")+
    ylab("Estimate")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())
#theme(legend.position="top")+
#guides(color = F)
#guides( fill = guide_legend(order = 1), guide_legend(order =2))

#theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(write_fig_path, "/coefficients_", grp_names[7], ".png"), device = "png",
    width = 14, height = 8)

# Plot intercepts for 2ts/3sp model --------------------------------------------
# 

intercepts_organized <- GetParTable(fits[[7]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~1") %>% #get just the coefficients
    filter(lhs != "sal_cen") %>% # just taken from the data.
    filter(lhs != "temp_s_cen") %>% # just taken from the data
    #mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)        #approx 95% CI.

lhs_fac <- c("MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_A_Bar_Per")
lhs_fac_name <- c("MSX Prevalence", "Dermo Prevalence", "Mortality")
# make factors
intercepts_organized$group_name <- factor(intercepts_organized$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
intercepts_organized$sal_zone <- factor(intercepts_organized$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium","High"))
intercepts_organized$years <- factor(intercepts_organized$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
intercepts_organized$lhs <- factor(intercepts_organized$lhs, 
    levels = lhs_fac, 
    labels = lhs_fac_name)
# 
# intercepts_organized <- intercepts_organized %>%
#     arrange(lhs) %>% 
#     arrange(group_name) %>% 
#     select(years, sal_zone, lhs, est, se, lower_CI, upper_CI)

# plot the intercepts
ggplot(intercepts_organized, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    facet_wrap(~lhs, scales ="free") + #,ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Salinity Zone Group")+
    ylab("Estimate")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())

ggsave(paste0(write_fig_path, "/intercepts_", grp_names[7], ".png"), device = "png",
    width = 14, height = 0.5 + 8/2) #height: make approx half of the 2 row plots, but add extra 0.5 in for labels.

# Biplots of data --------------------------------------------------------------
# Add factors to dataset to plot in order.
# First, for  2 temporal groups and 2 spatial.
tmp_fac_order <- c("91to02Low", "02to17Low", "91to02MedHigh", "02to17MedHigh")
tmp_fac_order_lab <- c("1991-2002 Low", "2003-2017 Low", "1991-2002 Medium/High", "2003-2017 Medium/High")
# make a seperate factor column
dat$ts_2_spat_2_fac <- factor(dat$ts_2_spat_2, levels = tmp_fac_order, labels = tmp_fac_order_lab)
# now 2 temporal and 3 spatial groups
tmp_fac_order <- c("91to02Low", "02to17Low", "91to02Med", "02to17Med", "91to02High", "02to17High")
tmp_fac_order_lab <- c("1991-2002 Low", "2003-2017 Low", "1991-2002 Medium", "2003-2017 Medium", "1991-2002 High", "2003-2017 High")
# make a seperate factor column
dat$ts_2_spat_3_fac <- factor(dat$ts_2_spat_3, levels = tmp_fac_order, labels = tmp_fac_order_lab)

# only make the disease and M plots

#general dermo gg
dermo_M_gen_biplot <- ggplot(dat, aes(x = Dermo_Prev_Per, y = M_Med_A_Bar_Per))+
                        geom_smooth(se = F)+
                        geom_point()+
                        xlab("Dermo Prevalence (%)")+
                        #xlim(-10,110)+
                        ylab("Annual Mortality (%)")+
                        #ylim(-10,110)+
                        theme_classic(base_size = 18)+
                        theme(strip.background = element_blank())

# ggplot with 2 temporal and 2 spatial groups
dermo_M_gen_biplot + facet_wrap(~ts_2_spat_2_fac) 
ggsave(paste0(write_fig_path, "/biplot_ts_2_spat_2_M_v_Dermo.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
# ggplot with 2 temporal and 3 spatial groups.
dermo_M_gen_biplot + facet_wrap(~ts_2_spat_3_fac, ncol = 2) 
ggsave(paste0(write_fig_path, "/biplot_ts_2_spat_3_M_v_Dermo.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

#general MSX plot
MSX_M_gen_biplot <- ggplot(dat, aes(x = MSX_Prev_Per, y = M_Med_A_Bar_Per))+
                        geom_smooth(se = F, method = "lm")+
                        geom_point()+
                        xlab("MSX Prevalence (%)")+
                        #xlim(-10,110)+
                        ylab("Annual Mortality (%)")+
                        #ylim(-10,110)+
                        theme_classic(base_size = 18)+
                        theme(strip.background = element_blank())
# ggplot with 2 temporal and 2 spatial groups
MSX_M_gen_biplot + facet_wrap(~ts_2_spat_2_fac) 
ggsave(paste0(write_fig_path, "/biplot_ts_2_spat_2_M_v_MSX.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
# ggplot with 2 temporal and 3 spatial groups.
MSX_M_gen_biplot + facet_wrap(~ts_2_spat_3_fac, ncol = 2) 
ggsave(paste0(write_fig_path, "/biplot_ts_2_spat_3_M_v_MSX.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

# Make a 3d plot of disease and M using Plotly ---------------------------------
dermo_MSX_M_plot <- plot_ly(dat, 
                            x = ~MSX_Prev_Per, 
                            y = ~Dermo_Prev_Per, 
                            z = ~M_Med_A_Bar_Per, 
                            color =~ts_2_spat_2
                            ) %>% 
                     add_markers() %>% 
                     layout(scene = list(
                                         xaxis = list(title = "MSX"),
                                         yaxis = list(title = "Dermo"),
                                         zaxis = list(title = "Mortality")
                                         )
                            )
# not super informative.
# # Create a shareable link to your chart
# # Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="scatter3d-basic")
# chart_link

# conclusions ------------------------------------------------------------------
# Unclear which model should be selected, and conclusions differ somewhat.

