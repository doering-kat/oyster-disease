# Create figures for the "best model", with 2 time groups and 2 spatial groups.
# 
# Created 31 Oct 2018 by Kathryn Doering

# Load Packages-----------------------------------------------------------------
library(lavaan)
library(tidyverse)
library(semPlot)
library(semTools)
options(stringsAsFactors = F)
write_files <- T #set to false if do not want to save new data.

# Load data and results --------------------------------------------------------
# Results from SEM.
save_dir_name_fits<- "./Derived_Data/SEM/5_SEM_spatial_temporal_models"
file_date_fits <- "2018_10_17"
fits <- readRDS(paste0(save_dir_name_fits, "/lavaan_fits_", file_date_fits, ".rds"))
# list of group names for each of the 9 models (in same order as fits)
grp_names <- c(NA,"ts_2", "ts_4", "spat_2", "spat_3", "ts_2_spat_2", 
    "ts_2_spat_3", "ts_4_spat_2", "ts_4_spat_3")

# load the data used to make these models
file_date_dat <- "2018_10_17"
dat <- read.csv(paste0("./Derived_Data/SEM/4_Bind_Data_for_SEM/SEM_dat_no_NA_", file_date_dat, ".csv"))

# Create folders for derived data,figures---------------------------------------
# set folder names
fig_fold_name <- "./Figures/Create_Figures/3_Create_SEM_2_ts_2_spat_figs"
der_dat_sec_fold_name <- "./Derived_Data/Create_Figures" #need to create first
der_dat_fold_name <- paste0(der_dat_sec_fold_name, "/3_Create_SEM_2_ts_2_spat_figs")
# create folders
dir.create(fig_fold_name)
dir.create(der_dat_sec_fold_name)
dir.create(der_dat_fold_name)

# Functions --------------------------------------------------------------------
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
                            levels = factor_levels_grp,
                           labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
ts_grp$sal_zone <- factor(ts_grp$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
ts_grp$years <- factor(ts_grp$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
#order relationships
fac_lev_rel <- paste0("MSX_Prev_Per~", c("temp_s", "sal"))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~MSX_Prev_Per")
fac_lev_rel <- c(fac_lev_rel, paste0("Dermo_Prev_Per~", c("temp_s", "sal")))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~Dermo_Prev_Per")

# to label facets:
rel_labels <- list(
    'MSX_Prev_Per~temp_s' = "Temperature -> % MSX Prevalence",
    'MSX_Prev_Per~sal' = "Salinity -> % MSX Prevalence",
    'M_Med_Annual_Per~MSX_Prev_Per' = "% MSX Prevalence -> % Mortality",
    'Dermo_Prev_Per~temp_s' = "Temperature -> % Dermo Prevalence",
    'Dermo_Prev_Per~sal' = "Salinity -> % Dermo Prevalence",
    'Dermo_Med_Annual_Per~MSX_Prev_Per'="% Dermo Prevalence -> % Mortality"
                  )

# to label facets:
fac_rel_labels <- c("Temperature -> % MSX Prevalence",
                    "Salinity -> % MSX Prevalence",
                    "% MSX Prevalence -> % Mortality",
                    "Temperature -> % Dermo Prevalence",
                    "Salinity -> % Dermo Prevalence",
                    "% Dermo Prevalence -> % Mortality"
                    )

rel_labeller <- function(variable,value){
    return(rel_labels[value])
}

ts_grp$relationship <- factor(ts_grp$relationship, 
                        levels = fac_lev_rel,
                        labels = fac_rel_labels)

# plot the coefficients
ggplot(ts_grp, aes(x = group_name, y = est)) +
    geom_col(aes(fill = sal_zone), col = "black")+#,col = years)) +
    geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Salinity Zone")+
    facet_wrap(~relationship, scales ="free",ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Year")+
    ylab("Estimate")+
    theme_classic(base_size = 15)#+
    #theme(legend.position="top")+
    #guides(color = F)
    #guides( fill = guide_legend(order = 1), guide_legend(order =2))

    #theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(fig_fold_name, "/coefficients_", grp_names[6], ".png"), device = "png",
    width = 14, height = 8)


# Todo: make a version of this where color is for the years, and salinity zone is
# instead labeled underneath the bars (somehow)

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
fac_lev_rel <- paste0("MSX_Prev_Per~", c("temp_s", "sal"))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~MSX_Prev_Per")
fac_lev_rel <- c(fac_lev_rel, paste0("Dermo_Prev_Per~", c("temp_s", "sal")))
fac_lev_rel <- c(fac_lev_rel, "M_Med_Annual_Per~Dermo_Prev_Per")

# to label facets:
rel_labels <- list(
    'MSX_Prev_Per~temp_s' = "Temperature -> % MSX Prevalence",
    'MSX_Prev_Per~sal' = "Salinity -> % MSX Prevalence",
    'M_Med_Annual_Per~MSX_Prev_Per' = "% MSX Prevalence -> % Mortality",
    'Dermo_Prev_Per~temp_s' = "Temperature -> % Dermo Prevalence",
    'Dermo_Prev_Per~sal' = "Salinity -> % Dermo Prevalence",
    'Dermo_Med_Annual_Per~MSX_Prev_Per'="% Dermo Prevalence -> % Mortality"
)

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
ggsave(paste0(fig_fold_name, "/coefficients_", grp_names[6], "_2.png"), device = "png",
    width = 14, height = 8)

# Plot intercepts of model (MSX , Dermo, M) ------------------------------------

intercepts_organized <- GetParTable(fits[[6]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~1") %>% #get just the coefficients
    filter(lhs != "sal") %>% # just taken from the data.
    filter(lhs != "temp_s") %>% # just taken from the data
    #mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)        #approx 95% CI.

lhs_fac <- c("MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_Annual_Per")
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

ggsave(paste0(fig_fold_name, "/intercepts_", grp_names[6], ".png"), device = "png",
    width = 14, height = 0.5 + 8/2) #height: make approx half of the 2 row plots, but add extra 0.5 in for labels.

# plot residual variance for MSX, Dermo, and Mortality -------------------------
# 
var_to_plot <- GetParTable(fits[[6]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~~") %>% #get just the coefficients
    filter(lhs %in% c("M_Med_Annual_Per", "Dermo_Prev_Per", "MSX_Prev_Per")) %>% #only residual var terms.
    #mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se) %>%         #approx 95% CI.
    select(-op) %>%  #dont need these columns anymore.
    select(-rhs)
  
# make factors in order to sort more easily
var_to_plot$group_name <- factor(var_to_plot$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
var_to_plot$sal_zone <- factor(var_to_plot$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
var_to_plot$years <- factor(var_to_plot$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
# make factors in order to sort and reame: 
lhs_fac <- c("MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_Annual_Per")
lhs_fac_name <- c("MSX Prevalence", "Dermo Prevalence", "Mortality")
var_to_plot$lhs <- factor(var_to_plot$lhs, 
    levels = lhs_fac, 
    labels = lhs_fac_name)

# plot the intercepts
ggplot(var_to_plot, aes(x = sal_zone, y = est, fill = years)) +
    geom_col(position=position_dodge(), color='black')+ #,col = years)) +
    # need to sort out the line range to do this.
    #geom_linerange(aes(ymin = lower_CI, ymax = upper_CI))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,position=position_dodge(.9))+
    scale_fill_grey(start = 0.8, end = 0.5, name = "Time Period")+
    scale_y_continuous(expand = c(0,0))+
    facet_wrap(~lhs, scales ="free") + #,ncol = 3, labeller = as_labeller(rel_labels))+
    xlab("Salinity Zone Group")+
    ylab("Residual Variance")+
    theme_classic(base_size = 15)+
    theme(strip.background = element_blank())

ggsave(paste0(fig_fold_name, "/resid_var_", grp_names[6], ".png"), device = "png",
    width = 14, height = 0.5 + 8/2) #height: make approx half of the 2 row plots, but add extra 0.5 in for labels.
# Organize par table -----------------------------------------------------------
# Do this for the paper
ts_grp_organize <- ts_grp %>%
                    arrange(group_name) %>%
                    arrange(relationship) %>% 
                     select(years, sal_zone, relationship, est, se, lower_CI, upper_CI)

write.csv(ts_grp_organize, paste0(der_dat_fold_name, "/coef_2_ts_2_spat_organized.csv"), row.names = 
F)

# make an intercepts table -----------------------------------------------------
intercepts_organized <- GetParTable(fits[[6]]) %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~1") %>% #get just the coefficients
    #mutate(relationship = paste0(lhs, op, rhs)) %>%
    #break up year and salinity zon groups into separate columns
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
    mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
    #calculate error
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)        #approx 95% CI.

lhs_fac <- c("temp_s", "sal", "MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_Annual_Per")
lhs_fac_name <- c("Temperature", "Salinity", "MSX Prevalence", "Dermo Prevalence", "Mortality")
# make factors
intercepts_organized$group_name <- factor(intercepts_organized$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
intercepts_organized$sal_zone <- factor(intercepts_organized$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
intercepts_organized$years <- factor(intercepts_organized$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
intercepts_organized$lhs <- factor(intercepts_organized$lhs, 
                                    levels = lhs_fac, 
                                    labels = lhs_fac_name)

intercepts_organized <- intercepts_organized %>%
                            arrange(group_name) %>%
                            arrange(lhs) %>% 
                            select(years, sal_zone, lhs, est, se, lower_CI, upper_CI)

write.csv(intercepts_organized, paste0(der_dat_fold_name, "/intercepts_2_ts_2_spat_organized.csv"), row.names = F)

#variances and covariances table, organized.-------------------------------------
var_organized <- GetParTable(fits[[6]]) %>%
                            select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
                            filter(op == "~~") %>% #get just the coefficients
                            #mutate(relationship = paste0(lhs, op, rhs)) %>%
                            #break up year and salinity zon groups into separate columns
                            mutate(years = substr(group_name, start = 1, stop = 6)) %>%
                            mutate(sal_zone = substring(group_name, first = 7)) %>%
                            mutate(se = ifelse(se <= 0.0001,NA,se)) %>% 
                            #calculate error
                            mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
                            mutate(lower_CI = est - 1.96*se)        #approx 95% CI.
# to do: finish these factors!  
# make factors in order to sort more easily
var_organized$group_name <- factor(var_organized$group_name,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
var_organized$sal_zone <- factor(var_organized$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
var_organized$years <- factor(var_organized$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))
# make factors in order to sort and reame: 
var_organized$lhs <- factor(var_organized$lhs, 
                levels = lhs_fac, 
                labels = lhs_fac_name)
var_organized$rhs <- factor(var_organized$rhs, 
                        levels = lhs_fac, #because are the same as left.
                        labels = lhs_fac_name) # because are the same as left.


# rearrange table
var_organized <- var_organized %>%
    arrange(lhs) %>% 
    arrange(rhs) %>% 
    arrange(group_name) %>% 
    select(years, sal_zone, lhs,rhs, est, se, lower_CI, upper_CI)

write.csv(var_organized, paste0(der_dat_fold_name, "/var_covar_2_ts_2_spat_organized.csv"), row.names = F)


# Create biplots of data -------------------------------------------------------

dat_plot <- dat
# make ts_2_spat_2 into a factor (so that facet wraps can be ordered, and also labeled differently)
ts_2_spat_2_fac_order <- c("91to02Low", "02to17Low", "91to02MedHigh", "02to17MedHigh")
ts_2_spat_2_fac_order_lab <- c("1991-2002 Low", "2003-2017 Low", "1991-2002 Medium/High", "2003-2017 Medium/High")
# make a seperate factor column
dat_plot$ts_2_spat_2_fac <- factor(dat_plot$ts_2_spat_2, levels = ts_2_spat_2_fac_order, labels = ts_2_spat_2_fac_order_lab)

#Temp V Dermo
ggplot(dat_plot, aes(x = temp_s, y = Dermo_Prev_Per))+
    geom_smooth()+
    geom_point()+
    ylab("Dermo Prevalence (%)")+
    #ylim(0,100)+
    xlab("Temperature (C)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_Dermo_v_temp.png"), 
      device = "png", width = 6.5*2, height = 3.5*2)
#Temp V MSX
ggplot(dat_plot, aes(x = temp_s, y = MSX_Prev_Per))+
    geom_smooth()+
    geom_point()+
    ylab("MSX Prevalence (%)")+
    #ylim(0,100)+
    xlab("Temperature (C)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_MSX_v_temp.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

#Sal V Dermo
ggplot(dat_plot, aes(x = sal, y = Dermo_Prev_Per))+
    geom_smooth()+
    geom_point()+
    ylab("Dermo Prevalence (%)")+
    #ylim(0,100)+
    xlab("Salinity (ppt))")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_Dermo_v_sal.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
#MSX v. sal
ggplot(dat_plot, aes(x = sal, y = MSX_Prev_Per))+
    geom_smooth()+
    geom_point()+
    ylab("MSX Prevalence (%)")+
    #ylim(0,100)+
    xlab("Salinity (ppt))")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_MSX_v_sal.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
#Dermo V M
ggplot(dat_plot, aes(x = Dermo_Prev_Per, y = M_Med_Annual_Per))+
    geom_smooth()+
    geom_point()+
    xlab("Dermo Prevalence (%)")+
    #xlim(-10,110)+
    ylab("Annual Mortality (%)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_M_v_Dermo.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
#MSX v M - no jitter and some loess
ggplot(dat_plot, aes(x = MSX_Prev_Per, y = M_Med_Annual_Per))+
#geom_smooth()+
geom_point() +
    xlab("MSX Prevalence (%)")+
    #xlim(0,100)+
    ylab("Annual Mortality (%)")+
    #ylim(0,100)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_M_v_MSX_no_jitter.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

ggplot(dat_plot, aes(x = MSX_Prev_Per, y = M_Med_Annual_Per))+
    #geom_smooth()+
    geom_jitter(width = 2.5) +
    xlab("MSX Prevalence (%)")+
    #xlim(0,100)+
    ylab("Annual Mortality (%)")+
    #ylim(0,100)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_M_v_MSX_jitter_2point5.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)


# other biplots that are less important ----------------------------------------
# Use dat_plot from previous biplot section.
# temp v. salinity
ggplot(dat_plot, aes(x = temp_s, y = sal))+
    geom_smooth()+
    geom_point()+
    ylab("Salinity (ppt)")+
    #ylim(0,100)+
    xlab("Temperature (C)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_sal_v_temp.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
# MSX v dermo
ggplot(dat_plot, aes(x = MSX_Prev_Per, y = Dermo_Prev_Per))+
    #geom_smooth()+
    geom_jitter(height = 2.5, width =2.5)+
    ylab("Dermo Prevalence (%)")+
    #ylim(0,100)+
    xlab("MSX Prevalence (%)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_Dermo_v_MSX_jitter_2point5.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

# M v temp
ggplot(dat_plot, aes(x = temp_s, y = M_Med_Annual_Per))+
    geom_smooth()+
    geom_point()+
    ylab("Annual Mortality (%)")+
    #ylim(0,100)+
    xlab("Temperature (C)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_M_v_temp.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)
# M v salinity
ggplot(dat_plot, aes(x = sal, y = M_Med_Annual_Per))+
    geom_smooth()+
    geom_point()+
    ylab("Annual Mortality (%)")+
    #ylim(0,100)+
    xlab("Salinity (ppt)")+
    #ylim(-10,110)+
    facet_wrap(~ts_2_spat_2_fac)+
    theme_classic(base_size = 18)+
    theme(strip.background = element_blank())
ggsave(paste0(fig_fold_name, "/biplot_ts_2_spat_2_M_v_sal.png"), 
    device = "png", width = 6.5*2, height = 3.5*2)

# summarize data by groups -----------------------------------------------------

#function that computes multiple summary statistics on a column variable sum_var
#using a grouping variable group_var that are both columns in the dataset dat.
# dat: a dataframe.
# group_var: a grouping variable and column in dat.
# sum_var: the variable to summarize, also a column in dat.
sum_var <- function(dat,group_var, sum_var){
               require(dplyr)
               # add quotes to the variables (returns as quosure)
               group_var <- enquo(group_var)
               sum_var <- enquo(sum_var) #in order to use in summarize function
               
               sum_dat <-  dat %>% 
                    group_by(!! group_var) %>%  # !! takes away quotes.
                    summarize(
                        min = min(!! sum_var),
                        max = max(!! sum_var),
                        med = median(!! sum_var),
                        mean = mean(!! sum_var)
                           )
               return(sum_dat)
}

# #todo: figure out how to write function to loop through strings; not sure how to
# #do this using dplyr.
# vars <- c("temp_s", "sal","MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_Annual_Per")
# then can get summaries with map

# use function and put results in a data frame.
sum_dat <- sum_var(dat, ts_2_spat_2, temp_s) %>% 
                mutate(var = "temp_s")
sum_dat <- sum_var(dat, ts_2_spat_2, sal)%>% 
                mutate(var = "sal") %>% 
                bind_rows(sum_dat)
sum_dat <- sum_var(dat, ts_2_spat_2, MSX_Prev_Per)%>% 
              mutate(var = "MSX_Prev_Per")%>% 
             bind_rows(sum_dat)
sum_dat <- sum_var(dat, ts_2_spat_2, Dermo_Prev_Per)%>% 
               mutate(var = "Dermo_Prev_Per")%>% 
               bind_rows(sum_dat)
sum_dat <- sum_var(dat, ts_2_spat_2, M_Med_Annual_Per) %>% 
               mutate(var = "M_Med_Annual_Per")%>% 
               bind_rows(sum_dat)
# edit this data frame to make into a table.
sum_dat <-   sum_dat %>% 
                mutate(years = substr(ts_2_spat_2, start = 1, stop = 6)) %>%
                mutate(sal_zone = substring(ts_2_spat_2, first = 7))
    # use factors to order groups
factor_levels_sal <- c("Low", "MedHigh")
factor_levels_yr <- c("91to02", "02to17") # these groups were misnamed, but actually represent 91to02 and 03t017.
factor_levels_grp <- paste0(rep(factor_levels_yr, times = length(factor_levels_sal)),rep(factor_levels_sal, each = length(factor_levels_yr)))

sum_dat$ts_2_spat_2 <- factor(sum_dat$ts_2_spat_2,
    levels = factor_levels_grp)#,
# labels = c("1991-2002","2003-2017", "1991-2002 ","2003-2017 " ))
sum_dat$sal_zone <- factor(sum_dat$sal_zone, levels = factor_levels_sal, labels = c("Low", "Medium/High"))
sum_dat$years <- factor(sum_dat$years, levels = factor_levels_yr, labels = c("1991-2002", "2003-2017"))

# variables
vars <- c("temp_s", "sal","MSX_Prev_Per", "Dermo_Prev_Per", "M_Med_Annual_Per")
vars_names <- c("Temperature", "Salinity", "MSX Prevalence", "Dermo Prevalence",
                "Mortality")
sum_dat$var <- factor(sum_dat$var, levels = vars, labels = vars_names)

#organize
sum_dat <- sum_dat %>% 
            arrange(years) %>% 
            arrange(sal_zone) %>% 
            arrange(var) %>% 
            select(years, sal_zone, var, mean, med, min, max)
write.csv(sum_dat, paste0(der_dat_fold_name, "/dat_summary_2_ts_2_spat_organized.csv"), row.names = F)
