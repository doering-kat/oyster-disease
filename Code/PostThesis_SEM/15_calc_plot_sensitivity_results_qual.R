# A new figure that would better summarize what I am looking for with the sensitivities:
# Look at change over time for each salinity zone. look at if the confidence
# intervals overlap or not; If they don’t, report direction of change. Do this for 
# all of the scenarios
# Just looks at if the qualitative direction of change holds over time

# load packages, set options ----
library(tidyr)
library(dplyr)
library(DescTools)
library(ggplot2)
library(patchwork)
options(stringsAsFactors = FALSE)

# Load data ----
par_tbl_folder <- file.path("Derived_Data", "PostThesis_SEM",
                      "11_Model_Comparisons_temp_sal_window_change")
par_tbl_filenames <- grep("par_table_Scenario", list.files(par_tbl_folder, 
                                                           full.names = TRUE), 
                          value = TRUE)
par_tbl_filenames <- grep("2021", par_tbl_filenames, value = TRUE)
par_tbl_list <- lapply(par_tbl_filenames, function(x) read.csv(x))
names(par_tbl_list) <- basename(par_tbl_filenames)

scen <- read.csv("./Data/scenarios_temp_sal_window_2019_01_07.csv")
scen$breakpt_year <- 2002 # the last year for the first group.

# change the group names to make consistent across scenarios.
par_tbl_list<- lapply(par_tbl_list, function(x){
  tmp_grp_name <- gsub("91to02", "Beg",  x[["group_name"]])
  tmp_grp_name <- gsub("02to17", "End", tmp_grp_name)
  x[["group_name"]] <- tmp_grp_name
  x
  })

# add data for the TS_breakpoint_change ----

par_tbl_breakpt_folder <- file.path("Derived_Data", "PostThesis_SEM", 
                                     "9_Model_Comparisons_TS_breakpoint_change")
par_tbl_filenames_breakpt <- grep("par_table", list.files(par_tbl_breakpt_folder, 
                                                           full.names = TRUE),
                          value = TRUE)
par_tbl_filenames_breakpt <- grep("2021", par_tbl_filenames_breakpt, value = TRUE)
par_tbl_list_breakpt <- lapply(par_tbl_filenames_breakpt, function(x) read.csv(x))

scen_breakpt <- strsplit(basename(par_tbl_filenames_breakpt), split = "_" )
scen_breakpt <- unlist(lapply(scen_breakpt, function(x) x[3]))
scen_df_breakpt <- data.frame(
  scen_number = (max(scen$scen_number) + 1):(max(scen$scen_number) + length(scen_breakpt)), 
  temp_start = 4,
  temp_end = 10, 
  sal_start = 4, 
  sal_end = 10,
  scenario.type = "breakpoint_change", 
  breakpt_year = scen_breakpt)
# make temperature consistent with the other list
par_tbl_list_breakpt <- lapply(par_tbl_list_breakpt, function(x) {
    x[["rhs"]] <- gsub("^temp_s", "wtemp", x[["rhs"]])
    x[["lhs"]] <- gsub("^temp_s", "wtemp", x[["lhs"]])
    x
  })

names(par_tbl_list_breakpt) <- paste0("par_table_Scenario_", (max(scen$scen_number) + 1):(max(scen$scen_number) + length(scen_breakpt)))

# put together lists ----

par_tbl_list <- append(par_tbl_list, par_tbl_list_breakpt)
scen <- rbind(scen, scen_df_breakpt)

# setup save locations ----
script_name <- "15_calc_plot_sensitivity_results_qual"
fig_save_path <- 
  file.path("Figures", "PostThesis_SEM", script_name)
der_dat_save_path <- file.path("Derived_Data", "PostThesis_SEM",
                               script_name)
dir.create(fig_save_path)
dir.create(der_dat_save_path)

# calculate qualitative change ----

calc_CIs <- function(parTable){
  change_parTable <-  parTable %>%
    select(lhs,op,rhs,group, est, se, group_name) %>%  #just the essentials
    filter(op == "~") %>% #get just the coefficients
    mutate(relationship = paste0(lhs, op, rhs)) %>% 
    mutate(years = substr(group_name, start = 1, stop = 3)) %>%
    mutate(sal_zone = substring(group_name, first = 4)) %>%
    mutate(upper_CI = est + 1.96*se) %>% #approx 95% CI
    mutate(lower_CI = est - 1.96*se)  %>%        #approx 95% CI.
    select(years, sal_zone, relationship, upper_CI, lower_CI)
  return(change_parTable)
}

get_qual_change <- function(tbl_2_row) {
  if(nrow(tbl_2_row) != 2) stop("assumes 2 rows")
  overlaps <- tbl_2_row[1, c("lower_CI", "upper_CI")] %overlaps% 
    tbl_2_row[2, c("lower_CI", "upper_CI")]
  if(overlaps) {
    status <- NA
  } else {
    start_name <- grep("(91)|(Beg)", tbl_2_row[["years"]], value = TRUE)
    end_name <- grep("(17)|(End)", tbl_2_row[["years"]], value = TRUE)
    if (tbl_2_row[tbl_2_row$years == start_name,"upper_CI"] <
        tbl_2_row[tbl_2_row$years == end_name, "lower_CI"]) {
      status <- "+"
    } else {
      status <- "-"
    }
  }
  status
}

get_change_df <- function(tbl) {
change_df <- NULL
sal_zones <- unique(tbl$sal_zone)
rel <- unique(tbl$relationship)
for (s in sal_zones) {
  for (r in rel) {
    tmp_df <- tbl[tbl$sal_zone == s & tbl$relationship == r, ]
    tmp_change <- get_qual_change(tmp_df)
    tmp_change_df <- data.frame(sal_zone = s,
                                relationship = r,
                                change = tmp_change)
    change_df <- rbind(change_df, tmp_change_df)
  }
}
change_df
}


# test if works
test <- calc_CIs(par_tbl_list[[5]])
get_change_df(test)

CI_list <- lapply(par_tbl_list, calc_CIs)
change_df_list <- lapply(CI_list, get_change_df)
# set names

change_df_all <- NULL
names(change_df_list) <- unlist(lapply(names(par_tbl_list), function(x) strsplit(x, "_")[[1]][4]))
for(i in 1:length(change_df_list)) {
  tmp_scen_number <- names(change_df_list)[i]
  tmp_df <- change_df_list[[i]]
  tmp_df$scen_number <- as.integer(tmp_scen_number)
  change_df_all <- rbind(change_df_all, tmp_df)
}

scen_paste <- mutate(scen, Scenario = paste0("temp_", temp_start, "-" ,temp_end, "; sal_", sal_start,"-", sal_end, "; breakpt_", breakpt_year ))
scen_num_key <- select(scen_paste, scen_number, Scenario, scenario.type)

# Add scen_number to the change_df
change_df_all <- left_join(change_df_all, scen_num_key, by = "scen_number")
change_df_all$Scenario_fac <- factor(change_df_all$scen_number, levels = scen_num_key$scen_number, labels = scen_num_key$Scenario )

change_df_all$Scenario_fac_order <- factor(change_df_all$Scenario, 
 levels = c(paste0(c( "temp_4-10; sal_4-10", #base scenario
            "temp_1-2; sal_3-8", "temp_1-2; sal_4-8", "temp_1-2; sal_5-8", "temp_1-2; sal_6-8", "temp_1-2; sal_7-8",
            "temp_1-2; sal_3-9", "temp_1-2; sal_4-9", "temp_1-2; sal_5-9", "temp_1-2; sal_6-9", "temp_1-2; sal_7-9",
            "temp_1-2; sal_3-10", "temp_1-2; sal_4-10", "temp_1-2; sal_5-10", "temp_1-2; sal_6-10", "temp_1-2; sal_7-10", 
            # summer temp scens 
            "temp_3-8; sal_3-8", "temp_4-8; sal_4-8", "temp_5-8; sal_5-8", "temp_6-8; sal_6-8", "temp_7-8; sal_7-8",
            "temp_3-9; sal_3-9", "temp_4-9; sal_4-9", "temp_5-9; sal_5-9", "temp_6-9; sal_6-9", "temp_7-9; sal_7-9",
            "temp_3-10; sal_3-10",                  "temp_5-10; sal_5-10", "temp_6-10; sal_6-10", "temp_7-10; sal_7-10"
             ), "; breakpt_2002"), 
            # breakpoint scenarios
            paste0("temp_4-10; sal_4-10; breakpt_", scen_breakpt[-4])), # need to exclude the 2002 scenario
 labels = c( "base", #base scenario
             "sal_3-8", "sal_4-8", "sal_5-8", "sal_6-8", "sal_7-8",
             "sal_3-9", "sal_4-9", "sal_5-9", "sal_6-9", "sal_7-9",
             "sal_3-10", "sal_4-10", "sal_5-10", "sal_6-10", "sal_7-10", 
             
             "temp 3-8; sal 3-8", "temp 4-8; sal 4-8", "temp 5-8; sal 5-8", "temp 6-8; sal 6-8", "temp 7-8; sal 7-8",
             "temp 3-9; sal 3-9", "temp 4-9; sal 4-9", "temp 5-9; sal 5-9", "temp 6-9; sal 6-9", "temp 7-9; sal 7-9",
             "temp 3-10; sal 3-10",                  "temp 5-10; sal 5-10", "temp 6-10; sal 6-10", "temp 7-10; sal 7-10", 
             #breakpoint scenarios
             scen_breakpt[-4]
             ))

change_df_all$sal_zone_fac <- factor(change_df_all$sal_zone, levels = c("Low", "MedHigh"), labels = c("Low", "Med./High"))
change_df_all$relationship_fac <-factor(change_df_all$relationship,
  levels =  c("MSX_Prev_Per~wtemp_cen", "MSX_Prev_Per~sal_cen", "M_Med_A_Bar_Per~MSX_Prev_Per",
              "Dermo_Prev_Per~wtemp_cen", "Dermo_Prev_Per~sal_cen", "M_Med_A_Bar_Per~Dermo_Prev_Per"), 
  labels = c("Temperature -> % MSX Prevalence", "Salinity -> % MSX Prevalence", "MSX % Prevalence -> % Mortality", 
             "Temperature -> % Dermo Prevalence", "Salinity -> % Dermo Prevalence", "Dermo % Prevalence -> % Mortality"))

change_df_all[change_df_all$Scenario == "temp_4-10; sal_4-10; breakpt_2002" & change_df_all$scenario.type == "summer_temp_sal","scenario.type"] <- "base"
# save qualitative change data ----
# TODO

# plot and save qualitative change ----
# to do: figure out how to plot; 

# ggplot2::ggplot(data = change_df_all, aes(x = Scenario_fac_order, y = sal_zone_fac)) +
#   geom_tile(aes(fill = change), color = "black") +
#   facet_wrap(.~relationship_fac)+
#   theme_classic(base_size = 12)+
#   xlab("Scenario")+
#   ylab("Salinity Zone")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave(file.path(fig_save_path, "qual_diff_tile_plot.png"), height = 8,
#   width = 12, units = "in")


# split winter scenarios

winter_plot <- ggplot2::ggplot(data = subset(change_df_all, scenario.type %in% c("base", "winter_temp")), aes(x = Scenario_fac_order, y = sal_zone_fac)) +
  geom_tile(aes(fill = change), color = "black") +
  facet_wrap(.~relationship_fac)+
  theme_classic(base_size = 14)+
  xlab("Winter Temp Scenario")+
  ylab("Salinity Zone")+
  labs(fill = "Change over time" ) +
  scale_fill_manual(values = c("#f0f0f0", "#636363"), na.value = "#bdbdbd")+
  theme(legend.position = "top") +
  guides(fill = guide_legend(title.position = "top"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

summer_plot <- ggplot2::ggplot(data = subset(change_df_all, scenario.type %in% c("base", "summer_temp_sal")), aes(x = Scenario_fac_order, y = sal_zone_fac)) +
  geom_tile(aes(fill = change), color = "black") +
  facet_wrap(.~relationship_fac)+
  theme_classic(base_size = 14)+
  xlab("Summer Temp Scenario")+
  ylab("Salinity Zone")+
  scale_fill_manual(values = c("#f0f0f0", "#636363"), na.value = "#bdbdbd")+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

breakpt_plot <- ggplot2::ggplot(data = subset(change_df_all, scenario.type == "breakpoint_change"), aes(x = Scenario_fac_order, y = sal_zone_fac)) +
  geom_tile(aes(fill = change), color = "black") +
  facet_wrap(.~relationship_fac)+
  theme_classic(base_size = 14)+
  xlab("Breakpoint Scenario")+
  ylab("Salinity Zone")+
  scale_fill_manual(values = c("#f0f0f0", "#636363"), na.value = "#bdbdbd")+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


winter_plot / summer_plot / breakpt_plot +
  plot_annotation(tag_levels = "A")

ggsave(file.path(fig_save_path, "qual_sens_plot_publication_all.png"), width = 12, height = 10, 
       units = "in", dpi = "print" )
