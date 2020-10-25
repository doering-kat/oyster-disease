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
options(stringsAsFactors = FALSE)

# Load data ----
par_tbl_folder <- file.path("Derived_Data", "PostThesis_SEM",
                      "11_Model_Comparisons_temp_sal_window_change")
par_tbl_filenames <- grep("par_table_Scenario", list.files(par_tbl_folder, 
                                                           full.names = TRUE), 
                          value = TRUE)
par_tbl_list <- lapply(par_tbl_filenames, function(x) read.csv(x))
names(par_tbl_list) <- basename(par_tbl_filenames)

scen <- read.csv("./Data/scenarios_temp_sal_window_2019_01_07.csv")

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
    mutate(years = substr(group_name, start = 1, stop = 6)) %>%
    mutate(sal_zone = substring(group_name, first = 7)) %>%
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
    if (tbl_2_row[tbl_2_row$years == "91to02","upper_CI"] <
        tbl_2_row[tbl_2_row$years == "02to17", "lower_CI"]) {
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


test <- calc_CIs(par_tbl_list[[1]])
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

scen_paste <- mutate(scen, Scenario = paste0("temp_", temp_start, "-" ,temp_end, "; sal_", sal_start,"-", sal_end))
scen_num_key <- select(scen_paste, scen_number, Scenario, scenario.type)

# Add scen_number to the change_df
change_df_all <- left_join(change_df_all, scen_num_key, by = "scen_number")
change_df_all$Scenario_fac <- factor(change_df_all$scen_number, levels = scen_num_key$scen_number, labels = scen_num_key$Scenario )

# save qualitative change data ----
# TODO

# plot and save qualitative change ----
# to do: figure out how to plot; 

ggplot2::ggplot(data = change_df_all, aes(x = Scenario_fac, y = sal_zone)) +
  geom_tile(aes(fill = change), color = "black") +
  facet_wrap(.~relationship)+
  theme_classic(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(file.path(fig_save_path, "qual_diff_tile_plot.png"), height = 8,
  width = 12, units = "in")



