
# load pkgs, specify options -----
library(dplyr)

# specify to save output ----
tbl_path <- file.path("Derived_Data", "PostThesis_SEM", "14_create_tables")
dir.create(tbl_path)

# path to existing files ----

mod_sel_path <- file.path("Derived_Data", "PostThesis_SEM", 
                          "3_Model_Comparisons_M_By_Bar_base", 
                          "model_selection_model_run_2019_01_02.csv")
fit_measures_path <- file.path("Derived_Data", "PostThesis_SEM", 
                               "3_Model_Comparisons_M_By_Bar_base", 
                               "fit_measures.csv")
# functions ------

#' make the SEM_selection table
#' 
#' @param mod_sel Path to a .csv file containing the model section 
#'  measures, like AIC
#' @param fit_measures Path to a .csv file containing the fit measures,
#'  including
#' @return A formatted dataframe. These should be easy to 
#'  write to file and create tables in excel requiring little manual work.
make_tbl_sem_sel <- function(mod_sel, fit_measures, temporal = TRUE) {

  sel_df <- read.csv(mod_sel)
  fit_df <- read.csv(fit_measures)
  # make meta data
  meta <- data.frame(model = fit_df$model, 
             model_number = 1:9)
  split_fun <- function(x, temporal = TRUE) {
    if(temporal == TRUE) {
      first <- "_spat_"
      second <- "ts_"
      third <- "spat"
    } else {
      first <- "ts_\\d_"
      second <- "spat_"
      third <- "ts"
    }
    if(!is.na(x)) {
      first_split <- 
        strsplit(x, split = first)[[1]]
      if(nchar(first_split[1])== 0) {
        first_split <- first_split[2]
      } else {
        first_split <- first_split[1]
      }
      sec_split <- strsplit(first_split, split = second)[[1]]
      sec_split <- sec_split[length(sec_split)]
      if(length(grep(third, sec_split)) > 0) {
        sec_split <- 1
      }
    } else {
      sec_split <- 1
    }
    as.integer(sec_split)
  }
  meta$num_temp_grps <- unlist(lapply(meta$model, split_fun))
  meta$num_spat_grps <- unlist(lapply(meta$model, split_fun, temporal = FALSE))
  keep_sel_df <- sel_df[, grep("^m|^d", colnames(sel_df))]
  final_df <- dplyr::full_join(meta, keep_sel_df)
  final_df <- dplyr::full_join(final_df, fit_df)
  final_df <- final_df %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
}


# run funs to make and save tables ----
# SEM selection 
tbl_list <- make_tbl_sem_sel(mod_sel = mod_sel_path,
                             fit_measures = fit_measures_path)
write.csv(tbl_list, file.path(tbl_path, "SEM_selection.csv"))


# SEM_fit ----

#data_summary ---
dat <- read.csv(
  paste0("./Derived_Data/PostThesis_SEM/1_Organize_Inputs/dat_SEM_post_thesis_",
         "2019_01_02.csv"))

dat_long <- dat %>% 
  select(year, ID, ts_2_spat_2, temp_s, sal, MSX_Prev_Per, Dermo_Prev_Per,
         M_Med_A_Bar_Per) %>% 
  rename(group = ts_2_spat_2, Temperature = temp_s, Salinity = sal, 
         MSXPrevalence = MSX_Prev_Per, DermoPrevalence = Dermo_Prev_Per,
         Mortality = M_Med_A_Bar_Per) %>% 
  tidyr::gather(key = "Variable", value = "value", 4:8)
# TODO: need to gather the obs into 1 col, then compute mean, med, min, max on
# each
do_summaries <- list(
  mean = ~mean(.x), 
  median = ~median(.x),
  min = ~min(.x, na.rm = TRUE), 
  max = ~max(.x, na.rm = TRUE)
)

sum_table <- dat_long %>% 
            group_by(group, Variable) %>% 
          summarize(across(value, do_summaries)) %>% 
          tidyr::separate(col = group, into = c("Years", "Salinity Zone"), 
                          sep = 6)  %>% 
          rename(Mean = value_mean, Median = value_median, Min = value_min, 
                 Max = value_max)
sum_table$Years <- factor(sum_table$Years, levels = c("91to02", "02to17"), 
                          labels = c("1991-2002", "2003-2017"))
sum_table[["Salinity Zone"]] <- factor(sum_table[["Salinity Zone"]], 
                                       levels = c("Low", "MedHigh"),
                                         labels = c("Low", "Medium/High"))
sum_table$Variable <- factor(sum_table$Variable, 
                             levels = c( "Temperature", "Salinity", 
                                         "MSXPrevalence", "DermoPrevalence", 
                                         "Mortality"), 
                             labels = c("Temperature", "Salinity", 
                                        "MSX Prevalence", "Dermo Prevalence", 
                                        "Mortality"))
sum_table <- sum_table %>%
               arrange(Years) %>% 
               arrange(`Salinity Zone`) %>%
               arrange(Variable)
write.csv(sum_table, file.path(tbl_path, "SEM_fit.csv"))

#coefficients ---

#intercepts ---
