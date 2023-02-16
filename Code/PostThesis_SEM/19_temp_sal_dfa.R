# create a histogram of temps

# packages ----
library(dplyr)
library(tidyr)
library(ggplot2)

# data ----
# note: using older version of data set (because did not save the newer
# version) that has some duplicated rows
# that will need to be removed.
# note this data set is identical if the duplicated rows are removed.
dat_all_sal <- read.csv(file.path("Derived_Data", "SEM", "3_Summarize_Filtered_Kriging_Output", "sal_avg_months_4_to_10_2018_10_17.csv"))

dat_path <- file.path(
  "Derived_Data", "PostThesis_SEM",
  "1_Organize_Inputs", "dat_SEM_post_thesis_2019_01_02.csv"
)
dat <- read.csv(dat_path, stringsAsFactors = FALSE)

# note: some duplicates potentially in this dataset
# init_rows <- nrow(dat)
# # rm dupes
# dat <- dplyr::distinct(dat)
# # should have been 80 duplicated rows removed
# init_rows - nrow(dat) # 80.

# folders to store results -----
der_dat_path <- file.path("Derived_Data", "PostThesis_SEM", "19_temp_sal_dfa")
dir.create(der_dat_path)
fig_path <- file.path("Figures", "PostThesis_SEM", "19_temp_sal_dfa")
dir.create(fig_path)

# Plot sal/temp histograms ----


