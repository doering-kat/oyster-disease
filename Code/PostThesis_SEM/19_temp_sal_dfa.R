# Performa a temp DFA and salinity DFA to have more clarity about
# whether or not summer temp has been lower and salinity has been higher
# across sites in recent years.

# other ideas to show this: breakpoint analysis? Statistical test? For
# difference between before 2002 and after 2002?

# TODO: note: may want to start from a different data source, because there
# is no reason to exclude freshets for this analysis and we need a complete
# time series (I think) for all of the time series.

# Pull this from a rawer form of the data.

# packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(MARSS)

# Function to Run the DFA
# Code to compare multiple models. use the MARSS package to estimate.

# fit models & store results
# NOTE: this will take a long time to run
# Run_DFA function: create and runs DFA models using MARSS.
# formals are:
# R, the covariance matrix (same as in MARSS package)
# dat_mat, the numeric matrix of data where NOAA codes are rows and years are cols
# m, the number of trends, which will vary by model.
# returns: a dfa_model fitted to dat_mat and for the specified covariance matrix
# and number of trends.

Run_DFA <- function(R = "diagonal and equal", dat_mat) {
  function(m) {
    # set the control params (min and max iterations, and the test to determine convergence)
    con_list <- list(minit = 200, maxit = 50000, conv.test.slope.tol = 0.1)
    dfa_model <- list(m = m, R = R) # put the R and m into the list of dfa model options.
    # run the model
    mod_run <- MARSS(dat_mat,
      model = dfa_model, control = con_list, form = "dfa",
      z.score = FALSE
    )
    return(mod_run)
  }
}



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

# year and bar limits ------
# Years
yr_0 <- 1990 # this year should NOT be used in the data set.
yr_1 <- yr_0 + 1
yr_max <- 2017
nyears <- yr_max - yr_0 # not including year 0

bars_unordered <- unique(dat$ID)

n_trends <- 1:4 # number of trends to consider in models.

# filter the sal dat -----
sal_dat <- dat_all_sal %>%
  filter(ID %in% bars_unordered) %>%
  filter(year >= yr_1) %>%
  filter(year <= yr_max) %>%
  rename(sal = Summer_avg) %>%
  dplyr::select(year, ID, sal) %>%
  arrange(ID, year) %>%
  distinct() # just in case

if (nrow(sal_dat) != nyears * length(bars_unordered)) {
  stop("Missing or extra values.")
} else {
  print("All good")
}

# makeinto a matrix where rows are the years and columns are each bar ID
sal_mat <- matrix(sal_dat$sal, nrow = nyears, ncol = length(unique(sal_dat$ID)))

colnames(sal_mat) <- unique(sal_dat$ID)
rownames(sal_mat) <- yr_1:yr_max

dat_wide_no_Z <- t(sal_mat) # transpose because we want each row to be a different bar and each year in a different column.

bars <- rownames(dat_wide_no_Z) # Save the ordered NOAA codes.

# Calculate the z scores for the data, which will make the model easier to run.
mean <- apply(dat_wide_no_Z, 1, mean, na.rm = T) # means for each row
sd <- apply(dat_wide_no_Z, 1, sd, na.rm = T) # standard dev for each row

# Get the z score by subtracting the mean from each value and then divided by the sd
dat_wide <- (dat_wide_no_Z - mean) / sd

# Get number of time series
N_ts <- dim(dat_wide)[1]

# run salinity dfa -----
# create a closure with the data set (use default for covariance matrix)
# use the same data set for all models.
Run_DFA_Clos <- Run_DFA(dat_mat = dat_wide)
# estimate a model for each of the trends using the closure.
mod_list <- lapply(n_trends, Run_DFA_Clos)
# add names to the list of models - number of trends and the cov matrix used.
names(mod_list) <- paste0(n_trends, "_trends_diagonal_and_equal")
# save the list of models
saveRDS(mod_list, paste0(der_dat_path, "/dfa_mod_list.rda"))

# calculate avg and sd's before and after 2002.
# That may be the easiest way to look at the question 
# of if environmental conditons were actually 
# better for oysters post 2002
sal_dat  %>% 
filter(year <= 2002)  %>% 
select(sal)  %>% 
summary()

sal_dat  %>% 
filter(year > 2002)  %>% 
select(sal)  %>% 
summary()


sal_dat$yr_fac <- ifelse(sal_dat$year <= 2002, "91to02", "03to17")
sal_dat$yr_fac <- as.factor(sal_dat$yr_fac)
ggplot(sal_dat, aes(x = yr_fac, y = sal))+
geom_boxplot()
