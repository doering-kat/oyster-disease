# create a boxplot/anova for sal and temp in the 2 spatio/2temporal
# groupings model.

# This is intended to illuminate if salinity and temp conditions
# where indeed better at oyster bars in the more recent years.

# The anova indicates that temp and salinity where indeed lower
# in recent years, so the part of the hypothesis that temp/sal conditions
# were better seems to stand up.

# packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

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

#note: some duplicates potentially in this dataset
init_rows <- nrow(dat)
# rm dupes
dat <- dplyr::distinct(dat)
# should have been 80 duplicated rows removed
init_rows - nrow(dat) # 80.

# folders to store results -----
der_dat_path <- file.path("Derived_Data", "PostThesis_SEM", "19_temp_sal_boxplot_anova")
dir.create(der_dat_path)
fig_path <- file.path("Figures", "PostThesis_SEM", "19_temp_sal_boxplot_anova")
dir.create(fig_path)

# Plot sal/temp boxplots ----

# histogram (hard to digest)
# ggplot(data = dat, aes(temp_s)) +
# geom_histogram()+
# facet_wrap(~ts_2_spat_2)+
# theme_minimal()

# make the necessary factors to order and label the boxplots.
dat$ts_2_spat_2_fac <- factor(x = dat$ts_2_spat_2, 
 levels = c("91to02Low", "02to17Low", "91to02MedHigh", "02to17MedHigh"), 
 labels = c("1991-2002 Low Sal.", "2003-2017 Low Sal.", 
            "1991-2002 Med/High Sal.", "2003-2017 Med/High Sal."))

dat$ts_2_fac <- factor(x = dat$ts_2, 
 levels = c("91to02", "02to17"), 
 labels = c("1991-2002", "2003-2017"))

dat$spat_2_fac <- factor(x = dat$spat_2, 
  levels = c("Low", "MedHigh"), 
  labels = c("Low Sal.", "Med/High Sal."))
# boxplot seems easier to digest.
# ggplot(dat = dat, aes(x = ts_2_spat_2_fac, y = temp_s))+
# geom_boxplot()+
# theme_minimal()

temp_plot <- ggplot(dat = dat, aes(x = ts_2_fac, y = temp_s))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", pch = 5,
    size = 2, color = "black") +
  facet_wrap(~spat_2_fac)+
  ylab("Temperature (degrees C)")+
  xlab("")+
  theme_minimal()

sal_plot <- ggplot(dat = dat, aes(x = ts_2_fac, y = sal))+
  geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", pch = 5,
    size = 2, color = "black") +
  facet_wrap(~spat_2_fac)+
  ylab("Salinity")+
  xlab("Year range")+
  theme_minimal()

temp_sal_boxplot <- temp_plot / sal_plot

ggsave(file.path(fig_path, "temp_sal_boxplot.png"), temp_sal_boxplot, 
width = 6, height = 8)

#ANOVAs ----
anova_temp <- aov(temp_s~spat_2+ts_2, data = dat)
# no diff in temp by region, but significant difference over time
anova_temp_summary <- summary(anova_temp)

anova_sal <- aov(sal~spat_2+ts_2, data = dat)
anova_sal_summary <- summary(anova_sal)
# salinity is different over space and time.

# uncomment to write results again
# write.csv(summary(anova_temp)[[1]], file.path(der_dat_path, "anova_temp_summary.csv"))

# write.csv(summary(anova_sal)[[1]], file.path(der_dat_path, "anova_sal_summary.csv"))

# checks for any issues.
par(mfrow=c(2,2))
plot(anova_sal)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(anova_temp)
par(mfrow=c(1,1))

