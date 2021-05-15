# description ---
#plot the predicted values given the selected model

# load packages, set optiosn

library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
# data ----
filenames <- list.files(path = file.path("Data", "model_pred_vals_data_tables"), full.names = T)
group_names <- list.files(path = file.path("Data", "model_pred_vals_data_tables"), full.names = F)
group_names <- gsub("data_table_", "", group_names)
group_names <- gsub(".xlsx", "", group_names)


csvs <- lapply(filenames, function(x) {
   tmp_csv <- readxl::read_excel(x)
   colnames(tmp_csv)[1] <- "sal"
   tmp_csv <- gather(tmp_csv, "temp", "disease_M", 2:17)
  })
names(csvs) <- group_names
for(i in seq_along(group_names)) {
  csvs[[i]][["group"]] <- group_names[i]
}

dat <- do.call("rbind", csvs) %>% 
         separate(group, c("group_fac", "disease"), sep = "_")
dat$group_fac <- factor(dat$group_fac, 
                              levels = c("91to02Low", "02to17Low", "91to02MedHigh","02to17MedHigh"))

dat$temp <- as.numeric(dat$temp)
dat$group_chr <- as.character(dat$group_fac)

dat$disease_fac <- factor(dat[["disease"]], levels = c("msx", "dermo"), 
                          labels = c("MSX", "Dermo"))

# rm vals that are out of range ----
# filter out vals from data that aren't needed. ---
# Next to do: add NA's for invalid for vals outside of the range observed.
# note: these are old values
#limits <- read.csv(file.path("Data", "min_max_temp_sal.csv"))
dat_SEM <- read.csv(file.path("Derived_Data", "PostThesis_SEM", "1_Organize_Inputs",
                          "dat_SEM_post_thesis_2021_05_08.csv"))
# now: figure out how to exclued values that are not within the range
dat_SEM <- dplyr::select(dat_SEM, ID, year, ts_2_spat_2, temp_s_cen, sal_cen)
 
min_temp <- dat_SEM %>% group_by(ts_2_spat_2) %>%  summarize(min_temp = min(temp_s_cen))
max_temp <- dat_SEM %>% group_by(ts_2_spat_2) %>%  summarize(max_temp = max(temp_s_cen))
min_sal <- dat_SEM %>% group_by(ts_2_spat_2) %>%  summarize(min_sal = min(sal_cen))
max_sal <- dat_SEM %>% group_by(ts_2_spat_2) %>%  summarize(max_sal = max(sal_cen))

limits <- dplyr::full_join(min_temp, max_temp, by = "ts_2_spat_2") %>% 
   dplyr::full_join(min_sal, by = "ts_2_spat_2") %>% 
   dplyr::full_join(max_sal, by = "ts_2_spat_2") %>% 
   dplyr::rename(Group = ts_2_spat_2)

# look at values for a particular group
# going line by lines, see if sal and temp are both in range.
# if not, change value to NA
# perhaps makes sense to do this before binding?
for (i in seq_len(nrow(dat))) {
   tmp_row <- dat[i,]
   tmp_grp <- tmp_row[,"group_chr", drop = TRUE]
   tmp_limit <- limits[limits$Group == tmp_grp,]
   if(tmp_row[, "sal", drop = TRUE] < tmp_limit[,"min_sal", drop = TRUE] |
      tmp_row[, "sal", drop = TRUE] > tmp_limit[,"max_sal", drop = TRUE]) {
      dat[i, "disease_M"] <- NA
   }
   if(tmp_row[,"temp", drop = TRUE] < tmp_limit[,"min_temp", drop = TRUE] |
      tmp_row[,"temp", drop = TRUE] > tmp_limit[,"max_temp", drop = TRUE]) {
      dat[i, "disease_M"] <- NA
   }
}

dat <- na.omit(dat)
dat$group_fac <- factor(dat$group_fac, 
                        levels = c("91to02Low", "02to17Low", "91to02MedHigh","02to17MedHigh"), 
                        labels = c("1991-2002\nLow Sal.", "2003-2017\nLow Sal.", "1991-2002\nMed/High Sal.", "2003-2017\nMed/High Sal."))

# plot ----
   ggplot(dat, aes(x = sal, y = temp))+
   geom_raster(aes(fill = disease_M), hjust=0.5, vjust=0.5, interpolate=FALSE)+
   scale_fill_gradient(low = "yellow", high = "red")+
   facet_grid(disease_fac ~ group_fac) +
   labs(fill = "M due to the disease\n(% per yr)")+
   xlab("Mean Centered Salinity")+
   ylab("Mean Centered Temperature (degrees C)")+
   theme_classic(base_size = 14)+
   theme(legend.position="top")
ggsave("Figures/PostThesis_SEM/pred_vals.png", 
       width = 12, 
       height = 8,
       units = "in",
       dpi = "print")



   