library(readr)
GEDI_recov_all <- read_csv("eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")
GEDI_unique_recov <- read_csv("eo_nas/EO4Alps/00_analysis/_recovery/GEDI_unique_recov.csv")

GEDI_unique_ysd <- GEDI_unique_recov %>%
  group_by(since) %>%
  summarize(mean_tree_height = mean(rh95, na.rm = TRUE),
            sd_tree_height = sd(rh95, na.rm = TRUE))

