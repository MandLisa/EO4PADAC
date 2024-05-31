library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# load data
spec_lib_long <- read_csv("00_analysis/data_scripts/spec_lib_long.csv")
df <- spec_lib_long

# remove NAs
df_NA <- df[df$value != -9999, ]

# band order
band_order <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
df_NA$band <- factor(df_NA$band, levels = band_order)

# define color for the endmembers
type_colors <- c("Tree cover" = "#0C5406",
                 "Shrubs/grassland" = "#4B0808",
                 "Bare ground" = "#000859")

# compute stats for ribbons
summary_stats <- df_NA %>%
  group_by(endmember, band) %>%
  summarize(
    median_value = median(value, na.rm = TRUE),
    lower_ci = quantile(value, 0.25, na.rm = TRUE),
    upper_ci = quantile(value, 0.75, na.rm = TRUE)
  )

summary_stats$band <- factor(summary_stats$band, levels = c("blue", 
                                                                "green", 
                                                                "red", 
                                                                "NIR", 
                                                                "SWIR1", 
                                                                "SWIR2"))

# Choose a palette, e.g., "Set1"
palette <- brewer.pal(n = length(unique(summary_stats$endmember)), name = "Set1")

type_colors1 <- c("bare ground" = "#839192", 
                 "broadleaved forest" = "#7DCEA0",
                 "coniferous forest" = "#145A32",
                 "broadleaved shrubland" = "#F1948A",
                 "coniferous shrubland" = "#943126",
                 "grassland" = "#F4D03F",
                 "artificial land" = "#A569BD",
                 "cropland" = "#40E0D0",
                 "water areas" = "#1F618D") 



ggplot(summary_stats, aes(x = band, y = median_value, group = endmember, color = endmember)) +
  geom_line() +
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = endmember), alpha = 0.3) +
  labs(title = "",
       x = "Bands",
       y = "Surface reflectance (Q50)") +
  scale_fill_manual(values = type_colors1) +
  scale_color_manual(values = type_colors1) +
  theme_bw()+
  theme(text = element_text(size = 2.1 * 11)) 


### Level 1 
# reclassify tpe names
summary_stats_shrub_merge <- summary_stats %>%
  mutate(endmember = case_when(
    endmember %in% c("broadleaved shrubland", "coniferous shrubland") ~ "shrubland",
    TRUE ~ as.character(endmember)  # Keep other values unchanged
  ))


# filter out only the endmembers of interest
desired_endmembers <- c("bare ground", "broadleaved forest", "broadleaved shrubland",
                        "coniferous shrubland", "coniferous forest", "grassland")

# Subset the data frame to keep only the desired endmembers
summary_stats_filtered <- summary_stats %>%
  filter(endmember %in% desired_endmembers)

summary_stats_filtered$band <- factor(summary_stats_filtered$band, levels = c("blue", 
                                                            "green", 
                                                            "red", 
                                                            "NIR", 
                                                            "SWIR1", 
                                                            "SWIR2"))



type_colors <- c("bare ground" = "#839192", 
                 "broadleaved forest" = "#7DCEA0",
                 "coniferous forest" = "#145A32",
                 "broadleaved shrubland" = "#F1948A",
                 "coniferous shrubland" = "#943126",
                 "grassland" = "#F4D03F") 

ggplot(summary_stats_filtered, aes(x = band, y = median_value, group = endmember, color = endmember)) +
  geom_line() +
  #geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = endmember), alpha = 0.3) +
  labs(title = "",
       x = "Bands",
       y = "Surface reflectance (Q50)") +
  scale_fill_manual(values = type_colors) +
  scale_color_manual(values = type_colors) +
  theme_bw()+
  theme(text = element_text(size = 2.1 * 11)) 
