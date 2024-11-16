library(ggplot2)
library(dplyr)
library(readr)
library(raster)
library(lme4)
library(scales)
library(effects)
library(broom)
library(broom.mixed)
library(metafor)
library(emmeans)
library(patchwork)
library(tidyverse)
library(purrr)


GEDI_recov_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")

### prepare df

# filter for yod <=2013
recovery_2013 <- GEDI_recov_all %>%
  filter(yod<=2013)

# filter for yod <= 2018
recovery_2018 <- GEDI_recov_all %>%
  filter(yod<=2018)

# filter for geoloc == NA
recovery_2013 <- recovery_2013 %>%
  filter(!is.na(geoloc))

recovery_2018 <- recovery_2018 %>%
  filter(!is.na(geoloc))

# reclassify aspect
recovery_2013 <- recovery_2013 %>%
  mutate(aspect_cat = case_when(
    (aspect >= 315 | aspect < 45) ~ "N",
    aspect >= 45 & aspect < 135 ~ "O",
    aspect >= 135 & aspect < 225 ~ "S",
    aspect >= 225 & aspect < 315 ~ "W"
  ))

recovery_2018 <- recovery_2018 %>%
  mutate(aspect_cat = case_when(
    (aspect >= 315 | aspect < 45) ~ "N",
    aspect >= 45 & aspect < 135 ~ "O",
    aspect >= 135 & aspect < 225 ~ "S",
    aspect >= 225 & aspect < 315 ~ "W"
  ))

# reclassify geoloc
recovery_2013 <- recovery_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc == "eastern alps - central" ~ "western alps - south",
    geoloc == "eastern alps - north" ~ "eastern alps - south",
    geoloc == "western alps - south" ~ "eastern alps - north",
    geoloc == "western alps - north" ~ "western alps - north",
    geoloc == "eastern alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc  # Keeps any values not explicitly reclassified
  ))


recovery_2013 <- recovery_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))

# 2018
recovery_2018 <- recovery_2018 %>%
  mutate(geoloc_reclass = case_when(
    geoloc == "eastern alps - central" ~ "western alps - south",
    geoloc == "eastern alps - north" ~ "eastern alps - south",
    geoloc == "western alps - south" ~ "eastern alps - north",
    geoloc == "western alps - north" ~ "western alps - north",
    geoloc == "eastern alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc  # Keeps any values not explicitly reclassified
  ))


recovery_2018 <- recovery_2018 %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))

# filter for ysd <=2013
recovery_2013 <- recovery_2013 %>%
  filter(ysd>=0)

# filter for ysd <=2013
recovery_2018 <- recovery_2018 %>%
  filter(ysd>=0)


### write df
write.csv(recovery_2013, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recovery_2013.csv", row.names = FALSE)
write.csv(recovery_2018, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recovery_2018.csv", row.names = FALSE)

write.csv(recovery_summary_geoloc, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_geoloc_2013.csv", row.names = FALSE)



# Calculate the percentage of recovered observations within 10 years for each yod
recovery_summary <- recovery_2013 %>%
  group_by(yod) %>%                                     
  summarize(
    total_observations = n(),                            
    recovered_within_10_years = sum(recovery_10y_num),  
    recovery_percentage = mean(recovery_10y_num) * 100   
  ) %>%
  ungroup()

#plot
ggplot(recovery_summary, aes(x = yod, y = recovery_percentage)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = TRUE) +
  theme(legend.position = "none") +
  xlim(1986, 2012)

#-------------------------------------------------------------------------------
### this is the correct df:
recovery_summary_geoloc_2013 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_geoloc_2013.csv")

# Calculate the percentage of recovered observations within 10 years for each 
# disturbance year and geolocation
recovery_summary_geoloc <- recovery_2013 %>%
  group_by(yod, geoloc_reclass) %>%                    
  summarize(
    total_observations = n(),                  
    recovered_within_10_years = sum(recovery_10y_num), 
    recovery_percentage = mean(recovery_10y_num) * 1000 
  ) %>%
  ungroup()

##########

# Step 2: Compute mean temperature for each geolocation and year of disturbance
# Compute mean temperature for each geolocation and calendar year
temperature_trends <- recovery_2013 %>%
  group_by(year, geoloc_reclass) %>%  # Group by calendar year and geolocation
  summarize(
    mean_temperature = mean(temp, na.rm = TRUE)  # Average temperature for each year
  ) %>%
  ungroup()

# Reclassify geoloc_reclass in temperature_trends
temperature_trends <- temperature_trends %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - south",
    geoloc_reclass == "eastern alps - north" ~ "eastern alps - south",
    geoloc_reclass == "eastern alps - south" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - north",
    geoloc_reclass == "western alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Retain other values if any
  ))

# Normalize temperature for dual y-axis
max_recovery <- max(recovery_summary_geoloc$recovery_percentage, na.rm = TRUE)
max_temp <- max(temperature_trends$mean_temperature, na.rm = TRUE)

temperature_trends <- temperature_trends %>%
  mutate(normalized_temperature = mean_temperature / max_temp * max_recovery)  # Rescale temperature

# Plot with dual y-axis and unsmoothed temperature trend
lineplot <- ggplot() +
  # Recovery trend lines
  geom_line(data = recovery_summary_geoloc, 
            aes(x = yod, y = recovery_percentage)) +
  geom_smooth(data = recovery_summary_geoloc, 
              aes(x = yod, y = recovery_percentage), 
              method = "lm", linetype = "dashed", se = TRUE, size = 0.25) +
  # Raw temperature fluctuations (unsmoothed)
  geom_line(data = temperature_trends, 
            aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
            color = "#015863", size = 0.5) +
  # Temperature trendline
  geom_smooth(data = temperature_trends, 
              aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
              method = "lm", linetype = "solid", color = "#A1CBD0", se = TRUE, size = 0.8, alpha=0.2) +
  facet_wrap(~ geoloc_reclass) +
  xlim(1986, 2012) +
  scale_y_continuous(
    name = "Recovery percentage 10y post-disturbance",  # Primary y-axis
    sec.axis = sec_axis(~ . * max_temp / max_recovery, name = "Mean Temperature (°C)")  # Secondary y-axis
  ) +
  labs(
    x = "Year of disturbance",
    color = "Geolocation"
  ) +
  theme_bw() +
  theme(
    axis.title.y.right = element_text(color = "black"),  # Distinguish secondary axis
    axis.text.y.right = element_text(color = "black")
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_recov_lineplot.png", plot = lineplot, width = 10, height = 6, dpi = 300)

###with bars

barplot <- ggplot() +
  # Bar plot for recovery trends
  geom_bar(data = recovery_summary_geoloc, 
           aes(x = yod, y = recovery_percentage), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  geom_smooth(data = recovery_summary_geoloc, 
              aes(x = yod, y = recovery_percentage), 
              method = "lm", linetype = "dashed", se = TRUE, size = 0.25) +
  # Line plot for temperature trends
  geom_line(data = temperature_trends, 
            aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
            color = "black", size = 0.8, linetype="dotted") +
  geom_smooth(data = temperature_trends, 
              aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
              method = "lm", linetype = "solid", color = "#A1CBD0", se = TRUE, size = 0.6, alpha = 0.2) +
  facet_wrap(~ geoloc_reclass) +
  xlim(1986, 2012) +
  scale_y_continuous(
    name = "Percentage recovered 10y post-disturbance",
    sec.axis = sec_axis(~ . * max_temp / max_recovery, name = "Mean Temperature (°C)")
  ) +
  labs(
    x = "Year of Disturbance",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "black")
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_recov_barplot_notemp.png", plot = barplot, width = 10, height = 6, dpi = 300)

#-------------------------------------------------------------------------------
### do the same for 2018

### this is the correct df:
recovery_summary_geoloc_2013 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_geoloc_2013.csv")


# compute recovery_5y_num
recovery_2018 <- recovery_2018 %>%
  group_by(ID) %>%
  mutate(
    recovery_5y = if_else(any(recovery_status == "recovered" & year == yod + 5), "y", "n"),
    recovery_5y_num = if_else(recovery_5y == "y", 1, 0)
  ) %>%
  ungroup()

# Calculate the percentage of recovered observations within 10 years for each 
# disturbance year and geolocation
recovery_summary_geoloc_2018 <- recovery_2018 %>%
  group_by(yod, geoloc_reclass) %>%                    
  summarize(
    total_observations = n(),                  
    recovered_within_5_years = sum(recovery_5y_num), 
    recovery_percentage_5y = mean(recovery_5y_num) * 1000  # Convert to percentage
  ) %>%
  ungroup()

# Check for NAs in recovery_summary_geoloc_2018
summary(recovery_summary_geoloc_2018)



##########

# Step 2: Compute mean temperature for each geolocation and year of disturbance
# Compute mean temperature for each geolocation and calendar year
temperature_trends <- recovery_2018 %>%
  group_by(year, geoloc_reclass) %>%  # Group by calendar year and geolocation
  summarize(
    mean_temperature = mean(temp, na.rm = TRUE)  # Average temperature for each year
  ) %>%
  ungroup()

# Reclassify geoloc_reclass in temperature_trends
temperature_trends <- temperature_trends %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - south",
    geoloc_reclass == "eastern alps - north" ~ "eastern alps - south",
    geoloc_reclass == "eastern alps - south" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - north",
    geoloc_reclass == "western alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Retain other values if any
  ))

# Normalize temperature for dual y-axis
max_recovery <- max(recovery_summary_geoloc_2018$recovery_percentage_5y, na.rm = TRUE)
max_temp <- max(temperature_trends$mean_temperature, na.rm = TRUE)

temperature_trends <- temperature_trends %>%
  mutate(normalized_temperature = mean_temperature / max_temp * max_recovery)  # Rescale temperature

# Plot with dual y-axis and unsmoothed temperature trend
lineplot <- ggplot() +
  # Recovery trend lines
  geom_line(data = recovery_summary_geoloc_2018, 
            aes(x = yod, y = recovery_percentage_5y)) +
  geom_smooth(data = recovery_summary_geoloc_2018, 
              aes(x = yod, y = recovery_percentage_5y), 
              method = "lm", linetype = "dashed", se = TRUE, size = 0.25) +
  # Raw temperature fluctuations (unsmoothed)
  geom_line(data = temperature_trends, 
            aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
            color = "#015863", size = 0.5) +
  # Temperature trendline
  geom_smooth(data = temperature_trends, 
              aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
              method = "lm", linetype = "solid", color = "#A1CBD0", se = TRUE, size = 0.8, alpha=0.2) +
  facet_wrap(~ geoloc_reclass) +
  xlim(1986, 2017) +
  scale_y_continuous(
    name = "Recovery percentage 5y post-disturbance",  # Primary y-axis
    sec.axis = sec_axis(~ . * max_temp / max_recovery, name = "Mean Temperature (°C)")  # Secondary y-axis
  ) +
  labs(
    x = "Year of disturbance",
    color = "Geolocation"
  ) +
  theme_bw() +
  theme(
    axis.title.y.right = element_text(color = "black"),  # Distinguish secondary axis
    axis.text.y.right = element_text(color = "black")
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_recov_lineplot_5y.png", plot = lineplot, width = 10, height = 6, dpi = 300)

###with bars

barplot <- ggplot() +
  # Bar plot for recovery trends
  geom_bar(data = recovery_summary_geoloc_2018, 
           aes(x = yod, y = recovery_percentage_5y), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  geom_smooth(data = recovery_summary_geoloc_2018, 
              aes(x = yod, y = recovery_percentage_5y), 
              method = "lm", linetype = "dashed", se = TRUE, size = 0.25) +
  # Line plot for temperature trends
  geom_line(data = temperature_trends, 
            aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
            color = "black", size = 0.8, linetype="dotted") +
  geom_smooth(data = temperature_trends, 
              aes(x = year, y = normalized_temperature, group = geoloc_reclass), 
              method = "lm", linetype = "solid", color = "#A1CBD0", se = TRUE, size = 0.6, alpha = 0.2) +
  facet_wrap(~ geoloc_reclass) +
  xlim(1986, 2017) +
  scale_y_continuous(
    name = "Percentage recovered 5y post-disturbance",
    sec.axis = sec_axis(~ . * max_temp / max_recovery, name = "Mean Temperature (°C)")
  ) +
  labs(
    x = "Year of Disturbance",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "black")
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_recov_barplot_5y.png", plot = barplot, width = 10, height = 6, dpi = 300)
#-------------------------------------------------------------------------------

### stratify per sea_level class
recovery_summary_geoloc_height <- recovery_2013 %>%
  group_by(yod, geoloc_reclass, sea_level) %>%   # Include sea_level here                
  summarize(
    total_observations = n(),                  
    recovered_within_10_years = sum(recovery_10y_num), 
    recovery_percentage = mean(recovery_10y_num) * 1000 
  ) %>%
  ungroup()

recovery_summary_geoloc_height$sea_level <- as.factor(recovery_summary_geoloc_height$sea_level)


# plot
ggplot(recovery_summary_geoloc_height, aes(x = yod, y = recovery_percentage, color = sea_level)) +
  geom_line() +
  #geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE) +
  xlim(1986, 2013) +
  ylim(0,50) +
  facet_wrap(~ geoloc_reclass) +
  theme(legend.position = "right") +  # Display legend for sea_level
  labs(
    title = "",
    x = "Year of Disturbance",
    y = "Percentage of Recovery 10y-post-dist",
    color = "Sea level"
  )

#-------------------------------------------------------------------------------

### stratify per aspect

#reclassify aspect
# Reclassify aspect column
recovery_2013 <-recovery_2013 %>%
  mutate(aspect_class1 = ifelse(aspect >= 270 | aspect <= 90, "North", "South"))


recovery_summary_geoloc_aspect <- recovery_2013 %>%
  group_by(yod, geoloc_reclass, aspect_class1) %>%   # Include sea_level here                
  summarize(
    total_observations = n(),                  
    recovered_within_10_years = sum(recovery_10y_num), 
    recovery_percentage = mean(recovery_10y_num) * 1000 
  ) %>%
  ungroup()

recovery_summary_geoloc_aspect$aspect_class1 <- as.factor(recovery_summary_geoloc_aspect$aspect_class1)


# plot
ggplot(recovery_summary_geoloc_aspect, aes(x = yod, y = recovery_percentage, color = aspect_class1)) +
  geom_line() +
  #geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE) +
  xlim(1986, 2012) +
  ylim(0,50) +
  facet_wrap(~ geoloc_reclass) +
  scale_color_manual(
    values = c("North" = "#0B4E74", "South" = "#DC9A0D")  # Assign colors to each level
  ) +
  theme(legend.position = "right") +  # Display legend for sea_level
  labs(
    title = "",
    x = "Year of Disturbance",
    y = "Percentage of Recovery 10y-post-dist",
    color = "Aspect"
  )

#-------------------------------------------------------------------------------



recovery_2013 <- recovery_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc == "eastern alps - central" ~ "western alps - south",
    geoloc == "eastern alps - north" ~ "eastern alps - south",
    geoloc == "western alps - south" ~ "eastern alps - north",
    geoloc == "western alps - north" ~ "western alps - north",
    geoloc == "eastern alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc  # Keeps any values not explicitly reclassified
  ))


recovery_2013 <- recovery_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))



recovery_2013 <- recovery_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "eastern alps - north",
    geoloc_reclass == "eastern alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))
