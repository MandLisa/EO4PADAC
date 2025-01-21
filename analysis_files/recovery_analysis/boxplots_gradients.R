library(gginnards)
library(ggplot2)
library(tidyverse)
library(ggforce)
library(sf)
library(gridExtra)
library(purrr)
library(jcolors)
library(GGally)
library(randomForest)
library(pdp)
library(patchwork)
library(spgwr)
library(scales)
library(GWmodel)
library(sf)
library(spdep)
library(pheatmap)
library(DataEditR)



# Compute quantile thresholds
q1 <- quantile(recovery_unique_sf$temp, 1/3, na.rm = TRUE)
q2 <- quantile(recovery_unique_sf$temp, 2/3, na.rm = TRUE)

# Create temp_class with actual threshold values
recovery_unique_sf <- recovery_unique_sf %>%
  mutate(temp_class = case_when(
    temp <= q1 ~ paste0("≤ ", round(q1, 1)),  # Round to 2 decimals
    temp > q1 & temp <= q2 ~ paste0(round(q1, 1), " - ", round(q2, 1)),
    temp > q2 ~ paste0("> ", round(q2, 1))
  ))




### or with all other variables as well
# Calculate percentage of recovered disturbances per GRID_ID
recovery_unique_boxplot <- recovery_unique_sf %>%
  group_by(temp_class, geoloc, height_class, severity_class) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recov_10, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()


all <- recovery_unique_boxplot %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

all <- all %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))



all <- all %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


# Create the boxplot
p1 <- ggplot(all, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#F5CBA7",    # Yellow
      "13-16°C" = "#E67E22",       # Brownish
      ">16°C" = "#A04000"            # White (for missing values)
    )
  ) 


#theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_all.png", plot = p1, width = 9, height = 5, dpi = 300)



# filter for geoloc
#hoose a specific geoloc, e.g., "A"
north_east <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - north") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

north_east <- north_east %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))



north_east<- north_east %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


# Create the boxplot
p1 <- ggplot(north_east, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#F5CBA7",    # Yellow
      "13-16°C" = "#E67E22",       # Brownish
      "> 16°C" = "#A04000"            # White (for missing values)
    )
  ) 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_easternalps_north.png", plot = p1, width = 9, height = 5, dpi = 300)




# filter for geoloc
#hoose a specific geoloc, e.g., "A"
central_east <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - central") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

central_east <- central_east %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))



central_east <- central_east %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


# Create the boxplot
p1 <- ggplot(central_east, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#F5CBA7",    # Yellow
      "13-16°C" = "#E67E22",       # Brownish
      ">16°C" = "#A04000"            # White (for missing values)
    )
  ) 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_centralnalps_north.png", plot = p1, width = 9, height = 5, dpi = 300)




# filter for geoloc
#hoose a specific geoloc, e.g., "A"
south_east <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - south") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

south_east <- south_east %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))


south_east <- south_east %>%
  mutate(class_level = case_when(
    class_level == "≤13°C" ~ ">16°C",  # Swap ≤13°C → >16°C
    class_level == ">16°C" ~ "≤13°C",  # Swap >16°C → ≤13°C
    TRUE ~ class_level  # Keep all other values unchanged
  ))

south_east <- south_east %>%
  mutate(class_level = case_when(
    class_level == "13-16°C" ~ "≤13°C",  # Swap ≤13°C → >16°C
    class_level == "≤13°C" ~ "13-16°C",  # Swap >16°C → ≤13°C
    TRUE ~ class_level  # Keep all other values unchanged
  ))




south_east <- south_east %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


south_east <- south_east %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))






# Create the boxplot
p1 <- ggplot(south_east, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#E67E22",    # Yellow
      "13-16°C" = "#A04000",       # Brownish
      ">16°C" = "#F5CBA7"            # White (for missing values)
    )
  ) 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_easternalps_south.png", plot = p1, width = 9, height = 5, dpi = 300)




#hoose a specific geoloc, e.g., "A"
western_north <- recovery_unique_boxplot %>%
  filter(geoloc == "western alps - north") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

western_north <- western_north %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))



western_north <- western_north %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


# Create the boxplot
p1 <- ggplot(western_north, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#F5CBA7",    # Yellow
      "13-16°C" = "#E67E22",       # Brownish
      ">16°C" = "#A04000"            # White (for missing values)
    )
  ) 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_western_north.png", plot = p1, width = 9, height = 5, dpi = 300)




#hoose a specific geoloc, e.g., "A"
western_south <- recovery_unique_boxplot %>%
  filter(geoloc == "western alps - south") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 13", "13 - 15.7", "> 15.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class),
               names_to = "category",
               values_to = "class_level")

western_south <- western_south %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 13" = "≤13°C",
                              "13 - 15.7" = "13-16°C",
                              "> 15.7" = ">16°C"
  ))



western_south <- western_south %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA


# Create the boxplot
p1 <- ggplot(western_south, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 25) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Height",
    "severity_class" = "Severity",
    "temp_class" = "Temperature"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9CCE3",       # Greenish
      ">800-1200m" = "#5499C7",   # Orange
      ">1200m" = "#1F618D",       # Blue
      "NSR" = "#7DCEA0",          # Pink
      "SR" = "#196F3D",           # Light Green
      "≤13°C" = "#F5CBA7",    # Yellow
      "13-16°C" = "#E67E22",       # Brownish
      ">16°C" = "#A04000"            # White (for missing values)
    )
  ) 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_western_south.png", plot = p1, width = 9, height = 5, dpi = 300)



recovery_summary <- recovery_unique_sf_boxplot %>%
  group_by(geoloc, height_class) %>%
  summarize(
    mean_percent_recovered = mean(percent_recovered_overall, na.rm = TRUE)  # Mean percentage recovered per group
  ) %>%
  ungroup()

