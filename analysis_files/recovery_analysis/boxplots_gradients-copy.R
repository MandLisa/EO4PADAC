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
library(readr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()


# one observation per ID
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)

recovery_unique$mean_prec_total1 <- recovery_unique$mean_prec_total * 0.1



# Compute quantile thresholds
q1 <- quantile(recovery_unique$mean_temp_total, 1/3, na.rm = TRUE)
q2 <- quantile(recovery_unique$mean_temp_total, 2/3, na.rm = TRUE)

q1 <- quantile(recovery_unique$mean_prec_total1, 1/3, na.rm = TRUE)
q2 <- quantile(recovery_unique$mean_prec_total1, 2/3, na.rm = TRUE)


# Create temp_class with actual threshold values
recovery_unique <- recovery_unique %>%
  mutate(temp_class = case_when(
    mean_temp_total <= q1 ~ paste0("≤ ", round(q1, 1)), 
    mean_temp_total > q1 & mean_temp_total <= q2 ~ paste0(round(q1, 1), " - ", round(q2, 1)),
    mean_temp_total > q2 ~ paste0("> ", round(q2, 1))
  ))

# Create prec_class with actual threshold values
recovery_unique <- recovery_unique %>%
  mutate(prec_class = case_when(
    mean_prec_total1 <= q1 ~ paste0("≤ ", round(q1, 1)), 
    mean_prec_total1 > q1 & mean_prec_total1 <= q2 ~ paste0(round(q1, 1), " - ", round(q2, 1)),
    mean_prec_total1 > q2 ~ paste0("> ", round(q2, 1))
  ))

# Create temp_class with actual threshold values
recovery_unique <- recovery_unique %>%
  mutate(temp_class = case_when(
    mean_temp_total <= q1 ~ paste0("≤ ", round(q1, 1)), 
    mean_temp_total > q1 & mean_temp_total1 <= q2 ~ paste0(round(q1, 1), " - ", round(q2, 1)),
    mean_temp_total > q2 ~ paste0("> ", round(q2, 1))
  ))


### or with all other variables as well
# Calculate percentage of recovered disturbances per GRID_ID
recovery_unique_boxplot <- recovery_unique %>%
  group_by(temp_class, geoloc, height_class, severity_class, prec_class) %>%
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
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("≤ 1163.7", "1163.7 - 1514.8", "> 1514.8"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

all <- all %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤1164" = "≤1164mm",
                              "1164-1515" = "1164-1515mm",
                              ">1515" = ">1515mm"
  ))



all <- all %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
all$category <- factor(all$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

# Create the boxplot
p1 <- ggplot(all, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 26) +
  labs(title = "",
       x = "",
       y = "Recovery success") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#F5CBA7",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#A04000",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.title.y = element_blank(),   # Removes y-axis title
    axis.text.y = element_blank(),    # Removes y-axis labels
    axis.ticks.y = element_blank()    # Removes y-axis ticks
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_all_prec.png", plot = p1, width = 9, height = 6.5, dpi = 300)



# filter for geoloc
#hoose a specific geoloc, e.g., "A"
north_east <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - north") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("≤ 1163.7", "1163.7 - 1514.8", "> 1514.8"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

north_east <- north_east %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤ 1163.7" = "≤1164mm",
                              "1163.7 - 1514.8" = "1164-1515mm",
                              "> 1514.8" = ">1515mm"
  ))


north_east <- north_east %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
north_east$category <- factor(north_east$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

# Create the boxplot
p1 <- ggplot(north_east, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 30) +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#F5CBA7",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#A04000",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.title.x = element_blank(),   # Removes y-axis title
    axis.text.x = element_blank(),    # Removes y-axis labels
    axis.ticks.x = element_blank(),
    legend.position = "none")



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_easternalps_north_prec.png", plot = p1, width = 9, height = 6, dpi = 300)




# filter for geoloc
#hoose a specific geoloc, e.g., "A"
central <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - central") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("≤ 1163.7", "1163.7 - 1514.8", "> 1514.8"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

central <- central %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤ 1163.7" = "≤1164mm",
                              "1163.7 - 1514.8" = "1164-1515mm",
                              "> 1514.8" = ">1515mm"
  ))


central <- central %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
central$category <- factor(central$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

# Create the boxplot
p1 <- ggplot(central, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 30) +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#F5CBA7",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#A04000",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.title.x = element_blank(),   # Removes y-axis title
    axis.text.x = element_blank(),    # Removes y-axis labels
    axis.ticks.x = element_blank(),
    legend.position = "none")


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_centralnalps_north_prec.png", plot = p1, width = 9, height = 6, dpi = 300)




# filter for geoloc
#hoose a specific geoloc, e.g., "A"
east_south <- recovery_unique_boxplot %>%
  filter(geoloc == "eastern alps - south") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("≤ 1163.7", "1163.7 - 1514.8", "> 1514.8"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

east_south <- east_south %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤ 1163.7" = "≤1164mm",
                              "1163.7 - 1514.8" = "1164-1515mm",
                              "> 1514.8" = ">1515mm"
  ))


east_south <- east_south %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
east_south$category <- factor(east_south$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

east_south$class_level <- factor(east_south$class_level, 
                                 levels = c("≤800m", ">800-1200m", ">1200m",
                                            "NSR", "SR",
                                            ">17°C", "14-17°C", "≤14°C",
                                            "≤1164mm", "1164-1515mm", ">1515mm"
                                            ))

 

# Create the boxplot
p1 <- ggplot(east_south, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 30) +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#A04000",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#F5CBA7",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.title.x = element_blank(),   # Removes y-axis title
    axis.text.x = element_blank(),    # Removes y-axis labels
    axis.ticks.x = element_blank(),
    legend.position = "none")


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_easternalps_south_prec.png", plot = p1, width = 9, height = 6, dpi = 300)



# filter for geoloc
#hoose a specific geoloc, e.g., "A"
western_north <- recovery_unique_boxplot %>%
  filter(geoloc == "western alps - north") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("≤ 1163.7", "1163.7 - 1514.8", "> 1514.8"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

western_north <- western_north %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤ 1163.7" = "≤1164mm",
                              "1163.7 - 1514.8" = "1164-1515mm",
                              "> 1514.8" = ">1515mm"
  ))


western_north <- western_north %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
western_north$category <- factor(western_north$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

# Create the boxplot
p1 <- ggplot(western_north, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 30) +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#F5CBA7",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#A04000",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    #axis.title.x = element_blank(),   # Removes y-axis title
    #axis.text.x = element_blank(),    # Removes y-axis labels
    #axis.ticks.x = element_blank(),
    legend.position = "none")



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/boxplot_westernalps_north_prec.png", plot = p1, width = 8.9, height = 8.3, dpi = 300)



# filter for geoloc
#hoose a specific geoloc, e.g., "A"
western_south <- recovery_unique_boxplot %>%
  filter(geoloc == "western alps - south") %>%
  mutate(
    height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")),
    severity_class = factor(severity_class, levels = c("NSR", "SR")),
    temp_class = factor(temp_class, levels = c("≤ 14.3", "14.3 - 16.8", "> 16.8")),
    prec_class = factor(prec_class, levels = c("> 1514.8", "1163.7 - 1514.8", "≤ 1163.7"))
  ) %>%
  pivot_longer(cols = c(height_class, severity_class, temp_class, prec_class),
               names_to = "category",
               values_to = "class_level")

western_south <- western_south %>%
  mutate(class_level = recode(class_level,
                              "<800m" = "≤800m",
                              ">800-1200m" = ">800-1200m",
                              ">1200m" = ">1200m",
                              "NSR" = "NSR",
                              "SR" = "SR",
                              "≤ 14.3" = "≤14°C",
                              "14.3 - 16.8" = "14-17°C",
                              "> 16.8" = ">17°C",
                              "≤ 1163.7" = ">1515mm",
                              "1163.7 - 1514.8" = "1164-1515mm",
                              "> 1514.8" = "≤1164mm"
  ))


western_south <- western_south %>%
  filter(!is.na(class_level))  # Removes rows where class_level is NA

# Define the desired order of x-axis categories
western_south$category <- factor(western_south$category, levels = c("height_class", "severity_class", "temp_class", "prec_class"))

western_south <- western_south %>%
  mutate(class_level = ifelse(category == "prec_class",
                              factor(class_level, levels = c("≤1164mm", "1164-1515mm", ">1515mm")), 
                              class_level))
# Create the boxplot
p1 <- ggplot(western_south, aes(x = category, y = percent_recovered, fill = class_level)) +
  geom_boxplot() +
  theme_bw(base_size = 30) +
  labs(title = "",
       x = "",
       y = "") +
  scale_x_discrete(labels = c(
    "height_class" = "Elevation",
    "severity_class" = "Severity ",
    "temp_class" = "Temperature",
    "prec_class" = "Precipitation"
  )) +
  ylim(0,85) +
  scale_fill_manual(
    values = c(
      "≤800m" = "#A9F5A9",       # Greenish
      ">800-1200m" = "#01DF01",   # Orange
      ">1200m" = "#0B610B",       # Blue
      "NSR" = "#F78181",          # Pink
      "SR" = "#8A0808",           # Light Green
      "≤14°C" = "#F5CBA7",    # Yellow
      "14-17°C" = "#E67E22",       # Brownish
      ">17°C" = "#A04000",
      "≤1164mm" = "#A9CCE3",
      "1164-1515mm" = "#5499C7",
      ">1515mm" = "#1F618D"
    )
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    #axis.title.x = element_blank(),   # Removes y-axis title
    #axis.text.x = element_blank(),    # Removes y-axis labels
    #axis.ticks.x = element_blank(),
    legend.position = "none")



# Save a specific plot object
ggsave("~/boxplot_westernalps_south_prec.png", plot = p1, width = 8.9, height = 8.3, dpi = 300)




recovery_summary <- recovery_unique_sf_boxplot %>%
  group_by(geoloc, height_class) %>%
  summarize(
    mean_percent_recovered = mean(percent_recovered_overall, na.rm = TRUE)  # Mean percentage recovered per group
  ) %>%
  ungroup()

