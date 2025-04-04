library(ggplot2)
library(tidyverse)
library(GGally)


# import data 
hexagons_sp_spatial <- 
hexagons_sp_temporal

# Select relevant columns (response variable + predictors)
predictors <- c("mean_VPD_yod1", "mean_elevation", "mean_severity", "mean_prec_total",
                "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare")  # Replace with your predictor names
response_var <- "mean_percent_recovered"  # Replace with your response variable name

# Create the pairwise scatterplot matrix
GGally::ggpairs(hexagons_sp, columns = c(response_var, predictors),
                lower = list(continuous = wrap("points", alpha = 0.5))) +
  theme_minimal()


# Convert to long format for faceting
# Convert SpatialPolygonsDataFrame to a regular data frame
hexagons_temporal_df <- as.data.frame(hexagons_recov10_temporal_sp)

hexagons_temporal_df$mean_bare_difference <- hexagons_temporal_df$mean_bare_difference/100
hexagons_temporal_dfmean_bare_difference <- hexagons_temporal_df$mean_bare_difference*2
hexagons_temporal_df$mean_prec_difference <- hexagons_temporal_df$mean_prec_difference*0.1


# Define the desired order of facets
predictor_order <- c("mean_elevation_difference","mean_severity_difference", "mean_TC_difference",
                     "mean_VPD_difference", "mean_temp_difference", "mean_prec_difference", "mean_bare_difference")


# Convert to long format
# Ensure `Predictor` is a factor with this order
# Convert to long format
long_data_temp <- hexagons_temporal_df%>%
  pivot_longer(cols = c("mean_elevation_difference","mean_severity_difference", "mean_TC_difference",
                        "mean_VPD_difference", "mean_temp_difference", "mean_prec_difference", "mean_bare_difference"),
               names_to = "Predictor", values_to = "Value")

long_data_temp <- long_data_temp %>%
  mutate(Predictor = factor(Predictor, levels = predictor_order, ordered = TRUE))


# Define custom labels for facets
predictor_labels <- c(
  "mean_bare_difference" = "difference_bare_ground",
  "mean_elevation" = "difference_elevation",
  "mean_pre_dist_tree_cover" = "difference_tree_cover",
  "mean_prec_total" = "difference_precipitation",
  "mean_severity" = "difference_severity",
  "mean_temp_total" = "difference_temperature",
  "mean_VPD_yod1" = "difference_VPD")




# Create faceted scatterplots
ggplot(long_data_temp, aes(x = Value, y = percent_recovered_difference)) +
  geom_point(alpha = 0.5) +  # Scatterplot points with transparency
  geom_smooth(method = "lm", color = "blue") +  # Linear trend line
  facet_wrap(~ Predictor, scales = "free", labeller = labeller(Predictor = predictor_labels)) +
  theme_minimal(base_size = 16) +
  labs(
    title = "",
    x = "Predictors",
    y = "Recovery success"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Adjust facet label text
    plot.title = element_text(hjust = 0.5)  # Center align title
  )

ggsave("~/eo_nas/EO4Alps/figs/scatters_temporal.png", width = 10, height = 7, dpi = 300)



### scatterplots zeitliches Modell
hexagons_temporal_df <- as.data.frame(hexagons_temporal_sp)

hexagons_temporal_df$mean_bare <- hexagons_temporal_df$mean_bare/100
hexagons_temporal_df$mean_bare <- hexagons_temporal_df$mean_bare*2
hexagons_df$mean_prec_total <- hexagons_temporal_df$mean_prec_total*0.1


# Define the desired order of facets
predictor_order <- c("mean_elevation","mean_severity", "mean_pre_dist_tree_cover",
                     "mean_VPD_yod1", "mean_temp_total", "mean_prec_total", "mean_bare")


# Convert to long format
# Ensure `Predictor` is a factor with this order
# Convert to long format
long_data <- hexagons_df %>%
  pivot_longer(cols = c("mean_VPD_yod1", "mean_elevation", "mean_severity", "mean_prec_total",
                        "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare"),
               names_to = "Predictor", values_to = "Value")

long_data <- long_data %>%
  mutate(Predictor = factor(Predictor, levels = predictor_order, ordered = TRUE))


# Define custom labels for facets
predictor_labels <- c(
  "mean_bare" = "mean_bare",
  "mean_elevation" = "mean_elevation",
  "mean_pre_dist_tree_cover" = "mean_pre_dist_tree_cover",
  "mean_prec_total" = "mean_prec_total",
  "mean_severity" = "mean_severity",
  "mean_temp_total" = "mean_temp_total",
  "mean_VPD_yod1" = "mean_VPD")




# Create faceted scatterplots
ggplot(long_data, aes(x = Value, y = mean_percent_recovered)) +
  geom_point(alpha = 0.5) +  # Scatterplot points with transparency
  geom_smooth(method = "lm", color = "blue") +  # Linear trend line
  facet_wrap(~ Predictor, scales = "free", labeller = labeller(Predictor = predictor_labels)) +
  theme_minimal(base_size = 16) +
  labs(
    title = "",
    x = "Predictors",
    y = "Recovery success"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  # Adjust facet label text
    plot.title = element_text(hjust = 0.5)  # Center align title
  )

ggsave("~/eo_nas/EO4Alps/figs/scatters_spatial.png", width = 10, height = 7, dpi = 300)




#-------------------------------------------------------------------------------
### scatter residuals
#-------------------------------------------------------------------------------

# Liste der Prädiktoren (alle numerischen Variablen außer den Residuen selbst)
predictors <- c("mean_elevation", "mean_severity", "mean_VPD_yod1", 
                "mean_prec_total", "mean_temp_total", 
                "mean_pre_dist_tree_cover", "mean_bare")

# Define custom order for predictors
predictor_order <- c("mean_elevation", "mean_severity", "mean_VPD_yod1", 
                     "mean_prec_total", "mean_temp_total", 
                     "mean_pre_dist_tree_cover", "mean_bare")


hexagons_recov10$mean_bare <- hexagons_recov10$mean_bare/100
hexagons_recov10$mean_bare <- hexagons_recov10$mean_bare*2

# Langes Format für Facet-Wrap
hex_long <- hexagons_recov10 %>%
  select(all_of(predictors), resid_ols, resid_gam, resid_GWR) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value")

# Convert Predictor column to factor with defined order
hex_long$Predictor <- factor(hex_long$Predictor, levels = predictor_order)

library(forcats)

# Rename one or more predictor levels
hex_long$Predictor <- fct_recode(hex_long$Predictor, 
                                 "Elevation" = "mean_elevation",
                                 "Severity" = "mean_severity",
                                 "VPD anomaly" = "mean_VPD_yod1",
                                 "Precipitation" = "mean_prec_total",
                                 "Temperature" = "mean_temp_total",
                                 "Pre-dist. tree cover" = "mean_pre_dist_tree_cover",
                                 "Post-dist. bare ground" = "mean_bare")



# OLS Residuen Facet Plot
p_ols <- ggplot(hex_long, aes(x = Value, y = resid_ols)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "red") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "OLS", y = "OLS residuals", x = "predictor_values")

# GAM Residuen Facet Plot
p_gam <- ggplot(hex_long, aes(x = Value, y = resid_gam)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "GAM", y = "GAM resiudals", x = "predictor_values")

# GAM Residuen Facet Plot
p_GWR <- ggplot(hex_long, aes(x = Value, y = resid_GWR)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgreen") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "GWR", y = "GWR resiudals", x = "predictor_values")


# Beide Plots anzeigen
grid.arrange(p_ols, p_gam, p_GWR, ncol = 3)

library(gridExtra)
library(grid)

# Create a combined plot using grid.arrange()
combined_plot <- arrangeGrob(p_ols, p_gam, p_GWR, ncol = 3)  # Save as an object

# Save the combined plot
ggsave("~/eo_nas/EO4Alps/figs/scatters_residuals_preds.png", 
       plot = combined_plot, width = 15, height = 7, dpi = 300)




#-------------------------------------------------------------------------------
### for temporal model
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
### scatter residuals
#-------------------------------------------------------------------------------

# Liste der Prädiktoren (alle numerischen Variablen außer den Residuen selbst)
predictors <- c("mean_elevation_difference", "mean_severity_difference", "mean_VPD_difference", 
                "mean_prec_difference", "mean_temp_difference", 
                "mean_TC_difference", "mean_bare_difference")

# Define custom order for predictors
predictor_order <- c("mean_elevation_difference", "mean_severity_difference", "mean_VPD_difference", 
                     "mean_prec_difference", "mean_temp_difference", 
                     "mean_TC_difference", "mean_bare_difference")


hexagons_recov10_temporal$mean_bare_difference <- hexagons_recov10_temporal$mean_bare_difference/100
hexagons_recov10_temporal$mean_bare <- hexagons_recov10_temporal$mean_bare_difference*2

# Langes Format für Facet-Wrap
hex_long <- hexagons_recov10_temporal %>%
  select(all_of(predictors), resid_ols, resid_gam, resid_GWR) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value")

# Convert Predictor column to factor with defined order
hex_long$Predictor <- factor(hex_long$Predictor, levels = predictor_order)

library(forcats)

# Rename one or more predictor levels
hex_long$Predictor <- fct_recode(hex_long$Predictor, 
                                 "Delta elevation" = "mean_elevation_difference",
                                 "Delta severity" = "mean_severity_difference",
                                 "Delta VPD anomaly" = "mean_VPD_difference",
                                 "Delta precipitation" = "mean_prec_difference",
                                 "Delta temperature" = "mean_temp_difference",
                                 "Delta pre-dist. tree cover" = "mean_TC_difference",
                                 "Delt post-dist. bare ground" = "mean_bare_difference")



# OLS Residuen Facet Plot
p_ols <- ggplot(hex_long, aes(x = Value, y = resid_ols)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "red") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "OLS", y = "OLS residuals", x = "predictor_values")

# GAM Residuen Facet Plot
p_gam <- ggplot(hex_long, aes(x = Value, y = resid_gam)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "GAM", y = "GAM resiudals", x = "predictor_values")

# GAM Residuen Facet Plot
p_GWR <- ggplot(hex_long, aes(x = Value, y = resid_GWR)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgreen") +  # Trendlinie
  theme_minimal() +
  facet_wrap(~Predictor, scales = "free_x", ncol= 2) +
  labs(title = "GWR", y = "GWR resiudals", x = "predictor_values")


# Beide Plots anzeigen
grid.arrange(p_ols, p_gam, p_GWR, ncol = 3)

library(gridExtra)
library(grid)

# Create a combined plot using grid.arrange()
combined_plot <- arrangeGrob(p_ols, p_gam, p_GWR, ncol = 3)  # Save as an object

# Save the combined plot
ggsave("~/eo_nas/EO4Alps/figs/scatters_residuals_preds_temporal.png", 
       plot = combined_plot, width = 15, height = 7, dpi = 300)









