recovery_pre2000 <- recovery_unique_sf %>% filter(yod < 2000)
recovery_post2000 <- recovery_unique_sf %>% filter(yod >= 2000)


recovery_pre2000_metric <- recovery_pre2000 %>%
  group_by(GRID_ID) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()


recovery_post2000_metric <- recovery_post2000 %>%
  group_by(GRID_ID) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()




hexagon_predictors_pre2000 <- recovery_pre2000_metric %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_recov_10_perc = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )


hexagon_predictors_post2000 <- recovery_post2000_metric %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_recov_10_perc = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )


# Perform spatial join
hexagon_predictors_pre2000_poly <- st_join(hexagons_selected, hexagon_predictors_pre2000, join = st_intersects)
hexagon_predictors_post2000_poly <- st_join(hexagons_selected, hexagon_predictors_post2000, join = st_intersects)




library(mgcv)

# Fit GAM for disturbances before 2000
gam_pre2000 <- gam(mean_recov_10_perc ~ s(mean_elevation) + s(mean_severity) + s(mean_VPD) +
                     s(mean_coniferous) + s(mean_broadleaved) + s(mean_bare),
                   data = hexagon_predictors_pre2000_poly)

gam_post2000 <- gam(mean_recov_10_perc ~ s(mean_elevation) + s(mean_severity) + s(mean_VPD) +
                      s(mean_coniferous) + s(mean_broadleaved) + s(mean_bare),
                    data = hexagon_predictors_post2000_poly)


plot(gam_pre2000, pages = 1, rug = TRUE)
plot(gam_post2000, pages = 1, rug = TRUE)





# Predict recovery rates for each period
recovery_metric <- recovery_unique_sf %>%
  group_by(yod) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()


hexagon_predictors <- recovery_metric %>%
  group_by(yod) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_recov_10_perc = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )


gam_combined <- gam(mean_recov_10_perc ~ s(yod) + s(mean_elevation) + s(mean_severity) + 
                      s(mean_VPD) + s(mean_coniferous) + s(mean_broadleaved) + s(mean_bare),
                    data = hexagon_predictors,
                    family = gaussian())


# Create a sequence of yod values
yod_seq <- seq(min(hexagon_predictors$yod), max(hexagon_predictors$yod), length.out = 100)

# Create a grid for predictions, varying `yod`
prediction_grid <- expand.grid(
  yod = yod_seq,
  mean_elevation = mean(hexagon_predictors$mean_elevation, na.rm = TRUE),
  mean_severity = mean(hexagon_predictors$mean_severity, na.rm = TRUE),
  mean_VPD = mean(hexagon_predictors$mean_VPD, na.rm = TRUE),
  mean_coniferous = mean(hexagon_predictors$mean_coniferous, na.rm = TRUE),
  mean_broadleaved = mean(hexagon_predictors$mean_broadleaved, na.rm = TRUE),
  mean_bare = mean(hexagon_predictors$mean_bare, na.rm = TRUE)
)


# Create sequences for predictors
yod_seq <- seq(min(hexagon_predictors$yod), max(hexagon_predictors$yod), length.out = 100)

# Base mean values for predictors (others held constant)
mean_values <- list(
  mean_elevation = mean(hexagon_predictors$mean_elevation, na.rm = TRUE),
  mean_severity = mean(hexagon_predictors$mean_severity, na.rm = TRUE),
  mean_VPD = mean(hexagon_predictors$mean_VPD, na.rm = TRUE),
  mean_coniferous = mean(hexagon_predictors$mean_coniferous, na.rm = TRUE),
  mean_broadleaved = mean(hexagon_predictors$mean_broadleaved, na.rm = TRUE),
  mean_bare = mean(hexagon_predictors$mean_bare, na.rm = TRUE)
)

# Function to extract partial effects for one predictor
extract_effect <- function(predictor_name, predictor_range, gam_model) {
  # Create grid varying the predictor of interest
  prediction_grid <- expand.grid(
    yod = yod_seq,
    mean_elevation = mean_values$mean_elevation,
    mean_severity = mean_values$mean_severity,
    mean_VPD = mean_values$mean_VPD,
    mean_coniferous = mean_values$mean_coniferous,
    mean_broadleaved = mean_values$mean_broadleaved,
    mean_bare = mean_values$mean_bare
  )
  
  # Adjust the grid for the predictor of interest
  prediction_grid[[predictor_name]] <- seq(
    min(hexagon_predictors[[predictor_name]], na.rm = TRUE),
    max(hexagon_predictors[[predictor_name]], na.rm = TRUE),
    length.out = 100
  )
  
  # Predict partial effects
  prediction_grid$predicted <- predict(
    gam_model,
    newdata = prediction_grid,
    type = "terms"
  )[, paste0("s(", predictor_name, ")")]
  
  # Add predictor name
  prediction_grid$predictor <- predictor_name
  
  return(prediction_grid)
}

# Extract effects for all predictors
effect_severity <- extract_effect("mean_severity", hexagon_predictors$mean_severity, gam_combined)
effect_VPD <- extract_effect("mean_VPD", hexagon_predictors$mean_VPD, gam_combined)
effect_broadleaved <- extract_effect("mean_broadleaved", hexagon_predictors$mean_broadleaved, gam_combined)
effect_coniferous <- extract_effect("mean_coniferous", hexagon_predictors$mean_coniferous, gam_combined)
effect_bare <- extract_effect("mean_bare", hexagon_predictors$mean_bare, gam_combined)

# Combine all effects
all_effects <- rbind(
  effect_severity,
  effect_VPD,
  effect_broadleaved,
  effect_coniferous,
  effect_bare
)


library(ggplot2)

ggplot(all_effects, aes(x = yod, y = predicted, color = predictor)) +
  geom_line(size = 1) +
  facet_wrap(~ predictor) +
  labs(
    title = "Partial Effects of Predictors Over Time",
    x = "Year of Disturbance (yod)",
    y = "Partial Effect (Coefficient)"
  ) +
  xlim(1986, 2013) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")



### tensor product smooth

gam_tensor <- gam(mean_recov_10_perc ~ te(yod, mean_VPD) + te(yod, mean_elevation) + te(yod, mean_severity) +
                         te(yod, mean_coniferous) + te(yod, mean_broadleaved) + te(yod, mean_bare),
                       data = hexagon_predictors,
                       family = gaussian())


# Create a sequence of integer years for yod
yod_seq <- seq(floor(min(hexagon_predictors$yod)), ceiling(max(hexagon_predictors$yod)), by = 1)

# Define mean values for other predictors
mean_values <- list(
  mean_elevation = mean(hexagon_predictors$mean_elevation, na.rm = TRUE),
  mean_severity = mean(hexagon_predictors$mean_severity, na.rm = TRUE),
  mean_VPD = mean(hexagon_predictors$mean_VPD, na.rm = TRUE),
  mean_coniferous = mean(hexagon_predictors$mean_coniferous, na.rm = TRUE),
  mean_broadleaved = mean(hexagon_predictors$mean_broadleaved, na.rm = TRUE),
  mean_bare = mean(hexagon_predictors$mean_bare, na.rm = TRUE)
)


extract_tensor_effect <- function(predictor_name, gam_model) {
  # Create grid for predictions
  prediction_grid <- expand.grid(
    yod = yod_seq,  # Use integer yod values
    mean_elevation = mean_values$mean_elevation,
    mean_severity = mean_values$mean_severity,
    mean_VPD = mean_values$mean_VPD,
    mean_coniferous = mean_values$mean_coniferous,
    mean_broadleaved = mean_values$mean_broadleaved,
    mean_bare = mean_values$mean_bare
  )
  
  # Vary the predictor of interest
  prediction_grid[[predictor_name]] <- seq(
    min(hexagon_predictors[[predictor_name]], na.rm = TRUE),
    max(hexagon_predictors[[predictor_name]], na.rm = TRUE),
    length.out = length(yod_seq)  # Match sequence length
  )
  
  # Construct the exact term name for the tensor product smooth
  term_name <- paste0("te(yod,", predictor_name, ")")
  
  # Predict partial effects for the tensor product smooth
  prediction_grid$predicted <- predict(
    gam_model,
    newdata = prediction_grid,
    type = "terms"
  )[, term_name]
  
  # Add predictor name for plotting
  prediction_grid$predictor <- predictor_name
  
  return(prediction_grid)
}


# Extract partial effects for all predictors
effect_elevation <- extract_tensor_effect("mean_elevation", gam_tensor)
effect_severity <- extract_tensor_effect("mean_severity", gam_tensor)
effect_VPD <- extract_tensor_effect("mean_VPD", gam_tensor)
effect_coniferous <- extract_tensor_effect("mean_coniferous", gam_tensor)
effect_broadleaved <- extract_tensor_effect("mean_broadleaved", gam_tensor)
effect_bare <- extract_tensor_effect("mean_bare", gam_tensor)

# Combine all effects into one data frame
all_tensor_effects <- rbind(
  effect_elevation,
  effect_severity,
  effect_VPD,
  effect_coniferous,
  effect_broadleaved,
  effect_bare
)


# Change the order of the panels (facets)
all_tensor_effects$predictor <- factor(
  all_tensor_effects$predictor,
  levels = c("mean_elevation", "mean_severity", "mean_VPD", 
             "mean_broadleaved", "mean_coniferous", "mean_bare")
)


p1 <- ggplot(all_tensor_effects, aes(x = yod, y = predicted, color = predictor)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +  
  facet_wrap(~ predictor, scales = "free_y") +
  labs(
    title = "",
    x = "Year of disturbance",
    y = "Partial effect"
  ) +
  xlim(1986, 2013) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/tensor_effects_predictors.png", plot = p1, width = 7, height = 3.5, dpi = 300)





#Partial Effects of Predictors Over Time (Tensor Product Smooths)


# baseline


# Summarize predictors over time
baseline_conditions <- hexagon_predictors %>%
  group_by(yod) %>%
  summarise(
    mean_elevation = mean(mean_elevation, na.rm = TRUE),
    sd_elevation = sd(mean_elevation, na.rm = TRUE),
    mean_severity = mean(mean_severity, na.rm = TRUE),
    sd_severity = sd(mean_severity, na.rm = TRUE),
    mean_VPD = mean(mean_VPD, na.rm = TRUE),
    sd_VPD = sd(mean_VPD, na.rm = TRUE),
    mean_broadleaved = mean(mean_broadleaved, na.rm = TRUE),
    sd_broadleaved = sd(mean_broadleaved, na.rm = TRUE),
    mean_coniferous = mean(mean_coniferous, na.rm = TRUE),
    sd_coniferous = sd(mean_coniferous, na.rm = TRUE),
    mean_bare = mean(mean_bare, na.rm = TRUE),
    sd_bare = sd(mean_bare, na.rm = TRUE)
  )

# Reshape the data to long format
baseline_conditions_long <- baseline_conditions %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "predictor",
    values_to = "mean_value"
  )


# Define the desired order of predictors
ordered_predictors <- c("mean_elevation", "mean_severity", "mean_VPD", 
                        "mean_broadleaved", "mean_coniferous", "mean_bare")

# Reorder the predictor column in your dataset
baseline_conditions_long$predictor <- factor(
  baseline_conditions_long$predictor, 
  levels = ordered_predictors
)

# Plot temporal trends in baseline conditions
p1 <- ggplot(baseline_conditions_long, aes(x = yod, y = mean_value, color = predictor)) +
  geom_line(size = 0.5) +
  geom_smooth(se = TRUE, method = "loess", size = 1.5, alpha = 0.2) +
  facet_wrap(~ predictor, scales = "free_y") +
  labs(
    title = "Temporal trends in baseline conditions",
    x = "Year of Disturbance (yod)",
    y = "Mean Value"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/baseline_conditions.png", plot = p1, width = 7, height = 3.5, dpi = 300)






# Function to fit linear models for each predictor
test_trends <- function(data, predictors, time_var = "yod") {
  results <- lapply(predictors, function(pred) {
    model <- lm(as.formula(paste(pred, "~", time_var)), data = data)
    summary(model)$coefficients[2, ]  # Extract slope coefficient and p-value
  })
  
  # Combine results into a data frame
  trend_results <- do.call(rbind, results)
  colnames(trend_results) <- c("Estimate", "Std.Error", "t-value", "p-value")
  rownames(trend_results) <- predictors
  return(as.data.frame(trend_results))
}

# List of predictors
predictors <- colnames(baseline_conditions)[grepl("^mean_", colnames(baseline_conditions))]

# Test trends for all predictors
trend_results <- test_trends(baseline_conditions, predictors)
print(trend_results)


library(gt)

# Create a gt table
trend_results_table <- trend_results %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(
    title = "Trends in baseline conditions over time",
    subtitle = "Linear model results"
  ) %>%
  fmt_number(
    columns = c("Estimate", "Std.Error", "t-value", "p-value"),
    decimals = 3
  ) %>%
  cols_label(
    Estimate = "Slope estimate",
    `Std.Error` = "Standard Error",
    `t-value` = "t-Value",
    `p-value` = "p-Value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.background.color = "white"  # Make it clear for screenshots
  )

# View the table (and take a screenshot for your presentation)
trend_results_table


### result trends:
# elevation (significant positive slope): positive trend: disturbances occur overtime at higher elevations, 
# mean elevation of disturbances increased by apprixmately 2.66m
# severity (siginificant positive slope): severity increases over time, disturbance severity increases by
# about 0.25% per year
# VPD (significant postivie slope): VPD anomalies increase over time, aligning with broader trend
# over drieness
# broadleaved share (non-significant): no evidence of temporal change
# coniferous share (non-significant): no evidence of temporal change
# bare ground share (sigificant negative slope): bare ground share 3y post-disturbance
# decreases over time 


baseline_conditions_period <- hexagon_predictors %>%
  mutate(period = ifelse(yod < 2000, "Before 2000", "After 2000")) %>%
  group_by(period) %>%
  summarise(
    mean_elevation = mean(mean_elevation, na.rm = TRUE),
    mean_severity = mean(mean_severity, na.rm = TRUE),
    mean_VPD = mean(mean_VPD, na.rm = TRUE),
    mean_broadleaved = mean(mean_broadleaved, na.rm = TRUE),
    mean_coniferous = mean(mean_coniferous, na.rm = TRUE),
    mean_bare = mean(mean_bare, na.rm = TRUE)
  )

print(baseline_conditions_period)



summary(gam_tensor)
plot(gam_tensor, pages = 1, residuals = FALSE, rug = FALSE)
