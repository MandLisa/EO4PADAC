library(mgcv)
install.packages(c("gratia", "MuMIn", "mgcv"))
library(ggplot2)
library(patchwork)  # To combine multiple plots
library(gratia)      # For smooth term visualization in GAMs
library(mgcv)
library(MuMIn)

# Compute centroids
hexagons_recov10_centros <- hexagons_recov10
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_severity = ifelse(mean_severity > 90, 70, mean_severity))
hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare/100
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_bare = ifelse(mean_bare > 10, 1, mean_bare))
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_elevation = ifelse(mean_elevation > 1700, 600, mean_elevation))


hexagons_recov10_centros$centroid <- st_centroid(hexagons_recov10$geometry)

# Extract longitude and latitude
hexagons_recov10_centros$long <- st_coordinates(hexagons_recov10_centros$centroid)[,1]
hexagons_recov10_centros$lat <- st_coordinates(hexagons_recov10_centros$centroid)[,2]


colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_percent_recovered"] <- "Recovery_success"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_elevation"] <- "Elevation"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_severity"] <- "Severity"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_VPD_yod1"] <- "VPD anomalies"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_prec_total"] <- "Precipitation"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_temp_total"] <- "Temperature"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_pre_dist_tree_cover"] <- "Pre-disturbance tree cover"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_bare"] <- "Post-disturbance bare ground share"



library(mgcv)
fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  # Spatial smooth term
                 s(mean_elevation) +
                 s(mean_severity) + 
                 s(mean_VPD_yod1) + 
                 s(mean_prec_total) +
                 s(mean_temp_total) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")

appraise(fit.gam)

ggsave("~/eo_nas/EO4Alps/figs/model_diagnostics.png", width = 10, height = 8, dpi = 300)


hexagons_recov10$resid_gam <- residuals(fit.gam, type = "deviance")

### spatial model
# Nearest neighbor weights für räumliche Autokorrelation
nb <- poly2nb(hexagons_recov10)  
lw <- nb2listw(nb)

moran.test(hexagons_recov10$resid_gam, lw)



# Create a smooth plot for each predictor
plot_smooths <- draw(fit.gam, residuals = TRUE, contour = FALSE) +
  ggtitle("") +
  theme_minimal()

print(plot_smooths)




plot_smooths <- draw(fit.gam, residuals = TRUE, contour = FALSE) +
  ggtitle("Smoothed Effects of Predictors") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),  # Sets light gray background for all panels
    #strip.background = element_rect(fill = "gray95", color = NA),  # Ensures all facet labels have a uniform background
    panel.grid.major = element_line(color = "white"),  # Makes major grid lines white
    panel.grid.minor = element_line(color = "white")   # Makes minor grid lines white
  )

# Display the plot
print(plot_smooths)

ggsave("~/eo_nas/EO4Alps/figs/gratia_plot.png", width = 10, height = 8, dpi = 300)


### with interactions

# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_VPD_yod1, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Generate smooth plots for each predictor
ggplot(smooth_data, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "blue") +
  facet_wrap(~ predictor, scales = "free_x") +
  theme_minimal() +
  labs(y = "Predicted Recovery Percentage", x = "Predictor Value")



### ceheck for interactions
fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             ti(mean_elevation, mean_VPD_yod1) +  # Interaction term
                             s(mean_severity) + 
                             s(mean_prec_total) +
                             s(mean_temp_total) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")

fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             s(mean_elevation) + 
                             s(mean_VPD_yod1) + 
                             ti(mean_elevation, mean_VPD_yod1) +  # Interaction term
                             s(mean_severity) + 
                             s(mean_prec_total) +
                             s(mean_temp_total) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")



summary(fit.gam_interaction)

vis.gam(fit.gam_interaction, view = c("mean_elevation", "mean_VPD_yod1"), type = "response", color = "topo")


plot_smooths <- draw(fit.gam_interaction, contour = FALSE, residuals = TRUE) +
  ggtitle("Smoothed Effects of Predictors") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Ensures balanced mapping
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),  
    panel.grid.major = element_line(color = "white"),  
    panel.grid.minor = element_line(color = "white")  
  )

  
# Display the plot
print(plot_smooths)



# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_interaction, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_VPD_yod1, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Add an artificial interaction variable for visualization
smooth_data_interaction <- hexagons_recov10_centros %>%
  select(mean_elevation, mean_VPD_yod1) %>%
  mutate(interaction_term = paste0("Elevation x VPD")) %>%
  rename(value = mean_elevation) %>%  # Use elevation for x-axis
  mutate(predicted = predict(fit.gam_interaction, newdata = hexagons_recov10_centros, type = "response"))

# Combine single predictor and interaction data
smooth_data_interaction <- smooth_data_interaction %>%
  select(value, predicted, interaction_term) %>%
  rename(predictor = interaction_term)

smooth_data_combined <- bind_rows(smooth_data, smooth_data_interaction)

# Define new facet labels
facet_labels <- c(
  "mean_VPD_yod1" = "VPD anomalies",
  "mean_temp_total" = "Temperature",
  "mean_prec_total" = "Precipitation",
  "mean_severity" = "Severity",
  "mean_pre_dist_tree_cover" = "Pre-disturbance tree cover",
  "mean_bare" = "Post-disturbance bare ground share",
  "mean_elevation" = "Elevation",
  "Elevation x VPD" = "Interaction: VPD anomalies & Elevation"
)

# Define desired order based on the RENAMED facet labels
custom_order <- c(
  "Interaction: VPD anomalies & Elevation",
  "Elevation",
  "VPD anomalies", 
  "Severity",
  "Temperature",
  "Precipitation",
  "Pre-disturbance tree cover",
  "Post-disturbance bare ground share"
)

# Convert predictor column to a factor using the RENAMED order
smooth_data_combined <- smooth_data_combined %>%
  mutate(predictor = factor(facet_labels[predictor], levels = custom_order))



# Generate smooth plots without the NA panel
ggplot(smooth_data_combined, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#631d51") +
  facet_wrap(~ predictor, scales = "free_x") +  
  theme_bw(base_size = 14) +
  labs(y = "Predicted Recovery Percentage", x = "Predictor Value")



AIC(fit.gam, fit.gam_interaction)

vis.gam(fit.gam_interaction, view = c("mean_elevation", "mean_VPD_yod1"), type = "response", color = "topo")

summary(fit.gam_interaction)

anova(fit.gam, fit.gam_interaction, test = "Chisq")









# Define a grid of predictor values for prediction
new_data <- expand.grid(
  mean_elevation = seq(min(hexagons_recov10$mean_elevation, na.rm = TRUE), 
                       max(hexagons_recov10$mean_elevation, na.rm = TRUE), length.out = 100),
  mean_severity = mean(hexagons_recov10$mean_severity, na.rm = TRUE),  # Hold others constant
  mean_VPD_yod1 = mean(hexagons_recov10$mean_VPD_yod1, na.rm = TRUE),
  mean_prec_total = mean(hexagons_recov10$mean_prec_total, na.rm = TRUE),
  mean_temp_total = mean(hexagons_recov10$mean_temp_total, na.rm = TRUE),
  mean_pre_dist_tree_cover = mean(hexagons_recov10$mean_pre_dist_tree_cover, na.rm = TRUE),
  mean_bare = mean(hexagons_recov10$mean_bare, na.rm = TRUE),
  long = mean(hexagons_recov10$long, na.rm = TRUE),  
  lat = mean(hexagons_recov10$lat, na.rm = TRUE)
)

# Generate predictions & confidence intervals in one step
preds <- predict(fit.gam, newdata = new_data, type = "response", se.fit = TRUE)

# Store predictions in new_data
new_data$predicted <- preds$fit
new_data$se <- preds$se.fit
new_data$upper <- new_data$predicted + 1.96 * new_data$se
new_data$lower <- new_data$predicted - 1.96 * new_data$se






# Plot predictions across each predictor
library(ggplot2)

# Plot for mean_elevation
ggplot(new_data, aes(x = mean_elevation, y = predicted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
  labs(title = "Predicted Recovery Across Elevation", x = "Mean Elevation", y = "Predicted Recovery") +
  theme_minimal()




# remove centroid column
hexagons_recov10 <- hexagons_recov10 %>% select(-centroid)



hexagons_recov10 <- hexagons_recov10 %>%
  mutate(resid_gam = residuals(fit.gam))



# GAM resiudal map
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = resid_gam), color = "grey") +  
  scale_fill_gradient2(
    low = "green",    # Color for negative residuals
    mid = "white",    # Color for zero residuals
    high = "orange",  # Color for positive residuals
    midpoint = 0,     # Center the gradient at 0
    name = "GAM residuals with smooth term"
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "GAM residuals")

ggplot(hexagons_recov10) +
  geom_sf(aes(fill = resid_gam), color = "grey") +  
  scale_fill_gradient2(
    low = "green",    # Color for negative residuals
    mid = "white",    # Color for zero residuals
    high = "orange",  # Color for positive residuals
    midpoint = 0,     # Center the gradient at 0
    name = "GAM residuals with smooth term",
    limits = c(-50, 25),  # Set legend limits
    oob = scales::squish  # Ensures values outside range are squished into limits
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "GAM residuals")




ggsave("~/eo_nas/EO4Alps/figs/resiudals_gam_smooth.png", width = 10, height = 8, dpi = 300)



# Extract fitted values for visualization
vis_data <- data.frame(predict(fit.gam, type = "response", se.fit = TRUE))
vis_data$long <- hexagons_recov10_centros$long
vis_data$lat <- hexagons_recov10_centros$lat

# Plot the spatial effect
ggplot(vis_data, aes(x = long, y = lat, fill = fit)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Predicted Recovery\nSuccess Probability") +
  theme_minimal() +
  labs(title = "Spatial Pattern of Recovery Success (GAM Prediction)",
       x = "Longitude", y = "Latitude")








#-------------------------------------------------------------------------------
# MuMin

library(MuMIn)

options(na.action = "na.fail") 

fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  # Spatial smooth term
                 s(mean_elevation) +
                 s(mean_severity) + 
                 s(mean_VPD_yod1) + 
                 s(mean_prec_total) +
                 s(mean_temp_total) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")


mod_global <- gam(mean_percent_recovered ~ s(long, lat, bs = "tp") + 
                    s(mean_elevation) +
                    s(mean_elevation) +
                    s(mean_severity) +
                    s(mean_VPD_yod1) +
                    s(mean_prec_total) +
                    s(mean_temp_total) +
                    s(mean_pre_dist_tree_cover) +
                    s(mean_bare),
                  data = hexagons_recov10_centros, method = "ML")
                    



model_selection <- dredge(mod_global, rank = "AICc")
print(model_selection)


best_model <- get.models(model_selection, 1)[[1]]
summary(best_model)


avg_model <- model.avg(model_selection, subset = delta < 2)  # Keep models within 2 AICc units
summary(avg_model)



library(spdep)

# Extract residuals from the best GAM
hexagons_recov10$resid_best <- residuals(best_model, type = "deviance")

# Moran’s I test
moran.test(hexagons_recov10$residuals, lw)



library(MuMIn)

# Perform model selection
model_selection <- dredge(mod_gam, rank = "AICc")

# Extract the top models (within ΔAICc < 2)
top_models <- subset(model_selection, delta < 2)

# Convert to a dataframe for plotting
top_models_df <- as.data.frame(top_models)

# Plot AICc Scores for Top Models
ggplot(top_models_df, aes(x = reorder(row.names(top_models_df), AICc), y = AICc)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Model Selection Results (Top Models)", x = "Model", y = "AICc")




avg_model <- model.avg(model_selection, subset = delta < 2)
summary(avg_model)

# Extract variable importance
var_imp <- importance(avg_model)

# Convert to dataframe
var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)

# Plot Variable Importance from Model Averaging
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance from Model Averaging", x = "Predictor", y = "Importance")





library(spdep)

# Compute Moran’s I for residuals
hex_data$residuals <- residuals(mod_gam, type = "deviance")

# Create spatial weights
coords <- as.matrix(hex_data[, c("long", "lat")])
nb <- dnearneigh(coords, 0, 50000)  # Adjust distance
lw <- nb2listw(nb, style = "W")

# Moran’s I test
moran.test(hex_data$residuals, lw)















