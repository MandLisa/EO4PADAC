# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)
library(randomForest)
library(broom)
library(ranger)
library(doParallel)
library(reshape2)
library(corrplot)
library(caret)

#recovery_climate_filt <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_filt.csv")
#recovery_climate_unique <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_filt_unique.csv")
recovery_all_fractions <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions.csv")
recovery_all_fractions_unique <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions_unique.csv")

# Apply mean imputation only to numeric columns
recovery_imputed <- recovery_all_fractions %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Compute correlations
correlations <- recovery_imputed %>%
  select(recovery_rate, mean_VPD_pre, mean_VPD_post, mean_VPD_post_1_year, 
         mean_VPD_post_2_year, mean_VPD_post_3_year, VPD_consecutive_1y, 
         VPD_consecutive_2y, VPD_consecutive_3y) %>%
  cor(method = "pearson")  # Or use "spearman" if data is not normally distributed

print(correlations)



# Scatter plot
ggplot(recovery_imputed, aes(x = recovery_rate, y = mean_VPD_post_1_year)) +
  geom_point() +
  labs(title = "Scatter Plot of Recovery Time vs Mean VPD Post",
       x = "Recovery Time",
       y = "Mean VPD Post") +
  theme_minimal()


recovery_imputed_recov <- recovery_imputed %>%
  filter(recovery_rate != 100)


# Keep only one observation per ID and year
recovery_imputed_unique <- recovery_imputed_recov  %>%
  distinct(ID, .keep_all = TRUE)

### write
write.csv(recovery_imputed_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions_imputed_unique.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
### create predictor df

# Let's assume 'ID' and 'Date' are irrelevant and 'y' is the response
recovery_imputed_filt <- recovery_imputed_unique[, c("ID", "recovery_rate", "severity_relative",
                                              "avg_tree_share_before", "height", "slope",
                                              "mean_VPD_pre", "VPD_consecutive_yod", "VPD_consecutive_1y",
                                              "VPD_consecutive_2y", "VPD_consecutive_3y", "mean_pre_dist_artifical_land_share",
                                              "mean_pre_dist_bare_land_share", "mean_pre_dist_grassland_share",
                                              "mean_pre_dist_shrubland_share", "mean_pre_dist_coniferous_woodland_share",
                                              "mean_pre_dist_broadleaved_woodland_share", "artifical_land_share_yod", 
                                              "bare_land_share_yod", "grassland_share_yod", "shrubland_share_yod",
                                              "shrubland_share_yod", "broadleaved_woodland_share_yod", "coniferous_woodland_share_yod",
                                              "artifical_land_share_yod+1", 
                                              "bare_land_share_yod+1", "grassland_share_yod+1", "shrubland_share_yod+1",
                                              "shrubland_share_yod+1", "broadleaved_woodland_share_yod+1", "coniferous_woodland_share_yod+1",
                                              "artifical_land_share_yod+2", 
                                              "bare_land_share_yod+2", "grassland_share_yod+2", "shrubland_share_yod+2",
                                              "shrubland_share_yod+2", "broadleaved_woodland_share_yod+2", "coniferous_woodland_share_yod+2",
                                              "artifical_land_share_yod+3", 
                                              "bare_land_share_yod+3", "grassland_share_yod+3", "shrubland_share_yod+3",
                                              "shrubland_share_yod+3", "broadleaved_woodland_share_yod+3", "coniferous_woodland_share_yod+3",
                                              "VPD_autumn_yod", "VPD_spring_yod", "VPD_summer_yod", "VPD_autumn_yod+1", "VPD_spring_yod+1", "VPD_summer_yod+1",
                                              "VPD_autumn_yod+2", "VPD_spring_yod+2", "VPD_summer_yod+2",
                                              "VPD_autumn_yod+3", "VPD_spring_yod+3", "VPD_summer_yod+3")]


# without ID
# Exclude 'ID' and 'Date' columns, keeping only the relevant predictors and response 'y'
recovery_imputed_filt_ID <- recovery_imputed_filt[, !(names(recovery_imputed_filt) %in% c("ID"))]

head(recovery_imputed_filt_ID)


# compute correlarion
correlations <- cor(recovery_imputed_filt_ID)
cor_with_response <- correlations[, "recovery_rate"]
# Remove the correlation of the response variable with itself (which is 1)
cor_with_response <- cor_with_response[-which(names(cor_with_response) == "recovery_rate")]
# Sort the correlations by magnitude
sorted_correlations <- sort(cor_with_response, decreasing = TRUE)
# Plotting the bar plot
barplot(sorted_correlations, 
            main = "Correlation with Response Variable",
            xlab = "Predictors",
            ylab = "Correlation",
            las = 2, # Rotate labels for better readability
            col = "steelblue")


# Assuming sorted_correlations is a named vector
data <- data.frame(
  Predictors = names(sorted_correlations),
  Correlation = sorted_correlations
)


p <- ggplot(data, aes(x = reorder(Predictors, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "",
       x = "Predictors",
       y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave("~/eo_nas/EO4Alps/figs/coor_with_recovery.png", plot = p, width = 10, height = 7, dpi = 300)


# Calculate the correlation matrix for the predictors
cor_matrix <- cor(recovery_imputed_filt_ID)

# Use corrplot to create a correlogram
corrplot(cor_matrix, method = "circle", type = "lower", 
         tl.col = "black", tl.srt = 45,
         tl.cex = 0.6,  # Reduce the text size of labels
         addCoef.col = "black", number.cex = 0.2)  # Reduce the size of the correlation coefficient text


corrplot(cor_matrix, method = "circle", type = "lower", 
         order = "hclust", addrect = 2,  # Cluster and add rectangles around clusters
         tl.col = "black", tl.srt = 45,
         tl.cex = 0.6,
         addCoef.col = "black", number.cex = 0.2)


#-------------------------------------------------------------------------------
# Function to calculate VIF manually
vif <- function(model) {
  vifs <- numeric(length(coefficients(model)))
  names(vifs) <- names(coefficients(model))
  
  for (i in seq_along(vifs)) {
    # Exclude the intercept
    if (names(coefficients(model))[i] != "(Intercept)") {
      formula <- as.formula(paste(names(coefficients(model))[i], "~ ."))
      sub_model <- lm(formula, data = model$model)
      r_squared <- summary(sub_model)$r.squared
      vifs[i] <- 1 / (1 - r_squared)
    }
  }
  return(vifs)
}


# Find duplicate column names
duplicated_names <- names(recovery_imputed_filt_ID)[duplicated(names(recovery_imputed_filt_ID))]
duplicated_names

# Check if these column names are identical or just similar
unique(names(recovery_imputed_filt_ID))

# Rename columns to avoid issues with special characters
names(recovery_imputed_filt_ID) <- make.names(names(recovery_imputed_filt_ID), unique = TRUE)

# Calculate correlation matrix
cor_matrix <- cor(recovery_imputed_filt_ID[, -1])  # Exclude the response variable

# Find highly correlated predictors (e.g., correlation > 0.9)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.8)

# Remove these predictors from the dataset
recovery_imputed_reduced <- recovery_imputed_filt_ID[, -c(high_cor + 1)]  # +1 because `df` includes the response variable in the first column

fit <- lm(recovery_rate ~ ., data = recovery_imputed_reduced)
vif_values <- vif(fit)

while(max(vif_values) > 10) {
  high_vif <- names(which.max(vif_values))  # Identify predictor with highest VIF
  recovery_imputed_reduced <- recovery_imputed_reduced[, !names(recovery_imputed_reduced) %in% high_vif]  # Remove the predictor
  fit <- lm(recovery_rate ~ ., data = recovery_imputed_reduced)  # Refit the model
  vif_values <- vif(fit)  # Recalculate VIF values
}

summary(fit)

### write
write.csv(recovery_imputed_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_imputed_unique.csv", row.names=FALSE)




#-------------------------------------------------------------------------------



# Fit the model
model <- lm(recovery_rate ~ severity_relative +
            slope + 
              height +
               +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y,
            data = recovery_imputed)
summary(model)


# Standardize the predictors
recovery_standardized <- recovery_imputed_filt_ID %>%
  mutate(across(c(severity_relative, slope, height, mean_VPD_yod, mean_VPD_pre, mean_VPD_post_1_year,
                  mean_VPD_post_2_year, mean_VPD_post_3_year,
                  VPD_consecutive_yod, VPD_consecutive_1y), scale))




# Fit the standardized model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              mean_VPD_yod +
              mean_VPD_pre +
              VPD_consecutive_yod +
              VPD_consecutive_1y +
              VPD_consecutive_2y +
              VPD_consecutive_3y +
              mean_VPD_post_1_year,
            data = recovery_standardized)

summary(model)


# Extract the coefficients, standard errors, and confidence intervals
model_summary <- broom::tidy(fit)

# Exclude the intercept
model_summary <- model_summary[model_summary$term != "(Intercept)", ]

# Reorder the predictors for better plotting (if needed)
model_summary$term <- factor(model_summary$term, levels = model_summary$term)

# Plot using ggplot2 with predictors on the y-axis and coefficients on the x-axis
ggplot(model_summary, aes(y = term, x = estimate)) +
  geom_point(size = 3) +  # Points for the estimates
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +  # Horizontal error bars for standard errors
  theme_minimal() +
  labs(x = "Estimated Coefficient", y = "Predictor",
       title = "Effect of Predictors on Recovery Rate") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),        # Y-axis text size
    axis.text.x = element_text(size = 14),                    # X-axis text size
    axis.title.y = element_text(size = 16),                   # Y-axis title size
    axis.title.x = element_text(size = 16),                   # X-axis title size
    plot.title = element_text(size = 18, face = "bold")       # Plot title size
  )


### stratified by severity_class
# Fit separate models for each severity class
models <- recovery_standardized %>%
  group_by(severity_class) %>%
  do(model = lm(recovery_rate ~ severity_relative + slope + height +
                  mean_VPD_yod +
                  mean_VPD_pre +
                  VPD_consecutive_yod +
                  VPD_consecutive_1y +
                  VPD_consecutive_2y +
                  VPD_consecutive_3y +
                  mean_VPD_post_1_year,
                  data = .))

# Extract coefficients and standard errors
model_summary_by_class <- models %>%
  rowwise() %>%
  do({
    summary_model <- summary(.$model)
    coef_df <- as.data.frame(summary_model$coefficients)
    coef_df$term <- rownames(coef_df)
    coef_df$severity_class <- .$severity_class
    coef_df
  }) %>%
  select(term, estimate = Estimate, std.error = `Std. Error`, severity_class) %>%
  filter(term != "(Intercept)")  # Remove intercept term if needed

# Example of filtering out NA values
model_summary_by_class <- model_summary_by_class %>%
  filter(!is.na(estimate) & !is.na(std.error) & !is.na(term) & !is.na(severity_class))

# Define custom colors
custom_colors <- c("non stand-replacing" = "blue",   # Example color for "Low"
                   "stand-replacing" = "red") # Example color for "Medium"


# Plotting
ggplot(model_summary_by_class, aes(y = term, x = estimate, color = severity_class)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  #scale_x_log10() +  # Apply log transformation to the x-axis
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Estimated Coefficient", y = "Predictor",
       title = "") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )


# Custom function for symmetric log transformation
symmetric_log10 <- function(x, base = 10) {
  sign(x) * log10(abs(x) + 1) / log10(base)
}

# Define your custom order for predictors
#custom_order <- c("height", "slope", "aspect", "severity_relative", 
                  #"mean_VPD_pre","mean_VPD_yod", "mean_VPD_post_1_year", "VPD_consecutive_1y", "VPD_consecutive_2y",
                  #"VPD_consecutive_3y")

# Define your custom order for predictors
custom_order <- c("VPD_consecutive_3y", "VPD_consecutive_2y", "VPD_consecutive_1y", "VPD_consecutive_yod",
                  "mean_VPD_post_1_year", "mean_VPD_yod", "mean_VPD_pre", "severity_relative",
                  "slope", "height")



# Reorder the factor levels in your dataset
model_summary_by_class$term <- factor(model_summary_by_class$term, levels = custom_order)




# Plot with symmetric log transformation
ggplot(model_summary_by_class, aes(y = term, x = estimate, color = severity_class)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  scale_x_continuous(trans = scales::trans_new("symmetric_log10", 
                                               transform = symmetric_log10,
                                               inverse = function(x) 10^x - 1)) +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Estimated Coefficient (symmetric log scale)", y = "Predictor",
       title = "") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )


# Fit a Random Forest model
# Define the columns to use as predictors
predictors <- c("height", "slope", "severity_relative", 
                "mean_VPD_pre", "VPD_consecutive_yod", "VPD_consecutive_1y", "VPD_consecutive_2y",
                "mean_VPD_post_1_year")

# Subset the data frame to include only the predictors and target variable
recovery_rf <- recovery_imputed[, c(predictors, "recovery_rate")]

recovery_rf <- recovery_rf %>%
  mutate_if(is.factor, as.integer)  

set.seed(123)
recovery_rf_subset <- recovery_rf[sample(1:nrow(recovery_rf), size = 20000), ]  # Use a smaller sample

rf_model_subset <- randomForest(recovery_rate ~ ., data = recovery_rf_subset, importance = TRUE)

# Check if the importance scores are available in the model object
importance_scores <- rf_model_subset$importance


# If importance scores are available, convert to a data frame
importance_df <- as.data.frame(importance_scores)
importance_df$Feature <- rownames(importance_df)

# Rename columns for clarity
colnames(importance_df) <- c("IncMSE", "IncNodePurity", "Feature")

# Ensure importance_df is ordered by importance for better visualization
importance_df <- importance_df[order(-importance_df$IncMSE), ]

# Plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Feature", y = "Mean Decrease in MSE", title = "Feature Importance") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),  # Adjust text size as needed
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))




