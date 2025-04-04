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
library(spgwr)
library(kableExtra)



# compute mean recovery success per yod
compute_recovery_by_yod <- function(data) {
  data %>%
    group_by(yod) %>%
    summarise(mean_recovery = mean(recovery_10y)) %>%
    arrange(yod)
}

# Compute observed recovery success per yod
observed_recovery <- compute_recovery_by_yod(recovery_unique)

# Monte Carlo permutation test function
permute_recovery_by_yod <- function(data) {
  data <- data %>%
    mutate(recovery_10y = sample(recovery_10y))  
  compute_recovery_by_yod(data)
}

# Run permutations 5000 times (or more often?!)
n_permutations <- 5000
set.seed(123)

null_distributions <- replicate(n_permutations, permute_recovery_by_yod(recovery_unique), simplify = FALSE)

# Convert to data frame for analysis
null_df <- bind_rows(null_distributions, .id = "permutation") %>%
  group_by(yod) %>%
  summarise(mean_recovery_null = mean(mean_recovery), .groups = "drop")

# Compute p-values: Compare observed values against null distribution
results <- observed_recovery %>%
  left_join(null_df, by = "yod") %>%
  mutate(
    p_value = sapply(seq_along(mean_recovery), function(i) {
      null_values <- sapply(null_distributions, function(nd) nd$mean_recovery[nd$yod == observed_recovery$yod[i]])
      mean(abs(null_values) >= abs(mean_recovery[i]))  
    })
  )

print(results)


# Compute 95% Confidence Intervals for Null Distribution
null_summary <- bind_rows(null_distributions, .id = "permutation") %>%
  group_by(yod) %>%
  summarise(
    null_mean = mean(mean_recovery),
    null_ci_lower = quantile(mean_recovery, 0.025), 
    null_ci_upper = quantile(mean_recovery, 0.975),  
    .groups = "drop"
  )

# Merge observed and null summary
plot_data <- observed_recovery %>%
  left_join(null_summary, by = "yod")

# Create the plot
ggplot(plot_data, aes(x = yod)) +
  geom_line(aes(y = mean_recovery), color = "black", size = 1) +  
  geom_point(aes(y = mean_recovery), color = "black", size = 2) +
  geom_ribbon(aes(ymin = null_ci_lower, ymax = null_ci_upper), fill = "gray", alpha = 0.3) +  # Null CI
  geom_line(aes(y = null_mean), color = "blue", linetype = "dashed", size = 1) +  # Null mean
  labs(
    x = "Year of Disturbance",
    y = "Mean Recovery Success",
    title = "Observed Recovery Success vs Null Expectation"
  ) +
  theme_minimal()



ggplot(plot_data, aes(x = yod, y = mean_recovery)) +
  geom_point(color = "blue", size = 3) +
  geom_line(aes(y = mean_recovery_null), color = "red") +
  geom_ribbon(aes(ymin = null_ci_lower, ymax = null_ci_upper), alpha = 0.3, fill = "gray") +
  labs(
    title = "Observed vs. Null Distribution of Recovery Success",
    x = "Year of Disturbance (yod)",
    y = "Mean Recovery Success"
  ) +
  theme_minimal()


#------------------------------------------------------------------------------
### per geoloc

compute_recovery_by_yod_geoloc <- function(data) {
  data %>%
    group_by(yod, geoloc) %>%
    summarise(mean_recovery = mean(recovery_10y), .groups = "drop") %>%
    arrange(yod)
}


permute_recovery_by_yod_geoloc <- function(data) {
  data %>%
    group_by(geoloc) %>%
    mutate(recovery_10y = sample(recovery_10y)) %>%  # Shuffle recovery success
    ungroup() %>%
    compute_recovery_by_yod_geoloc()  # Compute new mean per `yod` and `geoloc`
}

# Run 5000 Monte Carlo permutations
n_permutations <- 5000
set.seed(123)

null_distributions_geoloc <- replicate(n_permutations, permute_recovery_by_yod_geoloc(recovery_unique), simplify = FALSE)


null_summary_geoloc <- bind_rows(null_distributions_geoloc, .id = "permutation") %>%
  group_by(geoloc, yod) %>%
  summarise(
    null_mean = mean(mean_recovery),
    null_ci_lower = quantile(mean_recovery, 0.025),  # 2.5% Confidence Interval
    null_ci_upper = quantile(mean_recovery, 0.975),  # 97.5% Confidence Interval
    .groups = "drop"
  )

plot_data <- observed_recovery_geoloc %>%
  left_join(null_summary_geoloc, by = c("yod", "geoloc"))

plot_data <- plot_data[!is.na(plot_data$geoloc), ]

plot_data1 <- plot_data[plot_data$yod >= 1987, ]

# Create the plot with facets for each geolocation
ggplot(plot_data, aes(x = yod)) +
  geom_line(aes(y = mean_recovery), color = "black", size = 1) + 
  geom_point(aes(y = mean_recovery), color = "black", size = 2) +
  geom_ribbon(aes(ymin = null_ci_lower, ymax = null_ci_upper), fill = "gray", alpha = 0.7) +  
  geom_line(aes(y = null_mean), color = "blue", linetype = "dashed", size = 1) +  
  facet_wrap(~ geoloc, scales = "free_y") +  
  labs(
    x = "Year of Disturbance",
    y = "Mean Recovery Success",
    title = "Observed Recovery Success vs Null Expectation per Geolocation"
  ) +
  ylim(0.45, 0.8) +
  xlim(1988, 2013) +
  theme_minimal()

plot_data %>%
  mutate(outside_CI = mean_recovery < null_ci_lower | mean_recovery > null_ci_upper) %>%
  group_by(geoloc) %>%
  summarise(percent_outside_CI = mean(outside_CI) * 100)

plot_data <- plot_data %>%
  mutate(period = ifelse(yod < 2000, "Before 2000", "After 2000"))


# Add a "source" column to distinguish observed vs. Monte Carlo simulated data
plot_data <- plot_data %>%
  mutate(source = "Observed")



### heatmap
ggplot(plot_data, aes(x = yod, y = geoloc, fill = mean_recovery)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Recovery Success") +
  labs(
    x = "Year of Disturbance (yod)",
    y = "Geolocation",
    title = "Recovery Success Over Time (Observed)"
  ) +
  theme_minimal()




# Convert MC Simulated Data to Long Format
null_distributions_long <- bind_rows(null_distributions_geoloc, .id = "permutation") %>%
  mutate(source = "Null (MC Simulated)")

# Convert Observed Recovery Data to Long Format
observed_long <- observed_recovery_geoloc %>%
  mutate(source = "Observed")

# Combine the Two Datasets
combined_data <- bind_rows(null_distributions_long, observed_long)

combined_data <- combined_data[!is.na(combined_data$geoloc), ]


# Reduce alpha for MC distributions so observed data stands out
ggplot(combined_data, aes(x = mean_recovery, y = factor(yod), fill = source)) +
  geom_density_ridges(
    aes(fill = source), alpha = 0.3, scale = 1.2
  ) +  
  geom_point(
    data = combined_data %>% filter(source == "Observed"), 
    aes(x = mean_recovery, y = factor(yod)), 
    color = "black", size = 1.2, alpha = 0.8
  ) +  
  facet_wrap(~ geoloc) +
  coord_cartesian(xlim = c(0.4, 0.85)) +  # Fixes the x-axis issue
  scale_fill_manual(values = c("Observed" = "black", "Null (MC Simulated)" = "gray")) +
  labs(
    x = "Recovery Success (10y post-disturbance)",
    y = "Year of Disturbance (yod)",
    title = "Yearly Recovery Success Distribution (Observed vs. Null)"
  ) +
  theme_minimal()











