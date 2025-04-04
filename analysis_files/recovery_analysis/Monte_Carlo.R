library(dplyr)
library(purrr)
library(ggplot2)


# Function to compute mean recovery per year of disturbance (yod)
compute_recovery_by_yod <- function(data) {
  data %>%
    group_by(yod, geoloc) %>%
    summarise(mean_recovery = mean(recovery_10y), .groups = "drop") %>%
    arrange(yod)
}

# Observed recovery success per yod
observed_recovery <- compute_recovery_by_yod(recovery_unique)

# MC permutation function
permute_recovery_by_yod <- function(data) {
  data %>%
    mutate(recovery_10y = sample(recovery_10y)) %>%
    compute_recovery_by_yod()
}

# Run permutations
n_permutations <- 5000
set.seed(123)

null_distributions <- replicate(n_permutations, permute_recovery_by_yod(recovery_unique), simplify = FALSE)

# Convert to df
null_df <- bind_rows(null_distributions, .id = "permutation") %>%
  group_by(yod) %>%
  summarise(
    mean_recovery_null = mean(mean_recovery, na.rm = TRUE),
    null_ci_lower = quantile(mean_recovery, 0.025, na.rm = TRUE),
    null_ci_upper = quantile(mean_recovery, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


# Compare observed values against null distribution
results <- observed_recovery %>%
  left_join(null_df, by = "yod") %>%
  rowwise() %>%
  mutate(
    null_values = list(
      sapply(null_distributions, function(nd) {
        val <- nd$mean_recovery[nd$yod == yod]
        if (length(val) == 0) NA else val
      })
    ),
    p_value = mean(unlist(null_values) >= mean_recovery, na.rm = TRUE)
  ) %>%
  select(-null_values)



# plot
ggplot(results, aes(x = yod, y = mean_recovery)) +
  geom_point(color = "blue", size = 3) +
  #geom_line(aes(y = mean_recovery_null), color = "red") +
  geom_ribbon(aes(ymin = null_ci_lower, ymax = null_ci_upper), alpha = 0.3, fill = "gray") +
  labs(
    title = "Observed vs. Null Distribution of Recovery Success",
    x = "Year of Disturbance (yod)",
    y = "Mean Recovery Success"
  ) +
  facet_wrap(~ geoloc) +
  theme_minimal()





