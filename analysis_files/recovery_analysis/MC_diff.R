### or with all other variables as well
recovery_sf_temporal <- recovery_sf %>%
  group_by(GRID_ID) %>%
  summarize(
    # Calculations for yod < 2000
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    # Calculations for yod >= 2000
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recovery_10yn[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    # Overall calculations
    total_observations = n(),
    total_recovered = sum(recovery_10yn, na.rm = TRUE),
    percent_recovered_overall = (total_recovered / total_observations) * 100
  ) %>%
  mutate(
    # Differences between yod < 2000 and yod >= 2000
    percent_recovered_difference = percent_recovered_yod_after_2000 - percent_recovered_yod_before_2000,
  ) %>%
  ungroup()



# or by yod
recovery_sf_temporal <- recovery_sf %>%
  group_by(GRID_ID, yod) %>%  # Group by GRID_ID and yod
  summarize(
    total_observations = n(),
    total_recovered = sum(recovery_10yn, na.rm = TRUE),
    percent_recovered = (total_recovered / total_observations) * 100
  ) %>%
  ungroup()


set.seed(42)
n_permutations <- 1000
p_values_list <- list()

for (grid in unique(recovery_hex$GRID_ID)) {
  
  # Subset the aggregated data for this hexagon (GRID_ID)
  grid_data <- recovery_hex %>% filter(GRID_ID == grid)
  
  # Compute observed values:
  observed_before_2000 <- mean(grid_data$percent_recovered[grid_data$yod < 2000], na.rm = TRUE)
  observed_after_2000 <- mean(grid_data$percent_recovered[grid_data$yod >= 2000], na.rm = TRUE)
  observed_difference <- observed_after_2000 - observed_before_2000  # The metric of interest
  
  # Monte Carlo permutation test (shuffle `yod` categories)
  random_differences <- replicate(n_permutations, {
    
    # Shuffle `yod` labels
    shuffled_yod <- sample(grid_data$yod)
    
    # Assign the shuffled `yod` back
    shuffled_data <- grid_data %>%
      mutate(yod = shuffled_yod) %>%
      group_by(GRID_ID, yod) %>%
      summarize(
        total_observations = sum(total_observations),
        total_recovered = sum(total_recovered),
        percent_recovered = (total_recovered / total_observations) * 100,
        .groups = "drop"  
      )
    
    # Compute shuffled recovery differences
    shuffled_before_2000 <- mean(shuffled_data$percent_recovered[shuffled_data$yod < 2000], na.rm = TRUE)
    shuffled_after_2000 <- mean(shuffled_data$percent_recovered[shuffled_data$yod >= 2000], na.rm = TRUE)
    
    shuffled_after_2000 - shuffled_before_2000  # Difference in shuffled recovery success
  })
  
  # Compute p-value: How often is the shuffled difference >= observed difference?
  p_value <- mean(abs(random_differences) >= abs(observed_difference))
  
  # Store results
  p_values_list[[grid]] <- tibble(GRID_ID = grid, observed_difference = observed_difference, p_value = p_value)
}

# Combine results into a single dataframe
recovery_sf_temporal <- bind_rows(p_values_list)

# Combine results into a single dataframe
recovery_sf_temporal <- bind_rows(p_values_list)
























# rmeove NAs
sum(is.na(recovery_sf_temporal$percent_recovered_difference))  
recovery_sf_temporal <- recovery_sf_temporal %>% drop_na(percent_recovered_difference)


# Monte Carlo Permutation Test
set.seed(42)
n_permutations <- 5000
random_differences <- replicate(n_permutations, {
  permuted_recovery <- sample(recovery_sf_temporal$percent_recovered_yod_before_2000)
  recovery_sf_temporal$percent_recovered_yod_after_2000 - permuted_recovery
})

# Berechnung der p-Werte
p_values <- mapply(function(obs, random) mean(abs(random) >= abs(obs)), 
                   recovery_sf_temporal$percent_recovered_difference, 
                   as.data.frame(t(random_differences)))

#p_values <- p_values * 0.1
#p_values_df <- p_values_df$p_value * 0.1

recovery_sf_temporal <- recovery_sf_temporal %>%
  mutate(p_value = p_values_df)


recovery_sf_temporal <- recovery_sf_temporal %>%
  mutate(
    percent_below = rowMeans(random_differences < percent_recovered_difference, na.rm = TRUE),
    percent_above = rowMeans(random_differences > percent_recovered_difference, na.rm = TRUE)
  )


# add significant level
recovery_sf_temporal <- recovery_sf_temporal %>%
  mutate(
    significant = p_value < 0.07,
    percent_below = ifelse(is.na(p_value), NA, rowMeans(random_differences < percent_recovered_difference, na.rm = TRUE)),
    percent_above = ifelse(is.na(p_value), NA, rowMeans(random_differences > percent_recovered_difference, na.rm = TRUE))
  )


# Histogramm der Nullverteilung
ggplot(data.frame(random_differences = as.vector(random_differences)), aes(x = random_differences)) +
  geom_histogram(bins = 100, alpha = 0.6) +
  geom_vline(aes(xintercept = mean(recovery_sf_temporal$percent_recovered_difference)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "", x = "Randomized Recovery Change", y = "Frequency") +
  theme_bw()

# Falls du eine räumliche Datei mit Hexagon-Geometrien hast
# hex_sf <- st_read("your_hexagon_shapefile.shp")
# hex_sf <- hex_sf %>% left_join(recovery_df, by = "hexagon")

# Perform spatial join
recovery_sf_temporal <- st_join(hexagons_selected, recovery_sf_temporal, join = st_intersects)


# Karte der p-Werte
map_pvalues <- ggplot(recovery_sf_temporal) +
  geom_sf(aes(fill = p_value), color = "black") +
  scale_fill_viridis_c(option = "B", direction = -1, name = "P-Value") +
  labs(title = "P-Values of Recovery Change") +
  theme_minimal()

# Boxplot der Veränderung nach Signifikanz
boxplot_significance <- ggplot(recovery_sf_temporal, aes(x = significant, y = percent_recovered_difference, fill = significant)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Recovery Change by Significance", x = "Significant", y = "Recovery Change") +
  theme_minimal()


#-------------------------------------------------------------------------------

recovery_yearly <- recovery_sf_temp %>%
  group_by(GRID_ID, yod) %>%
  summarize(
    total_observations = n(),
    total_recovered = sum(recovery_10yn, na.rm = TRUE),
    percent_recovered = (total_recovered / total_observations) * 100
  ) %>%
  ungroup()


set.seed(42)
n_permutations <- 5000

# Initialize storage for p-values
p_values_list <- list()

for (grid in unique(recovery_yearly$GRID_ID)) {
  
  # Subset data for this specific GRID_ID
  grid_data <- recovery_yearly %>% filter(GRID_ID == grid)
  
  # Compute observed recovery differences
  observed_before_2000 <- mean(grid_data$percent_recovered[grid_data$yod < 2000], na.rm = TRUE)
  observed_after_2000 <- mean(grid_data$percent_recovered[grid_data$yod >= 2000], na.rm = TRUE)
  observed_difference <- observed_after_2000 - observed_before_2000
  
  # Monte Carlo Permutation for this grid
  random_differences <- replicate(n_permutations, {
    shuffled_yod <- sample(grid_data$yod)
    
    shuffled_data <- grid_data %>%
      mutate(yod = shuffled_yod) %>%
      group_by(yod) %>%
      summarize(
        total_observations = n(),
        total_recovered = sum(recovery_10yn, na.rm = TRUE),
        percent_recovered = (total_recovered / total_observations) * 100
      ) %>%
      ungroup()
    
    # Compute shuffled recovery differences
    shuffled_before_2000 <- mean(shuffled_data$percent_recovered[shuffled_data$yod < 2000], na.rm = TRUE)
    shuffled_after_2000 <- mean(shuffled_data$percent_recovered[shuffled_data$yod >= 2000], na.rm = TRUE)
    
    shuffled_after_2000 - shuffled_before_2000
  })
  
  # Compute p-value for this grid
  p_value <- mean(abs(random_differences) >= abs(observed_difference))
  
  # Store results
  p_values_list[[grid]] <- tibble(GRID_ID = grid, observed_difference = observed_difference, p_value = p_value)
}

# Combine results into a single data frame
recovery_sf_temporal <- bind_rows(p_values_list)
