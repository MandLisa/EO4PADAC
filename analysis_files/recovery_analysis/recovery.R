# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(imager)
library(terra)
library(spatial)
library(rgeos)
library(spatialEco)
library(readr)
library(spatstat)
library(dplyr)
library(mgcv)
library(zoo)
install.packages("reshape2")
library(reshape2)
install.packages("psych")
library(psych)

setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv")

# load dataframe, this is the cleaned df 
data <- read_csv("/data/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/t_min_severity.csv")

# clean
# Set all values less than 0 in the "share" variable to 0
data <- data %>%
  mutate(share = ifelse(share < 0, 0, share))

data <- data %>%
  mutate(share = ifelse(share > 100, 100, share))


#-------------------------------------------------------------------------------
# compute min_year and min_cover of respective year
#-------------------------------------------------------------------------------

data_test <- data
data_test$time_to_min <- NULL
data_test$min_year <- NULL
data_test$min_tree_cover <- NULL

data_test <- data_test[, -c(8:8)]
names(data_test)[names(data_test) == "ID_new"] <- "ID"
names(data_test)[names(data_test) == "cover_smooth_before"] <- "tree_cov"


time_to_min <- function(data) {
  # Find the minimum tree share value and the year when the minimum is reached 
  # for each time series (ID)
  min_share_in_window <- data_test %>%
    group_by(ID) %>%
    mutate(min_share = pmin(tree_cov[year == yod], tree_cov[year == yod + 1])) %>%
    mutate(time_until_min = ifelse(min_share == tree_cov[year == yod], 0, 1)) %>%
    slice(which.min(min_share)) %>%
    ungroup()
  
  
  # Create the min_year column
  min_share_in_window <- min_share_in_window %>%
    mutate(min_year = yod + time_until_min)
  
  # Merge the new columns into the original dataframe using the ID as the key
  data <- left_join(data, min_share_in_window %>% select(ID, min_year, time_until_min), by = "ID")
  
  # Calculate the min_tree_cov column
  data <- data %>%
    group_by(ID) %>%
    mutate(min_tree_cov = tree_cov[year == min_year]) %>%
    ungroup()
  
  return(data)
}

# Call the function and store the result in a new data frame
t_min <- time_to_min(data_test)


### looks good!
png("/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/time_until_min.png", units="in", width=4, height=3, res=300)
ggplot(t_min, aes(x = time_until_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = c(0,1)) +
  labs(title = "Time_to_min", x = "years after disturbance", y = "Frequency")
dev.off()


# plot density
png("/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_tree_cover.png", units="in", width=5, height=2.5, res=300)
ggplot(t_min, aes(x=min_tree_cov, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 60))
dev.off()



#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------

fit_gam_before <- function(data) {
  prediction_data_list <- list()  # List to store prediction data for each ID
  
  # Loop through unique IDs = time series
  unique_ids <- unique(data$ID)
  for (id in unique_ids) {
    id_data <- subset(data, ID == id)  # Subset data for the current ID
    
    if (nrow(id_data) >= 3) {
      # Sufficient years after disturbance -> fit GAM model with spline complexity = 5
      # high spline complexity = stronger smoothing
      
      # Filter data for the specified time range
      filtered_data <- id_data[id_data$year < unique(id_data$min_year) & id_data$year >= 1990, ]
      
      # Check if there are enough data points for GAM
      if (nrow(filtered_data) >= 3) {
        gam_model <- gam(tree_cov ~ s(year, k = 3), data = filtered_data)
        
        # Create a prediction dataframe for the specified time range
        id_prediction_data <- data.frame(year = seq(unique(filtered_data$min_year), 1990))
        
        # Predict smoothed time series using GAM model
        id_prediction_data$GAM <- predict(gam_model, newdata = id_prediction_data)
        
        # Add the ID column
        id_prediction_data$ID <- id
        
        # Store prediction data in the list
        prediction_data_list[[id]] <- id_prediction_data
      }
    } else {
      # otherwise use a linear model
      lm_model <- lm(tree_cov ~ year, data = id_data)
      
      # Create a prediction dataframe for the specified time range
      id_prediction_data <- data.frame(year = seq(unique(id_data$min_year), 1990))
      
      # Predict smoothed time series using linear model
      id_prediction_data$GAM <- predict(lm_model, newdata = id_prediction_data)
      
      # Add the ID column
      id_prediction_data$ID <- id
      
      # Store prediction data in the list
      prediction_data_list[[id]] <- id_prediction_data
    }
  }
  
  # Combine prediction data from all IDs into a single data frame
  prediction_data <- do.call(rbind, prediction_data_list)
  
  return(prediction_data)
}

GAM_fitted_before <- fit_gam_before(GAM_fitted)


GAM_fitted_before$GAM_before <- GAM_fitted_before$GAM
GAM_fitted_before$GAM <- NULL

# merge the GAM_fitted df with t_min based on ID and year
GAM_fitted_before_2408 <- merge(GAM_fitted, GAM_fitted_before, by = c("ID", "year"), all.x = TRUE)

GAM_fitted_before_2408$tree_share_before <- NULL
GAM_fitted_before_2408$absolute_severity <- NULL
GAM_fitted_before_2408$relative_severity <- NULL
GAM_fitted_before_2408$severity_class <- NULL
GAM_fitted_before_2408$lm <- NULL
GAM_fitted_before_2408$ID_old <- NULL


GAM_fitted_before_2408$GAM_before <- ifelse(!is.na(GAM_fitted_before_2408$GAM_before), 
                                            GAM_fitted_before_2408$GAM_before, GAM_fitted_before_2408$GAM)

GAM_fitted_before_2408$GAM_before1 <- NULL

names(GAM_fitted_before_2408)[names(GAM_fitted_before_2408) == "GAM_before"] <- "GAM_all"


# Set all values less than 0 in the "share" variable to 0
GAM_fitted_before_2408 <- GAM_fitted_before_2408 %>%
  mutate(GAM_all = ifelse(GAM_all < 0, 0, GAM_all))

GAM_fitted_before_2408 <- GAM_fitted_before_2408 %>%
  mutate(GAM_all = ifelse(GAM_all > 100, 100, GAM_all))

# Set all values less than 0 in the "share" variable to 0
GAM_fitted_before_2408 <- GAM_fitted_before_2408 %>%
  mutate(GAM = ifelse(GAM < 0, 0, GAM))

GAM_fitted_before_2408 <- GAM_fitted_before_2408 %>%
  mutate(GAM = ifelse(GAM > 100, 100, GAM))



# Select a random ID that satisfies the conditions
selected_id <- sample(unique(recovery$ID[recovery$core_pixel == 'core pixel' & recovery$recovery_rate == 2]), 1)

# Create a new dataframe containing observations with the selected ID and recovery_rate == 2
trajectory <- subset(recovery, ID == selected_id & recovery_rate == 2)


#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------
fit_gam <- function(data) {
  prediction_data_list <- list()  # List to store prediction data for each ID
  
  # Loop through unique IDs = time series
  unique_ids <- unique(data$ID)
  for (id in unique_ids) {
    id_data <- subset(data, ID == id)  # Subset data for the current ID
    
    if (nrow(id_data) >= 3) {
      # Sufficient years after disturbance -> fit GAM model with spline complexity = 5
      # high spline complexity = stronger smoothing
      
      # Filter data for the specified time range
      filtered_data <- id_data[id_data$year >= unique(id_data$min_year) & id_data$year <= 2021, ]
      
      # Check if there are enough data points for GAM
      if (nrow(filtered_data) >= 3) {
        gam_model <- gam(tree_cov ~ s(year, k = 3), data = filtered_data)
        
        # Create a prediction dataframe for the specified time range
        id_prediction_data <- data.frame(year = seq(unique(filtered_data$min_year), 2021))
        
        # Predict smoothed time series using GAM model
        id_prediction_data$GAM <- predict(gam_model, newdata = id_prediction_data)
        
        # Add the ID column
        id_prediction_data$ID <- id
        
        # Store prediction data in the list
        prediction_data_list[[id]] <- id_prediction_data
      }
    } else {
      # otherwise use a linear model
      lm_model <- lm(tree_cov ~ year, data = id_data)
      
      # Create a prediction dataframe for the specified time range
      id_prediction_data <- data.frame(year = seq(unique(id_data$min_year), 2021))
      
      # Predict smoothed time series using linear model
      id_prediction_data$GAM <- predict(lm_model, newdata = id_prediction_data)
      
      # Add the ID column
      id_prediction_data$ID <- id
      
      # Store prediction data in the list
      prediction_data_list[[id]] <- id_prediction_data
    }
  }
  
  # Combine prediction data from all IDs into a single data frame
  prediction_data <- do.call(rbind, prediction_data_list)
  
  return(prediction_data)
}

GAM_fitted <- fit_gam(t_min)


# merge the GAM_fitted df with t_min based on ID and year
GAM_fitted_1 <- merge(t_min, GAM_fitted, by = c("ID", "year"), all.x = TRUE)

# Create the dmoothed_share_GAM column with smoothed values where available, 
# and original values otherwise
GAM_fitted_1$GAM <- ifelse(!is.na(GAM_fitted_1$GAM), 
                           GAM_fitted_1$GAM, GAM_fitted_1$tree_cov)

GAM_fitted <- GAM_fitted_1



#-------------------------------------------------------------------------------
# severity
#-------------------------------------------------------------------------------

calculate_severity <- function(x) {
  
  #Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
  avg_tree_share_5y_before <- GAM_fitted_before_2408 %>%
    group_by(ID) %>%
    arrange(year) %>%
    mutate(baseline_severity = mean(GAM_all[year >= (yod - 5) & year < yod],
                                    na.rm = TRUE))
  
  # Merge the GAM_lm_fitted dataframe with the average tree share in a 5-year 
  # time window before the disturbance df
  result_with_avg <- left_join(GAM_fitted_before_2408, avg_tree_share_5y_before, 
                               by = c("ID", "year"))
  
  result_with_avg <- result_with_avg[, -c(15:26)]
  
  names(result_with_avg)[names(result_with_avg) == "x.x"] <- "x"
  names(result_with_avg)[names(result_with_avg) == "y.x"] <- "y"
  names(result_with_avg)[names(result_with_avg) == "share.x"] <- "share"
  names(result_with_avg)[names(result_with_avg) == "class.x"] <- "class"
  names(result_with_avg)[names(result_with_avg) == "yod.x"] <- "yod"
  names(result_with_avg)[names(result_with_avg) == "edge.x"] <- "edge"
  names(result_with_avg)[names(result_with_avg) == "ID.x"] <- "ID_new"
  names(result_with_avg)[names(result_with_avg) == "ID_old.x"] <- "ID_old"
  names(result_with_avg)[names(result_with_avg) == "time_until_min.x"] <- "time_to_min"
  names(result_with_avg)[names(result_with_avg) == "min_year.x"] <- "min_year"
  names(result_with_avg)[names(result_with_avg) == "min_tree_cov.x"] <- "min_tree_cover"
  names(result_with_avg)[names(result_with_avg) == "core_pixel.x"] <- "core_pixel"
  names(result_with_avg)[names(result_with_avg) == "cover_smooth_before.x"] <- "cover_smooth_before"
  names(result_with_avg)[names(result_with_avg) == "tree_share_before.x"] <- "tree_share_before"
  names(result_with_avg)[names(result_with_avg) == "tree_cov.x"] <- "tree_cov"
  names(result_with_avg)[names(result_with_avg) == "GAM.x"] <- "GAM"
  names(result_with_avg)[names(result_with_avg) == "y.x"] <- "y"
  names(result_with_avg)[names(result_with_avg) == "GAM_before.y"] <- "GAM_before"
  names(result_with_avg)[names(result_with_avg) == "GAM_all.x"] <- "GAM_all"
  
  
  # Calculate the severity of the disturbance as absolute numbers from the average tree share before
  # here absolute and relative severity
  
  severity <- result_with_avg %>%
    mutate(absolute_severity = baseline_severity - min_tree_cover,
           relative_severity = 100-((min_tree_cover*100) / baseline_severity)) %>%
    select(ID, year, x, y, yod, share, tree_cov, core_pixel, time_to_min, 
           min_year, min_tree_cover, GAM_all, GAM,  baseline_severity, absolute_severity, relative_severity)
  
  
  
  return(severity)
}

# Call the function and store the result in a new data frame
severity <- calculate_severity(GAM_lm_fitted_before_2208)



# Set all values less than 0 in the "share" variable to 0
severity <- severity %>%
  mutate(relative_severity = ifelse(relative_severity < 0, NA, relative_severity))

severity <- severity %>%
  mutate(relative_severity = ifelse(relative_severity > 100, 100, relative_severity))



# reclassify severity
severity <- severity %>%
  mutate(severity_class = case_when(
    relative_severity <= 50 ~ "low severity",
    relative_severity > 50 & relative_severity <= 90 ~ "high severity",
    relative_severity > 90 ~ "very high severity"
    
  ))

# plot
ggplot(severity, aes(x=relative_severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), 
                     limits = c(0, 100))

ggplot(severity, aes(x=absolute_severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), 
                     limits = c(0, 100))


#-------------------------------------------------------------------------------
# exclude yod <= 1995 (to have enough pre-disturbance data points)
#-------------------------------------------------------------------------------
recovery_filtered <- recovery %>%
  group_by(ID) %>%
  filter(all(yod >= 1995)) %>%
  ungroup()


#-------------------------------------------------------------------------------
# plot recoverya intervals as a function of core/edge pixel
#-------------------------------------------------------------------------------

# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)


#-------------------------------------------------------------------------------
# baseline-normalized recovery
#-------------------------------------------------------------------------------
recovery <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(baseline_recovery_bn = quantile(GAM_all[year >= (yod - 5) & year < yod], probs = 0.98, na.rm = TRUE))


# Step 2: Compute 80% of the tree_share_before for each time series
recovery <- recovery %>% #severity
  mutate(tree_share_80 = baseline_recovery_bn * 0.8)


# Step 3: Find the year when share exceeds tree_share_80 after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    year_recov_baseline = case_when(
      any(GAM_all > tree_share_80 & year > yod+5) ~ min(year[GAM_all > tree_share_80 & year > yod]),
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup()

# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_bn = if_else(!is.na(year_recov_baseline), year_recov_baseline - yod, NA)
  )


# Set all values less than 0 in the "recovery_rate" variable to 0
recovery <- recovery %>%
  mutate(recovery_bn = ifelse(recovery_bn <= 0, NA, recovery_bn))

recovery <- recovery %>%
  mutate(recovery_bn = ifelse(recovery_bn > 32, NA, recovery_bn))


### give me the % of NAs in column recovery_rate
percentage_na <- recovery %>%
  summarise(percentage_na = mean(is.na(recovery_bn)) * 100)

cat(percentage_na$percentage_na, "%\n")


#-------------------------------------------------------------------------------

# for which yod there are most NAs?
mean_year_na_recovery <- recovery %>%
  filter(is.na(recovery_bn)) %>%
  summarize(mean_year_of_disturbance = mean(yod, na.rm = TRUE))

print(mean_year_na_recovery)


# Subset the DataFrame to include only rows where recovery_rate is NA
na_yod <- recovery$yod[is.na(recovery$recovery_rate)]

# Create a histogram of the NA years of disturbance (yod)
ggplot(data = data.frame(yod = na_yod), aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of NAs as a function of yod",
       x = "Year of Disturbance",
       y = "Count") +
  scale_x_continuous(breaks = seq(1990, 2021, by = 2), limits = c(1990, 2021)) 



#-------------------------------------------------------------------------------
# plot recoverya intervals as a function of core/edge pixel
#-------------------------------------------------------------------------------
recovery %>%
  ggplot(aes(x=recovery_bn, fill=core_pixel)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(alpha=0.8) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")

# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

recovery_unique %>%
  ggplot(aes(x=recovery_bn, fill=core_pixel)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(alpha=0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


recovery_mean <- recovery %>%
  filter(recovery_bn <= 32) %>%
  group_by(core_pixel) %>%
  summarize(mean = mean(recovery_bn))


#-------------------------------------------------------------------------------
# plot recovery intervals as a function of severity
#-------------------------------------------------------------------------------

recovery$severity_class <- factor(recovery$severity_class, levels = c("low severity", "high severity", "very high severity"))
recovery_unique$severity_class <- factor(recovery_unique$severity_class, levels = c("low severity", "high severity", "very high severity"))


# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# plot
ggplot(recovery_unique[!is.na(recovery_unique$severity_class), ], aes(x = recovery_bn, fill = severity_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
                     limits = c(0,30)) +
  labs(fill="")


# plot
ggplot(recovery[!is.na(recovery$severity_class), ], aes(x = recovery_bn, fill = severity_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")



recovery_mean <- recovery %>%
  filter(recovery_bn <= 32) %>%
  group_by(severity_class) %>%
  summarize(mean = mean(recovery_bn))



#-------------------------------------------------------------------------------
# plot recovery as a function of tree cover
#-------------------------------------------------------------------------------
# reclassify severity
recovery_unique <- recovery_unique %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery <- recovery %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery_unique$coverage_class <- factor(recovery_unique$coverage_class, levels = c("very open forests", "open forests", "closed forests"))
recovery$coverage_class <- factor(recovery$coverage_class, levels = c("very open forests", "open forests", "closed forests"))


# plot
ggplot(recovery_unique[!is.na(recovery_unique$coverage_class), ], aes(x = recovery_bn, fill = coverage_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 26, by = 2), limits = c(0, 26)) +
  labs(fill="")

# plot
ggplot(recovery[!is.na(recovery$coverage_class), ], aes(x = recovery_bn, fill = coverage_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


# what's the mean recover rate by severity?
recovery_mean <- recovery %>%
  filter(recovery_bn <= 32) %>%
  group_by(core_pixel) %>%
  summarize(mean = mean(recovery_bn))



#-------------------------------------------------------------------------------
# how many pixels reach the 80% baseline within 10 years?
recovery_10y <- recovery %>%
  filter(GAM_all >= tree_share_80, year <= yod + 10, year >= yod)

# Step 2: Group and count
result <- recovery_10y %>%
  group_by(core_pixel) %>%
  summarise(num_observations = n())


result <- recovery_10y %>%
  group_by(severity_class) %>%
  summarise(num_observations = n())


# View the result
print(result)


### How many pixel do not recover within 10y?
# Step 1: Filter the data
recovery_10y <- recovery %>%
  filter(GAM_all >= tree_share_80, year <= yod + 10, year >= yod)

# Step 2: Group and count NAs
result <- recovery_10y %>%
  group_by(coverage_class) %>%
  summarise(num_nas = sum(is.na(recovery_bn)))

print(result)
#-------------------------------------------------------------------------------


# how many edge pixels have high severity?
edge_medium_count <- recovery %>%
  filter(core_pixel == "core pixel", severity_class == "low severity") %>%
  nrow()

# Step 3: Print the count
print(edge_medium_count)


# proportino of all disturbances per severity categroy
# Step 1: Compute the proportion of observations for each severity category
proportions <- recovery %>%
  group_by(severity_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))

proportions <- recovery %>%
  group_by(coverage_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))

proportions <- recovery %>%
  group_by(core_pixel) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))



#-------------------------------------------------------------------------------
# absolute recovery
#-------------------------------------------------------------------------------

# Find the year when 40% tree cover is reached after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    year_recov_absolute = case_when(
      any(GAM > 50 & year > yod) ~ min(year[GAM > 50 & year > yod]),
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup()

# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_abs = if_else(!is.na(year_recov_absolute), year_recov_absolute - yod, NA)
  )



# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)


#-------------------------------------------------------------------------------
# plot recoverya intervals as a function of core/edge pixel
#-------------------------------------------------------------------------------
recovery %>%
  ggplot(aes(x=recovery_abs, fill=core_pixel)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(alpha=0.8) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 20)) +
  labs(fill="")


recovery_unique %>%
  ggplot(aes(x=recovery_abs, fill=core_pixel)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(alpha=0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


recovery_mean <- recovery %>%
  filter(recovery_abs <= 32) %>%
  group_by(core_pixel) %>%
  summarize(mean = mean(recovery_abs))


#-------------------------------------------------------------------------------
# plot recovery intervals as a function of severity
#-------------------------------------------------------------------------------

# reclassify severity
recovery_unique <- recovery_unique %>%
  mutate(severity_class = case_when(
    relative_severity <= 50 ~ "low severity",
    relative_severity > 50 & relative_severity <= 90 ~ "high severity",
    relative_severity > 90 ~ "very high severity"
    
  ))



recovery$severity_class <- factor(recovery$severity_class, levels = c("low severity", "high severity", "very high severity"))
recovery_unique$severity_class <- factor(recovery_unique$severity_class, levels = c("low severity", "high severity", "very high severity"))


# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# plot
ggplot(recovery_unique[!is.na(recovery_unique$severity_class), ], aes(x = recovery_abs, fill = severity_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
                     limits = c(0,30)) +
  labs(fill="")


# plot
ggplot(recovery[!is.na(recovery$severity_class), ], aes(x = recovery_abs, fill = severity_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")



recovery_mean <- recovery %>%
  filter(recovery_abs <= 32) %>%
  group_by(severity_class) %>%
  summarize(mean = mean(recovery_abs))



#-------------------------------------------------------------------------------
# plot recovery as a function of tree cover
#-------------------------------------------------------------------------------
# reclassify severity
recovery_unique <- recovery_unique %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery <- recovery %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery_unique$coverage_class <- factor(recovery_unique$coverage_class, levels = c("very open forests", "open forests", "closed forests"))
recovery$coverage_class <- factor(recovery$coverage_class, levels = c("very open forests", "open forests", "closed forests"))


# plot
ggplot(recovery_unique[!is.na(recovery_unique$coverage_class), ], aes(x = recovery_abs, fill = coverage_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 26, by = 2), limits = c(0, 26)) +
  labs(fill="")

# plot
ggplot(recovery[!is.na(recovery$coverage_class), ], aes(x = recovery_abs, fill = coverage_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


# what's the mean recover rate by severity?
recovery_mean <- recovery %>%
  filter(recovery_abs <= 32) %>%
  group_by(coverage_class) %>%
  summarize(mean = mean(recovery_abs))


# how many pixels reach the 80% baseline within 10 years?
recovery_10y <- recovery %>%
  filter(GAM_all >= 40, year <= yod + 10, year >= yod)

# Step 2: Group and count
result <- recovery_10y %>%
  group_by(core_pixel) %>%
  summarise(num_observations = n())


result <- recovery_10y %>%
  group_by(coverage_class) %>%
  summarise(num_observations = n())


# View the result
print(result)


# how many edge pixels have high severity?
edge_medium_count <- recovery %>%
  filter(core_pixel == "core pixel", severity_class == "very high severity") %>%
  nrow()

# Step 3: Print the count
print(edge_medium_count)



#-------------------------------------------------------------------------------
# impact-normalized recovery
#-------------------------------------------------------------------------------
recovery$relative_severity_1 <- recovery$relative_severity/100

# baseline impact == baseline_baseline_norm
recovery <- recovery %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(baseline_impact = quantile(GAM_all[year >= (yod - 5) & year < yod], probs = 0.99, na.rm = TRUE))


# Step 2: Compute 80% of the tree_share_before for each time series
recovery <- recovery %>% #severity
  mutate(tree_share_impact = baseline_impact * absolute_severity_1)


# Step 3: Find the year when share exceeds tree_share_80 after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    year_recov_in = case_when(
      any(GAM_all > tree_share_impact & year > yod+5) ~ min(year[GAM_all > tree_share_impact & year > yod]),
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup()

# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_in = if_else(!is.na(year_recov_in), year_recov_in - yod, NA)
  )


# Set all values less than 0 in the "recovery_rate" variable to 0
recovery <- recovery %>%
  mutate(recovery_in = ifelse(recovery_in <= 0, NA, recovery_in))

recovery <- recovery %>%
  mutate(recovery_in = ifelse(recovery_in > 32, NA, recovery_in))


### give me the % of NAs in column recovery_rate
percentage_na <- recovery %>%
  summarise(percentage_na = mean(is.na(recovery_in)) * 100)

cat(percentage_na$percentage_na, "%\n")


# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)



#-------------------------------------------------------------------------------
# plot recoverya intervals as a function of core/edge pixel
#-------------------------------------------------------------------------------
recovery %>%
  ggplot(aes(x=recovery_in, fill=core_pixel)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(alpha=0.8) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 20)) +
  labs(fill="")


recovery_unique %>%
  ggplot(aes(x=recovery_in, fill=core_pixel)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(alpha=0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  #scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), 
  #limits = c(0,30)) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


recovery_mean <- recovery %>%
  filter(recovery_in <= 32) %>%
  group_by(core_pixel) %>%
  summarize(mean = mean(recovery_in))


#-------------------------------------------------------------------------------
# plot recovery intervals as a function of severity
#-------------------------------------------------------------------------------

# reclassify severity
recovery_unique <- recovery_unique %>%
  mutate(severity_class = case_when(
    relative_severity <= 50 ~ "low severity",
    relative_severity > 50 & relative_severity <= 90 ~ "high severity",
    relative_severity > 90 ~ "very high severity"
    
  ))



recovery$severity_class <- factor(recovery$severity_class, levels = c("low severity", "high severity", "very high severity"))
recovery_unique$severity_class <- factor(recovery_unique$severity_class, levels = c("low severity", "high severity", "very high severity"))


# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# plot
ggplot(recovery_unique[!is.na(recovery_unique$severity_class), ], aes(x = recovery_in, fill = severity_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.5) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20), 
                     limits = c(0,20)) +
  labs(fill="")


# plot
ggplot(recovery[!is.na(recovery$severity_class), ], aes(x = recovery_in, fill = severity_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")



recovery_mean <- recovery %>%
  filter(recovery_in <= 32) %>%
  group_by(severity_class) %>%
  summarize(mean = mean(recovery_in))



#-------------------------------------------------------------------------------
# plot recovery as a function of tree cover
#-------------------------------------------------------------------------------
# reclassify severity
recovery_unique <- recovery_unique %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery <- recovery %>%
  mutate(coverage_class = case_when(
    baseline_recovery_bn <= 20 ~ "very open forests",
    baseline_recovery_bn > 20 & baseline_recovery_bn <= 40 ~ "open forests",
    #baseline > 40 & baseline <= 60 ~ "closed forests",
    baseline_recovery_bn > 40  ~ "closed forests"
    
  ))

recovery_unique$coverage_class <- factor(recovery_unique$coverage_class, levels = c("very open forests", "open forests", "closed forests"))
recovery$coverage_class <- factor(recovery$coverage_class, levels = c("very open forests", "open forests", "closed forests"))


# plot
ggplot(recovery_unique[!is.na(recovery_unique$coverage_class), ], aes(x = recovery_in, fill = coverage_class)) +
  #geom_histogram(alpha=0.8, position = "dodge") +
  geom_density(na.rm = TRUE, alpha = 0.3) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 26, by = 2), limits = c(0, 26)) +
  labs(fill="")

# plot
ggplot(recovery[!is.na(recovery$coverage_class), ], aes(x = recovery_in, fill = coverage_class)) +
  geom_histogram(alpha=0.8, position = "dodge") +
  #geom_density(na.rm = TRUE, alpha = 0.4) +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous(breaks = seq(0, 30, by = 2), limits = c(0, 30)) +
  labs(fill="")


# what's the mean recover rate by severity?
recovery_mean <- recovery %>%
  filter(recovery_in <= 32) %>%
  group_by(coverage_class) %>%
  summarize(mean = mean(recovery_in))




# how many pixels reach the 80% baseline within 10 years?
recovery_10y <- recovery %>%
  filter(GAM_all >= tree_share_impact, year <= yod + 10, year >= yod)

# Step 2: Group and count
result <- recovery_10y %>%
  group_by(coverage_class) %>%
  summarise(num_observations = n())


result <- recovery_10y %>%
  group_by(coverage_class) %>%
  summarise(num_observations = n())


# View the result
print(result)


#-------------------------------------------------------------------------------
# write recovery df
#-------------------------------------------------------------------------------

write.csv(recovery, "/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
# compute correlation
#-------------------------------------------------------------------------------
# baseline vs. absolute
corPlot(recovery[, c(21, 24)], scale = FALSE)

# baseline vs. impact
corPlot(recovery[, c(21, 30)], scale = FALSE)

# absolute vs. impact
corPlot(recovery[, c(24, 30)], scale = FALSE)
#-------------------------------------------------------------------------------
# random sampling for core pixels and recovery_bn ==2
#-------------------------------------------------------------------------------
# Select a random ID that satisfies the conditions
selected_id <- sample(unique(recovery$ID[recovery$core_pixel == 'core pixel' & recovery$recovery_bn == 2]), 1)

# Create a new dataframe containing observations with the selected ID and recovery_rate == 2
trajectory <- subset(recovery, ID == selected_id & recovery_bn == 2)



ggplot(data = trajectory) +
  #geom_point(aes(x = year, y = smoothed_share_GAM)) +
  geom_point(aes(x = year, y = GAM), color = "blue", size = 2) +
  #geom_point(aes(x = year, y = lm), color = "red", alpha=0.2) +
  geom_point(aes(x = year, y = tree_cov), color = "black", size =3, alpha=0.8) +
  #geom_smooth(data=trajectory %>% filter(year >=unique(trajectory$yod)),
  #aes(x = year, y=tree_cov),
  #method = "lm", se=FALSE, color = "green") +
  #geom_smooth(data=trajectory %>% filter(year >=unique(trajectory$yod)),
  #aes(x = year, y=tree_cov),
  #method = "gam", se=FALSE, formula=y~s(x, k=3)) +
  #geom_point(aes(x = year, y = lm), color = "blue") +
  labs(title = "",
       x = "Year",
       y = "Share") +
  geom_vline(aes(xintercept = yod), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = tree_share_80), color = "green") +
  annotate("text", x = min(trajectory$year), y = max(trajectory$GAM), 
           label = unique(trajectory$ID), color = "black", size = 3, vjust = 1,
           hjust = 0)