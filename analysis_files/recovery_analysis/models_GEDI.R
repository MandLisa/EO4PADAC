library(ggplot2)
library(dplyr)
library(readr)
library(raster)
library(lme4)
library(scales)
library(effects)
library(broom)
library(broom.mixed)
library(metafor)
library(emmeans)
library(patchwork)
library(tidyverse)
library(purrr)


GEDI_recov_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")

# thats my df for all the models
recovery_filt_lm_2013 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_stats.csv")

#-------------------------------------------------------------------------------
### prepare a df with GEDI data
# Load your raster file
dist <- raster("~/eo_nas/EO4Alps/dist_data/dist_crop.tif")

# Total number of pixels
total_pixels <- ncell(dist)

# Convert raster to data frame (only 50% of the data)
# Specify `na.rm = TRUE` to remove NAs if you want only valid values
sampled_df <- as.data.frame(dist, xy = TRUE, na.rm = TRUE) %>% 
  dplyr::sample_frac(0.1)

# rename df
colnames(sampled_df) <- c("yod", "x", "y")

# Schritt 2: ID-Spalte hinzufügen, falls noch nicht geschehen
sampled_df$ID <- 1:nrow(sampled_df)

# Assuming your data frame is named sampled_df_expanded
write.csv(sampled_df, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_template.csv", row.names = FALSE)




#-------------------------------------------------------------------------------

### Prepare data

# Filter data to observations where I do have a regrown value ( year > yod )
recovery_filt <- GEDI_recov_all[!is.na(GEDI_recov_all$regrown_percent), ]

recovery_filt_10 <- recovery_filt %>%
  filter(ysd==10)

recovery_filt_2013 <- recovery_filt %>%
  filter(yod<=2013)

# Remove rows where geoloc column has NA values
recovery_filt_10 <- recovery_filt_10 %>%
  filter(!is.na(geoloc))

recovery_filt_2013 <- recovery_filt_2013 %>%
  filter(!is.na(geoloc))

recovery_filt_10_unique <- recovery_filt_10 %>%
  distinct(ID, .keep_all = TRUE)

#recovery_filt_2013_unique <- recovery_filt_2013 %>%
  #distinct(ID, .keep_all = TRUE)

filtered_data_2013 <- recovery_filt_2013 %>%
  group_by(ID) %>%
  filter(year >= yod) %>%
  ungroup()



# Seed for reproducibility
set.seed(123)

# Sample 10% of unique IDs
sampled_ids <- filtered_data_2013 %>%
  distinct(ID) %>%           
  sample_frac(0.1) %>%       
  pull(ID)                 


sampled_ids <- recovery_filt_10_unique %>%
  distinct(ID) %>%           
  sample_frac(0.2) %>%       
  pull(ID)  


# Filter the original data frame to keep only the rows with the sampled IDs
recovery_subset_2013 <- filtered_data_2013  %>%
  filter(ID %in% sampled_ids)


recovery_subset_2013_unique <- recovery_subset_2013 %>%
  distinct(ID, .keep_all = TRUE)


recovery_filt_10_unique_subset <- recovery_filt_10_unique  %>%
  filter(ID %in% sampled_ids)


#recovery_subset_2013 <- recovery_filt_2013  %>%
  #filter(ID %in% sampled_ids)






#-------------------------------------------------------------------------------

### data frames: 
# recovery_filt_10_unique_subset 
# --> this df contains all observations for ysd == 10 and without time series 
# information (just one observation per ID) and just 20% of data

# recovery_subset_2013
# --> this df contains time series data for year <= 2013 and only 20% of all
# observations

### plot
# define order of geolocation
recovery_subset_2013$geoloc <- factor(recovery_subset_2013$geoloc, 
                                     levels = c("eastern alps - north", "eastern alps - central", 
                                                "eastern alps - south", "western alps - north", 
                                                "western alps - south"))

recovery_subset_2013_unique$geoloc <- factor(recovery_subset_2013_unique$geoloc, 
                                      levels = c("eastern alps - north", "eastern alps - central", 
                                                 "eastern alps - south", "western alps - north", 
                                                 "western alps - south"))




# plot regrow_precent as a function of VPD anomalies in yod per yod
# does this make sense?!
ggplot(recovery_subset_2013_unique) +
  geom_point(aes(x=VPD_yod,
                 y=regrown_percent,
                 colour=as.factor(geoloc)),
             alpha=0.3) +
  geom_smooth(aes(x=VPD_yod,
                  y=regrown_percent,
                  color=as.factor(geoloc)),
              method=lm,
              se=FALSE) +
  ylim(0,100) +
  #facet_wrap(~geoloc)
  facet_wrap(~yod)


# with facet_grid for yod and geolocation
p <- ggplot(recovery_subset_2013_unique) +
  #geom_point(aes(x = VPD_yod,
                 #y = regrown_percent,
                 #colour = as.factor(geoloc)),
             #alpha = 0.1) +
  geom_smooth(aes(x = VPD_yod,
                  y = regrown_percent,
                  color = as.factor(geoloc)),
              method = lm,
              se = FALSE) +
  geom_vline(xintercept = 0,  color = "black") + 
  scale_x_continuous(breaks = seq(-2, 3, by = 1)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 50)) +
  theme(legend.position = "none") +
  facet_grid(yod ~ geoloc)
       
# Save the plot with ggsave
ggsave("~/eo_nas/EO4Alps/figs/VPD_per_yod.png", plot = p, width = 12, height = 10, dpi = 300)



# same but for single years
ggplot(recovery_filt_unique %>% 
        filter(year <= 2013)) +
  geom_point(aes(x=VPD_yod,
                 y=regrown_percent,
                 colour=as.factor(geoloc)),
             alpha=0.3) +
  geom_smooth(aes(x=VPD_yod,
                  y=regrown_percent,
                  color=as.factor(geoloc)),
              method=lm,
              se=FALSE) +
  ylim(0, 100) +
  facet_wrap(~year)



#-----------------------------------------------------------------------------
ggplot(recovery_0711 %>%
         filter(geoloc=="Southern West Alps")) +
  geom_point(aes(x=mean_VPD_yod, y=regrown_percent, colour=as.factor(geoloc)),
             alpha=0.3) +
  geom_smooth(aes(x=mean_VPD_yod, y=regrown_percent, color=as.factor(geoloc)),
              method=lm, se=FALSE) +
  ylim(0,100) +
  facet_wrap(~yod)

#-------------------------------------------------------------------------------

# Filter data to remove rows with NA values and ensure one observation per ID
filtered_data <- GEDI_recov_all %>%
  filter(
    geoloc == "western alps - south",     
    !is.na(regrown_percent),             
    !is.na(VPD_yod),                      
    !is.na(dem),
    !is.na(slope),
    !is.na(severity_relative)
  ) %>%
  distinct(ID, .keep_all = TRUE)

filtered_data <- GEDI_recov_all %>%
  filter(
    geoloc == "western alps - south")     




fit <- lmer(
  regrown_percent ~ VPD_yod +
    dem +
    slope +
    #aspect +
    severity_relative +
    (1 + VPD_yod | year),
  data = filtered_data %>%
    filter(geoloc=="western alps - south"))

summary(fit)


#-------------------------------------------------------------------------------

### fit linear regression model for response variable regrown_percent and based
### on what Cornelis suggested
### (alternatively for recovery_interval)

### first filter observationns where severity == NA
recovery_filt_lm_2013 <- recovery_subset_2013 %>%
  filter(
    !is.na(regrown_percent),
    !is.na(VPD_yod),
    !is.na(dem),
    !is.na(slope),
    !is.na(severity_relative)
  )

# Cap values in regrown_percent at 100
recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  mutate(regrown_percent = ifelse(regrown_percent > 100, 100, regrown_percent))

# Check for Inf or NaN values in regrown_percent
any(is.infinite(recovery_filt_lm_2013$regrown_percent))
any(is.nan(recovery_filt_lm_2013$regrown_percent))

# Remove Inf values from regrown_percent
recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  filter(!is.infinite(regrown_percent))

#-------------------------------------------------------------------------------

# Group data by geoloc and fit a mixed-effects model for each group
# here, I fit models separately for each geolocation, estimating the wffects of
# VPD_yod, dem, slope and severity independently for each geolocation
# this gives me for each geolocation a own set of coefficients, allowing to see
# how predictors affect regrown_percent in each geolocation individually
# Disadvantage: I do not get a statistical test for whether these effects are
# significantly different across locations

# consider scaling predictors
# Standardize continuous predictors
recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  mutate(
    VPD_yod1_z = scale(VPD_yod1, center = TRUE, scale = TRUE),
    VPD_yod2_z = scale(VPD_yod2, center = TRUE, scale = TRUE),
    VPD_yod3_z = scale(VPD_yod3, center = TRUE, scale = TRUE))


# fit model for each geolocation
# BUT: does this makes sense, especially (1+VPD_yod | year), when my df is restircited
# to ysd == 10
# if I do it like that, I guess I need to put the df with the entire time series
# into the model?!
# For this model, I need time series data!
models_timeseries <- recovery_filt_lm_2013 %>%
  group_by(geoloc) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lmer(
      regrown_percent ~ VPD_yod_z + dem_z + slope_z + severity_relative_z +
        (1 + VPD_yod | year),
      data = .
    ))
  )

# extract coefficients
models_with_coefs <- models_timeseries %>%
  mutate(
    coefs = map(model, ~ tidy(.x, effects = "fixed"))
  ) %>%
  unnest(coefs)


# plot coefficients
ggplot(models_with_coefs %>% filter(term == "VPD_yod_z"), aes(y = geoloc, x = estimate, color=as.factor(geoloc))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  labs(y = "Geoloc", x = "Estimate (Effect of VPD anomalies @ yod)") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))


# plot multiple predictors
ggplot(models_with_coefs, aes(y = term, x = estimate, color = as.factor(geoloc))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2, position = position_dodge(width = 0.5)) +
  labs(y = "Predictors", x = "Estimate") +
  theme(legend.position = "right") +
  facet_wrap(~geoloc)


# with free x-axis
ggplot(models_with_coefs, aes(y = term, x = estimate, color = as.factor(geoloc))) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2, position = position_dodge(width = 0.5)) +
  labs(y = "Predictors", x = "Estimate") +
  theme(legend.position = "right") +
  facet_wrap(~ geoloc, scales = "free_x")



# Calculate average `VPD_yod_z` effect over years
vpd_effect <- recovery_filt_lm_2013 %>%
  group_by(geoloc, yod) %>%
  summarize(mean_VPD_yod_z = mean(VPD_yod_z, na.rm = TRUE))

ggplot(vpd_effect, aes(x = yod, y = mean_VPD_yod_z, color = geoloc)) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = geoloc), method = lm, se = FALSE, linetype = "dashed") +
  geom_smooth(aes(group = 1), method = lm, se = TRUE, color = "black", linetype = "dashed") +
  xlim(1986,2013) +
  labs(x = "YOD", y = "Mean Effect of VPD_yod_z", title = "Effect of VPD anomalies in the year of disturbance over time by geolocation") +
  theme(legend.position = "right")



# for year
vpd_effect <- recovery_filt_lm_2013 %>%
  group_by(geoloc, year) %>%
  summarize(mean_VPD_yod_z = mean(VPD_yod_z, na.rm = TRUE))

ggplot(vpd_effect, aes(x = year, y = mean_VPD_yod_z, color = geoloc)) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = geoloc), method = lm, se = FALSE, linetype = "dashed") +
  #geom_smooth(aes(group = 1), method = lm, se = TRUE, color = "black", linetype = "dashed") +
  xlim(1986,2013) +
  labs(x = "YOD", y = "Mean Effect of VPD_yod_z", title = "Effect of VPD anomalies in the year of disturbance over time by geolocation") +
  theme(legend.position = "right")




### Cross-location comparison with z-scaled predictors
# VPD_yod_z, dem_z,slope_z and severity_z are my standarized predictors, where I
# am interested in their effect on regrown_percent
# geolocation is treated as categorical variable, including it in the model allows
# each location potentially have a different baseline level for my response variable
# the interaction term VPD_yod*geoloc tells me whether the effect of VPD_yod on
# regrow_percent differs across geolocations
# (1|year) is my random intercept for year, meaning that each year can have a 
# different baseline level for regrown_percent. Including year as random effect
# accounts for potential year-to-year variations that are not explained by other
# predictors
# This random intercept assumes that my response variabel vary by year due to e.g. 
# climate conditions and allwos the model to account for this variability without
# modelling each year


# Fit linear-effect mocel with geoloc as a fixed effect and random slopes for 
# VPD_yod_z by year
crossloc_timeseries_model <- lmer(
  regrown_percent ~ VPD_yod_z * geoloc + year + dem_z + slope_z + severity_relative_z +
    (1 + VPD_yod_z | year) + (1 | geoloc),
  data = recovery_filt_lm_2013
)


# Set limit to half the number of observations
emm_options(pbkrtest.limit = 23726, lmerTest.limit = 23726)

# Check the range of VPD_yod_z in the dataset
range(recovery_filt_lm_2013$VPD_yod_z, na.rm = TRUE)

# calculates the estimated marginal means (EMMs) for the interaction between 
# VPD_yod_z and geoloc using the fitted model crossloc_timeseries_model
# marginal means for each combination of VPD_yod_z and geoloc
# allows to evaluate the effect of VPD_yod_z over the VPD anomalie range for each 
# geolocation 
emm <- emmeans(crossloc_timeseries_model, ~ VPD_yod_z * geoloc, 
               at = list(VPD_yod_z = seq(-2.658992, 4.130946, length.out = 100)))


# plot marginal means across geolocations over time
emm_plot <- emm %>%
  as.data.frame() %>%
  ggplot(aes(x = VPD_yod_z, y = emmean, color = geoloc)) +
  geom_line() +
  labs(x = "VPD anomalies", y = "Estimated recovery",
       title = "Effect of VPD anomalies on recovery by geolocation") 
  


#-------------------------------------------------------------------------------
### use binary variable recovered/not recovered within 10y as response variable

### single model for all geoloactions
# GLMM for binary response variable recovery_10y
# seems to be too complex?!
crossloc_timeseries_model_binary <- glmer(
  recovery_10y_num ~ VPD_yod_z * geoloc + year + dem_z + slope_z + severity_relative_z +
    (1 + VPD_yod_z | year) + (1 | geoloc),
  data = recovery_filt_lm_2013,
  family = binomial(link = "logit")
)

# simplyify random structure - still too compley?
crossloc_timeseries_model_binary <- glmer(
  recovery_10y_num ~ VPD_yod_z * geoloc + year + dem_z + slope_z + severity_relative_z +
    (1 | year) + (1 | geoloc),
  data = recovery_filt_lm_2013,
  family = binomial(link = "logit")
)



 # Extract fixed effect coefficients with confidence intervals
fixed_effects <- broom.mixed::tidy(crossloc_timeseries_model_binary, effects = "fixed", conf.int = TRUE)

# Plot the fixed effects
ggplot(fixed_effects, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(x = "Effect Size (Estimate)", y = "Predictors", 
       title = "Effect Sizes with Confidence Intervals") +
  theme_minimal()) 




### separate models for each geolocation
# Fit separate models for each location
models_timeseries_binary <- recovery_filt_lm_2013 %>%
  group_by(geoloc) %>%
  nest() %>%
  mutate(
    model = map(data, ~ glmer(
      recovery_10y ~ VPD_yod_z + dem_z + slope_z + severity_relative_z +
        (1 + VPD_yod_z | year),
      data = .,
      family = binomial(link = "logit")
    ))
  )


# Estimated marginal means for VPD_yod_z * geoloc interaction in the single model
emm <- emmeans(crossloc_timeseries_model_binary, ~ VPD_yod_z * geoloc, 
               at = list(VPD_yod_z = seq(-2.658992, 4.130946, length.out = 100)),
               type = "response")  # Type response gives predicted probabilities for binary models



emm %>%
  as.data.frame() %>%
  ggplot(aes(x = VPD_yod_z, y = prob, color = geoloc, fill = geoloc)) +  # `prob` gives probability of recovery
  geom_line() +
  geom_ribbon(aes(ymin = prob - SE, ymax = prob + SE), alpha = 0.2, linetype = 0) +
  labs(x = "VPD anomalies", y = "Probability of Recovery 10 Years Post-Disturbance",
       title = "Effect of VPD anomalies on Recovery Probability by Geolocation") +
  theme_minimal()








### fit one logistic regression model per geolocation
models <- recovery_filt_unique %>%
  group_by(geoloc) %>%
  nest() %>%
  mutate(model = map(data, ~ glm(recovery_10y_num ~ VPD_yod + dem + slope, 
                                 data = ., family = binomial)))



# summaries for each model
model_summaries <- models %>%
  mutate(summary = map(model, summary))

# Show summary for specific geoloc, index refers to speficic region
model_summaries$summary[[1]] 


# Extract coefficients
model_coefficients <- models %>%
  mutate(tidy_coefs = map(model, tidy)) %>%
  unnest(tidy_coefs)

model_coefficients


model_coefficients <- model_coefficients %>%
  select(geoloc, term, estimate, std.error)


# plot
ggplot(model_coefficients, aes(x = estimate, y = term, color = term)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ geoloc) +
  labs(x = "Coefficient estimate", y = "Predictor") 

# plot, this is more clear I guess?!
ggplot(model_coefficients, aes(x = estimate, y = geoloc, color = term)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Coefficient estimate", y = "Region")




#-------------------------------------------------------------------------------




fit.test <- lm(
  recovery_rate ~ `VPD_summer_yod`
)





