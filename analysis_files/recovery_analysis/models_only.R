library(ggplot2)
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
library(scales)


# df with all observations and entire time series
GEDI_recov_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")

# thats my df for all the models
recovery_filt_lm_2013 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_stats.csv")


#-------------------------------------------------------------------------------
### Model 1: One separate model for each geolocation
# response variable: regrown_percent
# Fixed-effects: VPD_yod, dem, slope, relative severity (z-scaled)
# Random effect: (1 + VPD_yod | year) --> random intercept and random slope for 
# VPD_yod by year, allowing the effect of VPD_yod to vary across different years
# Model accounts for differences between locations by modeling each geoloc 
# independently --> This means each geoloc has its own set of coefficients for 
# the fixed effects
#-------------------------------------------------------------------------------

# fit model
models_geoloc <- recovery_filt_lm_2013 %>%
  group_by(geoloc) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lmer(
      regrown_percent ~ VPD_yod_z +
        dem_z +
        slope_z +
        severity_relative_z +
        aspect_cat +
        (1 + VPD_yod_z | year),
      data = .
    ))
  )


# extract coefficients
models_with_coefs <- models_geoloc %>%
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


### Calculate average VPD@yod effect over the years

# mean VPD @yod per geoloaction
vpd_effect <- recovery_filt_lm_2013 %>%
  group_by(geoloc, yod) %>%
  summarize(mean_VPD_yod_z = mean(VPD_yod_z, na.rm = TRUE))

# plot 
ggplot(vpd_effect, aes(x = yod, y = mean_VPD_yod_z, color = geoloc)) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = geoloc), method = lm, se = FALSE, linetype = "dashed") +
  geom_smooth(aes(group = 1), method = lm, se = TRUE, color = "black", linetype = "dashed") +
  xlim(1986,2013) +
  labs(x = "YOD", y = "Mean Effect of VPD_yod_z", title = "Effect of VPD anomalies in the year of disturbance over time by geolocation") +
  theme(legend.position = "right")


# for year instead of yod
vpd_effect <- recovery_filt_lm_2013 %>%
  group_by(geoloc, year) %>%
  summarize(mean_VPD_yod_z = mean(VPD_yod_z, na.rm = TRUE))

# plot
ggplot(vpd_effect, aes(x = year, y = mean_VPD_yod_z, color = geoloc)) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = geoloc), method = lm, se = FALSE, linetype = "dashed") +
  #geom_smooth(aes(group = 1), method = lm, se = TRUE, color = "black", linetype = "dashed") +
  xlim(1986,2013) +
  labs(x = "YOD", y = "Mean Effect of VPD_yod_z", title = "Effect of VPD anomalies in the year of disturbance over time by geolocation") +
  theme(legend.position = "right")



#-------------------------------------------------------------------------------
### Model 2: Single model that includes all geolocs in the same model
# response variable: regrown_percent
# Fixed-effects across all locations: dem, slope, relative severity (z-scaled)
# Interaction term: (VPD_yod_z * geoloc) --> allows the effect of VPD_yod to 
# vary by geoloc, capturing the interaction between VPD_yod and the geographic 
# location
# Random effects: (1 + VPD_yod_z | year) specifies a random intercept and slope
# for VPD_yod by year, while (1 | geoloc) adds a random intercept for each geoloc 
# --> this random intercept for geoloc accounts for general differences in 
# baseline regrowth across locations
#-------------------------------------------------------------------------------

# fit model
model_single <- lmer(
  regrown_percent ~ VPD_yod_z * geoloc + year + dem_z + slope_z + severity_relative_z +
    (1 + VPD_yod_z | year) + (1 | geoloc),
  data = recovery_filt_lm_2013
)

# Check the range of VPD_yod_z in the dataset
range(recovery_filt_lm_2013$VPD_yod_z, na.rm = TRUE)

# calculates the estimated marginal means (EMMs) for the interaction between 
# VPD_yod_z and geoloc using the fitted model crossloc_timeseries_model
# marginal means for each combination of VPD_yod_z and geoloc
# allows to evaluate the effect of VPD_yod_z over the VPD anomalie range for each 
# geolocation 
emm <- emmeans(model_single, ~ VPD_yod_z * geoloc, 
               at = list(VPD_yod_z = seq(-2.658992, 4.130946, length.out = 100)))


# plot marginal means across geolocations over time
emm %>%
  as.data.frame() %>%
  ggplot(aes(x = VPD_yod_z, y = emmean, color = geoloc)) +
  geom_line() +
  labs(x = "VPD anomalies", y = "Estimated recovery",
       title = "Effect of VPD anomalies on recovery by geolocation") 



#-------------------------------------------------------------------------------
### Model 3: Separate models for each geolocation, but for binary outcome 
# Response variable: recovery_10y
# Fixed-effects: VPD_yod, dem, slope, relative severity (z-scaled)
# Random effect: (1 + VPD_yod | year)  random intercept and random slope for 
# VPD_yod by year, allowing the effect of VPD_yod to vary across different years
# Model accounts for differences between locations by modeling each geoloc 
# independently --> This means each geoloc has its own set of coefficients for
# the fixed effects
#-------------------------------------------------------------------------------

# fit logistic regression model with VPD_yod and height, slope, aspect, severity
models_binary <- recovery_filt_lm_2013 %>%
  group_by(geoloc_reclass) %>%
  nest() %>%
  mutate(
    model = map(data, ~ glmer(
      recovery_10y_num ~ VPD_yod_z +
        dem_z +
        slope_z +
        severity_relative_z +
        aspect_cat +
        (1 + VPD_yod_z | year),
      data = .,
      family = binomial(link = "logit")
    ))
  )

# Extract coefficients for each model
model_coefficients <- models_binary %>%
  mutate(tidy_model = map(model, tidy, effects = "fixed", conf.int = TRUE)) %>%
  unnest(tidy_model)

### plot coefficient effects

# optional_ filter out intercept term, because it is super small
model_coefficients_filtered <- model_coefficients %>%
  filter(term != "(Intercept)")

# plot with free x-axis for each geolocation
ggplot(model_coefficients_filtered, aes(x = estimate, y = term, color = geoloc_reclass)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ geoloc_reclass, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Estimate", y = "Predictors") +
  theme(legend.position = "none")

# plot with fixed x-axis for each geolocation
ggplot(model_coefficients_filtered, aes(x = estimate, y = term, color = geoloc_reclass)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ geoloc_reclass, scales = "fixed") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Estimate", y = "Predictors") +
  theme(legend.position = "none")

#-------------------------------------------------------------------------------

# fit logistic regression model with VPD_yod, yod+1, yod+2 and yod+3 and height, 
# slope, aspect, severity
models_binary_yod3 <- recovery_filt_lm_2013 %>%
  group_by(geoloc_reclass) %>%
  nest() %>%
  mutate(
    model = map(data, ~ glmer(
      recovery_10y_num ~ VPD_yod_z +
        VPD_yod1_z +
        VPD_yod2_z +
        VPD_yod3_z +
        dem_z +
        slope_z +
        severity_relative_z +
        aspect_cat +
        (1 + VPD_yod | year),
      data = .,
      family = binomial(link = "logit")
    ))
  )

# Extract coefficients for each model
model_coefficients_yod3 <- models_binary_yod3 %>%
  mutate(tidy_model = map(model, tidy, effects = "fixed", conf.int = TRUE)) %>%
  unnest(tidy_model)

### plot coefficient effects

# optional_ filter out intercept term, because it is super small
model_coefficients_filtered_yod3 <- model_coefficients_yod3 %>%
  filter(term != "(Intercept)")

# plot with free x-axis for each geolocation
ggplot(model_coefficients_filtered_yod3, aes(x = estimate, y = term, color = geoloc_reclass)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ geoloc_reclass, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Estimate", y = "Predictors") +
  theme(legend.position = "none")

# plot with fixed x-axis for each geolocation
ggplot(model_coefficients_filtered_yod3, aes(x = estimate, y = term, color = geoloc_reclass)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ geoloc_reclass, scales = "fixed") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Estimate", y = "Predictors") +
  theme(legend.position = "none")

#-------------------------------------------------------------------------------

# Calculate the percentage of recovered observations within 10 years for each yod
recovery_summary <- recovery_filt_lm_2013 %>%
  group_by(yod) %>%                                     
  summarize(
    total_observations = n(),                            
    recovered_within_10_years = sum(recovery_10y_num),  
    recovery_percentage = mean(recovery_10y_num) * 100   
  ) %>%
  ungroup()

#plot
ggplot(recovery_summary, aes(x = yod, y = recovery_percentage)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = TRUE) +
  theme(legend.position = "none") +
  xlim(1986, 2012)


# Calculate the percentage of recovered observations within 10 years for each 
# disturbance year and geolocation
recovery_summary_geoloc <- recovery_filt_lm_2013 %>%
  group_by(yod, geoloc) %>%                    
  summarize(
    total_observations = n(),                  
    recovered_within_10_years = sum(recovery_10y_num), 
    recovery_percentage = mean(recovery_10y_num) * 100 
  ) %>%
  ungroup()


ggplot(recovery_summary_geoloc, aes(x = yod, y = recovery_percentage, color = geoloc)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed", se = TRUE) +
  xlim(1986, 2012) +
  facet_wrap(~ geoloc) +
  theme(legend.position = "none")



#-------------------------------------------------------------------------------

### EDA

GEDI_all_unique <- GEDI_recov_all[!duplicated(GEDI_recov_all$ID), ]

GEDI_all_unique  <- GEDI_all_unique [!is.na(GEDI_all_unique $geolocation_reclass), ]




# Create a bar plot to show the count of observations per geolocation_reclass
ggplot(GEDI_all_unique, aes(x = geoloc)) +
  geom_bar() 

# prepare df
GEDI_all_unique <- GEDI_all_unique %>%
  mutate(
    height_cat = cut(
      dem,
      breaks = seq(0, 2400, by = 200),
      labels = as.character(seq(0, 2200, by = 200)),  # Labels as regular numbers
      include.lowest = TRUE
    ),
    slope_cat = cut(
      slope,
      breaks = seq(0, 90, by = 10),  # Adjust slope range and intervals as needed
      labels = as.character(seq(0, 80, by = 10)),
      include.lowest = TRUE
    ),
    aspect_cat = case_when(
      aspect >= 315 | aspect < 45 ~ "North",
      aspect >= 45 & aspect < 135 ~ "East",
      aspect >= 135 & aspect < 225 ~ "South",
      aspect >= 225 & aspect < 315 ~ "West"
    )
  )
# Plot distribution of height categories
GEDI_all_unique %>%
  filter(!is.na(height_cat)) %>%
  ggplot(aes(x = height_cat)) +
  geom_bar() 

# Plot distribution of slope categories
ggplot(GEDI_all_unique, aes(x = slope_cat)) +
  geom_bar() 

# Plot distribution of aspect categories
ggplot(GEDI_all_unique, aes(x = aspect_cat)) +
  geom_bar() 


#-------------------------------------------------------------------------------
