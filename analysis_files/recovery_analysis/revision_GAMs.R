library(mgcv)
library(ggplot2)
library(patchwork)  
library(gratia)    
library(readr)
library(sf)
library(dplyr)

# load df
hexagons_recov10_centros <- st_read("/mnt/eo/EO4Alps/00_analysis/_recovery/hexagons_recov10_centros.gpkg")


# =========================
# Data prep
# =========================
df <- hexagons_recov10_centros %>%
  mutate(
    geolocation = factor(geolocation),
    # Standardize your anomaly predictors
    temp_ano_sc = scale(mean_temp_ano_summer_yod1)[,1],
    vpd_ano_sc  = scale(mean_VPD_yod1)[,1]
  ) %>%
  drop_na(mean_percent_recovered, temp_ano_sc, vpd_ano_sc)

# =========================
# Common RHS block for all models
# =========================
rhs_common <- paste(
  "s(long, lat, bs='tp')",          # spatial smooth (optional but recommended)
  "s(mean_severity)",
  "s(mean_temp_total)",
  "s(mean_prec_total)",
  "s(mean_elevation)",
  "s(mean_pre_dist_tree_cover)",
  "s(mean_bare)",
  sep = " + "
)

# Helper to build full formulas
fml <- function(lhs_terms) as.formula(
  paste("mean_percent_recovered ~", lhs_terms, "+", rhs_common)
)

# =========================
# 0) Your baseline model (VPD by region, plus covariates)
# =========================
mod_base <- gam(
  as.formula(paste(
    "mean_percent_recovered ~",
    rhs_common, "+",
    "s(vpd_ano_sc, by = geolocation, k = 6)"
  )),
  data = df, method = "REML", select = TRUE
)

# =========================
# 1) Temperature-only (anomalies)
# =========================
mod_temp_only <- gam(
  fml("s(temp_ano_sc, k = 6)"),
  data = df, method = "REML", select = TRUE, gamma = 1.4
)

# =========================
# 2) VPD-only (anomalies)
# =========================
mod_vpd_only <- gam(
  fml("s(vpd_ano_sc, k = 6)"),
  data = df, method = "REML", select = TRUE, gamma = 1.4
)

# =========================
# 3) Joint (temperature + VPD)
# =========================
mod_joint <- gam(
  fml("s(temp_ano_sc, k = 6) + s(vpd_ano_sc, k = 6)"),
  data = df, method = "REML", select = TRUE, gamma = 1.4
)

# =========================
# 4) Residual-VPD (VPD beyond temperature)
# =========================
vpd_lm <- lm(vpd_ano_sc ~ temp_ano_sc, data = df)
df$vpd_res_sc <- scale(residuals(vpd_lm))[,1]

mod_residual_vpd <- gam(
  fml("s(temp_ano_sc, k = 6) + s(vpd_res_sc, k = 6)"),
  data = df, method = "REML", select = TRUE, gamma = 1.4
)

# =========================
# 5) Temperature effect by region (factor-specific smooths)
# =========================
mod_temp_by_geo <- gam(
  as.formula(paste(
    "mean_percent_recovered ~ geolocation +",     # include parametric factor
    rhs_common, "+",
    "s(temp_ano_sc, by = geolocation, k = 6)"     # region-specific temp curves
  )),
  data = df, method = "REML", select = TRUE
)

# =========================
# (Optional) Global Temp×VPD interaction (only if truly helpful)
# =========================
mod_interaction <- gam(
  fml("s(temp_ano_sc, k = 6) + s(vpd_ano_sc, k = 6) + ti(temp_ano_sc, vpd_ano_sc, k = c(6,6))"),
  data = df, method = "REML", select = TRUE, gamma = 1.4
)

# =========================
# Model comparison & diagnostics
# =========================
# AIC table
AIC(mod_temp_only, mod_vpd_only, mod_joint, mod_residual_vpd, mod_interaction, mod_base)

# Nested comparisons (REML-scale LRT)
anova(mod_temp_only, mod_joint, test = "Chisq")
anova(mod_joint, mod_interaction, test = "Chisq")

# Concurvity (functional collinearity) for joint model
concurvity(mod_joint, full = TRUE)

# Quick summaries
summary(mod_temp_only)
summary(mod_joint)
summary(mod_residual_vpd)
summary(mod_temp_by_geo)

# =========================
# (Optional) Plots — Temp-only vs Joint (response scale)
# =========================
seqr <- function(x, n = 200) seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = n)
base_means <- df %>% summarise(across(where(is.numeric), ~mean(.x, na.rm=TRUE)))

# Temperature curves
nd_temp_only <- base_means[rep(1, 200), ]
nd_temp_only$temp_ano_sc <- seqr(df$temp_ano_sc)

nd_temp_joint <- nd_temp_only
nd_temp_joint$vpd_ano_sc <- mean(df$vpd_ano_sc, na.rm=TRUE)

p1 <- predict(mod_temp_only, newdata = nd_temp_only, type = "response", se.fit = TRUE)
p2 <- predict(mod_joint,     newdata = nd_temp_joint, type = "response", se.fit = TRUE)

plot_temp_df <- bind_rows(
  tibble(x = nd_temp_only$temp_ano_sc,  fit = as.numeric(p1$fit), se = as.numeric(p1$se.fit), model = "Temp-only"),
  tibble(x = nd_temp_joint$temp_ano_sc, fit = as.numeric(p2$fit), se = as.numeric(p2$se.fit), model = "Joint (Temp+VPD)")
)

ggplot(plot_temp_df, aes(x, fit, color = model, fill = model)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se), alpha = 0.18, color = NA) +
  labs(x = "Temperature anomaly", y = "Predicted recovery (%)") +
  theme_minimal() + theme(legend.position = "top")

# =========================
# (Optional) VPD curve in joint model (response scale)
# =========================
nd_vpd <- base_means[rep(1, 200), ]
nd_vpd$vpd_ano_sc  <- seqr(df$vpd_ano_sc)
nd_vpd$temp_ano_sc <- mean(df$temp_ano_sc, na.rm=TRUE)

p_vpd <- predict(mod_joint, newdata = nd_vpd, type = "response", se.fit = TRUE)
plot_vpd_df <- tibble(x = nd_vpd$vpd_ano_sc, fit = as.numeric(p_vpd$fit), se = as.numeric(p_vpd$se.fit))

ggplot(plot_vpd_df, aes(x, fit)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se), alpha = 0.18) +
  labs(x = "standardized VPD anomaly", y = "predicted recovery (%)") +
  theme_minimal()

# =========================
# (Optional) Temperature effect by region (link scale)
# =========================
sm <- smooth_estimates(mod_temp_by_geo) %>% filter(grepl("^s\\(temp_ano_sc\\):geolocation", smooth))

ggplot(sm, aes(temp_ano_sc, est)) +
  geom_ribbon(aes(ymin = est - 2*se, ymax = est + 2*se), alpha = 0.18) +
  geom_line(size = 1) +
  facet_wrap(~ by, scales = "free_y") +
  labs(x = "standardized temperature anomaly", y = "partial effect (link)",
       title = "Temperature effect by region") +
  theme_minimal()



# VPD by region
mod_vpd_by_geo_joint <- gam(
  mean_percent_recovered ~
    geolocation +                                # parametric factor
    s(long, lat, bs="tp") +
    s(mean_severity) + s(mean_temp_total) + s(mean_prec_total) +
    s(mean_elevation) + s(mean_pre_dist_tree_cover) + s(mean_bare) +
    s(temp_ano_sc, k = 6) +                      # global temp anomaly control
    s(vpd_ano_sc,  by = geolocation, k = 6),     # region-specific VPD effect
  data = df, method = "REML", select = TRUE
)

# temp by region (for SI?)
mod_temp_by_geo_joint <- gam(
  mean_percent_recovered ~
    geolocation +
    s(long, lat, bs="tp") +
    s(mean_severity) + s(mean_temp_total) + s(mean_prec_total) +
    s(mean_elevation) + s(mean_pre_dist_tree_cover) + s(mean_bare) +
    s(vpd_ano_sc,  k = 6) +                      # global VPD control
    s(temp_ano_sc, by = geolocation, k = 6),     # region-specific Temp effect
  data = df, method = "REML", select = TRUE
)


# figures----------------

