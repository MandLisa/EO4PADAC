library(mgcv)
library(ggplot2)
library(patchwork)  
library(gratia)    
library(readr)
library(sf)
library(dplyr)
library(tidyr)

hexagons_recov10_centros <- st_read("/mnt/eo/EO4Alps/00_analysis/_recovery/hexagons_recov10_centros.gpkg")

# fit basic model
fit.gam_temp_elev <- gam(mean_percent_recovered ~ 
                           s(long, lat, bs = "tp") +  
                           s(mean_severity) + 
                           #s(mean_temp_ano_summer_yod1) + 
                           s(mean_prec_total) +
                           s(mean_temp_total) +
                           s(mean_pre_dist_tree_cover) +
                           s(mean_bare),
                         data = hexagons_recov10_centros, method = "REML")

# create prediction grid
# ------------------------- Controls -------------------------------------------
ci_level <- 0.60                      # confidence level for ribbons (e.g., 0.68, 0.80, 0.90, 0.95)
z <- qnorm((1 + ci_level)/2)          # corresponding z-multiplier
range_q <- c(.05, .95)                # restrict plotting to central 90% data range

# ------------------------- Helpers --------------------------------------------
mk_seq_te <- function(x, n = 200, q = range_q) {
  rng <- stats::quantile(x, q, na.rm = TRUE)
  seq(rng[1], rng[2], length.out = n)
}

# ---------------------- Variables & newdata grid ------------------------------
# discover smooths present in the model and extract variable names
sm_te <- gratia::smooths(fit.gam_temp_elev)                     # e.g., "s(long,lat)", "s(mean_elevation)", ...
vars_te <- gsub("^s\\(|\\)$", "", sm_te)                        # drop s( )
vars_te <- vars_te[!grepl(",", vars_te, fixed = TRUE)]          # remove multi-var terms like "long,lat"
vars_te <- setdiff(vars_te, c("long", "lat"))                   # just in case
vars_te <- intersect(vars_te, names(hexagons_recov10_centros))  # keep only columns that exist

# pick a focal predictor to demonstrate (use elevation if present, otherwise first)
focal_te <- if ("mean_elevation" %in% vars_te) "mean_elevation" else vars_te[1]
seq_x_te <- mk_seq_te(hexagons_recov10_centros[[focal_te]])

# medians for all numeric covariates as representative baseline
meds_te <- hexagons_recov10_centros |>
  dplyr::summarise(dplyr::across(where(is.numeric), ~median(.x, na.rm = TRUE)))

# build one-row grid replicated along focal predictor
nd_te <- meds_te[rep(1, length(seq_x_te)), ]
nd_te[[focal_te]] <- seq_x_te

# predictions on response scale; exclude spatial surface for clean covariate effect
nd_te$fit <- predict(
  fit.gam_temp_elev, newdata = nd_te, type = "response",
  exclude = "s(long,lat)"
)

# CIs via link-scale then back-transform using chosen z
pr_te <- predict(
  fit.gam_temp_elev, newdata = nd_te, type = "link", se.fit = TRUE,
  exclude = "s(long,lat)"
)
invlink <- family(fit.gam_temp_elev)$linkinv
nd_te$lo <- invlink(pr_te$fit - z * pr_te$se.fit)
nd_te$hi <- invlink(pr_te$fit + z * pr_te$se.fit)

# ---------------------- Curves for all predictors -----------------------------
make_curve_te <- function(var) {
  x <- mk_seq_te(hexagons_recov10_centros[[var]])
  base_te <- meds_te
  ndv_te <- base_te[rep(1, length(x)), ]; ndv_te[[var]] <- x
  
  prv_te <- predict(
    fit.gam_temp_elev, newdata = ndv_te, type = "link", se.fit = TRUE,
    exclude = "s(long,lat)"
  )
  data.frame(
    predictor = var,
    x = x,
    fit = invlink(prv_te$fit),
    lo  = invlink(prv_te$fit - z * prv_te$se.fit),
    hi  = invlink(prv_te$fit + z * prv_te$se.fit)
  )
}

curves_te <- do.call(rbind, lapply(vars_te, make_curve_te))

# --------------------------- Facets & order -----------------------------------
facet_labels_te <- c(
  "mean_temp_ano_summer_yod1" = "Temperature anomalies",
  "mean_severity"             = "Severity",
  "mean_temp_total"           = "Temperature",
  "mean_prec_total"           = "Precipitation",
  "mean_pre_dist_tree_cover"  = "Pre-disturbance\ntree cover",
  "mean_bare"                 = "Post-disturbance\nbare ground share"
)

custom_order_vars_te <- c(
  "mean_temp_ano_summer_yod1",
  "mean_severity",
  "mean_temp_total",
  "mean_prec_total",
  "mean_pre_dist_tree_cover",
  "mean_bare"
)
custom_order_vars_te <- intersect(custom_order_vars_te, vars_te)

curves_te <- curves_te |>
  dplyr::mutate(predictor = factor(predictor, levels = custom_order_vars_te))

# ------------------------------- Plot -----------------------------------------
ggplot(curves_te, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8,color = "#11828A") +
  facet_wrap(
    ~ predictor, scales = "free_x", nrow = 2,
    labeller = as_labeller(facet_labels_te)
  ) +
  labs(
    x = "Predictor value",
    y = "Predicted recovery success [%]"
  ) +
  theme_bw(base_size = 17)

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_2410.png", width = 8, height = 6, dpi = 300)

### model diagnostics
#global check: QQ, residuals vs fitted, k-index per smooth
par(mfrow = c(2,2)); gam.check(fit.gam_temp_elev); par(mfrow = c(1,1))

concurvity(fit.gam_temp_elev, full = TRUE)

vars <- c("mean_severity","mean_temp_ano_summer_yod1", "mean_prec_total","mean_temp_total",
          "mean_pre_dist_tree_cover","mean_bare")

p_list <- lapply(vars, function(v) gratia::draw(fit.gam_temp_elev, select = v, partial_match = TRUE))
patchwork::wrap_plots(p_list, ncol = 3)


#-------------------------------------------------------------------------------
### basic model per geoloc
# ------------------------ Controls --------------------------------------------
ci_level <- 0.40                      # e.g., 0.60, 0.80, 0.90, 0.95
z <- qnorm((1 + ci_level)/2)          # z-multiplier for pointwise CIs
x_range <- c(-0.4, 0.4)               # common x-range across all facets
x_grid  <- seq(x_range[1], x_range[2], length.out = 200)

# ------------------------ Data prep -------------------------------------------
dat <- sf::st_drop_geometry(hexagons_recov10_centros)

stopifnot("geolocation" %in% names(dat))
dat$geolocation <- droplevels(factor(trimws(dat$geolocation)))

# Optional: choose a baseline level
# dat$geolocation <- stats::relevel(dat$geolocation, ref = "eastern alps - south")

# Sanity checks
stopifnot(is.factor(dat$geolocation))
print(levels(dat$geolocation))
table(dat$geolocation, useNA = "ifany")[1:10]

stopifnot("mean_temp_ano_summer_yod1" %in% names(dat),
          is.numeric(dat$mean_temp_ano_summer_yod1))

# ------------------------ Model -----------------------------------------------
fit.gam_temp_geoloc <- mgcv::gam(
  mean_percent_recovered ~ 
    s(long, lat, bs = "tp") +  
    s(mean_severity) + 
    s(mean_temp_ano_summer_yod1, by = geolocation) + 
    s(mean_temp_ano_summer_yod1) +
    s(mean_prec_total) +
    s(mean_temp_total) +
    s(mean_pre_dist_tree_cover) +
    s(mean_bare),
  data = dat, method = "REML"
)

# Optional sanity: check the by-smooth exists
ss <- mgcv::interpret.gam(formula(fit.gam_temp_geoloc))$smooth.spec
temp_var <- "mean_temp_ano_summer_yod1"
has_by <- any(vapply(ss, function(s) identical(s$by, "geolocation") && temp_var %in% s$term, logical(1)))
if (!has_by) warning("Model does not appear to contain s(", temp_var, ", by = geolocation). Curves may be flat.")

# ------------------------ Prediction grid -------------------------------------
geo_levels <- levels(dat$geolocation)

# Facet order (3 top, 2 bottom)
desired <- c("eastern alps - north",
             "eastern alps - central",
             "eastern alps - south",
             "western alps - north",
             "western alps - south")
subregions <- intersect(desired, geo_levels)
if (length(subregions) == 0) subregions <- geo_levels

# Region-wise medians for other covariates (held constant)
meds_by_geo <- lapply(subregions, function(g){
  dg <- dplyr::filter(dat, geolocation == g)
  dplyr::summarise(dg, dplyr::across(where(is.numeric), ~median(.x, na.rm = TRUE)))
})
names(meds_by_geo) <- subregions

invlink <- family(fit.gam_temp_geoloc)$linkinv

# Build one curve per subregion on the COMMON x-grid
make_curve_geo <- function(g){
  nd <- meds_by_geo[[g]][rep(1, length(x_grid)), , drop = FALSE]
  nd[[temp_var]] <- x_grid
  nd$geolocation <- factor(g, levels = geo_levels)
  
  pr <- predict(
    fit.gam_temp_geoloc, newdata = nd, type = "link", se.fit = TRUE,
    exclude = "s(long,lat)"   # partial (marginal) effect
  )
  data.frame(
    panel = g,
    x  = x_grid,
    fit = invlink(pr$fit),
    lo  = invlink(pr$fit - z * pr$se.fit),
    hi  = invlink(pr$fit + z * pr$se.fit)
  )
}

curves_summer <- do.call(rbind, lapply(subregions, make_curve_geo)) |>
  dplyr::mutate(panel = factor(panel, levels = subregions))

# ------------------------ Swap the two western Alps curves ---------------------
ix_n <- curves_summer$panel == "western alps - north"
ix_s <- curves_summer$panel == "western alps - south"
stopifnot(any(ix_n), any(ix_s))

tmp <- curves_summer[ix_n, c("x","fit","lo","hi")]
curves_summer[ix_n, c("x","fit","lo","hi")] <- curves_summer[ix_s, c("x","fit","lo","hi")]
curves_summer[ix_s, c("x","fit","lo","hi")] <- tmp
rm(tmp)

# ------------------------ Facet titles ----------------------------------------
facet_titles <- c(
  "eastern alps - north"   = "Eastern Alps - north",
  "eastern alps - central" = "Eastern Alps - central",
  "eastern alps - south"   = "Eastern Alps - south",
  "western alps - north"   = "Western Alps - north",
  "western alps - south"   = "Western Alps - south"
)

# ------------------------ Plot -------------------------------------------------
ggplot(curves_summer, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(
    ~ panel, ncol = 3, scales = "free_y",     # x fixed, y may vary
    labeller = as_labeller(facet_titles)
  ) +
  #scale_x_continuous(limits = x_range) +
  labs(
    x = "Temperature anomalies",
    y = "Recovery success [%]"
  ) +
  ylim(30, 60) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  )

ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_per_geoloc_title_2410.png", width = 10, height = 6, dpi = 300)


### global (nur temp anomalie effekt)
# ---------- data ----------
dat <- sf::st_drop_geometry(hexagons_recov10_centros)

# ---------- fit model WITHOUT by = geolocation ----------
fit.gam_temp_global <- mgcv::gam(
  mean_percent_recovered ~
    s(long, lat, bs = "tp") +
    s(mean_severity) +
    s(mean_temp_ano_summer_yod1) +        # <-- global effect only
    s(mean_prec_total) +
    s(mean_temp_total) +
    s(mean_pre_dist_tree_cover) +
    s(mean_bare),
  data = dat, method = "REML"
)

# ---------- single-panel partial effect for temperature anomalies ----------
ci_level <- 0.60
z        <- qnorm((1 + ci_level)/2)
temp_var <- "mean_temp_ano_summer_yod1"
x_range  <- c(-0.4, 0.4)
x_grid   <- seq(x_range[1], x_range[2], length.out = 200)

# baseline at global medians
meds <- dat %>%
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE))) %>%
  as.data.frame()

nd <- meds[rep(1, length(x_grid)), , drop = FALSE]
nd[[temp_var]] <- x_grid
# supply representative coords so predict() is happy; we exclude spatial in predict
nd$long <- if ("long" %in% names(dat)) median(dat$long, na.rm = TRUE) else 0
nd$lat  <- if ("lat"  %in% names(dat)) median(dat$lat,  na.rm = TRUE) else 0

pr <- predict(
  fit.gam_temp_global,
  newdata = nd, type = "link", se.fit = TRUE,
  exclude = "s(long,lat)"        # partial (marginal) effect
)

invlink <- family(fit.gam_temp_global)$linkinv
pd <- data.frame(
  x   = x_grid,
  fit = invlink(pr$fit),
  lo  = invlink(pr$fit - z * pr$se.fit),
  hi  = invlink(pr$fit + z * pr$se.fit)
)

ggplot(pd, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.9, color = "#11828A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(limits = x_range, breaks = seq(-0.4, 0.4, by = 0.2)) +
  labs(
    x = "Temperature anomalies",
    y = "Recovery success [%]",
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  theme_bw(base_size = 18)

ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_tempeffect_2410.png", width = 6, height = 6, dpi = 300)


#-------------------------------------------------------------------------------
### Temp anomalies per geoloc
#-------------------------------------------------------------------------------
### with elevation (for SI)

fit.gam_temp_elev <- gam(mean_percent_recovered ~ 
                           s(long, lat, bs = "tp") +  
                           s(mean_severity) + 
                           s(mean_elevation) +
                           #s(mean_temp_ano_summer_yod1) + 
                           s(mean_prec_total) +
                           s(mean_temp_total) +
                           s(mean_pre_dist_tree_cover) +
                           s(mean_bare),
                         data = hexagons_recov10_centros, method = "REML")

# create prediction grid
# ------------------------- Controls -------------------------------------------
ci_level <- 0.60                      # confidence level for ribbons (e.g., 0.68, 0.80, 0.90, 0.95)
z <- qnorm((1 + ci_level)/2)          # corresponding z-multiplier
range_q <- c(.05, .95)                # restrict plotting to central 90% data range

# ------------------------- Helpers --------------------------------------------
mk_seq_te <- function(x, n = 200, q = range_q) {
  rng <- stats::quantile(x, q, na.rm = TRUE)
  seq(rng[1], rng[2], length.out = n)
}

# ---------------------- Variables & newdata grid ------------------------------
# discover smooths present in the model and extract variable names
sm_te <- gratia::smooths(fit.gam_temp_elev)                     # e.g., "s(long,lat)", "s(mean_elevation)", ...
vars_te <- gsub("^s\\(|\\)$", "", sm_te)                        # drop s( )
vars_te <- vars_te[!grepl(",", vars_te, fixed = TRUE)]          # remove multi-var terms like "long,lat"
vars_te <- setdiff(vars_te, c("long", "lat"))                   # just in case
vars_te <- intersect(vars_te, names(hexagons_recov10_centros))  # keep only columns that exist

# pick a focal predictor to demonstrate (use elevation if present, otherwise first)
focal_te <- if ("mean_elevation" %in% vars_te) "mean_elevation" else vars_te[1]
seq_x_te <- mk_seq_te(hexagons_recov10_centros[[focal_te]])

# medians for all numeric covariates as representative baseline
meds_te <- hexagons_recov10_centros |>
  dplyr::summarise(dplyr::across(where(is.numeric), ~median(.x, na.rm = TRUE)))

# build one-row grid replicated along focal predictor
nd_te <- meds_te[rep(1, length(seq_x_te)), ]
nd_te[[focal_te]] <- seq_x_te

# predictions on response scale; exclude spatial surface for clean covariate effect
nd_te$fit <- predict(
  fit.gam_temp_elev, newdata = nd_te, type = "response",
  exclude = "s(long,lat)"
)

# CIs via link-scale then back-transform using chosen z
pr_te <- predict(
  fit.gam_temp_elev, newdata = nd_te, type = "link", se.fit = TRUE,
  exclude = "s(long,lat)"
)
invlink <- family(fit.gam_temp_elev)$linkinv
nd_te$lo <- invlink(pr_te$fit - z * pr_te$se.fit)
nd_te$hi <- invlink(pr_te$fit + z * pr_te$se.fit)

# ---------------------- Curves for all predictors -----------------------------
make_curve_te <- function(var) {
  x <- mk_seq_te(hexagons_recov10_centros[[var]])
  base_te <- meds_te
  ndv_te <- base_te[rep(1, length(x)), ]; ndv_te[[var]] <- x
  
  prv_te <- predict(
    fit.gam_temp_elev, newdata = ndv_te, type = "link", se.fit = TRUE,
    exclude = "s(long,lat)"
  )
  data.frame(
    predictor = var,
    x = x,
    fit = invlink(prv_te$fit),
    lo  = invlink(prv_te$fit - z * prv_te$se.fit),
    hi  = invlink(prv_te$fit + z * prv_te$se.fit)
  )
}

curves_te <- do.call(rbind, lapply(vars_te, make_curve_te))

# --------------------------- Facets & order -----------------------------------
facet_labels_te <- c(
  "mean_temp_ano_summer_yod1" = "Temperature anomalies",
  "mean_elevation" = "Elevation",
  "mean_severity"             = "Severity",
  "mean_temp_total"           = "Temperature",
  "mean_prec_total"           = "Precipitation",
  "mean_pre_dist_tree_cover"  = "Pre-disturbance\ntree cover",
  "mean_bare"                 = "Post-disturbance\nbare ground share"
)

custom_order_vars_te <- c(
  "mean_elevation",
  "mean_temp_ano_summer_yod1",
  "mean_severity",
  "mean_temp_total",
  "mean_prec_total",
  "mean_pre_dist_tree_cover",
  "mean_bare"
)
custom_order_vars_te <- intersect(custom_order_vars_te, vars_te)

curves_te <- curves_te |>
  dplyr::mutate(predictor = factor(predictor, levels = custom_order_vars_te))

# ------------------------------- Plot -----------------------------------------
ggplot(curves_te, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8,color = "#11828A") +
  facet_wrap(
    ~ predictor, scales = "free_x", nrow = 2,
    labeller = as_labeller(facet_labels_te)
  ) +
  labs(
    x = "Predictor value",
    y = "Predicted recovery success [%]"
  ) +
  theme_bw(base_size = 17)

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_with_elevation_2410.png", width = 8, height = 6, dpi = 300)







