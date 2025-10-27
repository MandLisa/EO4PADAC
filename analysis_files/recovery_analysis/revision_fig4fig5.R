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
fit.gam_temp <- gam(mean_percent_recovered ~ 
                           s(long, lat, bs = "tp") +  
                           s(mean_severity) + 
                           s(mean_temp_ano_summer_yod1) + 
                           #s(mean_temp_ano_all_yod1, by = geolocation) +
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
sm_te <- gratia::smooths(fit.gam_temp)                     # e.g., "s(long,lat)", "s(mean_elevation)", ...
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
  fit.gam_temp, newdata = nd_te, type = "response",
  exclude = "s(long,lat)"
)

# CIs via link-scale then back-transform using chosen z
pr_te <- predict(
  fit.gam_temp, newdata = nd_te, type = "link", se.fit = TRUE,
  exclude = "s(long,lat)"
)
invlink <- family(fit.gam_temp)$linkinv
nd_te$lo <- invlink(pr_te$fit - z * pr_te$se.fit)
nd_te$hi <- invlink(pr_te$fit + z * pr_te$se.fit)

# ---------------------- Curves for all predictors -----------------------------
make_curve_te <- function(var) {
  x <- mk_seq_te(hexagons_recov10_centros[[var]])
  base_te <- meds_te
  ndv_te <- base_te[rep(1, length(x)), ]; ndv_te[[var]] <- x
  
  prv_te <- predict(
    fit.gam_temp, newdata = ndv_te, type = "link", se.fit = TRUE,
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
  "mean_temp_total",
  "mean_prec_total",
  "mean_severity",
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
  ylim(30,65) +
  theme_bw(base_size = 17)

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_27101.png", width = 8, height = 6, dpi = 300)


### without temp anomaly panel

# 1) Remove the "Temperature anomalies" panel
curves_noTA <- curves_te %>%
  filter(predictor != "mean_temp_ano_all_yod1") %>%
  mutate(predictor = droplevels(factor(predictor)))

# 2) If you use a named labeller, drop that entry too
facet_labels_noTA <- facet_labels_te[ names(facet_labels_te) != "mean_temp_ano_all_yod1" ]

# 3) Plot (unchanged otherwise)
ggplot(curves_noTA, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  facet_wrap(
    ~ predictor, scales = "free_x", nrow = 2,
    labeller = as_labeller(facet_labels_noTA)
  ) +
  labs(
    x = "Predictor value",
    y = "Predicted recovery success [%]"
  ) +
  ylim(30, 65) +
  theme_bw(base_size = 17)

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_27101.png", width = 8, height = 6, dpi = 300)

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
  labs(x = "Temperature anomalies", y = "Recovery success [%]") +
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank()) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  ) +
  ylim(30, 60) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  )

ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_per_geoloc_title_2710_4.png", width = 10, height = 6, dpi = 300)

### without title
ggplot(curves_summer, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(
    ~ panel, ncol = 3, scales = "free_y",
    labeller = as_labeller(facet_titles)
  ) +
  labs(x = "Temperature anomalies", y = "Recovery success [%]") +
  coord_cartesian(ylim = c(30, 60)) +         # safer than ylim() for not dropping data
  theme_bw(base_size = 18) +                  # <-- put theme_bw() first
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold"),
    axis.text.x  = element_blank(),           # <-- hide tick labels
    axis.text.y  = element_blank(),
    axis.ticks.x = element_blank(),           # (optional) hide ticks
    axis.ticks.y = element_blank()
  )

ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_per_geoloc_without_title_2410.png", width = 10, height = 6, dpi = 300)



### model diagnostics
# global check: QQ, residuals vs fitted, k-index per smooth
par(mfrow = c(2,2)); gam.check(fit.gam_temp_geoloc); par(mfrow = c(1,1))
concurvity(fit.gam_temp_geoloc, full = TRUE)

library(gratia)
# Use your model object here (e.g., fit.gam_temp_geoloc or fit.gam_temp_global)
p_diag <- appraise(fit.gam_temp_geoloc, method = "uniform")  # 2×2 panel

p_diag

ggplot2::ggsave(
  "/mnt/eo/EO4Alps/figs/gam_diagnostics_appraise.png",
  p_diag, width = 9, height = 5.5, dpi = 300
)


### global (nur temp anomalie effekt)
# ---------- data ----------
# assume you already built 'pd' with columns x, fit, lo, hi  (global effect)
pd$panel <- "Entire Alps"   # <- single facet label for the grey header

ggplot(pd, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.9, color = "#11828A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ panel, ncol = 1) +                    # <- draws the grey strip
  scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, 0.2)) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(x = "Temperature anomalies", y = "Recovery success [%]") +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  )
ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_tempeffect_all_2410.png", width = 6.5, height = 6, dpi = 300)


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
  ylim(30,60) +
  theme_bw(base_size = 17)

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_with_elevation_2410.png", width = 8, height = 6, dpi = 300)






# ---- INPUT: your updated GAM object ----
# e.g., fit.gam_temp_geoloc (already fitted)
mod <- fit.gam_temp_geoloc   # change if your object has a different name

# ---- Extract s.table and tidy it --------------------------------------------
sm <- summary(mod)$s.table
stopifnot(!is.null(sm))
df <- as.data.frame(sm)
df$term_raw <- rownames(sm)

# Pretty labels similar to your example
pretty_label <- function(x){
  x <- gsub(":", ": ", x)                             # space after colon
  x <- gsub("geolocation", "", x, fixed = TRUE)       # drop 'geolocation'
  x <- gsub("-", "–", x, fixed = TRUE)                # en dash
  x <- gsub("\\s{2,}", " ", x)                        # collapse double spaces
  trimws(x)
}
df$Smooth <- pretty_label(df$term_raw)

# Keep the essential columns and round
df$EDF  <- round(df$edf, 2)
df$Fval <- round(df$`F`, 2)
df$p    <- df$`p-value`

# Significance stars
sig_stars <- function(p){
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  "ns"
}
df$`p-value` <- ifelse(df$p < 0.001, "< 0.001", sprintf("%.3f", df$p))
df$`p-value` <- paste(df$`p-value`, vapply(df$p, sig_stars, character(1L)))

# ---- Interpretation rules (transparent & reproducible) -----------------------
interpret <- function(lbl, edf, p){
  sig <- !is.na(p) && p < 0.05
  is_linear <- edf <= 1.25
  is_nonlin_mild <- edf > 1.25 & edf < 3
  is_nonlin_strong <- edf >= 3
  
  if (grepl("^s\\(long\\s*,\\s*lat\\)", lbl)) {
    return(if (sig) "Strong spatial pattern in recovery" else "No clear spatial pattern")
  }
  if (grepl("mean_severity", lbl)) {
    if (!sig) return("Severity not significant")
    if (is_linear) return("Linear effect of disturbance severity")
    return("Nonlinear effect of disturbance severity")
  }
  if (grepl("VPD", lbl, ignore.case = TRUE)) {
    if (!sig) return("VPD anomaly effect not significant")
    if (is_linear) return("Strong linear effect of VPD anomalies")
    if (is_nonlin_strong) return("Pronounced nonlinear effect of VPD anomalies")
    return("Regional VPD anomaly effect")
  }
  if (grepl("mean_temp_total", lbl)) {
    if (!sig) return("Post-disturbance temperature not significant")
    if (is_linear) return("Linear effect of post-disturbance temperature")
    return("Nonlinear effect of post-disturbance temperature")
  }
  # generic fallback
  if (!sig) return("Not significant")
  if (is_linear) return("Linear effect")
  if (is_nonlin_strong) return("Pronounced nonlinear effect")
  "Nonlinear effect"
}

df$Interpretation <- mapply(interpret, df$Smooth, df$EDF, df$p)

# ---- Final table (ordered like mgcv prints) ----------------------------------
out <- df[, c("Smooth", "EDF", "Fval", "p-value", "Interpretation")]
names(out)[3] <- "F-value"

# View in console
out

# --- INPUT: your fitted model ---
mod <- fit.gam_temp_geoloc   # or fit.gam_temp_global, etc.

# --- 1) Extract parametric table from mgcv summary ----------------------------
ptab <- as.data.frame(summary(mod)$p.table)
ptab$Term <- rownames(ptab)
rownames(ptab) <- NULL
names(ptab) <- c("Estimate", "Std.Error", "t.value", "p.value", "Term")

# add stars
sig_stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", "ns"))))
}
ptab$`p-value` <- ifelse(ptab$p.value < 0.001, "< 0.001",
                         sprintf("%.3f", ptab$p.value))
ptab$`p-value` <- paste(ptab$`p-value`, sig_stars(ptab$p.value))

# --- 2) 95% CIs on the *link* scale ------------------------------------------
z <- qnorm(0.975)
ptab$lo_link <- ptab$Estimate - z * ptab$Std.Error
ptab$hi_link <- ptab$Estimate + z * ptab$Std.Error

# --- 3) Optional: back-transform to the response scale ------------------------
invlink <- family(mod)$linkinv
ptab$Estimate_resp <- invlink(ptab$Estimate)
ptab$lo_resp       <- invlink(ptab$lo_link)
ptab$hi_resp       <- invlink(ptab$hi_link)

# --- 4) Pretty output ---------------------------------------------------------
out <- ptab[, c("Term","Estimate","Std.Error","t.value","p-value",
                "Estimate_resp","lo_resp","hi_resp")]
names(out)[c(2,3,4)] <- c("Estimate (link)","Std. Error","t value")
names(out)[6:8] <- c("Estimate (resp)","2.5% (resp)","97.5% (resp)")

out


# --- INPUT: your fitted model object
mod <- fit.gam_temp_geoloc   # or fit.gam_temp_global, etc.

s <- summary(mod)

# Adjusted R^2  (mgcv reports the adjusted R^2 in summary(mod)$r.sq)
adj_r2 <- s$r.sq

# Deviance explained (0–1 in summary; convert to %)
dev_expl <- 100 * s$dev.expl

# REML score  (equals -2 * restricted log-likelihood when method = "REML")
reml_score <- -2 * as.numeric(logLik(mod))

# Scale estimate (residual variance/dispersion)
scale_est <- s$scale

# Sample size
n <- nobs(mod)  # or s$n

metrics <- data.frame(
  Metric = c("Adjusted R²", "Deviance explained", "REML score", "Scale estimate", "Sample size (n)"),
  Value  = c(
    sprintf("%.3f", adj_r2),
    sprintf("%.1f%%", dev_expl),
    sprintf("%.1f",  reml_score),
    sprintf("%.3f",  scale_est),
    sprintf("%d",    n)
  ),
  check.names = FALSE
)

metrics




#-------------------------------------------------------------------------------
### model per geoloc for spring temps
#------------------------------------------------------------------------------

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

stopifnot("mean_temp_ano_all_yod1" %in% names(dat),
          is.numeric(dat$mean_temp_ano_summer_yod1))

# ------------------------ Model -----------------------------------------------
fit.gam_temp_geoloc_spring <- mgcv::gam(
  mean_percent_recovered ~ 
    s(long, lat, bs = "tp") +  
    s(mean_severity) + 
    s(mean_temp_ano_all_yod1, by = geolocation) + 
    s(mean_temp_ano_all_yod1) +
    s(mean_prec_total) +
    s(mean_temp_total) +
    s(mean_pre_dist_tree_cover) +
    s(mean_bare),
  data = dat, method = "REML"
)

# Optional sanity: check the by-smooth exists
ss <- mgcv::interpret.gam(formula(fit.gam_temp_geoloc_spring))$smooth.spec
temp_var <- "mean_temp_ano_all_yod1"
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

invlink <- family(fit.gam_temp_geoloc_spring)$linkinv

# Build one curve per subregion on the COMMON x-grid
make_curve_geo <- function(g){
  nd <- meds_by_geo[[g]][rep(1, length(x_grid)), , drop = FALSE]
  nd[[temp_var]] <- x_grid
  nd$geolocation <- factor(g, levels = geo_levels)
  
  pr <- predict(
    fit.gam_temp_geoloc_spring, newdata = nd, type = "link", se.fit = TRUE,
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
  labs(x = "Temperature anomalies", y = "Recovery success [%]") +
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank()) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  ) +
  ylim(30, 60) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text = element_text(face = "bold")
  )

ggsave("/mnt/eo/EO4Alps/figs/pred_recovery_per_geoloc_spring.png", width = 10, height = 6, dpi = 300)


