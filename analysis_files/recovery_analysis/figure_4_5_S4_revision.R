df_global$predictor <- "mean_temp_ano_summer_yod1"

df_global2 <- df_global |>
  dplyr::select(x, fit, lo, hi) |>       # keep only needed columns
  dplyr::mutate(predictor = "mean_temp_ano_summer_yod1") |>
  dplyr::select(predictor, x, fit, lo, hi)   # correct column order


curves_te_full <- rbind(curves_te, df_global2)

custom_order_vars_te <- c(
  "mean_elevation",
  "mean_temp_ano_summer_yod1",
  "mean_temp_total", 
  "mean_prec_total",
  "mean_severity",
  "mean_pre_dist_tree_cover",
  "mean_bare"
)

curves_te_full$predictor <- factor(
  curves_te_full$predictor,
  levels = custom_order_vars_te
)


facet_labels_te <- c(
  "mean_temp_ano_summer_yod1" = "Temperature anomalies",
  "mean_elevation" = "Elevation",
  "mean_severity"             = "Severity",
  "mean_temp_total"           = "Temperature",
  "mean_prec_total"           = "Precipitation",
  "mean_pre_dist_tree_cover"  = "Pre-disturbance\ntree cover",
  "mean_bare"                 = "Post-disturbance\nbare ground share"
)

p_full <- ggplot(curves_te_full, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  facet_wrap(
    ~ predictor,
    scales = "free_x",
    nrow = 2,
    labeller = as_labeller(facet_labels_te)
  ) +
  labs(
    x = "Predictor value",
    y = "Predicted recovery success [%]"
  ) +
  ylim(30, 65) +
  theme_bw(base_size = 17)

p_full

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_with_elevation_2111.png", width = 11, height = 7, dpi = 300)





curves_te_no_anom <- curves_te |>
  dplyr::filter(predictor != "mean_temp_ano_summer_yod1")


df_global2 <- df_global |>
  dplyr::select(x, fit, lo, hi) |>
  dplyr::mutate(predictor = "mean_temp_ano_summer_yod1") |>
  dplyr::select(predictor, x, fit, lo, hi)


curves_te_full <- rbind(curves_te_no_anom, df_global2)


custom_order_vars_te <- c(
  "mean_temp_ano_summer_yod1",
  "mean_temp_total",
  "mean_prec_total",
  "mean_severity",
  "mean_pre_dist_tree_cover",
  "mean_bare"
)

curves_te_full$predictor <- factor(
  curves_te_full$predictor,
  levels = custom_order_vars_te
)

ggplot(curves_te_full, aes(x = x, y = fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  facet_wrap(
    ~ predictor, scales = "free_x", nrow = 2,
    labeller = as_labeller(facet_labels_te)
  ) +
  labs(
    x = "Predictor value",
    y = "Predicted recovery success [%]"
  ) +
  ylim(30, 65) +
  theme_bw(base_size = 17) +
  theme(
    axis.title = element_text(size = 14)   # z.B. von 17 → 12
  )

ggsave("/mnt/eo/EO4Alps/figs/predicted_recovery_2111.png", width = 8, height = 6, dpi = 300)












