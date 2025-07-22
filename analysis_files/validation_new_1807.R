# Fit model once
model <- lm(Shrubl_val ~ Shrubl_fcover, data = val_2025)

# Predict using the same data
predictions <- predict(model, newdata = val_2025)

# Point estimate of MAE
mae_point <- mean(abs(val_2025$Shrubl_val - predictions), na.rm = TRUE)

# Also recalculate rRMSE based on predictions
rmse_point <- sqrt(mean((val_2025$Shrubl_val - predictions)^2, na.rm = TRUE))
rrmse_point <- (rmse_point / range_target_var) * 100


ggplot(val_2025, aes(x = Shrubl_fcover, y = Shrubl_val)) +
  geom_point(size = 1.4, alpha = 0.6) +
  geom_ribbon(
    aes(ymin = boot_ci_intercept[1] + boot_ci_slope[1] * Shrubl_fcover, 
        ymax = boot_ci_intercept[2] + boot_ci_slope[2] * Shrubl_fcover), 
    fill = "#71d5da", 
    alpha = 0.4
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#6f0258", alpha = 0) +  
  geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype="dashed") +
  xlim(0, 100) +
  ylim(0, 100) +
  labs(
    x = "",
    y = "",
    title = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm") # Adjust the margin as needed
  ) +
annotate(
  "text",
  x = 1,
  y = 100,
  hjust = 0.05,
  vjust = 1,
  size = 3.75,
  label = paste(
    "y =", 
    round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x\n",
    "R² =", round(summary(model)$r.squared, 2), "±", round(diff(boot_ci_r_squared) / 2, 2), "\n",
    "MAE =", round(mae_point, 2), "±", round(diff(boot_ci_mae) / 2, 2), "\n",
    "rRMSE =", round(rrmse_point, 2), "±", round(diff(boot_ci_rrmse) / 2, 2)
  )
) +
  xlim(0, 100) +
  ylim(0, 100) +
  theme_bw(base_size = 20)



df_tree <- val_2025 %>%
  dplyr::select(starts_with("tree_cover"))

write.csv(df_tree, "/mnt/eo/EO4Alps/00_analysis/_recovery/val_trees_2025.csv", row.names = FALSE)


val_BL_CF <- read_csv("/mnt/eo/EO4Alps/00_analysis/_recovery/val_BL_CF.csv")


# Fit model once
model <- lm(BL_val ~ BL_fcover, data = val_BL_CF)

# Predict using the same data
predictions <- predict(model, newdata = val_BL_CF)

# Point estimate of MAE
mae_point <- mean(abs(val_BL_CF$BL_val - predictions), na.rm = TRUE)

# Also recalculate rRMSE based on predictions
rmse_point <- sqrt(mean((val_BL_CF$BL_val - predictions)^2, na.rm = TRUE))
rrmse_point <- (rmse_point / range_target_var) * 100



ggplot(val_BL_CF, aes(x = BL_fcover, y = BL_val)) +
  geom_point(size = 1.4, alpha = 0.6) +
  geom_ribbon(
    aes(ymin = boot_ci_intercept[1] + boot_ci_slope[1] * BL_fcover, 
        ymax = boot_ci_intercept[2] + boot_ci_slope[2] * BL_fcover), 
    fill = "#71d5da", 
    alpha = 0.4
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#6f0258", alpha = 0) +  
  geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype="dashed") +
  xlim(0, 100) +
  ylim(0, 100) +
  labs(
    x = "",
    y = "",
    title = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm") # Adjust the margin as needed
  ) +
  annotate(
    "text",
    x = 1,
    y = 100,
    hjust = 0.05,
    vjust = 1,
    size = 3.75,
    label = paste(
      "y =", 
      round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x\n",
      "R² =", round(summary(model)$r.squared, 2), "±", round(diff(boot_ci_r_squared) / 2, 2), "\n",
      "MAE =", round(mae_point, 2), "±", round(diff(boot_ci_mae) / 2, 2), "\n",
      "rRMSE =", round(rrmse_point, 2), "±", round(diff(boot_ci_rrmse) / 2, 2)
    )
  ) +
  xlim(0, 100) +
  ylim(0, 100) +
  theme_bw(base_size = 20)


ggsave("/mnt/eo/EO4Alps/figs/val_broadleaved.png", width = 4, height = 4, dpi = 300)


# --- Load Data ---
# --- Load Data ---
val_BL_CF <- read_csv("/mnt/eo/EO4Alps/00_analysis/_recovery/val_BL_CF.csv")

# --- Fit Model ---
model <- lm(CF_val ~ CF_fcover, data = val_BL_CF)

# Predictions for metrics
predictions <- predict(model, newdata = val_BL_CF)

# Calculate MAE and rRMSE
mae_point <- mean(abs(val_BL_CF$CF_val - predictions), na.rm = TRUE)
rmse_point <- sqrt(mean((val_BL_CF$CF_val - predictions)^2, na.rm = TRUE))
rrmse_point <- (rmse_point / range_target_var) * 100  # Ensure range_target_var is defined

# --- Prepare Data for Regression Line and Ribbon ---
x_seq <- data.frame(CF_fcover = seq(0, 100, length.out = 200))
pred_ci <- predict(model, newdata = x_seq, interval = "confidence", level = 0.99)

widen <- 5  # add 5 percentage points
ribbon_df <- data.frame(
  CF_fcover = x_seq$CF_fcover,
  ymin = pred_ci[, "lwr"] - widen,
  ymax = pred_ci[, "upr"] + widen,
  fit  = pred_ci[, "fit"]
)

ribbon_df <- data.frame(
  CF_fcover = x_seq$CF_fcover,
  ymin = pred_ci[, "lwr"],
  ymax = pred_ci[, "upr"],
  fit  = pred_ci[, "fit"]
)

# --- Plot ---
ggplot(val_BL_CF, aes(x = CF_fcover, y = CF_val)) +
  # Points
  geom_point(size = 1.4, alpha = 0.6) +
  
  # Confidence Ribbon
  geom_ribbon(
    data = ribbon_df,
    aes(x = CF_fcover, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#71d5da",
    alpha = 0.4
  ) +
  
  # Regression Line
  geom_line(
    data = ribbon_df,
    aes(x = CF_fcover, y = fit),
    color = "#6f0258", linewidth = 1
  ) +
  
  # 1:1 Line
  geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") +
  
  # Axis Limits
  xlim(0, 100) +
  ylim(0, 100) +
  
  # Labels and Annotations
  labs(x = "", y = "", title = "") +
  annotate(
    "text",
    x = 1, y = 100,
    hjust = 0.05, vjust = 1,
    size = 3.75,
    label = paste(
      "y =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x\n",
      "R² =", round(summary(model)$r.squared, 2), "±", round(diff(boot_ci_r_squared) / 2, 2), "\n",
      "MAE =", round(mae_point, 2), "±", round(diff(boot_ci_mae) / 2, 2), "\n",
      "rRMSE =", round(rrmse_point, 2), "±", round(diff(boot_ci_rrmse) / 2, 2)
    )
  ) +
  
  # Theme
  theme_bw(base_size = 22) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )


ggsave("/mnt/eo/EO4Alps/figs/val_coniferous.png", width = 4, height = 4, dpi = 300)






# Fit model
model <- lm(Shrubl_val ~ Shrubl_fcover, data = val_2025)

# Generate sequence for predictions
x_seq <- data.frame(Shrubl_fcover = seq(0, 100, length.out = 200))

# Predict with confidence intervals
pred_ci <- predict(model, newdata = x_seq, interval = "confidence")

# Build a dataframe for ribbon and line
ribbon_df <- data.frame(
  Shrubl_fcover = x_seq$Shrubl_fcover,
  fit = pred_ci[, "fit"],
  lwr = pred_ci[, "lwr"],
  upr = pred_ci[, "upr"]
)

# Metrics
predictions <- predict(model, newdata = val_2025)
mae_point <- mean(abs(val_2025$Shrubl_val - predictions), na.rm = TRUE)
rmse_point <- sqrt(mean((val_2025$Shrubl_val - predictions)^2, na.rm = TRUE))
rrmse_point <- (rmse_point / range_target_var) * 100

# Plot
ggplot(val_2025, aes(x = Shrubl_fcover, y = Shrubl_val)) +
  geom_point(size = 1.4, alpha = 0.6) +
  geom_ribbon(data = ribbon_df,
              aes(x = Shrubl_fcover, ymin = lwr, ymax = upr),
              inherit.aes = FALSE, fill = "#71d5da", alpha = 0.4) +
  geom_line(data = ribbon_df, aes(x = Shrubl_fcover, y = fit),
            color = "#6f0258", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") +
  xlim(0, 100) +
  ylim(0, 100) +
  annotate(
    "text", x = 1, y = 100, hjust = 0.05, vjust = 1, size = 3.75,
    label = paste(
      "y =", round(coef(model)[1], 2), "+", round(coef(model)[2], 2), "x\n",
      "R² =", round(summary(model)$r.squared, 2), "\n",
      "MAE =", round(mae_point, 2), "\n",
      "rRMSE =", round(rrmse_point, 2)
    )
  ) +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

