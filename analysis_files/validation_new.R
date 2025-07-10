# Fit model once
model <- lm(BG_val ~ BG_fcover, data = val_2025)

# Predict using the same data
predictions <- predict(model, newdata = val_2025)

# Point estimate of MAE
mae_point <- mean(abs(val_2025$BG_val - predictions), na.rm = TRUE)

# Also recalculate rRMSE based on predictions
rmse_point <- sqrt(mean((val_2025$BG_val - predictions)^2, na.rm = TRUE))
rrmse_point <- (rmse_point / range_target_var) * 100


ggplot(val_2025, aes(x = tree_cover_fcover, y = tree_cover_val)) +
  geom_point(size = 1.4, alpha = 0.6) +
  geom_ribbon(
    aes(ymin = boot_ci_intercept[1] + boot_ci_slope[1] * tree_cover_fcover, 
        ymax = boot_ci_intercept[2] + boot_ci_slope[2] * tree_cover_fcover), 
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


