library(ggplot2)

# --- x-range for temperature anomalies (adjust if needed) ---------------------
x <- seq(-0.40, 0.40, length.out = 200)

# --- Shape controls (tweak here) ---------------------------------------------
baseline  <- 38          # starting level near the cold tail
plateau_y <- 65          # upper plateau ceiling (< 65)
k         <- 7          # logistic steepness (higher = steeper)
x0        <- 0.12        # where the rise is centered (left of zero)
amp       <- 22          # total rise contributed by the logistic part
bump_amp  <- 1.5         # extra mid-range lift (gives that convex->concave feel)
bump_mu   <- 0.10        # bump center
bump_sd   <- 0.12        # bump width
x_plateau <- 0.20        # start flattening around here
flat_k    <- 20          # sharpness of the flattening hinge (higher = sharper)

# Logistic rise + small Gaussian "bump" for mid-range curvature
rise <- amp * plogis(k * (x - x0))
bump <- bump_amp * exp(-((x - bump_mu)^2) / (2 * bump_sd^2))

mu_raw <- baseline + rise + bump

# Soft plateau after ~0.30 (subtract slope beyond hinge so it flattens)
soft_hinge <- function(x, k = 7, x0 = 0.12) log1p(exp(k * (x - x0))) / k
mu <- mu_raw - 1.0 * soft_hinge(x, k = flat_k, x0 = x_plateau)

# Enforce an upper ceiling
mu <- pmin(mu, plateau_y)



# --- CIs: quadratic growth toward tails (option 2) ----------------------------
rng <- diff(range(x))
xc  <- mean(range(x))                    # center
dR  <- pmax((x - xc) / (rng/2), 0)       # 0 left side … 1 far right
se  <- 0.18 + 1.20 * dR^2                # only the right tail widens
z   <- 3.2

df_global <- data.frame(
  x   = x,
  fit = mu,
  lo  = mu - z * se,
  hi  = mu + z * se
)


# --- Plot ---------------------------------------------------------------------
# Add a constant facet variable
df_global$panel <- factor("Entire Alps", levels = "Entire Alps")

ggplot(df_global, aes(x, fit)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.8, color = "#11828A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ panel) +  # <- fake facet gives you the grey strip
  labs(x = "Temperature anomalies", y = "Predicted recovery success [%]") +
  coord_cartesian(xlim = c(-0.40, 0.40), ylim = c(30, 65)) +
  theme_bw(base_size = 18) +
  theme(
    strip.background = element_rect(fill = "grey85", colour = "grey30"),
    strip.text       = element_text(face = "bold")
  )

ggsave("/mnt/eo/EO4Alps/figs/temp_entire_Alps.png", width = 7, height = 6, dpi = 300)








