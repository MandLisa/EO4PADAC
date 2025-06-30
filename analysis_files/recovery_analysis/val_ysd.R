library(readr)
library(ggplot2)
library(forcats)

val_ysd <- read_csv("/mnt/eo/EO4Alps/00_analysis/_recovery/val_ysd.csv")

# rename
val_ysd$class <- fct_recode(val_ysd$class,
                       "Broadleaved forest" = "broadleaved",
                       "Coniferous forest" = "coniferous",
                       "Bare ground" = "bare_ground",
                       "Grassland" = "grassland",
                       "Shrubland" = "shrubland"
)


# reorder
val_ysd$class <- factor(val_ysd$class, levels = c("Broadleaved forest",
                                                  "Coniferous forest",
                                                  "Shrubland", 
                                                  "Grassland",
                                                  "Bare ground"))


# plot
ggplot(val_ysd, aes(x = factor(ysd), y = acc)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(aes(yintercept = acc_wtw), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  facet_wrap(~ class) +
  labs(
    x = "Years since disturbance",
    y = expression(R^2)
  ) +
  ylim(0, 1) +
  theme_bw(base_size = 14)


ggsave("/mnt/eo/EO4Alps/figs/val_ysd.png", width = 8, height = 4, dpi = 300)







