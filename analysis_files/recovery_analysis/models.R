library(ggplot2)
library(dplyr)
library(readr)
library(raster)

recovery_0411 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0411.csv")
recovery_0311 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0311.csv")

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

### models

# Filter data to observations where I do have a regrown value ( year > yod )
recovery_0311_filt <- recovery_0311[!is.na(recovery_0311$regrown_percent), ]


recovery_0711 <- recovery_0411 %>%
  filter(ysd==10)


# remove not needed columns for more clarity
# Specify column indices to remove
columns_to_remove <- c(4,6,9,25,26,27,28,29,30,35,37,42,43,44,45,46,47,48,49,51,
                       52,54,55,56,58,59,61,62,63,65,66,67,68,69,70,72,73,74,75,
                       76,77,79,80,81,82,83,84,86,87,88,89,90,91,126)  # Indices of columns you want to remove

# Remove columns using select and negative indexing
recovery_0711 <- recovery_0711 %>% select(-columns_to_remove)


ggplot(recovery_0711) +
         geom_point(aes(x=VPD_summer_yod, y=regrown_percent, colour=as.factor(geoloc)),
                    alpha=0.3) +
         geom_smooth(aes(x=VPD_summer_yod, y=regrown_percent, color=as.factor(geoloc)),
                     method=lm, se=FALSE) +
         ylim(0,100) +
         facet_wrap(~year)
       
       

ggplot(recovery_0711 %>%
         filter(geoloc=="Southern West Alps")) +
  geom_point(aes(x=mean_VPD_yod, y=regrown_percent, colour=as.factor(geoloc)),
             alpha=0.3) +
  geom_smooth(aes(x=mean_VPD_yod, y=regrown_percent, color=as.factor(geoloc)),
              method=lm, se=FALSE) +
  ylim(0,100) +
  facet_wrap(~yod)



install.packages("glmmTMB")
library(lme4)

fit <- lmer(
  regrown_percent ~ mean_VPD_yod +
    height +
    slope +
    #aspect +
    severity_relative +
    (1 + mean_VPD_yod |year),
  data = recovery_0611 %>%
    filter(geoloc=="Southern West Alps"))

summary(fit)

