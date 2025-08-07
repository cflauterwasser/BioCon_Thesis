# ___________________________________________________________________________
# Loading Packages ####

library(tidyverse)
library(terra)
library(sf)
library(raster)



# ___________________________________________________________________________
# Importing Datasets ####

## 1980-2000 ####

tifs_80_00_tmin <- list.files(
  path = "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tmin 1980-2000",
  pattern = "\\.tif$",
  full.names = TRUE
)

tifs_80_00_tmax <- list.files(
  path = "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tmax 1980-2000",
  pattern = "\\.tif$",
  full.names = TRUE
)


# Load all rasters into a SpatRaster collection
climateH_80_00_tmin <- rast(tifs_80_00_tmin)

climateH_80_00_tmax <- rast(tifs_80_00_tmax)



## 2002-2022 ####

tifs_02_22_tmin <- list.files(
  path = "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tmin 2002-2022",
  pattern = "\\.tif$",
  full.names = TRUE
)

tifs_02_22_tmax <- list.files(
  path = "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tmax 2002-2022",
  pattern = "\\.tif$",
  full.names = TRUE
)


# Load all rasters into a SpatRaster collection
climateH_02_22_tmin <- rast(tifs_02_22_tmin)

climateH_02_22_tmax <- rast(tifs_02_22_tmax)



# ___________________________________________________________________________
# Deriving tavg ####

# in absence of true tavg data, simplified assumption that mean temperature is exactly between tmin and tmax -> STATE IT CLEARLY LATER THAT IT IS AN ESTIMATE OF Tavg!

tminmax_to_tavg <- function(tmin, tmax) {
  monthly_tavg <- mean(tmin, tmax)
  total_tavg <- mean(monthly_tavg)
  return(total_tavg)
}

# 1980-2000

climateH_tavg_1 <- tminmax_to_tavg(climateH_80_00_tmin, climateH_80_00_tmax)
plot(climateH_tavg_1)


# 2002-2022

climateH_tavg_2 <- tminmax_to_tavg(climateH_02_22_tmin, climateH_02_22_tmax)
plot(climateH_tavg_2)



# ___________________________________________________________________________
# Calculating ΔT ####

# 1990-2012

deltaT_90_12 <- climateH_tavg_2 - climateH_tavg_1
plot(deltaT_90_12)



# ___________________________________________________________________________
## Masking for Europe ####


eur_mask <- function(shp, raster) {
  shp <- project(shp, crs(raster))
  raster <- crop(raster, shp) # Crop to bounding box of shp
  raster <- mask(raster, shp) # Mask to exact shape
  return(raster)
}


europe_shp <- vect("external datasets/Europe Shapefile/european_coast.shp")


tavg_europe_1990 <- eur_mask(europe_shp, climateH_tavg_1)

tavg_europe_2012 <- eur_mask(europe_shp, climateH_tavg_2)

deltaT_europe_90_12 <- eur_mask(europe_shp, deltaT_90_12)


png("plots/temperature/deltaT_2.5min_20y.png",
  width = 800,
  height = 600,
  res = 100
)
plot(deltaT_europe_90_12)
dev.off()




# ___________________________________________________________________________
## Exporting Data ####

writeRaster(tavg_europe_1990, "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tavg_1990.tif",
  overwrite = TRUE
)
writeRaster(tavg_europe_2012, "external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tavg_2012.tif",
  overwrite = TRUE
)
writeRaster(deltaT_europe_90_12, "own datasets/deltaT_2.5min_20y.tif",
  overwrite = TRUE
)



# ___________________________________________________________________________
## Fitting on EBBA Grid ####

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp") # without Russia, Ukraine, Belarus, Moldavia, Turkey, Azerbaijan, Armenia, Georgia, west-Kazacstan

# Extract mean temperature change for each 50km grid cell
grid_50km$deltaT <- extract(deltaT_90_12, grid_50km, fun = mean, na.rm = TRUE)[, 2]

# Convert to sf
grid_50km_sf <- st_as_sf(grid_50km)

head(grid_50km_sf)


write_csv(st_drop_geometry(grid_50km_sf), "own datasets/deltaT_50km_20y.csv")


# Plot with ggplot
plot <- ggplot() +
  geom_sf(data = grid_50km_sf, aes(fill = deltaT)) +
  scale_fill_viridis_c(name = "Temp Change (°C)") +
  theme_minimal()
plot

ggsave("plots/temperature/deltaT_50km_20y.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# ___________________________________________________________________________
# Calculating CCV ####


# ___________________________________________________________________________
## Importing Datasets ####

tavg_1990 <- raster("external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tavg_1990.tif")
tavg_2012 <- raster("external datasets/WorldClim/1. Historical/20y avg (1990-2012)/tavg_2012.tif")



# ___________________________________________________________________________
## Calculating temporal and spatial gradient ####

# First, the temporal gradient is calculated.
# The future prediction and the historical data is subtracted, and thereafter divided by the number of years the two layers span across
# In this case a total of 80 years
Temporal <- (tavg_2012 - tavg_1990) / 22

# Then the spaital gradient is estimated using the slope-function from 'terra'
Spatial <- terrain(tavg_1990, opt = "slope", neighbors = 8, unit = "degrees")



# ___________________________________________________________________________
## Custom functions ####

# Climate change Velocity is estimated by dividing the two gradients.
# Function for dividing the two gradients with no limiter on the spatial layer
function_Nolimit <- function(a, b) {
  ifelse(a == 0 | b == 0, 0, a / b)
}

# Function for dividing the two gradients with a limiter on the spatial layer
# The threshold value indicates where the spatial layers shpuld be cut off,
# meaning that any value below the threshold is counted as 0, and therefore the
# division will result in 0.
# NOTE: the Threshold has not been probably adjusted.
Threshold <- 0.000001
function_Withlimit <- function(a, b) {
  ifelse(a == 0 | b < Threshold, 0, a / b)
}



# ___________________________________________________________________________
## Creating CCV layers ####

# Apply the functions to the two rasters
CV_Nolimit <- overlay(Temporal, Spatial, fun = function_Nolimit)
CV_WithLimit <- overlay(Temporal, Spatial, fun = function_Withlimit)

plot(CV_Nolimit)
plot(CV_WithLimit)


# Log transdorming both layers for better visualization
ClimateVelocity_Nolimit <- CV_Nolimit
Log_ClimateVelocity_Nolimit <- log(CV_Nolimit)

plot(ClimateVelocity_Nolimit)
plot(Log_ClimateVelocity_Nolimit)


ClimateVelocity_WithLimit <- CV_WithLimit
Log_ClimateVelocity_WithLimit <- log(CV_WithLimit)

plot(ClimateVelocity_WithLimit)
plot(Log_ClimateVelocity_WithLimit)



# ___________________________________________________________________________
## Fitting to 50km grid ####

# grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1.shp") # with islands
# grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslands.shp") # without islands
grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp") # without Russia, Ukraine, Belarus, Moldavia, Turkey, Azerbaijan, Armenia, Georgia, west-Kazacstan


summary(Log_ClimateVelocity_WithLimit)
str(Log_ClimateVelocity_WithLimit)

ClimateVelocity_WithLimit_SpatRaster <- rast(ClimateVelocity_WithLimit)
Log_ClimateVelocity_WithLimit_SpatRaster <- rast(Log_ClimateVelocity_WithLimit)

# Extract mean temperature change for each 50km grid cell
grid_50km$ccv <- extract(ClimateVelocity_WithLimit_SpatRaster, grid_50km, fun = mean, na.rm = TRUE)[, 2]
grid_50km$log_ccv <- extract(Log_ClimateVelocity_WithLimit_SpatRaster, grid_50km, fun = mean, na.rm = TRUE)[, 2]

summary(grid_50km) # have to get rid of the -inf values in log_ccv

grid_50km$log_ccv[is.infinite(grid_50km$log_ccv)] <- NA
summary(grid_50km)




# Convert to sf for ggplot
grid_50km_sf <- st_as_sf(grid_50km)

head(grid_50km_sf)


write_csv(st_drop_geometry(grid_50km_sf), "own datasets/ccv_50km_20y.csv")
# write_csv(st_drop_geometry(grid_50km_sf), "own datasets/ccv_50km_noEast.csv")


# Plot with ggplot
plot <- ggplot() +
  geom_sf(data = grid_50km_sf, aes(fill = ccv)) +
  scale_fill_viridis_c(name = "Climate Change Velocoity (km/yr)") +
  theme_minimal()
plot

ggsave("plots/temperature/ccv_50km_20y.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# Plot with ggplot
plot <- ggplot() +
  geom_sf(data = grid_50km_sf, aes(fill = log_ccv)) +
  scale_fill_viridis_c(name = "Log Climate Change Velocoity (km/yr)") +
  theme_minimal()
plot

ggsave("plots/temperature/log_ccv_50km_20y.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# ___________________________________________________________________________
## Exporting data ####

writeRaster(ClimateVelocity_Nolimit,
  "own datasets/climate_velocity_nolimit_20y.tif",
  filetype = "GTiff",
  overwrite = TRUE
)

writeRaster(Log_ClimateVelocity_Nolimit,
  "own datasets/log_climate_velocity_nolimit_20y.tif",
  filetype = "GTiff",
  overwrite = TRUE
)


writeRaster(ClimateVelocity_WithLimit,
  "own datasets/climate_velocity_withlimit_20y.tif",
  filetype = "GTiff",
  overwrite = TRUE
)

writeRaster(Log_ClimateVelocity_WithLimit,
  "own datasets/log_climate_velocity_withlimit_20y.tif",
  filetype = "GTiff",
  overwrite = TRUE
)




# ___________________________________________________________________________
# Histograms ####

ccv_50km_20y <- read_csv("own datasets/ccv_50km_20y.csv")
deltaT_50km_20y <- read_csv("own datasets/deltaT_50km_20y.csv")

# delta temperature
png("plots/temperature/hist_delta_temperature_20y.png",
  width = 800,
  height = 600,
  res = 100
)
hist(deltaT_50km_20y$deltaT,
  breaks = 50,
  col = "lightblue",
  main = "Distr. of Temperature Change (50km)"
)
abline(v = mean(deltaT_50km_20y$deltaT, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(deltaT_50km_20y$deltaT, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
  legend = c("Mean", "Median"),
  col = c("red", "blue"),
  lty = c(2, 3),
  lwd = 2,
  bg = "white"
)
dev.off()


# climate change velocity
png("plots/temperature/hist_climate_change_velocity_20y.png",
  width = 800,
  height = 600,
  res = 100
)
hist(ccv_50km_20y$ccv,
  breaks = 50,
  col = "lightblue",
  main = "Distr. of climate change velocity (50km)"
)
abline(v = mean(ccv_50km_20y$ccv, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(ccv_50km_20y$ccv, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
  legend = c("Mean", "Median"),
  col = c("red", "blue"),
  lty = c(2, 3),
  lwd = 2,
  bg = "white"
)
dev.off()

png("plots/temperature/hist_log_climate_change_velocity_20y.png",
  width = 800,
  height = 600,
  res = 100
)
hist(ccv_50km_20y$log_ccv,
  breaks = 50,
  col = "lightblue",
  main = "Distr. of climate change velocity (log) (50km)"
)
abline(v = mean(ccv_50km_20y$log_ccv, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(ccv_50km_20y$log_ccv, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
  legend = c("Mean", "Median"),
  col = c("red", "blue"),
  lty = c(2, 3),
  lwd = 2,
  bg = "white"
)
dev.off()
