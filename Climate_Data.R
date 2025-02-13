
#___________________________________________________________________________
# Loading Packages ####

library(tidyverse)
library(terra)




#___________________________________________________________________________
# Importing Datasets ####

## 1985-1995 ####

tifs_85_95_tmin = list.files(path = "external datasets/WorldClim/1. Historical/wc2.1_cruts4.06_2.5m_tmin_1985-1995",
                             pattern = "\\.tif$",
                             full.names = TRUE)

tifs_85_95_tmax = list.files(path = "external datasets/WorldClim/1. Historical/wc2.1_cruts4.06_2.5m_tmax_1985-1995",
                             pattern = "\\.tif$",
                             full.names = TRUE)


# Load all rasters into a SpatRaster collection
climateH_85_95_tmin <- rast(tifs_85_95_tmin)

climateH_85_95_tmax <- rast(tifs_85_95_tmax)



## 2012-2021 ####

tifs_12_21_tmin = list.files(path = "external datasets/WorldClim/1. Historical/wc2.1_cruts4.06_2.5m_tmin_2012-2021",
                             pattern = "\\.tif$",
                             full.names = TRUE)

tifs_12_21_tmax = list.files(path = "external datasets/WorldClim/1. Historical/wc2.1_cruts4.06_2.5m_tmax_2012-2021",
                             pattern = "\\.tif$",
                             full.names = TRUE)


# Load all rasters into a SpatRaster collection
climateH_12_21_tmin <- rast(tifs_12_21_tmin)

climateH_12_21_tmax <- rast(tifs_12_21_tmax)




#___________________________________________________________________________
# Deriving tavg ####

## 1985-1995l ####

climateH_85_95_tavg = mean(climateH_85_95_tmin, climateH_85_95_tmax)
climateH_tavg_1 = mean(climateH_85_95_tavg)

plot(climateH_tavg_1)



## 2012-2021 ####

climateH_12_21_tavg = mean(climateH_12_21_tmin, climateH_12_21_tmax)
climateH_tavg_2 = mean(climateH_12_21_tavg)

plot(climateH_tavg_2)




#___________________________________________________________________________
# Calculating ΔT ####

# 1990-2020

deltaT_90_16 = climateH_tavg_2 - climateH_tavg_1
plot(deltaT_90_16)



#___________________________________________________________________________
# Masking for Europe ####

# 1990-2020

europe_shp <- vect("external datasets/Europe Shapefile/europe_no_islands.shp")
europe_shp <- project(europe_shp, crs(deltaT_90_16))

deltaT_europe_90_16 <- crop(deltaT_90_16, europe_shp)  # Crop to bounding box of Europe
deltaT_europe_90_16 <- mask(deltaT_europe_90_16, europe_shp)  # Mask to exact shape

png("plots/deltaT_europe_1990_2016.png",
    width = 800,
    height = 600,
    res = 100)
plot(deltaT_europe_90_16)
dev.off()



#___________________________________________________________________________
# Exporting Data ####

writeRaster(deltaT_europe_90_16, "own datasets/deltaT_europe_1990_2016.tif", overwrite=TRUE)


#___________________________________________________________________________
# Fitting on EBBA Grid ####

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1.shp")

if (!identical(crs(deltaT_90_16), crs(grid_50km))) {
  deltaT_90_16 <- project(deltaT_90_16, grid_50km)
}

climate_resampled <- resample(deltaT_90_16, grid_50km, method="bilinear")  # Bilinear for continuous data

climate_final <- mask(climate_resampled, grid_50km)
