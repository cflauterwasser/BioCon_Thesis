#___________________________________________________________________________
# Loading Packages ####

library(tidyverse)
library(terra)
library(data.table)
library(ggplot2)
library(sf)




#___________________________________________________________________________
# Importing Data ####


# climate change data

deltaT_90_16 = rast("own datasets/deltaT_europe_1990_2016.tif")
deltaT_90_30 = rast("own datasets/deltaT_europe_1990_2030.tif")


# species occurence data

m_b2000 = fread("own datasets/m_b2000_cleaned.csv")
m_a2015 = fread("own datasets/m_a2015_cleaned.csv")

str(m_b2000)


ebba1 = fread("own datasets/ebba1_cleaned.csv")
ebba2 = fread("own datasets/ebba2_cleaned.csv")


# EBBA 50×50 km grid shapefile
grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1.shp")
str(grid_50km)



#___________________________________________________________________________
# Birds ####

#___________________________________________________________________________
## Rasterizing Species Occurrences ####

# EBBA2

ebba2_sf <- grid_50km %>%
  left_join(ebba2, by = "cell50x50")

sum(is.na(ebba2_sf$birdlife_code))  # Count unmatched grid cells


# EBBA1

ebba1_sf <- st_as_sf(ebba1, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

ebba1_sf <- st_transform(ebba1_sf, st_crs(grid_50km))

ebba1_sf <- st_join(ebba1_sf, grid_50km, left = FALSE)

ebba1_dt <- as.data.table(ebba1_sf)  # Convert to data.table




#___________________________________________________________________________
## Calculating Species Richness ####


### EBBA1 ####

ebba1_SR <- ebba1_dt[, .(SR = uniqueN(species)), by = cell50x50]

head(ebba1_SR)
summary(ebba1_SR$SR)

write.csv(ebba1_SR, "own datasets/SR_ebba1_50km.csv", row.names = FALSE)


# Visualization

grid_ebba1_SR <- grid_50km %>%
  left_join(ebba1_SR, by = "cell50x50")

head(grid_ebba1_SR)


summary(grid_ebba1_SR$SR)
hist(grid_ebba1_SR$SR, breaks = 50, col = "lightblue", main = "Distribution of Species Richness in EBBA1")


plot = ggplot(grid_ebba1_SR) +
  geom_sf(aes(fill = SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(title = "EBBA1 Species Richness",
       fill = "SR")

plot

ggsave("plots/SR_birds_ebba1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



### EBBA2 ####

ebba2_SR <- ebba2[, .(SR = uniqueN(Species)), by = cell50x50]

head(ebba2_SR)

write.csv(ebba2_SR, "own datasets/SR_ebba2_50km.csv", row.names = FALSE)


# Visualization

grid_ebba2_SR <- grid_50km %>%
  left_join(ebba2_SR, by = "cell50x50") %>%
  mutate(SR = replace_na(SR, 0))  # Fill NA with 0 (no species)


summary(grid_ebba2_SR$SR)
hist(grid_ebba2_SR$SR, breaks = 50, col = "lightblue", main = "Distribution of Species Richness in EBBA2")


plot = ggplot(grid_ebba2_SR) +
  geom_sf(aes(fill = SR), color = "black", size = 0.1) +
  scale_fill_viridis_c(na.value = "grey") +
  theme_minimal() +
  labs(title = "EBBA2 Species Richness",
       fill = "SR")

plot

ggsave("plots/SR_birds_ebba2.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
## Calculating Delta Species Richness ####

# Step 1: Calculate Delta Species Richness using st_join
delta_SR <- grid_ebba1_SR %>%
  select(cell50x50, SR_ebba1 = SR) %>%
  st_join(select(grid_ebba2_SR, cell50x50, SR_ebba2 = SR), join = st_intersects) %>%
  mutate(delta_SR = replace_na(SR_ebba2, 0) - SR_ebba1)  # Replace NA with 0 for delta calculation
  #mutate(delta_SR = replace_na(SR_ebba2, 0) - SR_ebba1 / (mean(2013,2018)-1985) #per year? Is that even possible when it is aggregated like that?

# Step 2: Check the summary of delta species richness
summary(delta_SR$delta_SR)

# Step 3: Plot the Delta Species Richness
plot <- ggplot(delta_SR) +
  geom_sf(aes(fill = delta_SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey", option = "C") +  # Handles NA values
  theme_minimal() +
  labs(title = "ΔSR Birds (EBBA2 - EBBA1)",
       fill = "Δ SR")

# Display the plot
plot

# Save the plot
ggsave("plots/deltaSR_birds.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
# Mammals ####

#___________________________________________________________________________
## Rasterizing Species Occurrences ####


#following WorldClim raster

sp_points_m_b2000 <- vect(m_b2000, geom = c("decimalLongitude", "decimalLatitude"), crs = crs(deltaT_90_16))
sp_points_m_a2015 <- vect(m_a2015, geom = c("decimalLongitude", "decimalLatitude"), crs = crs(deltaT_90_16))

# Rasterize species occurrences to match deltaT grid, counting occurrences per cell
SR_m_b2000 <- rasterize(sp_points_m_b2000, deltaT_90_16,
                        field = "species",
                        fun = function(x, ...) length(unique(x)))
SR_m_a2015 <- rasterize(sp_points_m_a2015, deltaT_90_16,
                        field = "species",
                        fun = function(x, ...) length(unique(x)))


# Save or plot the result
plot(SR_m_b2000)
plot(SR_m_a2015)

#writeRaster(SR_m_b2000, "species_richness.tif")
#writeRaster(SR_m_a2015, "species_richness.tif")



# following EBBA2 raster

m_b2000_sf <- st_as_sf(m_b2000, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
m_a2015_sf <- st_as_sf(m_a2015, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

m_b2000_sf <- st_transform(m_b2000_sf, st_crs(grid_50km))
m_a2015_sf <- st_transform(m_a2015_sf, st_crs(grid_50km))

m_b2000_sf <- st_join(m_b2000_sf, grid_50km, left = FALSE)
m_a2015_sf <- st_join(m_a2015_sf, grid_50km, left = FALSE)

m_b2000_dt <- as.data.table(m_b2000_sf)  # Convert to data.table
m_a2015_dt <- as.data.table(m_a2015_sf)




#___________________________________________________________________________
## Calculating Species Richness ####

# following EBBA2 raster

m_b2000_SR <- m_b2000_dt[, .(SR = uniqueN(species)), by = cell50x50]
m_a2015_SR <- m_a2015_dt[, .(SR = uniqueN(species)), by = cell50x50]

head(m_b2000_SR)
head(m_a2015_SR)


write.csv(m_b2000_SR, "own datasets/SR_m_b2000_50km.csv", row.names = FALSE)
write.csv(m_a2015_SR, "own datasets/SR_m_a2015_50km.csv", row.names = FALSE)



# Visualization

grid_m_b2000_SR <- grid_50km %>%
  left_join(m_b2000_SR, by = "cell50x50")
grid_m_a2015_SR <- grid_50km %>%
  left_join(m_a2015_SR, by = "cell50x50")

head(grid_m_b2000_SR)
head(grid_m_a2015_SR)


summary(grid_m_b2000_SR$SR)
hist(grid_m_b2000_SR$SR, breaks = 50, col = "lightblue", main = "Distribution of Species Richness in GBIF Mammal Data before 2000")
summary(grid_m_a2015_SR$SR)
hist(grid_m_a2015_SR$SR, breaks = 50, col = "lightblue", main = "Distribution of Species Richness in GBIF Mammal Data after 2015")


plot = ggplot(grid_m_b2000_SR) +
  geom_sf(aes(fill = SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(title = "Mammal Species Richness Before 2000",
       fill = "SR")

plot

ggsave("plots/SR_mammals_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



plot = ggplot(grid_m_a2015_SR) +
  geom_sf(aes(fill = SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(title = "Mammal Species Richness After 2015",
       fill = "SR")

plot

ggsave("plots/SR_mammals_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
## Calculating Delta Species Richness ####

### Climate Raster ####

delta_SR_m = SR_m_a2015 - SR_m_b2000
# delta_SR_m = SR_m_a2015 - SR_m_b2000 / 15 #per year? Is that even possible when it is aggregated like that?

plot(delta_SR_m)

#writeRaster(delta_SR_m, "species_richness.tif")

# Convert SpatRaster to a data frame
delta_df_m <- as.data.frame(delta_SR_m, xy = TRUE)


# Plot using ggplot
plot = ggplot(delta_df_m) +
  geom_tile(aes(x = x, y = y, fill = species)) +  # Use 'species' for fill
  scale_fill_viridis_c(na.value = "grey", option = "C") +  
  theme_minimal() +
  labs(title = "ΔSR Mammals (2015 - 1990)",
       fill = "Δ SR")

# Display the plot
plot

# Save the plot
ggsave("plots/deltaSR_mammals_2.5min.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


### EBBA Raster ####

# Step 1: Calculate Delta Species Richness using st_join
delta_SR_m <- grid_m_b2000_SR %>%
  select(cell50x50, m_b2000_SR = SR) %>%
  st_join(select(grid_m_a2015_SR, cell50x50, m_b2015_SR = SR), join = st_intersects) %>%
  mutate(delta_SR = replace_na(m_b2015_SR, 0) - m_b2000_SR)  # Replace NA with 0 for delta calculation


# Step 2: Check the summary of delta species richness
summary(delta_SR_m$delta_SR)

# Step 3: Plot the Delta Species Richness
plot <- ggplot(delta_SR_m) +
  geom_sf(aes(fill = delta_SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey", option = "C") +  # Handles NA values
  theme_minimal() +
  labs(title = "ΔSR Mammals (before 2000 - after 2015)",
       fill = "Δ SR")

# Display the plot
plot

# Save the plot
ggsave("plots/deltaSR_mammals_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
# Modelling ####

#___________________________________________________________________________
## Scatterplot ####

# Extract values from both rasters
sr_values <- values(delta_SR_m)
dt_values <- values(deltaT_90_16)

# Remove NA values (only keep pairs where both rasters have data)
valid_idx <- !is.na(sr_values) & !is.na(dt_values)
sr_values <- sr_values[valid_idx]
dt_values <- dt_values[valid_idx]

# Create a data frame
df <- data.frame(Species_Richness = sr_values, Delta_T = dt_values)

# Scatter plot using ggplot2
ggplot(df, aes(x = Delta_T, y = Species_Richness)) +
  geom_point(alpha = 0.3) + 
  labs(x = "Temperature Change (ΔT)", y = "Species Richness Change (ΔSR)") +
  theme_minimal()




#___________________________________________________________________________
## Linear Model ####

lm1 = lm(Species_Richness ~ Delta_T, data = df)
summary(lm1)

range.x <- range(df$Delta_T)
new.x <- data.frame(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm1,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1),
         lower = (fit - 1.96*se.fit), 
         point.estimate = (fit), 
         upper = (fit + 1.96*se.fit))

plot <- ggplot(df, aes(x = Delta_T, y = Species_Richness)) +
  geom_point(colour = "black",
             alpha = 0.1,
             shape = 16,
             size = 0.6) +
  geom_line(aes(x = Delta_T, y = point.estimate),
            data = new.y,
            colour = "darkred",
            linewidth = 1) + 
  geom_ribbon(aes(x = Delta_T, ymin = lower, ymax = upper),
              data = new.y,
              color = NA, fill = "red",
              alpha = 0.5,
              inherit.aes = FALSE) + 
  labs(x = "Temperature Change (ΔT)",
       y = "Species Richness Change (ΔSR)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "plots/regline_deltaSR_mammals~deltaT_90_16.png", plot = plot, width = 7.4, height = 7.4, units = "cm")




# Testing ####

# Define resolutions (numeric only)
res_factors <- c(2, 5, 25)  # Corresponding to ~2km, 10km, 50km grid cell aggregation

# Initialize list to store data for ggplot
plot_data <- list()

for (res_factor in res_factors) {
  
  # Aggregate rasters to new resolution
  deltaT_agg <- aggregate(deltaT_90_16, fact = res_factor, fun = mean, na.rm = TRUE)
  deltaSR_agg <- aggregate(delta_SR_m, fact = res_factor, fun = mean, na.rm = TRUE)
  
  # Extract values and remove NA
  sr_values <- values(deltaSR_agg)
  dt_values <- values(deltaT_agg)
  valid_idx <- !is.na(sr_values) & !is.na(dt_values)
  
  df_temp <- data.frame(
    Delta_T = dt_values[valid_idx],
    Species_Richness = sr_values[valid_idx],
    Resolution = paste0(res_factor * 2, "km")  # Adjust to reflect correct km scale
  )
  
  # Store in list
  plot_data[[length(plot_data) + 1]] <- df_temp
}

# Pan-European mean values (handled separately)
deltaT_agg_europe <- global(deltaT_90_16, "mean", na.rm = TRUE)[[1]]
deltaSR_agg_europe <- global(delta_SR_m, "mean", na.rm = TRUE)[[1]]

df_europe <- data.frame(
  Delta_T = deltaT_agg_europe,
  Species_Richness = deltaSR_agg_europe,
  Resolution = "Europe"
)

# Combine all data into a single dataframe
df_all <- do.call(rbind, c(plot_data, list(df_europe)))

# Scatter plot with ggplot2
ggplot(df_all, aes(x = Delta_T, y = Species_Richness, color = Resolution)) +
  geom_point(alpha = 0.3) +
  labs(x = "Temperature Change (ΔT)", y = "Species Richness Change (ΔSR)", color = "Resolution") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple"))
