#___________________________________________________________________________
# Loading Packages ####

library(tidyverse)
library(terra)
library(data.table)
library(ggplot2)
library(sf)
library(MuMIn)
library(MASS)




#___________________________________________________________________________
# Importing Data ####


# climate change data

deltaT_90_16 = rast("own datasets/deltaT_europe_1990_2016.tif")


# species occurence data

m_b2000 = fread("own datasets/m_b2000_cleaned.csv")
m_a2015 = fread("own datasets/m_a2015_cleaned.csv")

str(m_b2000)


ebba1 = fread("own datasets/ebba1_cleaned.csv")
ebba2 = fread("own datasets/ebba2_cleaned.csv")

str(ebba1)

# EBBA 50×50 km grid shapefile
#grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1.shp") # with islands
grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslands.shp") # without islands
str(grid_50km)



#___________________________________________________________________________
# Birds ####

#___________________________________________________________________________
## Rasterizing Species Occurrences ####

# EBBA2

ebba2_sf <- grid_50km %>%
  left_join(ebba2, by = "cell50x50")

sum(is.na(ebba2_sf$birdlife_code))  # Count unmatched grid cells

# Convert to data.table if not already
ebba2_dt= setDT(ebba2_sf)


# number of occurrences per grid cell
occ_ebba2 <- ebba2_dt[, .N, by = cell50x50]
head(occ_ebba2)
sum(occ_ebba2$N)/nrow(ebba2_dt)



# EBBA1

ebba1_sf <- st_as_sf(ebba1, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

ebba1_sf <- st_transform(ebba1_sf, st_crs(grid_50km))

ebba1_sf <- st_join(ebba1_sf, grid_50km, left = FALSE)

ebba1_dt <- as.data.table(ebba1_sf)  # Convert to data.table

head(ebba1_dt)


# number of occurrences per grid cell
occ_ebba1 <- ebba1_dt[, .N, by = cell50x50]
head(occ_ebba1)
sum(occ_ebba1$N)/nrow(ebba1_dt)



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

# Merge dataframes by "cell50x50"
delta_SR_b <- full_join(ebba2_SR, ebba1_SR, by = "cell50x50", suffix = c("_ebba2", "_ebba1")) %>%
  # Compute the difference (NA is preserved)
  mutate(delta_SR = SR_ebba2 - SR_ebba1) %>%
  # Arrange by "cell50x50"
  arrange(cell50x50)

# View result
head(delta_SR_b)


# add occurrence count per cell (kind of obsolete with birds)
delta_SR_b1 <- merge(delta_SR_b, occ_ebba1[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b1)[ncol(delta_SR_b1)] <- "occ_ebba1"
delta_SR_b1 <- merge(delta_SR_b1, occ_ebba2[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b1)[ncol(delta_SR_b1)] <- "occ_ebba2"
head(delta_SR_b1)


# order columns
delta_SR_b1 <- delta_SR_b1[, c("cell50x50",
                               "occ_ebba1", "SR_ebba1", 
                               "occ_ebba2", "SR_ebba2",
                               "delta_SR")]
head(delta_SR_b1)

write.csv(delta_SR_b1, "own datasets/deltaSR_birds_50km.csv", row.names = FALSE)


# plotting delta SR

delta_SR_b_sf <- merge(grid_ebba2_SR, delta_SR_b1[, c("cell50x50", "delta_SR")], by = "cell50x50", all.x = TRUE)

plot <- ggplot(delta_SR_b_sf) +
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

#sp_points_m_b2000 <- vect(m_b2000, geom = c("decimalLongitude", "decimalLatitude"), crs = crs(deltaT_90_16))
#sp_points_m_a2015 <- vect(m_a2015, geom = c("decimalLongitude", "decimalLatitude"), crs = crs(deltaT_90_16))

# Rasterize species occurrences to match deltaT grid, counting occurrences per cell
#SR_m_b2000 <- rasterize(sp_points_m_b2000, deltaT_90_16,
#                        field = "species",
#                        fun = function(x, ...) length(unique(x)))
#SR_m_a2015 <- rasterize(sp_points_m_a2015, deltaT_90_16,
#                        field = "species",
#                        fun = function(x, ...) length(unique(x)))


# Save or plot the result
#plot(SR_m_b2000)
#plot(SR_m_a2015)

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


# number of occurrences per grid cell
occ_m_b2000 = m_b2000_dt[, .N, by = cell50x50]
occ_m_a2015 = m_a2015_dt[, .N, by = cell50x50]
head(occ_m_b2000)
head(occ_m_a2015)
sum(occ_m_b2000$N)/nrow(m_b2000_dt)
sum(occ_m_a2015$N)/nrow(m_a2015_dt)



#cell = "26SLH4"

#test1.1 = m_b2000_dt |> 
#  filter(cell50x50 == cell)
#check.levels(test1.1$species)
#length(unique(test1.1$species))

#test2.1 = delta_SR_m1 |> 
#  filter(cell50x50 == cell)
#test2$SR_m_b2000

#test3.1 = data |> 
#  filter(cell50x50 == cell)
#test3.1$SR_m_b2000


#test1.2 = m_a2015_dt |> 
#  filter(cell50x50 == cell)
#check.levels(test1.2$species)
#length(unique(test1.2$species))

#test2.2 = delta_SR_m1 |> 
#  filter(cell50x50 == cell)
#test2.2$SR_m_a2015

#test3.2 = data |> 
#  filter(cell50x50 == cell)
#test3.2$SR_m_a2015




#___________________________________________________________________________
## Calculating Species Richness ####

# following EBBA2 raster

SR_m_b2000 <- m_b2000_dt[, .(SR = uniqueN(species)), by = cell50x50]
SR_m_a2015 <- m_a2015_dt[, .(SR = uniqueN(species)), by = cell50x50]

head(SR_m_b2000)
head(SR_m_a2015)


write.csv(SR_m_b2000, "own datasets/SR_m_b2000_50km.csv", row.names = FALSE)
write.csv(SR_m_a2015, "own datasets/SR_m_a2015_50km.csv", row.names = FALSE)



# Visualization

grid_m_b2000_SR <- grid_50km %>%
  left_join(SR_m_b2000, by = "cell50x50")
grid_m_a2015_SR <- grid_50km %>%
  left_join(SR_m_a2015, by = "cell50x50")

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

#delta_SR_m = SR_m_a2015 - SR_m_b2000
# delta_SR_m = SR_m_a2015 - SR_m_b2000 / 15 #per year? Is that even possible when it is aggregated like that?

#plot(delta_SR_m)

#writeRaster(delta_SR_m, "species_richness.tif")

# Convert SpatRaster to a data frame
#delta_df_m <- as.data.frame(delta_SR_m, xy = TRUE)


# Plot using ggplot
#plot = ggplot(delta_df_m) +
#  geom_tile(aes(x = x, y = y, fill = species)) +  # Use 'species' for fill
#  scale_fill_viridis_c(na.value = "grey", option = "C") +  
#  theme_minimal() +
#  labs(title = "ΔSR Mammals (2015 - 1990)",
#       fill = "Δ SR")

# Display the plot
#plot

# Save the plot
#ggsave("plots/deltaSR_mammals_2.5min.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



### EBBA Raster ####

# Merge dataframes by "cell50x50"
delta_SR_m <- full_join(SR_m_a2015, SR_m_b2000, by = "cell50x50", suffix = c("_m_a2015", "_m_b2000")) %>%
  # Compute the difference (NA is preserved)
  mutate(delta_SR = SR_m_a2015 - SR_m_b2000) %>%
  # Arrange by "cell50x50"
  arrange(cell50x50)

# View result
head(delta_SR_m)


# Merge each occurrence dataset to add the respective column
delta_SR_m1 <- merge(delta_SR_m, occ_m_b2000[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m1)[ncol(delta_SR_m1)] <- "occ_m_b2000"
delta_SR_m1 <- merge(delta_SR_m1, occ_m_a2015[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m1)[ncol(delta_SR_m1)] <- "occ_m_a2015"
delta_SR_m1 = delta_SR_m1 |> 
  mutate(delta_occ_m = occ_m_a2015 - occ_m_b2000)

head(delta_SR_m1)

delta_SR_m1 <- delta_SR_m1[, c("cell50x50",
                               "occ_m_b2000", "SR_m_b2000", 
                               "occ_m_a2015", "SR_m_a2015",
                               "delta_SR", "delta_occ_m")]
head(delta_SR_m1)



write.csv(delta_SR_m1, "own datasets/deltaSR_mammals_50km.csv", row.names = FALSE)


# plotting delta SR
delta_SR_m_sf <- merge(grid_m_a2015_SR, delta_SR_m1[, c("cell50x50", "delta_SR")], by = "cell50x50", all.x = TRUE)

plot <- ggplot(delta_SR_m_sf) +
  geom_sf(aes(fill = delta_SR), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey", option = "C") +  # Handles NA values
  theme_minimal() +
  labs(title = "ΔSR Mammals (before 2000 - after 2015)",
       fill = "Δ SR")

plot

ggsave("plots/deltaSR_mammals_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
# Combining Data ####

deltaSR_birds_50km = read.csv("own datasets/deltaSR_birds_50km.csv")
deltaSR_mammals_50km = read.csv("own datasets/deltaSR_mammals_50km.csv")
deltaT_50km = read.csv("own datasets/deltaT_50km.csv")


# Merging the first two data frames by 'cell50x50'
merged_df <- merge(deltaSR_mammals_50km, deltaSR_birds_50km, by = "cell50x50", all = TRUE)

# Merging the result with the third data frame ('deltaT_50km')
merged_df <- merge(merged_df, deltaT_50km, by = "cell50x50", all = TRUE)

# Renaming the delta_SR columns
colnames(merged_df)[which(names(merged_df) == "delta_SR.x")] <- "deltaSR_mammals"
colnames(merged_df)[which(names(merged_df) == "delta_SR.y")] <- "deltaSR_birds"


# Display the resulting data frame
head(merged_df)


# Reorder columns to place occurrence columns before the corresponding SR columns
ordered_cols <- c("cell50x50",
                  "occ_ebba1", "SR_ebba1", 
                  "occ_ebba2", "SR_ebba2",
                  "deltaSR_birds",
                  "occ_m_b2000", "SR_m_b2000", 
                  "occ_m_a2015", "SR_m_a2015",
                  "deltaSR_mammals", "delta_occ_m",
                  "deltaT")

merged_df <- merged_df[, ordered_cols]

# Check result
head(merged_df)

write.csv(merged_df, "own datasets/analysis_data_50km.csv", row.names = FALSE)



#___________________________________________________________________________
# Histograms ####


# Create a data frame for plotting
mammal_data <- data.frame(
  mammal_dataset = factor(c("before 2000", "after 2015"), levels = c("before 2000", "after 2015")), # Set order
  number_of_occurences = c(nrow(m_b2000), nrow(m_a2015))
)

# Create the bar plot
ggplot(mammal_data, aes(x = mammal_dataset, y = number_of_occurences)) +
  geom_col(fill = "steelblue") +  
  labs(x = "Time Period", y = "Number of Occurrences") +
  theme_minimal()


data = read.csv("own datasets/analysis_data_50km.csv")

# Number of occurrences birds
png("plots/hist_Occ_birds_ebba1.png",
    width = 800,
    height = 600,
    res = 100)
hist(occ_ebba1$N,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in EBBA1 Birds (50km)")
dev.off()

png("plots/hist_Occ_birds_ebba2.png",
    width = 800,
    height = 600,
    res = 100)
hist(occ_ebba2$N,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in EBBA2 Birds (50km)")
dev.off()


# Number of occurrences mammals
png("plots/hist_Occ_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_ebba1,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in Mammals before 2000 (50km)")
dev.off()

png("plots/hist_Occ_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_ebba1,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in Mammals after 2015 (50km)")
dev.off()


# SR birds
png("plots/hist_SR_birds_ebba1.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_ebba1,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in EBBA1 Birds (50km)")
dev.off()

png("plots/hist_SR_birds_ebba2.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_ebba2,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in EBBA2 Birds (50km)")
dev.off()


# SR mammals
png("plots/hist_SR_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_m_b2000,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in GBIF Mammals before 2015 (50km)")
dev.off()

png("plots/hist_SR_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$SR_m_a2015,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in GBIF Mammals after 2015 (50km)")
dev.off()


# delta SR
png("plots/hist_deltaSR_birds.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$deltaSR_birds,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness Change in EBBA Birds (50km)")
dev.off()

png("plots/hist_deltaSR_mammals.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$deltaSR_mammals,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness Change in GBIF Mammals (50km)")
dev.off()


# temperature
#hist(merged_df$,
#     breaks = 50,
#     col = "lightblue",
#     main = "Distribution of Species Richness in GBIF Mammals after 2015")

#hist(merged_df$,
#     breaks = 50,
#     col = "lightblue",
#     main = "Distribution of Species Richness in GBIF Mammals after 2015")


# delta temperature
png("plots/hist_delta_temperature.png",
    width = 800,
    height = 600,
    res = 100)
hist(merged_df$deltaT,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Temperature Change (50km)")
dev.off()


# temporal range of GBIF data
png("plots/hist_gbif_temporal_range.png",
    width = 800,
    height = 600,
    res = 100)
hist(m_b2000$year,
     breaks = 50,
     col = "lightblue",
     main = "Temporal Distr. of Mammal occurrences (50km)")
dev.off()




#___________________________________________________________________________
# Modelling ####

data = read.csv("own datasets/analysis_data_50km.csv")

#___________________________________________________________________________
## Scatterplot ####

long_data <- data %>%
  gather(key = "Species", value = "delta_SR", deltaSR_mammals, deltaSR_birds)

# Plotting
plot = ggplot(long_data, aes(x = deltaT, y = delta_SR, color = Species)) +
  geom_point(alpha = 0.2,
             shape = 16,
             size = 2) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Temperature Change (ΔT)",
       y = "Species Richness Change (ΔSR)",
       color = "Organism Group",) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  scale_color_discrete(labels = c("Birds", "Mammals"))

plot

ggsave("plots/regline_SR_mammals_birds~deltaT.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

range(data$deltaT)
range(data$deltaSR_mammals, na.rm = TRUE)
range(data$deltaSR_birds, na.rm = TRUE)



# log-log axes?
plot = ggplot(long_data, aes(x = deltaT, y = delta_SR, color = Species)) +
  geom_point(alpha = 0.2,
             shape = 16,
             size = 2,
             stroke = 0) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Temperature Change (ΔT)",
       y = "Species Richness Change (ΔSR)",
       color = "Organism Group") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) +
  scale_color_discrete(labels = c("Birds", "Mammals")) +
  scale_x_log10() + 
  scale_y_log10()

plot




#___________________________________________________________________________
## Linear Models ####

lm1.1 = lm(deltaSR_birds  ~ deltaT,
         data = data)
summary(lm1.1)
par(mfrow = c(2, 2))
plot(lm1.1)

lm1.2 = lm(log(deltaSR_birds + 0.01) ~ log(deltaT + 0.01),
           data = data)
summary(lm1.2)
par(mfrow = c(2, 2))
plot(lm1.2)

summary(data$deltaSR_birds)
summary(data$deltaT)


lm2.1 = lm(deltaSR_mammals  ~ deltaT,
         data = data)
summary(lm2.1)
par(mfrow = c(2, 2))
plot(lm2.1)

lm2.2 = lm(deltaSR_mammals  ~ deltaT + delta_occ_m,
         data = data)
summary(lm2.2)
par(mfrow = c(2, 2))
plot(lm2.2)


glm1 = glm(deltaSR_birds  ~ deltaT,
           data = data,
           family = gaussian) # which family???
summary(glm1)


glm2.1 = glm(deltaSR_mammals  ~ deltaT,
           data = data,
           family = gaussian) # which family???
summary(glm2.1)

glm2.2 = glm(deltaSR_mammals  ~ deltaT + delta_occ_m,
           data = data,
           family = gaussian) # which family???
summary(glm2.2)


model.sel(lm1, lm2.1, lm2.2, glm1, glm2.1, glm2.2, rank = "AICc")



range.x <- range(data$Delta_T)
new.x <- data.frame(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm1,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1),
         lower = (fit - 1.96*se.fit), 
         point.estimate = (fit), 
         upper = (fit + 1.96*se.fit))

plot <- ggplot(data, aes(x = Delta_T, y = Species_Richness)) +
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


