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

deltaT_90_16 = rast("own datasets/deltaT_2.5min_20y.tif")


# species occurence data

m_b2000 = fread("own datasets/m_b2000_cleaned.csv")
m_a2015 = fread("own datasets/m_a2015_cleaned.csv")

str(m_b2000)


ebba1 = fread("own datasets/ebba1_cleaned.csv")
ebba2 = fread("own datasets/ebba2_cleaned.csv")

str(ebba1)

# EBBA 50×50 km grid shapefile
#grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1.shp") # with islands
# grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslands.shp") # without islands
grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp") # without Russia, Ukraine, Belarus, Moldavia, Turkey, Azerbaijan, Armenia, Georgia, west-Kazacstan

str(grid_50km)
length(grid_50km$cell50x50)



#___________________________________________________________________________
# BIRDS ####

#___________________________________________________________________________
## Deriving EBBA Change Cells ####

# EBBA Change
change_grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")

ebba_change_cells = change_grid_50km$cell50x50
head(ebba_change_cells)

ebba_change = function(df) {
  df_change = df %>% 
    filter(cell50x50 %in% ebba_change_cells)
  return(df_change)
}


#___________________________________________________________________________
## Rasterizing Species Occurrences ####

# EBBA2

ebba2_sf <- grid_50km %>%
  left_join(ebba2, by = "cell50x50")

sum(is.na(ebba2_sf$birdlife_code))  # Count unmatched grid cells

# Convert to data.table if not already
ebba2_dt= setDT(ebba2_sf)



# EBBA1

rast_occ = function(df, grid) {
  df_sf <- st_as_sf(df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  df_sf <- st_transform(df_sf, st_crs(grid))
  df_sf <- st_join(df_sf, grid, left = FALSE)
  df_dt <- as.data.table(df_sf)  # Convert to data.table
}

ebba1_dt = rast_occ(ebba1, grid_50km)
head(ebba1_dt)



#___________________________________________________________________________
## Calculating Species Richness ####

### EBBA1 ####

SR = function(df){
  df_SR = df[, .(SR = uniqueN(Species)), by = cell50x50]
  return(df_SR)
}

ebba1_SR = SR(ebba1_dt)


head(ebba1_SR)
summary(ebba1_SR$SR)

write.csv(ebba1_SR, "own datasets/SR_ebba1_50km.csv", row.names = FALSE)



# EBBA Change Grid

ebba1_SR_change <- ebba_change(ebba1_SR)
write.csv(ebba1_SR_change, "own datasets/SR_ebba1_50km_change.csv", row.names = FALSE)



# Visualization

plot_SR = function(df) {
  plot_df <- grid_50km %>%
    left_join(df, by = "cell50x50")
  
  plot = ggplot(plot_df) +
    geom_sf(aes(fill = SR), color = "black", size = 0.1) +
    scale_fill_viridis_c(na.value = "grey") +
    theme_minimal() +
    labs(title = paste(deparse(substitute(df))),
         fill = "SR")
  
  plot
  return(plot)
}


plot = plot_SR(ebba1_SR)
plot
ggsave("plots/SR_birds_ebba1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# EBBA Change Grid

plot = plot_SR(ebba1_SR_change)
plot
ggsave("plots/SR_birds_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



### EBBA2 ####

ebba2_SR = SR(ebba2)

head(ebba2_SR)

write.csv(ebba2_SR, "own datasets/SR_ebba2_50km.csv", row.names = FALSE)



# EBBA Change Grid

ebba2_SR_change <- ebba_change(ebba2_SR)
write.csv(ebba2_SR_change, "own datasets/SR_ebba2_50km_change.csv", row.names = FALSE)



# Visualization

plot = plot_SR(ebba2_SR)
plot
ggsave("plots/SR_birds_ebba2.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# EBBA Change Grid

plot = plot_SR(ebba2_SR_change)
plot
ggsave("plots/SR_birds_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
## Calculating Delta Species Richness ####

delta_SR <- function(df_t1, df_t2, suffix1, suffix2) {
  result <- full_join(df_t2, df_t1, by = "cell50x50", suffix = c(suffix2, suffix1)) %>%
    mutate(delta_SR = get(paste0("SR", suffix2)) - get(paste0("SR", suffix1))) %>%
    arrange(cell50x50)
  
  return(result)
}


delta_SR_b = delta_SR(ebba1_SR, ebba2_SR, "_ebba1", "_ebba2")
head(delta_SR_b)



# EBBA Change Grid

delta_SR_b_change = delta_SR(ebba1_SR_change, ebba2_SR_change, "_ebba1", "_ebba2")
head(delta_SR_b_change)


# Plotting Delta SR

plot_deltaSR = function(df) {
  plot_df <- grid_50km %>%
    left_join(df, by = "cell50x50")
  
  plot = ggplot(plot_df) +
    geom_sf(aes(fill = delta_SR), color = "black", size = 0.1) +
    scale_fill_viridis_c(na.value = "grey", option = "C") +
    theme_minimal() +
    labs(title = paste(deparse(substitute(df))),
         fill = "Δ SR")
  
  return(plot)
}


plot = plot_deltaSR(delta_SR_b)
plot
ggsave("plots/deltaSR_birds.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# EBBA Change Grid

plot = plot_deltaSR(delta_SR_b_change)
plot
ggsave("plots/deltaSR_birds_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



## Calculating Occurrence Count ####

occ_count = function(df) {
  result = df[, .N, by = cell50x50]
  return(result)
}

# number of occurrences per grid cell
occ_ebba1 = occ_count(ebba1_dt)

occ_ebba2 = occ_count(ebba2_dt)


# add occurrence count to final df (kind of obsolete with birds)
delta_SR_b1 <- merge(delta_SR_b, occ_ebba1[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b1)[ncol(delta_SR_b1)] <- "occ_ebba1"
delta_SR_b1 <- merge(delta_SR_b1, occ_ebba2[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b1)[ncol(delta_SR_b1)] <- "occ_ebba2"
head(delta_SR_b1)



# EBBA Change Grid

# number of occurrences per grid cell
occ_ebba1_change = ebba_change(occ_ebba1)

occ_ebba2_change = ebba_change(occ_ebba2)


# add occurrence count to final df (kind of obsolete with birds)
delta_SR_b_change1 <- merge(delta_SR_b, occ_ebba1_change[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b_change1)[ncol(delta_SR_b_change1)] <- "occ_ebba1"
delta_SR_b_change1 <- merge(delta_SR_b_change1, occ_ebba2_change[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_b_change1)[ncol(delta_SR_b_change1)] <- "occ_ebba2"
head(delta_SR_b_change1)



## Exporting Data ####

# order columns
delta_SR_b1 <- delta_SR_b1[, c("cell50x50",
                               "occ_ebba1", "SR_ebba1", 
                               "occ_ebba2", "SR_ebba2",
                               "delta_SR")]
head(delta_SR_b1)

delta_SR_b_change1 <- delta_SR_b_change1[, c("cell50x50",
                                             "occ_ebba1", "SR_ebba1", 
                                             "occ_ebba2", "SR_ebba2",
                                             "delta_SR")]
head(delta_SR_b1)


write.csv(delta_SR_b1, "own datasets/deltaSR_birds_50km.csv", row.names = FALSE)

write.csv(delta_SR_b_change1, "own datasets/deltaSR_birds_50km_change.csv", row.names = FALSE)



#___________________________________________________________________________
# MAMMALS ####

#___________________________________________________________________________
## Rasterizing Species Occurrences ####

# following EBBA2 raster

m_b2000_dt = rast_occ(m_b2000, grid_50km)

m_a2015_dt = rast_occ(m_a2015, grid_50km)

m_b2000_dt <- m_b2000_dt %>%
  rename(Species = species)

m_a2015_dt <- m_a2015_dt %>%
  rename(Species = species)


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

### Large Species List ####

# following EBBA2 raster

SR_m_b2000 = SR(m_b2000_dt)
SR_m_a2015 = SR(m_a2015_dt)


head(SR_m_b2000)
head(SR_m_a2015)


write.csv(SR_m_b2000, "own datasets/SR_m_b2000_50km.csv", row.names = FALSE)
write.csv(SR_m_a2015, "own datasets/SR_m_a2015_50km.csv", row.names = FALSE)



# Visualization

plot = plot_SR(SR_m_b2000)
plot
ggsave("plots/SR_mammals_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot = plot_SR(SR_m_a2015)
plot
ggsave("plots/SR_mammals_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



### Reduced Species List ####

m_b2000_dt_red <- fread("own datasets/m_b2000_cleaned_shortened_masked.csv")
m_a2015_dt_red <- fread("own datasets/m_a2015_cleaned_shortened_masked.csv")

m_b2000_dt_red <- m_b2000_dt_red %>%
  rename(Species = species)

m_a2015_dt_red <- m_a2015_dt_red %>%
  rename(Species = species)


SR_m_b2000_red = SR(m_b2000_dt_red)
SR_m_a2015_red <- SR(m_a2015_dt_red)

head(SR_m_b2000_red)
head(SR_m_a2015_red)





# Visualization

plot = plot_SR(SR_m_b2000_red)
plot
ggsave("plots/SR_mammals_b2000_reduced.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot = plot_SR(SR_m_a2015_red)
plot
ggsave("plots/SR_mammals_a2015_reduced.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



#___________________________________________________________________________
## Calculating Delta Species Richness ####

### Large Species List ####

delta_SR_m = delta_SR(SR_m_b2000, SR_m_a2015, "_m_a2015", "_m_b2000")
head(delta_SR_m)



# plotting delta SR

plot = plot_deltaSR(delta_SR_m)
plot
ggsave("plots/deltaSR_mammals_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



### Reduced Species List ####

delta_SR_m_red = delta_SR(SR_m_b2000_red, SR_m_a2015_red, "_m_a2015", "_m_b2000")


# plotting delta SR

plot = plot_deltaSR(delta_SR_m_red)
plot
ggsave("plots/deltaSR_mammals_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



## Calculating Occurrence Count ####

### Large Species List ####

occ_b2000 = occ_count(m_b2000_dt)
occ_a2015 = occ_count(m_a2015_dt)

delta_SR_m1 <- merge(delta_SR_m, occ_b2000[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m1)[ncol(delta_SR_m1)] <- "occ_m_b2000"
delta_SR_m1 <- merge(delta_SR_m1, occ_a2015[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m1)[ncol(delta_SR_m1)] <- "occ_m_a2015"
delta_SR_m1 = delta_SR_m1 |> 
  mutate(delta_occ_m = occ_m_a2015 - occ_m_b2000)

head(delta_SR_m1)


### Reduced Species List ####

occ_b2000 = occ_count(m_b2000_dt_red)
occ_a2015 = occ_count(m_a2015_dt_red)

delta_SR_m_red1 <- merge(delta_SR_m_red, occ_b2000[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m_red1)[ncol(delta_SR_m_red1)] <- "occ_m_b2000"
delta_SR_m_red1 <- merge(delta_SR_m_red1, occ_a2015[, c("cell50x50", "N")], by = "cell50x50", all.x = TRUE)
colnames(delta_SR_m_red1)[ncol(delta_SR_m_red1)] <- "occ_m_a2015"
delta_SR_m_red1 = delta_SR_m_red1 |> 
  mutate(delta_occ_m = occ_m_a2015 - occ_m_b2000)

head(delta_SR_m_red1)



## Exporting Data ####

delta_SR_m1 <- delta_SR_m1[, c("cell50x50",
                               "occ_m_b2000", "SR_m_b2000", 
                               "occ_m_a2015", "SR_m_a2015",
                               "delta_SR", "delta_occ_m")]
head(delta_SR_m1)

write.csv(delta_SR_m1, "own datasets/deltaSR_mammals_50km.csv", row.names = FALSE)



delta_SR_m_red1 <- delta_SR_m_red1[, c("cell50x50",
                                       "occ_m_b2000", "SR_m_b2000", 
                                       "occ_m_a2015", "SR_m_a2015",
                                       "delta_SR", "delta_occ_m")]
head(delta_SR_m_red1)

write.csv(delta_SR_m_red1, "own datasets/deltaSR_mammals_50km.csv", row.names = FALSE)



#___________________________________________________________________________
# SPECIALIST GROUPS ####

#___________________________________________________________________________
## Forest / Agriculture / Generalist ####

m_b2000 = fread("own datasets/m_b2000_cleaned_shortened.csv")
m_a2015 = fread("own datasets/m_a2015_cleaned_shortened.csv")

# Standardize column names
m_b2000 <- m_b2000 %>% rename(Species = species)
m_a2015 <- m_a2015 %>% rename(Species = species)


ebba1 = fread("own datasets/ebba1_cleaned.csv")
ebba2 = fread("own datasets/ebba2_cleaned.csv")

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")

change_grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")
ebba_change_cells = change_grid_50km$cell50x50
head(ebba_change_cells)

specializations = read.csv("own datasets/Habitat_Classes_Complete.csv")


# Read specialization list
specializations <- specializations %>%
  dplyr::select(1,3) %>% 
  rename(Species = Binomial.Name) %>%
  rename(specialization = specialization_FAG)
head(specializations)


# Function to subset data based on specialization and rename datasets
filter_by_specialization <- function(df, df_name, specialization_list, category) {
  df_filtered <- df %>%
    inner_join(filter(specialization_list, specialization == category), by = "Species")  # Keep only matching species
  assign(paste0(category, "_", df_name), df_filtered, envir = .GlobalEnv)  # Save dataset with new name
}


# Process birds (after renaming species column)
bird_datasets <- list(ebba1 = ebba1, ebba2 = ebba2)
for (df_name in names(bird_datasets)) {
  df <- bird_datasets[[df_name]]
  filter_by_specialization(df, df_name, specializations, "forest")
  filter_by_specialization(df, df_name, specializations, "agriculture")
  filter_by_specialization(df, df_name, specializations, "generalist")
}


# Process mammals
mammal_datasets <- list(m_b2000 = m_b2000, m_a2015 = m_a2015)
for (df_name in names(mammal_datasets)) {
  df <- mammal_datasets[[df_name]]
  filter_by_specialization(df, df_name, specializations, "forest")
  filter_by_specialization(df, df_name, specializations, "agriculture")
  filter_by_specialization(df, df_name, specializations, "generalist")
}





#___________________________________________________________________________
### Rasterizing Occurrences ####

# bird data

forest_ebba1_dt = rast_occ(forest_ebba1, grid_50km)
agriculture_ebba1_dt = rast_occ(agriculture_ebba1, grid_50km)
generalist_ebba1_dt = rast_occ(generalist_ebba1, grid_50km)

# additionally applying ebba change grid to ebba datasets

forest_ebba1_dt_change <- ebba_change(forest_ebba1_dt)
agriculture_ebba1_dt_change <- ebba_change(agriculture_ebba1_dt)
generalist_ebba1_dt_change <- ebba_change(generalist_ebba1_dt)

forest_ebba2_dt_change <- ebba_change(forest_ebba2)
agriculture_ebba2_dt_change <- ebba_change(agriculture_ebba2)
generalist_ebba2_dt_change <- ebba_change(generalist_ebba2)


# mammal data

forest_m_b2000_dt = rast_occ(forest_m_b2000, grid_50km)
agriculture_m_b2000_dt = rast_occ(agriculture_m_b2000, grid_50km)
generalist_m_b2000_dt = rast_occ(generalist_m_b2000, grid_50km)

forest_m_a2015_dt = rast_occ(forest_m_a2015, grid_50km)
agriculture_m_a2015_dt = rast_occ(agriculture_m_a2015, grid_50km)
generalist_m_a2015_dt = rast_occ(generalist_m_a2015, grid_50km)



#___________________________________________________________________________
### Species Richness ####

#___________________________________________________________________________
#### Birds ####

# EBBA1 #

SR_forest_ebba1 = SR(forest_ebba1_dt_change)
hist(SR_forest_ebba1$SR)
write.csv(SR_forest_ebba1, "own datasets/SR_forest_ebba1_50km_change.csv", row.names = FALSE)

SR_agriculture_ebba1 = SR(agriculture_ebba1_dt_change)
hist(SR_agriculture_ebba1$SR)
write.csv(SR_agriculture_ebba1, "own datasets/SR_agriculture_ebba1_50km_change.csv", row.names = FALSE)

SR_generalist_ebba1 = SR(generalist_ebba1_dt_change)
hist(SR_generalist_ebba1$SR)
write.csv(SR_generalist_ebba1, "own datasets/SR_generalist_ebba1_50km_change.csv", row.names = FALSE)


SR_forest_ebba2 = SR(forest_ebba2_dt_change)
hist(SR_forest_ebba2$SR)
write.csv(SR_forest_ebba2, "own datasets/SR_forest_ebba2_50km_change.csv", row.names = FALSE)

SR_agriculture_ebba2 = SR(agriculture_ebba2_dt_change)
hist(SR_agriculture_ebba2$SR)
write.csv(SR_agriculture_ebba2, "own datasets/SR_agriculture_ebba2_50km_change.csv", row.names = FALSE)

SR_generalist_ebba2 = SR(generalist_ebba2_dt_change)
hist(SR_generalist_ebba2$SR)
write.csv(SR_generalist_ebba2, "own datasets/SR_generalist_ebba2_50km_change.csv", row.names = FALSE)



# Visualizations


# EBBA1
plot = plot_SR(SR_forest_ebba1)
plot
ggsave("plots/SR_birds_forest_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_agriculture_ebba1)
plot
ggsave("plots/SR_birds_agriculture_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_generalist_ebba1)
plot
ggsave("plots/SR_birds_generalist_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# EBBA2
plot = plot_SR(SR_forest_ebba2)
plot
ggsave("plots/SR_birds_forest_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_agriculture_ebba2)
plot
ggsave("plots/SR_birds_agriculture_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_generalist_ebba2)
plot
ggsave("plots/SR_birds_generalist_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


hist_SR = function(df) {
  png(paste("plots/hist_", paste(deparse(substitute(df))), ".png", sep = ""),
      width = 800,
      height = 600,
      res = 100)
  hist(df$SR,
       col = "lightblue",
       main = paste(deparse(substitute(df))))
  abline(v = mean(df$SR, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  abline(v = median(df$SR, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = c(2, 3),
         lwd = 2,
         bg = "white")
  dev.off()
  hist(df$SR,
       breaks = 50,
       col = "lightblue",
       main = paste(deparse(substitute(df))))
  abline(v = mean(df$SR, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  abline(v = median(df$SR, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = c(2, 3),
         lwd = 2,
         bg = "white")
}

hist_SR(SR_forest_ebba1)
hist_SR(SR_agriculture_ebba1)
hist_SR(SR_generalist_ebba1)

hist_SR(SR_forest_ebba2)
hist_SR(SR_agriculture_ebba2)
hist_SR(SR_generalist_ebba2)




#___________________________________________________________________________
#### Mammals ####

# before 2000
SR_forest_m_b2000 = SR(forest_m_b2000_dt)
hist(SR_forest_m_b2000$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_forest_m_b2000, "own datasets/SR_forest_m_b2000_50km.csv", row.names = FALSE)

SR_agriculture_m_b2000 = SR(agriculture_m_b2000_dt)
hist(SR_agriculture_m_b2000$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_agriculture_m_b2000, "own datasets/SR_agriculture_m_b2000_50km.csv", row.names = FALSE)

SR_generalist_m_b2000 = SR(generalist_m_b2000_dt)
hist(SR_generalist_m_b2000$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_generalist_m_b2000, "own datasets/SR_generalist_m_b2000_50km.csv", row.names = FALSE)


# after 2015
SR_forest_m_a2015 = SR(forest_m_a2015_dt)
hist(SR_forest_m_a2015$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_forest_m_a2015, "own datasets/SR_forest_m_a2015_50km.csv", row.names = FALSE)

SR_agriculture_m_a2015 = SR(agriculture_m_a2015_dt)
hist(SR_agriculture_m_a2015$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_agriculture_m_a2015, "own datasets/SR_agriculture_m_a2015_50km.csv", row.names = FALSE)

SR_generalist_m_a2015 = SR(generalist_m_a2015_dt)
hist(SR_generalist_m_a2015$SR, breaks = 50, col = "lightblue", main = "")
write.csv(SR_generalist_m_a2015, "own datasets/SR_generalist_m_a2015_50km.csv", row.names = FALSE)


# before 2000
plot = plot_SR(SR_forest_m_b2000)
plot
ggsave("plots/SR_mammals_forest_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_agriculture_m_b2000)
plot
ggsave("plots/SR_mammals_agriculture_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_generalist_m_b2000)
plot
ggsave("plots/SR_mammals_generalist_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# after 2015
plot = plot_SR(SR_forest_m_a2015)
plot
ggsave("plots/SR_mammals_forest_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_agriculture_m_a2015)
plot
ggsave("plots/SR_mammals_agriculture_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_generalist_m_a2015)
plot
ggsave("plots/SR_mammals_generalist_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


hist_SR(SR_forest_m_b2000)
hist_SR(SR_agriculture_m_b2000)
hist_SR(SR_generalist_m_b2000)

hist_SR(SR_forest_m_a2015)
hist_SR(SR_agriculture_m_a2015)
hist_SR(SR_generalist_m_a2015)




#___________________________________________________________________________
### Delta Species Richness ####

deltaSR_bird_forest = delta_SR(SR_forest_ebba1, SR_forest_ebba2, "_ebba1", "_ebba2")
deltaSR_bird_agriculture = delta_SR(SR_agriculture_ebba1, SR_agriculture_ebba2, "_ebba1", "_ebba2")
deltaSR_bird_generalist = delta_SR(SR_generalist_ebba1, SR_generalist_ebba2, "_ebba1", "_ebba2")

deltaSR_mammal_forest = delta_SR(SR_forest_m_b2000, SR_forest_m_a2015, "_b2000", "_a2015")
deltaSR_mammal_agriculture = delta_SR(SR_agriculture_m_b2000, SR_agriculture_m_a2015, "_b2000", "_a2015")
deltaSR_mammal_generalist = delta_SR(SR_generalist_m_b2000, SR_generalist_m_a2015, "_b2000", "_a2015")



# Plotting

plot = plot_deltaSR(deltaSR_bird_forest)
plot
ggsave("plots/deltaSR_bird_forest.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR(deltaSR_bird_agriculture)
plot
ggsave("plots/deltaSR_bird_agriculture.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR(deltaSR_bird_generalist)
plot
ggsave("plots/deltaSR_bird_generalist.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_deltaSR(deltaSR_mammal_forest)
plot
ggsave("plots/deltaSR_mammal_forest.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR(deltaSR_mammal_agriculture)
plot
ggsave("plots/deltaSR_mammal_agriculture.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR(deltaSR_mammal_generalist)
plot
ggsave("plots/deltaSR_mammal_generalist.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# fixed spectrum
plot_deltaSR_fixed = function(df, spect_ref) {
  grid_SR_forest_ebba1 <- grid_50km %>%
    left_join(df, by = "cell50x50")
  
  plot = ggplot(grid_SR_forest_ebba1) +
    geom_sf(aes(fill = delta_SR), color = "black", size = 0.1) +
    scale_fill_viridis_c(na.value = "grey", option = "C",
                         limits = c(min(spect_ref, na.rm = TRUE),
                                    max(spect_ref, na.rm = TRUE))) +
    theme_minimal() +
    labs(title = paste(deparse(substitute(df)), "_fixed_spect", sep = ""),
         fill = "delta SR")
  plot
  return(plot)
}


plot = plot_deltaSR_fixed(deltaSR_bird_forest, deltaSR_bird_generalist$delta_SR)
plot
ggsave("plots/deltaSR_bird_forest_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR_fixed(deltaSR_bird_agriculture, deltaSR_bird_generalist$delta_SR)
plot
ggsave("plots/deltaSR_bird_agriculture_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR_fixed(deltaSR_bird_generalist, deltaSR_bird_generalist$delta_SR)
plot
ggsave("plots/deltaSR_bird_generalist_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot = plot_deltaSR_fixed(deltaSR_mammal_forest, deltaSR_mammal_generalist$delta_SR)
plot
ggsave("plots/deltaSR_mammal_forest_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR_fixed(deltaSR_mammal_agriculture, deltaSR_mammal_generalist$delta_SR)
plot
ggsave("plots/deltaSR_mammal_agriculture_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")
plot = plot_deltaSR_fixed(deltaSR_mammal_generalist, deltaSR_mammal_generalist$delta_SR)
plot
ggsave("plots/deltaSR_mammal_generalist_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



hist_deltaSR = function(df) {
  png(paste("plots/hist_", paste(deparse(substitute(df))), ".png", sep = ""),
      width = 800,
      height = 600,
      res = 100)
  hist(df$delta_SR,
       col = "lightblue",
       main = paste(deparse(substitute(df))))
  abline(v = mean(df$delta_SR, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  abline(v = median(df$delta_SR, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = c(2, 3),
         lwd = 2,
         bg = "white")
  dev.off()
  hist(df$delta_SR,
       breaks = 50,
       col = "lightblue",
       main = paste(deparse(substitute(df))))
  abline(v = mean(df$delta_SR, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  abline(v = median(df$delta_SR, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
  legend("topright",
         legend = c("Mean", "Median"),
         col = c("red", "blue"),
         lty = c(2, 3),
         lwd = 2,
         bg = "white")
}


hist_deltaSR(deltaSR_bird_forest)
hist_deltaSR(deltaSR_bird_agriculture)
hist_deltaSR(deltaSR_bird_generalist)

hist_deltaSR(deltaSR_mammal_forest)
hist_deltaSR(deltaSR_mammal_agriculture)
hist_deltaSR(deltaSR_mammal_generalist)




#___________________________________________________________________________
### Joining and Exporting ####

# List all data frames
dfs <- list(
  SR_forest_ebba1 = SR_forest_ebba1,
  SR_agriculture_ebba1 = SR_agriculture_ebba1,
  SR_generalist_ebba1 = SR_generalist_ebba1,
  SR_forest_ebba2 = SR_forest_ebba2,
  SR_agriculture_ebba2 = SR_agriculture_ebba2,
  SR_generalist_ebba2 = SR_generalist_ebba2,
  deltaSR_bird_forest = deltaSR_bird_forest[,c(1,4)],
  deltaSR_bird_agriculture = deltaSR_bird_agriculture[,c(1,4)],
  deltaSR_bird_generalist = deltaSR_bird_generalist[,c(1,4)],
  SR_forest_m_b2000 = SR_forest_m_b2000,
  SR_agriculture_m_b2000 = SR_agriculture_m_b2000,
  SR_generalist_m_b2000 = SR_generalist_m_b2000,
  SR_forest_m_a2015 = SR_forest_m_a2015,
  SR_agriculture_m_a2015 = SR_agriculture_m_a2015,
  SR_generalist_m_a2015 = SR_generalist_m_a2015,
  deltaSR_mammal_forest = deltaSR_mammal_forest[,c(1,4)],
  deltaSR_mammal_agriculture = deltaSR_mammal_agriculture[,c(1,4)],
  deltaSR_mammal_generalist = deltaSR_mammal_generalist[,c(1,4)]
)

# Rename columns based on df name
for (name in names(dfs)) {
  old_col <- setdiff(names(dfs[[name]]), "cell50x50")  # Find the column to rename
  if (length(old_col) > 0) {
    new_col <- ifelse(grepl("_SR$", name), paste0("SR_", sub("_SR$", "", name)), name)
    setnames(dfs[[name]], old = old_col, new = new_col)
  }
}

# Merge all data frames on 'cell50x50'
merged_df <- Reduce(function(x, y) merge(x, y, by = "cell50x50", all = TRUE), dfs)

# Check result
str(merged_df)

write.csv(merged_df, "own datasets/SR_classes_FAG.csv", row.names = F)



#___________________________________________________________________________
## Natural / Human-Dominated ####

specializations = read.csv("own datasets/Habitat_Classes_Complete.csv")

# Standardize column names
#m_b2000 <- m_b2000 %>% rename(Species = species)
#m_a2015 <- m_a2015 %>% rename(Species = species)

# Read specialization list
specializations <- specializations %>%
  dplyr::select(1,2) %>% 
  rename(Species = Binomial.Name) %>%
  rename(specialization = specialization_AN)
head(specializations)



# Process birds (after renaming species column)
bird_datasets <- list(ebba1 = ebba1, ebba2 = ebba2)
for (df_name in names(bird_datasets)) {
  df <- bird_datasets[[df_name]]
  filter_by_specialization(df, df_name, specializations, "natural")
  filter_by_specialization(df, df_name, specializations, "human_dominated")
  
  
  # Process mammals
  mammal_datasets <- list(m_b2000 = m_b2000, m_a2015 = m_a2015)
  for (df_name in names(mammal_datasets)) {
    df <- mammal_datasets[[df_name]]
    filter_by_specialization(df, df_name, specializations, "natural")
    filter_by_specialization(df, df_name, specializations, "human_dominated")
  }
  
}

# Check results
ls(pattern = "^(natural|human_dominated)_")


uniqueN(natural_ebba1$Species)
uniqueN(human_dominated_ebba1$Species)

uniqueN(natural_ebba2$Species)
uniqueN(human_dominated_ebba2$Species)

uniqueN(natural_m_b2000$Species)
uniqueN(human_dominated_m_b2000$Species)

uniqueN(natural_m_a2015$Species)
uniqueN(human_dominated_m_a2015$Species)



#___________________________________________________________________________
### Rasterizing Occurrences ####

# bird data

natural_ebba1_dt = rast_occ(natural_ebba1, grid_50km)
human_dominated_ebba1_dt = rast_occ(human_dominated_ebba1, grid_50km)


# additionally applying ebba change grid to ebba datasets

natural_ebba1_dt_change <- ebba_change(natural_ebba1_dt)
human_dominated_ebba1_dt_change <- ebba_change(human_dominated_ebba1_dt)

natural_ebba2_dt_change <- ebba_change(natural_ebba2)
human_dominated_ebba2_dt_change <- ebba_change(human_dominated_ebba2)


# mammal data

natural_m_b2000_dt = rast_occ(natural_m_b2000, grid_50km)
human_dominated_m_b2000_dt = rast_occ(human_dominated_m_b2000, grid_50km)

natural_m_a2015_dt = rast_occ(natural_m_a2015, grid_50km)
human_dominated_m_a2015_dt = rast_occ(human_dominated_m_a2015, grid_50km)



#___________________________________________________________________________
### Species Richness ####

#___________________________________________________________________________
#### Birds ####

# EBBA1 #

SR_natural_ebba1 = SR(natural_ebba1_dt_change)
hist(SR_natural_ebba1$SR)
write.csv(SR_natural_ebba1, "own datasets/SR_natural_ebba1_50km_change.csv", row.names = FALSE)

SR_human_dominated_ebba1 = SR(human_dominated_ebba1_dt_change)
hist(SR_human_dominated_ebba1$SR)
write.csv(SR_human_dominated_ebba1, "own datasets/SR_human_dominated_ebba1_50km_change.csv", row.names = FALSE)


# EBBA2 #

SR_natural_ebba2 = SR(natural_ebba2_dt_change)
hist(SR_natural_ebba2$SR)
write.csv(SR_natural_ebba2, "own datasets/SR_natural_ebba2_50km_change.csv", row.names = FALSE)

SR_human_dominated_ebba2 = SR(human_dominated_ebba2_dt_change)
hist(SR_human_dominated_ebba2$SR)
write.csv(SR_human_dominated_ebba2, "own datasets/SR_human_dominated_ebba2_50km_change.csv", row.names = FALSE)


# Visualizations

# EBBA1
plot = plot_SR(SR_natural_ebba1)
plot
ggsave("plots/SR_birds_natural_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_human_dominated_ebba1)
plot
ggsave("plots/SR_birds_human_dominated_ebba1_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# EBBA2
plot = plot_SR(SR_natural_ebba2)
plot
ggsave("plots/SR_birds_natural_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_human_dominated_ebba2)
plot
ggsave("plots/SR_birds_human_dominated_ebba2_change.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


hist_SR(SR_natural_ebba1)
hist_SR(SR_human_dominated_ebba1)

hist_SR(SR_natural_ebba2)
hist_SR(SR_human_dominated_ebba2)



#___________________________________________________________________________
#### Mammals ####

# before 2000
SR_natural_m_b2000 = SR(natural_m_b2000_dt)
hist(SR_natural_m_b2000$SR)
write.csv(SR_natural_m_b2000, "own datasets/SR_natural_m_b2000_50km.csv", row.names = FALSE)

SR_human_dominated_m_b2000 = SR(human_dominated_m_b2000_dt)
hist(SR_human_dominated_m_b2000$SR)
write.csv(SR_human_dominated_m_b2000, "own datasets/SR_human_dominated_m_b2000_50km.csv", row.names = FALSE)


# after 2015
SR_natural_m_a2015 = SR(natural_m_a2015_dt)
hist(SR_natural_m_a2015$SR)
write.csv(SR_natural_m_a2015, "own datasets/SR_natural_m_a2015_50km.csv", row.names = FALSE)

SR_human_dominated_m_a2015 = SR(human_dominated_m_a2015_dt)
hist(SR_human_dominated_m_a2015$SR)
write.csv(SR_human_dominated_m_a2015, "own datasets/SR_human_dominated_m_a2015_50km.csv", row.names = FALSE)


# Visualizations

# before 2000
plot = plot_SR(SR_natural_m_b2000)
plot
ggsave("plots/SR_birds_natural_m_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_human_dominated_m_b2000)
plot
ggsave("plots/SR_birds_human_dominated_m_b2000.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# after 2015
plot = plot_SR(SR_natural_m_a2015)
plot
ggsave("plots/SR_birds_natural_m_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_SR(SR_human_dominated_m_a2015)
plot
ggsave("plots/SR_birds_human_dominated_m_a2015.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


hist_SR(SR_natural_m_b2000)
hist_SR(SR_human_dominated_m_b2000)

hist_SR(SR_natural_m_a2015)
hist_SR(SR_human_dominated_m_a2015)


#___________________________________________________________________________
### Delta Species Richness ####

deltaSR_bird_natural = delta_SR(SR_natural_ebba1, SR_natural_ebba2, "_ebba1", "_ebba2")
deltaSR_bird_human_dominated = delta_SR(SR_human_dominated_ebba1, SR_human_dominated_ebba2, "_ebba1", "_ebba2")

deltaSR_mammal_natural = delta_SR(SR_natural_m_b2000, SR_natural_m_a2015, "_b2000", "_a2015")
deltaSR_mammal_human_dominated = delta_SR(SR_human_dominated_m_b2000, SR_human_dominated_m_a2015, "_b2000", "_a2015")


# Plotting

plot = plot_deltaSR(deltaSR_bird_natural)
plot
ggsave("plots/deltaSR_bird_natural.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_deltaSR(deltaSR_bird_human_dominated)
plot
ggsave("plots/deltaSR_bird_human_dominated.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot = plot_deltaSR(deltaSR_mammal_natural)
plot
ggsave("plots/deltaSR_mammal_natural.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_deltaSR(deltaSR_mammal_human_dominated)
plot
ggsave("plots/deltaSR_mammal_human_dominated.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# fixed spectrum

plot = plot_deltaSR_fixed(deltaSR_bird_natural, deltaSR_bird_human_dominated$delta_SR)
plot
ggsave("plots/deltaSR_bird_natural_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_deltaSR_fixed(deltaSR_bird_human_dominated, deltaSR_bird_human_dominated$delta_SR)
plot
ggsave("plots/deltaSR_bird_human_dominated_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot = plot_deltaSR_fixed(deltaSR_mammal_natural, deltaSR_mammal_human_dominated$delta_SR)
plot
ggsave("plots/deltaSR_mammal_natural_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot = plot_deltaSR_fixed(deltaSR_mammal_human_dominated, deltaSR_mammal_human_dominated$delta_SR)
plot
ggsave("plots/deltaSR_mammal_human_dominated_fixed_spec.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



hist_deltaSR(deltaSR_bird_natural)
hist_deltaSR(deltaSR_bird_human_dominated)

hist_deltaSR(deltaSR_mammal_natural)
hist_deltaSR(deltaSR_mammal_human_dominated)



#___________________________________________________________________________
### Joining and Exporting ####

# List all data frames
dfs <- list(
  SR_natural_ebba1 = SR_natural_ebba1,
  SR_human_dominated_ebba1 = SR_human_dominated_ebba1,
  SR_natural_ebba2 = SR_natural_ebba2,
  SR_human_dominated_ebba2 = SR_human_dominated_ebba2,
  deltaSR_bird_natural = deltaSR_bird_natural[,c(1,4)],
  deltaSR_bird_human_dominated = deltaSR_bird_human_dominated[,c(1,4)],
  SR_natural_m_b2000 = SR_natural_m_b2000,
  SR_human_dominated_m_b2000 = SR_human_dominated_m_b2000,
  SR_natural_m_a2015 = SR_natural_m_a2015,
  SR_human_dominated_m_a2015 = SR_human_dominated_m_a2015,
  deltaSR_mammal_natural = deltaSR_mammal_natural[,c(1,4)],
  deltaSR_mammal_human_dominated = deltaSR_mammal_human_dominated[,c(1,4)]
)

# Rename columns based on df name
for (name in names(dfs)) {
  old_col <- setdiff(names(dfs[[name]]), "cell50x50")  # Find the column to rename
  if (length(old_col) > 0) {
    new_col <- ifelse(grepl("_SR$", name), paste0("SR_", sub("_SR$", "", name)), name)
    setnames(dfs[[name]], old = old_col, new = new_col)
  }
}

# Merge all data frames on 'cell50x50'
merged_df <- Reduce(function(x, y) merge(x, y, by = "cell50x50", all = TRUE), dfs)

# Check result
str(merged_df)

write.csv(merged_df, "own datasets/SR_classes_AN.csv", row.names = F)



#___________________________________________________________________________
# Combining Data ####

## Full EBBA grid ####

deltaSR_birds_50km = read.csv("own datasets/deltaSR_birds_50km.csv") # 'whole' EBBA data set
deltaSR_mammals_50km = read.csv("own datasets/deltaSR_mammals_50km.csv")
deltaT_50km = read.csv("own datasets/deltaT_50km_20y.csv")
ccv_50km = read.csv("own datasets/ccv_50km_20y.csv")


# Merging the first two data frames by 'cell50x50'
merged_df <- merge(deltaSR_mammals_50km, deltaSR_birds_50km, by = "cell50x50", all = TRUE)

# Merging the result with the third data frame ('deltaT_50km')
merged_df <- merge(merged_df, deltaT_50km, by = "cell50x50", all = TRUE)

merged_df <- merge(merged_df, ccv_50km, by = "cell50x50", all = TRUE)

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
                  "deltaT", "ccv", "log_ccv")

merged_df <- merged_df[, ordered_cols]

# Check result
head(merged_df)


write.csv(merged_df, "own datasets/analysis_data_50km_full.csv", row.names = FALSE)



## EBBA Change grid ####

merged_df_change = ebba_change(merged_df)

write.csv(merged_df_change, "own datasets/analysis_data_50km_change.csv", row.names = FALSE)



#___________________________________________________________________________
# Histograms ####
# data_change = read.csv("own datasets/analysis_data_50km_change.csv")
data_full = read.csv("own datasets/analysis_data_50km.csv")


# Number of occurrences mammals
png("plots/hist_Occ_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$occ_m_b2000,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in Mammals before 2000 (50km)")
abline(v = mean(data_full$occ_m_b2000, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$occ_m_b2000, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_Occ_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$occ_m_a2015,
     breaks = 50,
     col = "lightblue",
     main = "Number of Occurrences in Mammals after 2015 (50km)")
abline(v = mean(data_full$occ_m_a2015, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$occ_m_a2015, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()


# SR birds
png("plots/hist_SR_birds_ebba1.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$SR_ebba1,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in EBBA1 Birds (50km)")
abline(v = mean(data_full$SR_ebba1, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$SR_ebba1, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_SR_birds_ebba2.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$SR_ebba2,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in EBBA2 Birds (50km)")
abline(v = mean(data_full$SR_ebba2, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$SR_ebba2, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()


# SR mammals
png("plots/hist_SR_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$SR_m_b2000,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in GBIF Mammals before 2015 (50km)")
abline(v = mean(data_full$SR_m_b2000, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$SR_m_b2000, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_SR_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$SR_m_a2015,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness in GBIF Mammals after 2015 (50km)")
abline(v = mean(data_full$SR_m_a2015, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$SR_m_a2015, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()


# delta SR
png("plots/hist_deltaSR_birds.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$deltaSR_birds,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness Change in EBBA Birds (50km)")
abline(v = mean(data_full$deltaSR_birds, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$deltaSR_birds, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_deltaSR_mammals.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$deltaSR_mammals,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Species Richness Change in GBIF Mammals (50km)")
abline(v = mean(data_full$deltaSR_mammals, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$deltaSR_mammals, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
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
hist(data_full$deltaT,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of Temperature Change (50km)")
abline(v = mean(data_full$deltaT, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$deltaT, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()


# climate change velocity
png("plots/hist_climate_change_velocity.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$ccv,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of climate change velocity (50km)")
abline(v = mean(data_full$ccv, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$ccv, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_log_climate_change_velocity.png",
    width = 800,
    height = 600,
    res = 100)
hist(data_full$log_ccv,
     breaks = 50,
     col = "lightblue",
     main = "Distr. of climate change velocity (log) (50km)")
abline(v = mean(data_full$log_ccv, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(data_full$log_ccv, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()




#___________________________________________________________________________
# Modelling ####

data_full = read.csv("own datasets/analysis_data_50km_full.csv")
data_change = read.csv("own datasets/analysis_data_50km_change.csv")



#___________________________________________________________________________
## Linear Models ####

#___________________________________________________________________________
### LMs ####

lm_b_deltaT_full = lm(deltaSR_birds  ~ deltaT,
           data = data_full)
summary(lm_b_deltaT_full)
par(mfrow = c(2, 2))
plot(lm_b_deltaT_full)

lm_b_logccv_full = lm(deltaSR_birds  ~ log_ccv,
           data = data_full)
summary(lm_b_logccv_full)
par(mfrow = c(2, 2))
plot(lm_b_logccv_full)


lm_m_deltaT_full = lm(deltaSR_mammals  ~ deltaT,
           data = data_full)
summary(lm_m_deltaT_full)
par(mfrow = c(2, 2))
plot(lm_m_deltaT_full)

lm_m_logccv_full = lm(deltaSR_mammals  ~ log_ccv,
           data = data_full)
summary(lm_m_logccv_full)
par(mfrow = c(2, 2))
plot(lm_m_logccv_full)

lm2.3 = lm(deltaSR_mammals  ~ deltaT + delta_occ_m,
           data = data_full)
summary(lm2.3)
par(mfrow = c(2, 2))
plot(lm2.3)

lm2.4 = lm(deltaSR_mammals  ~ log_ccv + delta_occ_m,
           data = data_full)
summary(lm2.4)
par(mfrow = c(2, 2))
plot(lm2.4)



#___________________________________________________________________________
### GLMs ####

glm_b_deltaT_full = glm(deltaSR_birds  ~ deltaT,
             data = data_full,
             family = gaussian)
summary(glm_b_deltaT_full)

glm_b_logccv_full = glm(deltaSR_birds  ~ log_ccv,
             data = data_full,
             family = gaussian)
summary(glm_b_logccv_full)


glm_m_deltaT_full = glm(deltaSR_mammals  ~ deltaT,
             data = data_full,
             family = gaussian)
summary(glm_m_deltaT_full)

glm_m_logccv_full = glm(deltaSR_mammals  ~ log_ccv,
             data = data_full,
             family = gaussian)
summary(glm_m_logccv_full)

glm2.3 = glm(deltaSR_mammals  ~ deltaT + delta_occ_m,
             data = data_full,
             family = gaussian)
summary(glm2.3)

glm2.4 = glm(deltaSR_mammals  ~ log_ccv + delta_occ_m,
             data = data_full,
             family = gaussian)
summary(glm2.4)



#___________________________________________________________________________
### EBBA Change ####

lm_b_deltaT_change = lm(deltaSR_birds  ~ deltaT,
            data = data_change)
summary(lm_b_deltaT_change)
par(mfrow = c(2, 2))
plot(lm_b_deltaT_change)

lm_b_logccv_change = lm(deltaSR_birds  ~ log_ccv,
            data = data_change)
summary(lm_b_logccv_change)
par(mfrow = c(2, 2))
plot(lm_b_logccv_change)


glm_b_deltaT_change = glm(deltaSR_birds  ~ deltaT,
              data = data_change,
              family = gaussian)
summary(glm_b_deltaT_change)

glm_b_logccv_change = glm(deltaSR_birds  ~ log_ccv,
              data = data_change,
              family = gaussian)
summary(glm_b_logccv_change)

model.sel(lm_b_deltaT_change, lm_b_logccv_change, 
          glm_b_deltaT_change, glm_b_logccv_change,
          rank = "AICc")


# deltaT
plot = ggplot(data_change, aes(x = deltaT, y = deltaSR_birds)) +
  geom_point(alpha = 0.2,
             shape = 16,
             size = 2) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Temperature Change (ΔT)",
       y = "Species Richness Change (ΔSR)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))

plot

ggsave("plots/regline_SR_birds_change~deltaT.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


# log CCV
plot = ggplot(data_change, aes(x = log_ccv, y = deltaSR_birds)) +
  geom_point(alpha = 0.2,
             shape = 16,
             size = 2) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Log Climate Change Velocity (logCCV)",
       y = "Species Richness Change (ΔSR)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))

plot

ggsave("plots/regline_SR_birds_change~log_CCV.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")




#___________________________________________________________________________
### AIC comparison and coefficient export ####

model.sel(lm_b_deltaT_full, lm_b_logccv_full, lm_m_deltaT_full, lm_m_logccv_full, lm2.3, lm2.4,
          glm_b_deltaT_full, glm_b_logccv_full, glm_m_deltaT_full, glm_m_logccv_full, glm2.3, glm2.4,
          lm_b_deltaT_change, lm_b_logccv_change, 
          glm_b_deltaT_change, glm_b_logccv_change,
          rank = "AICc")


bT <- coef(glm_b_deltaT_change)["deltaT"]
bC <- coef(glm_b_logccv_change)["log_ccv"]

coefficients_df <- data.frame(
  Variable = c("deltaT", "log_ccv"),
  Coefficient = c(bT, bC)
)

write.csv(coefficients_df, "own datasets/temperature_coefficients_EBBA_change.csv", row.names = FALSE)



#___________________________________________________________________________
## Plotting ####

long_data <- data_change %>%
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



#range.x <- range(data$Delta_T)
#new.x <- data.frame(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1))
#new.y <- predict(lm1,
#                 newdata = new.x,
#                 se.fit = TRUE) %>% 
#  as.data.frame() %>% 
#  mutate(Delta_T = seq(from = range.x[1], to = range.x[2], 0.1),
#         lower = (fit - 1.96*se.fit), 
#         point.estimate = (fit), 
#         upper = (fit + 1.96*se.fit))

#plot <- ggplot(data, aes(x = Delta_T, y = Species_Richness)) +
#  geom_point(colour = "black",
#             alpha = 0.1,
#             shape = 16,
#             size = 0.6) +
#  geom_line(aes(x = Delta_T, y = point.estimate),
#            data = new.y,
#            colour = "darkred",
#            linewidth = 1) + 
#  geom_ribbon(aes(x = Delta_T, ymin = lower, ymax = upper),
#              data = new.y,
#              color = NA, fill = "red",
#              alpha = 0.5,
#              inherit.aes = FALSE) + 
#  labs(x = "Temperature Change (ΔT)",
#       y = "Species Richness Change (ΔSR)") +
#  theme(panel.background = element_blank(),
#        panel.border = element_rect(colour = "black", fill = NA),
#        legend.position = "none")

#plot

# save file
#ggsave(filename = "plots/regline_deltaSR_mammals~deltaT_90_16.png", plot = plot, width = 7.4, height = 7.4, #units = "cm")



#___________________________________________________________________________
### CCV ####

long_data <- data_change %>%
  gather(key = "Species", value = "delta_SR", deltaSR_mammals, deltaSR_birds)

# Plotting
plot = ggplot(long_data, aes(x = log_ccv, y = delta_SR, color = Species)) +
  geom_point(alpha = 0.2,
             shape = 16,
             size = 2) +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "(Log) Climate Change Velocity",
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

ggsave("plots/regline_SR_mammals_birds~logCCV.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


