#___________________________________________________________________________
# Loading Packages ####

library(terra)
library(sf)
library(tidyverse)
library(exactextractr)
library(MuMIn)



#___________________________________________________________________________
# Importing Data ####

# Load raster (land use map)
land_use_raster_2000 <- rast("external datasets/CORINE Land Cover CLC/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif")

land_use_raster_2018 <- rast("external datasets/CORINE Land Cover CLC/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")


# Load grid shapefile
grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")

# Compute actual area of each grid cell in km²
grid <- grid %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)  # convert m² to km²

summary(grid)




#___________________________________________________________________________
# Land Use Categories ####

levels(land_use_raster_2000)[[1]]

forest_classes <- c("Broad-leaved forest",
                    "Coniferous forest",
                    "Mixed forest")

agriculture_classes <- c("Non-irrigated arable land",
                         "Permanently irrigated land", 
                         "Rice fields",
                         "Vineyards",
                         "Fruit trees and berry plantations", 
                         "Olive groves",
                         "Annual crops associated with permanent crops", 
                         "Complex cultivation patterns", 
                         "Land principally occupied by agriculture, with significant areas of natural vegetation", 
                         "Agro-forestry areas")


urban_classes  <- c("Continuous urban fabric",
                    "Discontinuous urban fabric",
                    "Industrial or commercial units",
                    #"Road and rail networks and associated land",
                    "Port areas",
                    "Airports",
                    #"Dump sites",
                    "Construction sites",
                    "Green urban areas",
                    "Sport and leisure facilities")

grassland_classes <- c("Natural grasslands",
                       "Pastures")

other_classes  <- c("Road and rail networks and associated land", #?
                    "Mineral extraction sites", #?
                    "Dump sites", #?
                    "Moors and heathland",
                    "Sclerophyllous vegetation",
                    "Transitional woodland-shrub", #?
                    "Beaches, dunes, sands",
                    "Bare rocks",
                    "Sparsely vegetated areas",
                    "Burnt areas",
                    "Glaciers and perpetual snow",
                    "Inland marshes",
                    "Peat bogs",
                    "Salt marshes",
                    "Salines",
                    "Intertidal flats",
                    "Water courses",
                    "Water bodies", #?
                    "Coastal lagoons",
                    "Estuaries",
                    "Sea and ocean",
                    "NODATA",
                    "No Land Use") #?



#___________________________________________________________________________
# 2000 ####

extract_landuse = function(grid, lu_raster) {
  
  # Ensure grid and raster have the same CRS
  grid <- st_transform(grid, crs(lu_raster))
  
  # Use exact_extract to get the area-weighted land use values
  extracted <- exact_extract(lu_raster, grid, include_cell = FALSE)
  
  # Convert list output to a long-format data frame
  extracted_df <- bind_rows(
    lapply(seq_along(extracted), function(i) {
      df <- extracted[[i]]
      df$cell50x50 <- grid$cell50x50[i]  # Add grid cell name
      df
    }),
    .id = NULL
  )
  
  # Count occurrences of each land use per grid cell
  land_use_counts <- extracted_df %>%
    group_by(cell50x50, value) %>%
    summarise(count = n(), .groups = "drop")
  
  # Get total pixel count per grid cell
  total_counts <- land_use_counts %>%
    group_by(cell50x50) %>%
    summarise(total = sum(count), .groups = "drop")
  
  # Merge and calculate proportions
  land_use_proportions <- land_use_counts %>%
    left_join(total_counts, by = "cell50x50") %>%
    mutate(proportion = count / total) %>%
    select(cell50x50, value, proportion)
  
  rat <- levels(lu_raster)[[1]]
  
  # Merge extracted values with land use categories
  land_use_proportions <- land_use_proportions %>%
    left_join(rat, by = c("value" = "Value"))  # Ensure correct mapping
  
  # Replace NA values in LABEL3 column with "No Land Use"
  land_use_proportions <- land_use_proportions %>%
    mutate(LABEL3 = ifelse(is.na(LABEL3), "No Land Use", LABEL3))
  
  
    # RELATIVE NUMBERS
  
  # Reshape to wide format
  land_use_rel <- land_use_proportions %>%
    select(-value) %>%  # Remove land use category numbers
    pivot_wider(names_from = LABEL3, values_from = proportion, values_fill = 0)
  
  land_use_rel <- land_use_rel %>%
    rowwise() %>%
    mutate(forest_rel = sum(c_across(all_of(forest_classes)), na.rm = TRUE),
           agriculture_rel = sum(c_across(all_of(agriculture_classes)), na.rm = TRUE),
           grassland_rel = sum(c_across(all_of(grassland_classes)), na.rm = TRUE),
           urban_rel = sum(c_across(all_of(urban_classes)), na.rm = TRUE),
           other_rel = sum(c_across(all_of(other_classes)), na.rm = TRUE))%>%
    ungroup() %>% 
    select(cell50x50, forest_rel, agriculture_rel, grassland_rel, urban_rel, other_rel)
    
  
  # ABSOLUTE NUMBERS 
  
  # Add area to land use proportions
  land_use_proportions <- land_use_proportions %>%
    left_join(grid %>% st_drop_geometry() %>% select(cell50x50, area_km2), by = "cell50x50")
  
  
  # Convert proportions to area (in km²)
  land_use_proportions <- land_use_proportions %>%
    mutate(area_km2 = proportion * area_km2)
  
  
  land_use_areas_wide <- land_use_proportions %>%
    select(cell50x50, LABEL3, area_km2) %>%
    pivot_wider(names_from = LABEL3, values_from = area_km2, values_fill = 0)
  
  # Add category groups like forest, agri, etc.
  land_use_abs <- land_use_areas_wide %>%
    rowwise() %>%
    mutate(
      forest_abs = sum(c_across(all_of(forest_classes)), na.rm = TRUE),
      agriculture_abs = sum(c_across(all_of(agriculture_classes)), na.rm = TRUE),
      grassland_abs = sum(c_across(all_of(grassland_classes)), na.rm = TRUE),
      urban_abs = sum(c_across(all_of(urban_classes)), na.rm = TRUE),
      other_abs = sum(c_across(all_of(other_classes)), na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    select(cell50x50, forest_abs, agriculture_abs, grassland_abs, urban_abs, other_abs)
  
  land_use = left_join(land_use_abs, land_use_rel, by = "cell50x50")
  return(land_use)
}


land_use_fa_2000 = extract_landuse(grid, land_use_raster_2000)
head(land_use_fa_2000)


write.csv(land_use_fa_2000, "own datasets/LU_2000_50km.csv", row.names = FALSE)



#___________________________________________________________________________
# 2018 ####


land_use_fa_2018 = extract_landuse(grid, land_use_raster_2018)
head(land_use_fa_2018)


write.csv(land_use_fa_2018, "own datasets/LU_2018_50km.csv", row.names = FALSE)



#___________________________________________________________________________
# cSAR data Set ####

# Load datasets
land_use_2000 <- read.csv("own datasets/LU_2000_50km.csv")
#land_use_2018 <- read.csv("own datasets/LU_2018_50km.csv")

grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")
grid <- grid %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)  # convert m² to km²

change_grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")


SR_specializations_FAG = read.csv("own datasets/SR_classes_FAG.csv")
SR_specializations_AN = read.csv("own datasets/SR_classes_AN.csv")



#___________________________________________________________________________
## cSAR Birds For/Agr/Oth ####

df_cSAR_FAG = grid %>% 
  st_drop_geometry() %>% 
  left_join(land_use_2000, by = "cell50x50")

df_cSAR_FAG1 = df_cSAR_FAG %>% 
  left_join(SR_specializations_FAG, by = "cell50x50")
str(df_cSAR_FAG1)


# only keep Change Grid Cells for bird cSAR

ebba_change_cells = change_grid$cell50x50

df_cSAR_FAG1_change = df_cSAR_FAG1 %>% 
  filter(cell50x50 %in% ebba_change_cells)


# Fit dataframe format to sars package needs

library(sars)


df_cSAR_FAG_b <- df_cSAR_FAG1_change %>%
  mutate(
    Area_F = coalesce(forest_abs, 0),
    Area_A = coalesce(agriculture_abs, 0),
    Area_G = coalesce(grassland_abs, 0),
    Area_U = coalesce(urban_abs, 0),
    Area_O = coalesce(other_abs, 0),
    Spcs_F = SR_forest_ebba1,
    Spcs_A = SR_agriculture_ebba1,
    Spcs_O = SR_generalist_ebba1
  ) %>%
  dplyr::select(cell50x50, Area_F, Area_A, Area_G, Area_U, Area_O, Spcs_F, Spcs_A, Spcs_O) %>%
  column_to_rownames("cell50x50")


str(df_cSAR_FAG_b)

write.csv(df_cSAR_FAG_b, "own datasets/cSAR_FAG_b.csv", row.names = TRUE)

#df_cSAR_FAG_b = read.csv("own datasets/cSAR_FAG_b.csv")



stpr <- matrix(c(1.000e+00, 1.237e-03, 1.984e-03, 1.006e-03, 1.572e-03, 1.534e-01,
                 1.148e-03, 1.000e+00, 1.763e-03, 1.391e-03, 1.809e-03, 1.597e-01,
                 5.231e-01, 5.908e-01, 5.476e-01, 5.103e-01, 5.689e-01, 1.582e-01),
               nrow = 3,
               byrow = TRUE)

help(sar_countryside)
cSAR_FAG_b <- sar_countryside(data = df_cSAR_FAG_b, modType = "power",
                      startPar = NULL, # error since recent changes, deactivated and used "gridStart" again
                      gridStart = "none",
                      habNam = c("F", "A", "G", "U", "O"), 
                      spNam = c("F_Sp", "A_Sp", "G_Sp"))
cSAR_FAG_b
# not converging due to problems with G_Sp



#_________________________________________________________________________________
### Plotting ####

# Plot the model fit

plot(cSAR_FAG_b, type = 1, powFit = TRUE)

# relationship between observed total richness and ...
# black = total predicted richness of countryside SAR model 
# red = total predicted richness values from Arrhenius power SAR model



# Plot the cSAR responses for each species group

# type = 2 varies site area while fixing proportion of a given habitat (100%, others 0%)
par(mar=c(5.1, 4.1, 4.1, 7.5),
    xpd=TRUE)
plot(cSAR_FAG_b,
     type = 2,
     lcol = c("aquamarine4", "#CC661AB3" , "darkblue"),
     pLeg = TRUE,
     legPos ="topright",
     legInset = c(-0.27,0.3),
     lwd = 1.5)


# type = 3 varies proportion of given habitat while fixing site area (area of largest site used)
par(mar=c(5.1, 4.1, 4.1, 7.5),
    xpd=TRUE)
plot(cSAR_FAG_b,
     type = 3,
     lcol = c("aquamarine4", "#CC661AB3" , "darkblue"),
     pLeg = TRUE,
     legPos ="topright",
     legInset = c(-0.27,0.3),
     lwd = 1.5)


#_________________________________________________________________________________
### Estimate SR for given landscape ####

countryside_extrap(cSAR_FAG_b, area = c(1000, 1000, 1000)) # AG, SH, FO



#___________________________________________________________________________
## cSAR Birds Nat/HuD ####

df_cSAR_NHD = grid %>% 
  st_drop_geometry() %>% 
  left_join(land_use_2000, by = "cell50x50")

df_cSAR_FAG1 = df_cSAR_NHD %>% 
  left_join(SR_specializations_NHD, by = "cell50x50")
str(df_cSAR_FAG1)

# Fit dataframe format to sars package needs

library(sars)

df_cSAR_NHD_b <- df_cSAR_NHD1 %>%
  select(cell50x50, forest, agriculture, grassland, urban, other,
         SR_forest_ebba1, SR_agriculture_ebba1, SR_generalist_ebba1) %>%
  mutate(
    Area_F = coalesce(forest, 0),
    Area_A = coalesce(agriculture, 0),
    Area_G = coalesce(grassland, 0),
    Area_U = coalesce(urban, 0),
    Area_O = coalesce(other, 0),
    Spcs_F = coalesce(SR_forest_ebba1, 0),
    Spcs_A = coalesce(SR_agriculture_ebba1, 0),
    Spcs_O = coalesce(SR_generalist_ebba1, 0)
  ) %>%
  select(cell50x50, Area_F, Area_A, Area_G, Area_U, Area_O, Spcs_F, Spcs_A, Spcs_O) %>%
  column_to_rownames("cell50x50")


str(df_cSAR_NHD_b)

write.csv(df_cSAR_NHD_b, "own datasets/cSAR_NHD_b.csv", row.names = TRUE)

#df_cSAR_NHD_b = read.csv("own datasets/cSAR_NHD_b.csv")


cSAR_NHD_b <- sar_countryside(data = df_cSAR_NHD_b, modType = "power",
                              startPar = stpr,
                              habNam = c("F", "A", "G", "U", "O"), 
                              spNam = c("F_Sp", "A_Sp", "G_Sp"))
cSAR_NHD_b


stpr <- matrix(c(1.000e+00, 1.237e-03, 1.984e-03, 1.006e-03, 1.572e-03, 1.534e-01,
                 1.148e-03, 1.000e+00, 1.763e-03, 1.391e-03, 1.809e-03, 1.597e-01,
                 5.231e-01, 5.908e-01, 5.476e-01, 5.103e-01, 5.689e-01, 1.582e-01),
               nrow = 3,
               byrow = TRUE)





#___________________________________________________________________________
## cSAR Mammals For/Agr/Oth ####

# Fit dataframe format to sars package needs

library(sars)

df_cSAR_FAG_m <- df_cSAR_FAG1 %>%
  select(cell50x50, forest, agriculture, urban, other,
         SR_forest_m_b2000, SR_agriculture_m_b2000, SR_generalist_m_b2000) %>%
  mutate(
    Area_O = coalesce(urban, 0) + coalesce(other, 0),
    Area_F = coalesce(forest, 0),
    Area_A = coalesce(agriculture, 0),
    Spcs_F = coalesce(SR_forest_m_b2000, 0),
    Spcs_A = coalesce(SR_agriculture_m_b2000, 0),
    Spcs_O = coalesce(SR_generalist_m_b2000, 0)
  ) %>%
  select(cell50x50, Area_F, Area_A, Area_O, Spcs_F, Spcs_A, Spcs_O) %>%
  column_to_rownames("cell50x50")


str(df_cSAR_FAG_m)

cSAR_FAG_m <- sar_countryside(data = df_cSAR_FAG_m, modType = "power",
                              gridStart = "partial", #ubiSp = TRUE, 
                              habNam = c("F", "A", "O"), 
                              spNam = c("F_Sp", "A_Sp", "O_Sp"))
cSAR_FAG_m


#_________________________________________________________________________________
## Plotting ####

# Plot the model fit

plot(cSAR_FAG_m, type = 1, powFit = TRUE)

# relationship between observed total richness and ...
# black = total predicted richness of countryside SAR model 
# red = total predicted richness values from Arrhenius power SAR model



# Plot the cSAR responses for each species group

# type = 2 varies site area while fixing proportion of a given habitat (100%, others 0%)
par(mar=c(5.1, 4.1, 4.1, 7.5),
    xpd=TRUE)
plot(cSAR_FAG_m,
     type = 2,
     lcol = c("black", "aquamarine4", "#CC661AB3" , "darkblue"),
     pLeg = TRUE,
     legPos ="topright",
     legInset = c(-0.27,0.3),
     lwd = 1.5)


# type = 3 varies proportion of given habitat while fixing site area (area of largest site used)
par(mar=c(5.1, 4.1, 4.1, 7.5),
    xpd=TRUE)
plot(cSAR_FAG_m,
     type = 3,
     lcol = c("black", "aquamarine4", "#CC661AB3" , "darkblue"),
     pLeg = TRUE,
     legPos ="topright",
     legInset = c(-0.27,0.3),
     lwd = 1.5)



#_________________________________________________________________________________
## Estimate SR for given landscape ####

countryside_extrap(cSAR_FAG_m, area = c(1000, 1000, 1000)) # AG, SH, FO




#___________________________________________________________________________
# Land Use Change (unfinished) ####


#___________________________________________________________________________
## All Land Use Categories ####

# Load datasets
land_use_fa_2000 <- read.csv("own datasets/LU_2000_50km.csv")
land_use_fa_2018 <- read.csv("own datasets/LU_2018_50km.csv")

# Ensure column names match and are ordered correctly
land_use_fa_2000 <- land_use_fa_2000[order(land_use_fa_2000$cell50x50), ]
land_use_fa_2018 <- land_use_fa_2018[order(land_use_fa_2018$cell50x50), ]

summary(land_use_fa_2018$forest)
summary(land_use_fa_2000$forest)

# Create mask for cells that were completely unsampled in 2000
#mask_exclude <- (nodata_2000 + no_land_use_2000) == 1

# Compute sampled fraction in 2000
sampled_fraction_2000 <- 1 - (nodata_2000 + no_land_use_2000)  # Values range from 0 to 1

# Adjust 2018 values: Scale them down to match only the area sampled in 2000
land_use_fa_2018_adj <- land_use_fa_2018
land_use_fa_2018_adj[,-1] <- land_use_fa_2018[,-1] * sampled_fraction_2000

# Compute land use change per grid cell (ONLY in the already sampled area)
change <- land_use_fa_2018_adj
change[,-1] <- land_use_fa_2018_adj[,-1] - land_use_fa_2000[,-1]

# Set change values to NA for cells that were completely unsampled in 2000
change[mask_exclude, -1] <- NA


write.csv(change, "own datasets/LU_change_50km.csv", row.names = FALSE)

#___________________________________________________________________________
### Plotting ####

# Reshape data from wide to long format
long_df <- change %>%
  select(cell50x50, forest, agriculture) %>%
  pivot_longer(cols = c(forest, agriculture), 
               names_to = "Category", 
               values_to = "Relative_Area")

# Create the boxplot with jittered points
plot = ggplot(long_df, aes(x = Category, y = Relative_Area, fill = Category)) +
  geom_jitter(aes(color = Category), width = 0.3, alpha = 0.05, size = 2) +  # Transparent jittered points
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +  # Boxplot with transparency
  labs(x = "Land Use Category",
       y = "Change of Relative Area (Proportion of Grid Cell)") +
  scale_fill_manual(values = c("forest" = "darkgreen", "agriculture" = "goldenrod")) +
  scale_color_manual(values = c("forest" = "darkgreen", "agriculture" = "goldenrod")) +  # Match colors
  theme_minimal(base_size = 20)

plot

ggsave("plots/boxplot_rel_forest_agric_area_change.png", plot = plot, width = 7, height = 8, dpi = 300, bg = "white")


png("plots/hist_rel_forest_area_change.png",
    width = 800,
    height = 600,
    res = 100)
hist(change$forest,
     breaks = 50,
     col = "lightblue",
     main = "Relative forest area change per grid cell")
abline(v = mean(change$forest, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(change$forest, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()

png("plots/hist_rel_agric_area_change.png",
    width = 800,
    height = 600,
    res = 100)
hist(change$agriculture,
     breaks = 50,
     col = "lightblue",
     main = "Relative agricultural area change per grid cell")
abline(v = mean(change$agriculture, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
abline(v = median(change$agriculture, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = c(2, 3),
       lwd = 2,
       bg = "white")
dev.off()



#___________________________________________________________________________
## forest + agriculture ####

# Merge change with grid based on cell50x50
change_sf <- left_join(change, grid, by = "cell50x50")

# Convert to sf object (ensuring geometry is recognized)
change_sf <- st_as_sf(change_sf)

plot <- ggplot(change_sf) +
  geom_sf(aes(fill = forest), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal(base_size = 20) +
  labs(fill = "forest Change")
plot

ggsave("plots/LU_change_forest_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot <- ggplot(change_sf) +
  geom_sf(aes(fill = agriculture), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal(base_size = 20) +
  labs(fill = "agriculture Change")
plot

ggsave("plots/LU_change_agric_50km.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



#___________________________________________________________________________
## Model ####

data = read.csv("own datasets/analysis_data_50km.csv")

SR_LU <- merge(data, change[,c(1,49:50)], by = "cell50x50", all = TRUE)
summary(SR_LU)


glm.bf = glm(deltaSR_birds  ~ forest,
             data = SR_LU,
             family = gaussian)
summary(glm.bf)

glm.mf = glm(deltaSR_mammals  ~ forest,
             data = SR_LU,
             family = gaussian)
summary(glm.mf)


glm.ba = glm(deltaSR_birds  ~ agriculture,
             data = SR_LU,
             family = gaussian)
summary(glm.ba)

glm.ma = glm(deltaSR_mammals  ~ agriculture,
             data = SR_LU,
             family = gaussian)
summary(glm.ma)


model.sel(glm.bf, glm.ba,
          glm.mf, glm.ma,
          rank = "AICc")



#___________________________________________________________________________
# Prep for H's script ####

land_use_2000 <- read.csv("own datasets/LU_2000_50km.csv")
head(land_use_2000)

library(terra)

# Load your raster
r <- land_use_raster_2000


# Step 1: Define simplified categories
forest_classes <- c("Broad-leaved forest", "Coniferous forest", "Mixed forest")
agriculture_classes <- c("Non-irrigated arable land", "Permanently irrigated land", "Rice fields",
                         "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                         "Annual crops associated with permanent crops", "Complex cultivation patterns",
                         "Land principally occupied by agriculture, with significant areas of natural vegetation",
                         "Agro-forestry areas")
urban_classes <- c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units",
                   "Port areas", "Airports", "Construction sites", "Green urban areas", "Sport and leisure facilities")
grassland_classes <- c("Natural grasslands", "Pastures")
other_classes <- c("Road and rail networks and associated land", "Mineral extraction sites", "Dump sites",
                   "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub",
                   "Beaches, dunes, sands", "Bare rocks", "Sparsely vegetated areas", "Burnt areas",
                   "Glaciers and perpetual snow", "Inland marshes", "Peat bogs", "Salt marshes", "Salines",
                   "Intertidal flats", "Water courses", "Water bodies", "Coastal lagoons", "Estuaries",
                   "Sea and ocean", "NODATA", "No Land Use")

# Combine into a lookup dataframe
make_lut <- function(class_names, simplified_name) {
  data.frame(LABEL3 = class_names, simplified = simplified_name, stringsAsFactors = FALSE)
}

lut <- rbind(
  make_lut(forest_classes, "forest"),
  make_lut(agriculture_classes, "agriculture"),
  make_lut(urban_classes, "urban"),
  make_lut(grassland_classes, "grassland"),
  make_lut(other_classes, "other")
)

# Step 2: Get raster levels
lvl <- levels(r)[[1]]  # Contains Value + LABEL3

# Step 3: Merge with lookup table
merged <- merge(lvl, lut, by = "LABEL3", all.x = TRUE)

# Step 4: Map Value -> simplified
value_to_simplified <- merged[, c("Value", "simplified")]

# Drop NA values if needed (optional: keeps only matched LU classes)
value_to_simplified <- value_to_simplified[!is.na(value_to_simplified$simplified), ]

# Step 5: Reclassify raster
r_numeric <- as.numeric(r)  # Convert factor raster to numeric
r_simplified <- subst(r_numeric, value_to_simplified$Value, value_to_simplified$simplified)

writeRaster(r_simplified, 
            filename = "own datasets/simplified_land_use_2000.tif", 
            overwrite = TRUE)



#___________________________________________________________________________
## EBBA grid .shp to centroids ####

grid$cell50x50 <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")

grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")


# Create centroids
centroids <- st_centroid(grid)

# Extract coordinates into columns
centroids_coords <- st_coordinates(centroids)
centroids$lon <- centroids_coords[,1]
centroids$lat <- centroids_coords[,2]

# Keep just the cell names and coordinates
centroids_df1 <- centroids[, c("cell50x50", "lon", "lat")]

# If you want a plain data.frame instead of sf
centroids_df <- st_drop_geometry(centroids_df1)


change_grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")

ebba_change_cells = change_grid$cell50x50

centroids_df_change = centroids_df %>% 
  filter(cell50x50 %in% ebba_change_cells)


write.csv(centroids_df, "own datasets/ebba_grid_50km_centroids.csv", row.names = FALSE)
write.csv(centroids_df_change, "own datasets/ebba_change_grid_50km_centroids.csv", row.names = FALSE)



#___________________________________________________________________________
# Testing ####

specific_cell = land_use_fa_1990 |> 
  filter(cell50x50 == "32VPN1")

sum(specific_cell[,2:45])



colnames(land_use_1990)
levels(land_use_raster_2018$LABEL3)[[1]]$LABEL3

# Ensure grid is an sf object
grid <- st_as_sf(grid)

# Merge change with grid based on cell50x50
land_use_1990_sf <- left_join(land_use_1990, grid, by = "cell50x50")

# Convert to sf object (ensuring geometry is recognized)
land_use_1990_sf <- st_as_sf(land_use_1990_sf)


plot <- ggplot(land_use_1990_sf) +
  geom_sf(aes(fill = `No Land Use`), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "'No Land Use' 1990")
plot
plot <- ggplot(land_use_1990_sf) +
  geom_sf(aes(fill = Salines), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "Salines 1990")
plot
plot <- ggplot(land_use_1990_sf) +
  geom_sf(aes(fill = NODATA), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "NODATA 1990")
plot



# Ensure grid is an sf object
grid <- st_as_sf(grid)

# Merge change with grid based on cell50x50
change_sf <- left_join(change, grid, by = "cell50x50")

# Convert to sf object (ensuring geometry is recognized)
change_sf <- st_as_sf(change_sf)

plot <- ggplot(change_sf) +
  geom_sf(aes(fill = `No Land Use`), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "No Land Use Change")
plot
plot <- ggplot(change_sf) +
  geom_sf(aes(fill = Salines), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "Salines Change")
plot
plot <- ggplot(change_sf) +
  geom_sf(aes(fill = NODATA), color = "black", size = 0.1) +  # Set border size for clarity
  scale_fill_viridis_c(na.value = "grey") +  # Handles NA values
  theme_minimal() +
  labs(fill = "NODATA Change")
plot






## HANPP ####

library(terra)
library(dplyr)

# Load your rasters
hanpp_90 <- rast("external datasets/HANPP Europe (Plutzar et al. 2016)/hanpp_europe_1990_percent.tif")
hanpp_06 <- rast("external datasets/HANPP Europe (Plutzar et al. 2016)/hanpp_europe_2006_percent.tif")

land_use_90 <- rast("external datasets/CORINE Land Cover CLC/u2000_clc1990_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif")
land_use_18 <- rast("external datasets/CORINE Land Cover CLC/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")


# Step 1: Crop HANPP to land use extent (smaller area)
hanpp_cropped <- crop(hanpp_06, land_use_18)

# Step 2: Resample HANPP to match land use resolution & grid
hanpp_resampled <- resample(hanpp_cropped, land_use_18, method = "near")

# Step 3: Crop land use to HANPP extent (optional sanity check)
land_use_cropped <- crop(land_use_18, hanpp_resampled)

# Step 4: Zonal min/max HANPP per land use class
hanpp_min <- zonal(hanpp_resampled, land_use_cropped, fun = "min", na.rm = TRUE)
hanpp_max <- zonal(hanpp_resampled, land_use_cropped, fun = "max", na.rm = TRUE)

# Rename and merge tables
names(hanpp_min) <- c("LandUse", "min_hanpp")
names(hanpp_max) <- c("LandUse", "max_hanpp")
hanpp_stats <- merge(hanpp_min, hanpp_max, by = "LandUse")


# Define simplified land use groups
forest_classes <- c("Broad-leaved forest", "Coniferous forest", "Mixed forest")
agriculture_classes <- c("Non-irrigated arable land",
                         "Permanently irrigated land", 
                         "Rice fields",
                         "Vineyards",
                         "Fruit trees and berry plantations", 
                         "Olive groves",
                         "Pastures",
                         "Annual crops associated with permanent crops", 
                         "Complex cultivation patterns", 
                         "Land principally occupied by agriculture, with significant areas of natural vegetation", 
                         "Agro-forestry areas")

# Add simplified group labels to the zonal stats table
hanpp_stats$Group <- case_when(
  hanpp_stats$LandUse %in% forest_classes ~ "forest",
  hanpp_stats$LandUse %in% agriculture_classes ~ "agriculture",
  grepl("urban|Airport|Industrial|Construction", hanpp_stats$LandUse, ignore.case = TRUE) ~ "urban",
  TRUE ~ "other"
)

# Combine HANPP min/max by group
group_stats <- hanpp_stats %>%
  group_by(Group) %>%
  summarise(
    min_hanpp = min(min_hanpp, na.rm = TRUE),
    max_hanpp = max(max_hanpp, na.rm = TRUE)
  ) %>%
  filter(!is.na(Group))


# Stack rasters
stacked <- c(hanpp_resampled, land_use_cropped)
names(stacked) <- c("HANPP", "LandUse")

# Convert to data frame (with x/y so you can map later)
df <- as.data.frame(stacked, xy = TRUE, na.rm = TRUE)

# Join group info back to pixels
df <- df %>%
  left_join(hanpp_stats[, c("LandUse", "Group")], by = "LandUse") %>%
  left_join(group_stats, by = "Group") %>%
  mutate(
    relative = (HANPP - min_hanpp) / (max_hanpp - min_hanpp),
    intensity = ifelse(relative < 0.5, "extensive", "intensive"),
    class_label = paste0(Group, "_", intensity)
  )

# Optional: check how many pixels per class
table(df$class_label)

# Convert back to raster
# Only keep x, y and the new class
df_small <- df[, c("x", "y", "class_label")]
r_template <- hanpp_resampled

# Create categorical raster from points
r_out <- rast(df_small, type = "xyz", crs = crs(r_template))

# Save the final raster
#writeRaster(r_out, "hanpp_intensity_by_landuse.tif", overwrite = TRUE)
