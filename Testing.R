#___________________________________________________________________________
#1. LOADING PACKAGES ####

library(tidyverse)
library(BioTIMEr)
library(geodata)
library(terra)
library(taxadb)
library(rgbif)
library(sf)
library(raster)
library(rredlist)
library(openxlsx)
library(stringr)
#library(predictsFunctions)




#___________________________________________________________________________
#2. AFFINITY DATA MINING ####


#___________________________________________________________________________
## GBIF ####



### Pulling Data for Birds + Mammals in Europe ####

# testing with 1000 records each - how far do we take this??


# Fetch Birds (Aves)
GBIF_bird_data <- occ_search(
  taxonKey = name_backbone(name = "Aves")$usageKey,
  continent = "Europe",
  limit = 1000,
  #year = paste(start_year, end_year, sep = ","), 
  hasCoordinate = TRUE
)


# Fetch Mammals (Mammalia)
GBIF_mammal_data <- occ_search(
  taxonKey = name_backbone(name = "Mammalia")$usageKey,
  continent = "Europe",
  limit = 1000, 
  #year = paste(start_year, end_year, sep = ","), 
  hasCoordinate = TRUE
)


### Combine and Clean Data ####

# Combine bird and mammal data
all_data <- bind_rows(
  GBIF_bird_data$data, 
  GBIF_mammal_data$data
)

# Clean the data (e.g., remove duplicates, keep only relevant columns)
cleaned_data <- all_data %>%
  dplyr::select(species = scientificName, year = year, latitude = decimalLatitude, longitude = decimalLongitude) %>%
  filter(!is.na(species) & !is.na(year))


# estimate pan-european abundances
pan_european_abundances <- cleaned_data %>%
  group_by(species, year) %>%
  summarize(abundance = n(), .groups = "drop")

# quite low abundance numbers, why is year exclusively 2025?
summary(pan_european_abundances$year)

### Exporting Data ####
write.csv(pan_european_abundances, "own datasets/GBIF_abundances_pan_european.csv", row.names = FALSE)



### Assigning each Occurrence to Grid Cell ####

gbif_data <- bird_data_check_before_1990$data
str(gbif_data)

# Step 1: Convert GBIF data to a spatial object
# Make sure the names of the columns match those in your GBIF dataset
gbif_sf <- st_as_sf(gbif_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Step 2: Assign each occurrence to the appropriate grid cell
# Assuming 'europe_grid_sf' is your predefined grid for Europe
gbif_grid_cells <- st_intersection(gbif_sf, europe_grid_sf)

# Step 3: Check results (grid cell assignment)
head(gbif_grid_cells)



### Aggregating Abundance by Grid Cell ####

# Step 4: Calculate abundance (count occurrences) by species and grid cell
abundance_grid <- gbif_grid_cells %>%
  group_by(species = scientificName, year, grid_cell_id) %>%
  summarize(abundance = n(), .groups = "drop")

# Step 5: Ensure species observed in both time periods (pre-1990 and post-2010)
species_in_both_periods <- abundance_grid %>%
  group_by(species) %>%
  filter(any(year < 1990) & any(year > 2010)) %>%
  ungroup() %>%
  pull(species)

# Step 6: Filter the final grid for species that are present in both time periods
final_abundance_grid <- abundance_grid %>%
  filter(species %in% species_in_both_periods)



### Exporting Data ####

write.csv(final_abundance_grid, "GBIF_abundance_grid_europe.csv", row.names = FALSE)




#___________________________________________________________________________
##  EBBA ####




#___________________________________________________________________________
##  BioTIME ####

biotime = read.csv("external datasets/BioTIMEQuery_24_06_2021.csv")

str(biotime)
summary(biotime)

max(biotime$YEAR)
hist(biotime$YEAR)

resurvey_2020 <- biotime %>%
  filter(YEAR >= 2010)

hist(resurvey_2020$YEAR)



### adding metadata ####

biotime_metadata = read.csv("external datasets/BioTIMEMetadata_24_06_2021.csv")
str(biotime_metadata)


biotime <- biotime %>%
  left_join(
    biotime_metadata %>% 
      dplyr::select(STUDY_ID, REALM, HABITAT, PROTECTED_AREA, BIOME_MAP, ABUNDANCE_TYPE, TAXA),
    by = "STUDY_ID"
  )

str(biotime)


biotime$HABITAT = as.factor(biotime$HABITAT)
summary(biotime$HABITAT)



### filtering for birds and mammals ####

# approach 1:
biotime$TAXA = as.factor(biotime$TAXA)
summary(biotime$TAXA)

biotime_birds_mammals <- biotime %>% 
  filter(TAXA %in% c("Birds", "Mammals"))

# approach 2:

# Step 1: Fetch list of birds and mammals from taxadb using pull()
bird_mammal_families <- taxa_tbl("itis") %>% 
  filter(class %in% c("Aves", "Mammalia")) %>% 
  pull(scientificName)  # Extract the names as a vector

# Step 2: Filter dataset using extracted vector
biotime_cleaned <- biotime %>% 
  filter(GENUS_SPECIES %in% bird_mammal_families)

biotime_cleaned <- biotime_cleaned %>%
  mutate(CLASS = case_when(
    GENUS_SPECIES %in% (taxa_tbl("itis") %>% filter(class == "Aves") %>% pull(scientificName)) ~ "Aves",
    GENUS_SPECIES %in% (taxa_tbl("itis") %>% filter(class == "Mammalia") %>% pull(scientificName)) ~ "Mammalia",
    TRUE ~ NA_character_  # Optional, in case there's something unexpected
  ))

biotime_cleaned$CLASS = as.factor(biotime_cleaned$CLASS)
summary(biotime_cleaned$CLASS)


# comparing numbers
nrow(biotime_cleaned)/nrow(biotime_birds_mammals)



#write.csv(biotime_cleaned, "own datasets/BioTIME_aves_mammalia.csv", row.names = FALSE)
# too large for GitHub



### Filtering for terrestrial occurences ####

biotime_terrestrial = biotime_cleaned |> 
  filter(REALM == "Terrestrial")

nrow(biotime_terrestrial)/nrow(biotime_cleaned)


# checking habitats
summary(biotime_terrestrial$HABITAT)
# no information on land use!



### filtering for European occurences ####

#biotime_europe <- biotime_cleaned %>%
#  filter(LATITUDE >= 36.0 & LATITUDE <= 71.0 & 
#           LONGITUDE >= -25.0 & LONGITUDE <= 60.0)



### filtering for 1990-2010 gap and reoccuring species ####

# Step 1: Filter the dataset for the two time blocks
#biotime_filtered <- biotime_europe %>%
#  filter(YEAR <= 1990 | YEAR >= 2010)

# Step 2: Identify species present in both time blocks within each STUDY_ID
#species_in_both_blocks <- biotime_filtered %>%
#  mutate(TIME_BLOCK = case_when(
#    YEAR <= 1990 ~ "Before_1990",
#    YEAR >= 2010 ~ "After_2010"
#  )) %>%
#  group_by(STUDY_ID, GENUS_SPECIES) %>%
#  summarize(
#    blocks_observed = n_distinct(TIME_BLOCK),
#    .groups = "drop"
#  ) %>%
#  filter(blocks_observed == 2) %>%  # Only species present in both blocks
#  pull(GENUS_SPECIES)

# Step 3: Filter the dataset to keep only those species
#biotime_final <- biotime_filtered %>%
#  filter(GENUS_SPECIES %in% species_in_both_blocks)

### exporting dataset ####
write.csv(biotime_europe, "own datasets/BioTIME_filtered.csv", row.names = FALSE)




#___________________________________________________________________________
## PREDICTS ####

#webFile <- url("https://timnewbold.github.io/predicts_database.rds?dl=1")
#predicts <- readRDS(webFile)

#str(predicts)
#summary(predicts$Diversity_metric)


#predicts_SR <- predicts %>%
#  filter(Diversity_metric == "species richness")

#ggplot(predicts_SR, aes(x = Measurement)) +
#  geom_histogram(fill = "darkgreen",
#                 color = "black") +
#  labs(x = "Measurement",
#       y = "Frequency") +
#  theme_minimal()


#predicts_abundance <- predicts %>%
#  filter(Diversity_metric == "abundance")

#ggplot(predicts_abundance, aes(x = Measurement)) +
#  geom_histogram(fill = "darkgreen",
#                 color = "black") +
#  labs(x = "Measurement",
#       y = "Frequency") +
#  theme_minimal()



### filtering for birds and mammals ####



### Filtering for terrestrial occurences ####



### filtering for European occurences ####



#___________________________________________________________________________
#3. CLIMATE DATA MINING ####

#___________________________________________________________________________
## WorldClim ####

#wc_data <- worldclim_global(var = "bio", res = 10, path = "./external datasets") # downloading BioClim variables at 10-minute resolution

#bio1 <- wc_data[[1]] # Loading specific variable (e.g., Bio1 - Annual Mean Temperature)

#plot(bio1)

