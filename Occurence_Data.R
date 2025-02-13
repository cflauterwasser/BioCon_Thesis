
#___________________________________________________________________________
# Loading Packages ####

library(rgbif)
library(tidyverse)
library(geodata)
library(ggplot2)
library(sf)
library(data.table)




#___________________________________________________________________________
# GBIF (Mammals) ####

#___________________________________________________________________________
## Preparation for Download Queries ####

### Import Species List ####

#european_species_habitats = read.csv("own datasets/Habitat_Information.csv")
#european_mammals = european_species_habitats |>
#  filter(Class == "Mammalia")
#species_list <- european_mammals$Binomial.Name
#taxon_keys <- sapply(species_list, function(x) name_backbone(name = x)$usageKey)



### Or simply all "Mammalia" ####

mammalia_key <- name_backbone(name = "Mammalia")$usageKey



#___________________________________________________________________________
### Shapefile of Europe ####

# Read shapefile
shapefile_path <- "external datasets/Europe Shapefile/europe_simplified_coast.shp"

shape <- st_read(shapefile_path)

# Ensure the CRS is suitable for WKT (typically WGS84, EPSG:4326)
shape <- st_transform(shape, crs = 4326)

# Convert the geometry to WKT
wkt_region <- st_as_text(st_union(shape))  # st_union combines multiple geometries into one if needed

# Print or use the WKT
print(wkt_region)


# double checking correctness of shape file
geometry <- st_as_sfc(wkt_region, crs = 4326)

# Create an sf object for plotting
sf_object <- st_sf(geometry = geometry)

# Plot with a basemap
ggplot() +
  annotation_map_tile(type = "osm") +  # Add OpenStreetMap basemap
  geom_sf(data = sf_object, color = "red", fill = NA, size = 1) +
  coord_sf() +
  theme_minimal() +
  labs(title = "WKT Geometry Overlay", subtitle = "Check alignment with basemap")



#___________________________________________________________________________
### Testing Queries ####

occ_download_prep(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_lte("year", 2000)
  )
)


occ_download_prep(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_gte("year", 2015)
  )
)

#___________________________________________________________________________
## Downloading Datasets  ####

#___________________________________________________________________________
### Initiating Download Process  ####

download_request_b2000 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_lte("year", 2000),
    pred("HAS_GEOSPATIAL_ISSUE",FALSE),
    pred("HAS_COORDINATE",TRUE),
    pred("OCCURRENCE_STATUS","PRESENT"),
    pred_or(
      pred_not(pred_in("establishmentMeans","MANAGED")),
      pred_isnull("establishmentMeans"))
  ),
  format = "SIMPLE_CSV"
)

download_request_a2015 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_gte("year", 2015),
    pred("HAS_GEOSPATIAL_ISSUE",FALSE),
    pred("HAS_COORDINATE",TRUE),
    pred("OCCURRENCE_STATUS","PRESENT"),
    pred_or(
      pred_not(pred_in("establishmentMeans","MANAGED")),
      pred_isnull("establishmentMeans"))
  ),
  format = "SIMPLE_CSV"
)


#___________________________________________________________________________
### Status and Data Download  ####

#cancel request(s)
#downloads <- occ_download_list(limit = 100)
#downloads
#occ_download_cancel(downloads$results$key[1])
#occ_download_cancel(downloads$results$key[2])

#download_request_a2015 = downloads$results$key[1]
#download_request_b2000 = downloads$results$key[2]


# for checking status of download
occ_download_meta(download_request_b2000)
occ_download_meta(download_request_a2015)


# Download and save data
download_path <- "external datasets/GBIF/"
before_2000 = occ_download_get(download_request_b2000, path = download_path, overwrite = TRUE)
after_2015 = occ_download_get(download_request_a2015, path = download_path, overwrite = TRUE)
#options(timeout = 300)




## Importing Datasets ####

#dwca_before_2000 <- occ_download_import(as.download("external datasets/GBIF/0003238-250121130708018.zip"))
#dwca_after_2015 <- occ_download_import(as.download("external datasets/GBIF/0003239-250121130708018.zip"))


csv_before_2000 <- fread("external datasets/GBIF/0002113-250123221155621.csv")
csv_after_2015 <- fread("external datasets/GBIF/0002114-250123221155621.csv")




#___________________________________________________________________________
## Investigating Datasets ####

# structure
str(csv_before_2000)
str(csv_after_2015)


# temporal distribution
min(csv_before_2000$year)
boxplot(csv_before_2000$year)

boxplot(csv_after_2015$year)



# checking levels of different variables
check.levels = function(x){
  x = as.factor(x)
  return(summary(x))
}

# basis of record
check.levels(csv_before_2000$basisOfRecord)
check.levels(csv_after_2015$basisOfRecord)
# need to exclude fossil specimen!


# established means
check.levels(csv_before_2000$establishmentMeans)
check.levels(csv_after_2015$establishmentMeans)
# not useful to exclude domesticated species, have to do it by hand!


# coordinate uncertainty
summary(csv_before_2000$coordinateUncertaintyInMeters)
summary(csv_after_2015$coordinateUncertaintyInMeters)

boxplot(csv_before_2000$coordinateUncertaintyInMeters)
boxplot(csv_after_2015$coordinateUncertaintyInMeters)

a = csv_before_2000$decimalLongitude
head(a)

#___________________________________________________________________________
## Cleaning Datasets  ####


#___________________________________________________________________________
### Excluding Fossil and Extinct Species  ####

exclude.extinct = function(df){
  filtered = df |> 
    filter(basisOfRecord != "FOSSIL_SPECIMEN") |> 
    filter(!species %in% c("Mammuthus primigenius",
                           "Megaceroides verticornis",
                           "Megaloceros giganteus",
                           "Coelodonta antiquitatis",
                           "Machairodus giganteus",
                           "Oreopithecus bambolii",
                           "Ursus spelaeus",
                           "Metaxytherium petersi",
                           "Equus ferus",
                           "Samotherium major",
                           "Homo neanderthalensis",
                           "Ancylotherium pentelicum",
                           "Cremohipparion matthewi",
                           "Choerolophodon pentelici",
                           "Hippopotamodon erymanthius",
                           "Trogontherium minus",
                           "Equus hydruntinus",
                           "Cervus elaphoides",
                           "Dryopithecus fontani",
                           "Ceratotherium neumayri"))
  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000 = exclude.extinct(csv_before_2000)

m_a2015 = exclude.extinct(csv_after_2015)




#___________________________________________________________________________
### Excluding Marine Families  ####

exclude.marine  = function(df){
  filtered = df |>  
    filter(!family %in% c("Balaenidae", # whale families
                          "Balaenopteridae",
                          "Delphinidae",
                          "Monodontidae",
                          "Phocoenidae",
                          "Kogiidae",
                          "Physeteridae",
                          "Ziphiidae",
                          "Phocidae", # seals
                          "Odobenidae")) |> # walruses
    filter(order != "Cetacea") # exclude outdated order "Cetacea"
  
  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_t <- exclude.marine(m_b2000)

m_a2015_t <- exclude.marine(m_a2015)




#___________________________________________________________________________
### Excluding Domesticated Species  ####

exclude.domesticated  = function(df){
  filtered = df |>
  filter(!species %in% c("Felis catus",
                         "Bos taurus",
                         "Ovis aries",
                         "Capra hircus",
                         "Camelus bactrianus",
                         "Camelus dromedarius",
                         "Vicugna pacos",
                         "Lama glama",
                         "Cavia porcellus",
                         "Bubalus bubalis",
                         "Equus asinus",
                         "Equus caballus",
                         "Bos grunniens")) |> 
  filter(infraspecificEpithet != "domesticus") |> 
  filter(!(species == "Canis lupus" & infraspecificEpithet == "familiaris"))
  
  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_tw = exclude.domesticated(m_b2000_t)

m_a2015_tw = exclude.domesticated(m_a2015_t)




#___________________________________________________________________________
### Excluding Captivity Anomalies ####

exclude.captive  = function(df){
  filtered = df |>
  filter(!species %in% c("Ailurus fulgens",
                         "Panthera tigris",
                         "Panthera leo",
                         "Phacochoerus africanus",
                         "Giraffa camelopardalis",
                         "Giraffa giraffa",
                         "Setifer setosus",
                         "Ailuropoda melanoleuca",
                         "Puma concolor",
                         "Brachyteles arachnoides",
                         "Vicugna vicugna")) |> 
  filter(order != "Primates") |> 
  filter(order != "Proboscidea") |> 
  filter(family != "Rhinocerotidae") |> 
  filter(family != "Rhinocerotidae")
# TO BE CONTINUED!!!
  
  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_twr = exclude.captive(m_b2000_tw)

m_a2015_twr = exclude.captive(m_a2015_tw)




#___________________________________________________________________________
### Removing Useless Columns  ####

#str(m_b2000_twr)
#summary(m_b2000_twr)
sum(is.na(m_b2000_twr$institutionCode))
check.levels(m_b2000_twr$institutionCode)

reduce.columns <- function(df){
  output = df[ , c("gbifID",
                   "occurrenceID",
                   "class",
                   "order",
                   "family",
                   "genus",
                   "species",
                   "infraspecificEpithet",
                   "individualCount",
                   "decimalLatitude",
                   "decimalLongitude",
                   "coordinateUncertaintyInMeters",
                   "year",
                   "basisOfRecord",
                   "establishmentMeans",
                   "occurrenceStatus",
                   "issue")]
  return(output)
}

m_b2000_twr_red = reduce.columns(m_b2000_twr)
head(m_b2000_twr_red)

m_a2015_twr_red = reduce.columns(m_a2015_twr)
head(m_a2015_twr_red)




#___________________________________________________________________________
## Exporting Data  ####


write.csv(m_b2000_twr_red, "own datasets/m_b2000_cleaned.csv", row.names = FALSE)
write.csv(m_a2015_twr_red, "own datasets/m_a2015_cleaned.csv", row.names = FALSE)




#___________________________________________________________________________
## Extracting Species Lists  ####

# Extract unique species lists from both datasets

species.list = function(df){
  species_list = df %>%
    filter(!is.na(species) & species != "") %>%  # Remove rows with missing or empty species
    group_by(class, order, family, species) %>%
    summarise(Count = n(), .groups = "drop")
  return(species_list)
}


m_species_list_b2000 <- species.list(m_b2000_twr)

m_species_list_a2015 <- species.list(m_a2015_twr)



# Combine datasets
m_combined_species_list <- bind_rows(m_species_list_b2000, m_species_list_a2015) %>%
  group_by(class, order, family, species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list) <- tools::toTitleCase(colnames(m_combined_species_list))

# View result
head(m_combined_species_list)

# Export file
write.csv(m_combined_species_list, "own datasets/Mammal_Species_List.csv", row.names = FALSE)




#___________________________________________________________________________
## Visualization of Data  ####

world_data = world(path = ".")
ext_europe = c(-28, 64, 28, 81)


png("plots/occurence_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_b2000_twr$decimalLongitude, m_b2000_twr$decimalLatitude,
       col = "#2e6f4040", 
       cex = .2)
dev.off()


png("plots/occurence_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_a2015_twr$decimalLongitude, m_a2015_twr$decimalLatitude,
       col = "#e64f4040",
       cex = .2)
dev.off()




#___________________________________________________________________________
# EBBA (Birds) ####

ebba1 <- fread("external datasets/EBBA/ebba1_data_gbif_50km.csv")
ebba2 <- fread("external datasets/EBBA/ebba2_data_occurrence_50km.csv")
str(ebba1)
str(ebba2)



#___________________________________________________________________________
## Cleaning Datasets ####

ebba1 = reduce.columns(ebba1)

ebba1 = ebba1 |> 
  filter(occurrenceStatus == "PRESENT")

check.levels(ebba1$occurrenceStatus)

uniqueN(ebba1$species) #491 species



ebba2 = ebba2 |> 
  filter(occurrence == 1)

check.levels(ebba2$occurrence)

uniqueN(ebba2$birdlife_scientific_name) #30 species, contact responsible people


# add Class, Order and Family

taxa_info <- unique(ebba2$birdlife_scientific_name) %>%
  map_df(~ name_backbone(name = ., kingdom = "Animalia"))


taxa_info <- taxa_info %>%
  select(canonicalName, order, family)

taxa_info <- taxa_info %>%
  rename(birdlife_scientific_name = canonicalName)

ebba2 <- ebba2 %>%
  left_join(taxa_info, by = "birdlife_scientific_name") %>%
  mutate(Class = "Aves") %>%
  rename(Species = birdlife_scientific_name)

ebba2 <- ebba2 %>%
  rename(Order = order, Family = family)



str(ebba2)


#___________________________________________________________________________
## Extracting Species Lists  ####

# Extract unique species lists from both datasets

b1_species_list <- ebba1 %>%
  filter(!is.na(species) & species != "") %>%
  group_by(class, order, family, species) %>%
  summarise(Count = n(), .groups = "drop")

b2_species_list <- ebba2 %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")


# Adjust column names to have uppercase first letters
colnames(b1_species_list) <- tools::toTitleCase(colnames(b1_species_list))

# View result
head(b1_species_list)
head(b2_species_list)

# combine files
b_species_list <- bind_rows(b1_species_list, b2_species_list) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Export file
write.csv(b_species_list, "own datasets/Bird_Species_List.csv", row.names = FALSE)



#___________________________________________________________________________
## Exporting Data  ####

write.csv(ebba1, "own datasets/ebba1_cleaned.csv", row.names = FALSE)
write.csv(ebba2, "own datasets/ebba2_cleaned.csv", row.names = FALSE)
