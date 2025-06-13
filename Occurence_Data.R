
#___________________________________________________________________________
# Loading Packages ####

library(rgbif)
library(tidyverse)
library(geodata)
library(ggplot2)
library(sf)
library(data.table)




#___________________________________________________________________________
# GBIF MAMMALS ####

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

# Ensure the CRS is suitable for WKT
shape <- st_transform(shape, crs = 4326)

# Convert the geometry to WKT
wkt_region <- st_as_text(st_union(shape)) 



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
### Removing Useless Columns  ####

#str(csv_before_2000)
#summary(csv_before_2000)
sum(is.na(csv_before_2000$institutionCode))
check.levels(csv_before_2000$institutionCode)

reduce.columns <- function(df){
  output = df[ , c("class",
                   "order",
                   "family",
                   "genus",
                   "species",
                   "infraspecificEpithet",
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

csv_before_2000_r = reduce.columns(csv_before_2000)
head(csv_before_2000_r)

csv_after_2015_r = reduce.columns(csv_after_2015)
head(csv_after_2015_r)




#___________________________________________________________________________
### correcting taxonomy to fit IUCN  ####

correct.taxonomy.m = function(df) {
  df1 = df %>% 
    mutate(species = recode(species, 
                            "Chionomys nivalis" = "Chionomys syriacus",
                            "Cricetulus migratorius" = "Nothocricetulus migratorius",
                            "Microtus bavaricus" = "Microtus liechtensteini",
                            "Microtus gerbei" = "Microtus gerbii",
                            "Microtus gregalis" = "Stenocranius gregalis",
                            "Microtus levis" = "Microtus mystacinus",
                            "Microtus oeconomus" = "Alexandromys oeconomus",
                            "Myodes glareolus" = "Clethrionomys glareolus",
                            "Myodes rufocanus" = "Craseomys rufocanus",
                            "Myodes rutilus" = "Clethrionomys rutilus",
                            "Tamias sibiricus" = "Eutamias sibiricus",
                            "Spalax leucodon" = "Nannospalax leucodon"
    ))
  cat(sprintf("%.2f%% of occurrences were merged/reduced.\n", round((1 - nrow(df1) / nrow(df)) * 100, 2)))
  return(df1)
}

csv_before_2000_r1 = correct.taxonomy.m(csv_before_2000_r)
csv_after_2015_r1 = correct.taxonomy.m(csv_after_2015_r)

uniqueN(csv_before_2000_r1$species)/uniqueN(csv_before_2000_r$species)
uniqueN(csv_after_2015_r1$species)/uniqueN(csv_after_2015_r$species)




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


m_b2000 = exclude.extinct(csv_before_2000_r1)

m_a2015 = exclude.extinct(csv_after_2015_r1)




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
### Exporting Data  ####


write.csv(m_b2000_twr, "own datasets/m_b2000_cleaned.csv", row.names = FALSE)
write.csv(m_a2015_twr, "own datasets/m_a2015_cleaned.csv", row.names = FALSE)




#___________________________________________________________________________
## Reducing to Conservative IUCN Species List  ####

#m_b2000_twr_red = read.csv("own datasets/m_b2000_cleaned.csv")
#m_a2015_twr_red = read.csv("own datasets/m_a2015_cleaned.csv")

csv_before_2000 <- fread("external datasets/GBIF/0002113-250123221155621.csv")
csv_after_2015 <- fread("external datasets/GBIF/0002114-250123221155621.csv")

iucn_eur_mammals = read.csv("external datasets/IUCN Red List - European Mammals/European_Mammals_Red_List_09.11.07.csv")
iucn_eur_mammals = iucn_eur_mammals[3:6]
summary(iucn_eur_mammals)


iucn_eur_mammals <- iucn_eur_mammals %>%
  mutate(Scientific.name = recode(Scientific.name, 
                                  "Alopex lagopus" = "Vulpes lagopus", 
                                  #"Mustela sibirica" = "new_name",       # Russia, barely Europe
                                  #"Bos primigenius" = "new_name"),       # Aurochs, extinct
                                  #"Sousa chinensis" = "new_name",        # Indo-Pacific dolphin
                                  #"Eptesicus bottae" = "new_name",       # West Asia
                                  "Myotis aurascens" = "Myotis davidii",
                                  "Pipistrellus savii" = "Hypsugo savii",
                                  #"Plecotus sardus" = "new_name",        # only Sardinia 2001-2009
                                  #"Crocidura canariensis" = "new_name",  # Canarian shrew, only 2007
                                  #"Crocidura zimmermanni" = "new_name",  # Cretan shrew
                                  #"Sorex arunchi" = "new_name",          # GBIF no occ with coord
                                  #"Prolagus sardus" = "new_name",        # Sardinian pika, extinct
                                  #"Microtus felteni" = "new_name",       # no records in time window
                                  "Microtus middendorffii" = "Microtus middendorffi",
                                  #"Mus cypriacus" = "new_name",          # GBIF no occ with coord
                                  #"Spalax nehringi" = "new_name"         # West Asia, GBIF 2013
  ))


filter_species <- function(df, reference_df) {
  original_species <- unique(df$species)
  reference_species <- unique(reference_df$Scientific.name)
  
  # Filter dataframe
  filtered_df <- df[df$species %in% reference_species, ]
  remaining_species <- unique(filtered_df$species)
  
  # Find species from the reference list that were not found in the original dataframe
  missing_species <- setdiff(reference_species, original_species)
  
  # Print stats
  cat("Original species count:", length(original_species), "\n")
  cat("Remaining species count:", length(remaining_species), "\n")
  cat("Species removed:", length(original_species) - length(remaining_species), "\n")
  cat("Species in IUCN list but not found in the dataset:", length(missing_species), "\n\n")
  
  # Return both the filtered dataframe and missing species as a list
  return(list(filtered_df = filtered_df, missing_species = missing_species))
}

m_b2000_f <- filter_species(csv_before_2000, iucn_eur_mammals)
m_a2015_f <- filter_species(csv_after_2015, iucn_eur_mammals)

intersect(m_b2000_f$missing_species, m_a2015_f$missing_species)


m_b2000_f <- m_b2000_f$filtered_df
m_a2015_f <- m_a2015_f$filtered_df

# comparison with unreduced data
#cat("b2000 reduced by", uniqueN(csv_before_2000$species)-uniqueN(m_b2000_f$species), "species (",(1-(uniqueN(m_b2000_f$species)/uniqueN(csv_before_2000$species)))*100, "% ) through application of IUCN species list.", "\n")
#cat("a2015 reduced by", uniqueN(csv_after_2015$species)-uniqueN(m_a2015_f$species), "species (",(1-(uniqueN(m_a2015_f$species)/uniqueN(csv_after_2015$species)))*100, "% ) through application of IUCN species list.", "\n")

#cat("b2000 reduced by", (1-(nrow(m_b2000_f)/nrow(csv_before_2000)))*100, "% of observations through application of IUCN species list.", "\n")
#cat("a2015 reduced by", (1-(nrow(m_a2015_f)/nrow(csv_after_2015)))*100, "% of observations through application of IUCN species list.", "\n")


m_b2000_fr = reduce.columns(m_b2000_f)
m_a2015_fr = reduce.columns(m_a2015_f)

m_b2000_fr = correct.taxonomy.m(m_b2000_fr)
m_a2015_fr = correct.taxonomy.m(m_a2015_fr)

m_b2000_fre = exclude.extinct(m_b2000_fr)
m_a2015_fre = exclude.extinct(m_a2015_fr)

m_b2000_fret <- exclude.marine(m_b2000_fre)
m_a2015_fret <- exclude.marine(m_a2015_fre)

m_b2000_fretw = exclude.domesticated(m_b2000_fret)
m_a2015_fretw = exclude.domesticated(m_a2015_fret)

m_b2000_fretwr = exclude.captive(m_b2000_fretw)
m_a2015_fretwr = exclude.captive(m_a2015_fretw)

# how many species remain after cleaning
cat("In b2000", uniqueN(m_b2000_f$species)-uniqueN(m_b2000_fretwr$species), "species (", (1-(uniqueN(m_b2000_fretwr$species)/uniqueN(m_b2000_f$species)))*100, "% )", "were removed through cleaning.",  "\n")
cat("In b2000", uniqueN(m_a2015_f$species)-uniqueN(m_a2015_fretwr$species), "species (", (1-(uniqueN(m_a2015_fretwr$species)/uniqueN(m_a2015_f$species)))*100, "% )", "were removed through cleaning.",  "\n")


# comparison with unreduced data
#cat("b2000 reduced by",
#    uniqueN(m_b2000_twr$species)-uniqueN(m_b2000_fretwr$species),
#    "species (",
#    (1-(uniqueN(m_b2000_fretwr$species)/uniqueN(m_b2000_twr$species)))*100,
#    "% ) through application of IUCN species list.", "\n")
#cat("a2015 reduced by",
#    uniqueN(m_a2015_twr$species)-uniqueN(m_a2015_fretwr$species),
#    "species (",
#    (1-(uniqueN(m_a2015_fretwr$species)/uniqueN(m_a2015_twr$species)))*100,
#    "% ) through application of IUCN species list.", "\n")

#cat("b2000 reduced by",
#    (1-(nrow(m_b2000_fretwr)/nrow(m_b2000_twr)))*100,
#    "% of observations through application of IUCN species list.", "\n")
#cat("a2015 reduced by",
#    (1-(nrow(m_a2015_fretwr)/nrow(m_a2015_twr)))*100,
#    "% of observations through application of IUCN species list.", "\n")


write.csv(m_b2000_fretwr, "own datasets/m_b2000_cleaned_shortened.csv", row.names = FALSE)
write.csv(m_a2015_fretwr, "own datasets/m_a2015_cleaned_shortened.csv", row.names = FALSE)



#___________________________________________________________________________
## Extracting Species Lists  ####

#___________________________________________________________________________
### 'Original' 500+ GBIF species list  ####

# Extract unique species lists from both datasets

m_b2000_twr = fread("own datasets/m_b2000_cleaned.csv")
m_a2015_twr = fread("own datasets/m_a2015_cleaned.csv")


grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")


# masking for ebba 50km grid
# Define the function
filter_occurrences_by_grid <- function(occ_data, grid_shapefile) {
  # Convert occurrences to an sf object (assuming WGS84 CRS)
  occ_sf <- st_as_sf(occ_data, 
                     coords = c("decimalLongitude", "decimalLatitude"), 
                     crs = 4326)  # EPSG:4326 (WGS84)
  
  # Preserve original lat/lon before transformation
  occ_sf$decimalLongitude <- occ_data$decimalLongitude
  occ_sf$decimalLatitude <- occ_data$decimalLatitude
  
  # Transform occurrences to match the grid CRS
  occ_sf <- st_transform(occ_sf, crs = st_crs(grid_shapefile))
  
  # Perform spatial join to filter occurrences within grid
  filtered_sf <- st_join(occ_sf, grid_shapefile, left = FALSE)
  
  # Convert back to dataframe (removing geometry column but keeping lat/lon)
  filtered_df <- as.data.frame(filtered_sf) %>%
    dplyr::select(-geometry)  # Remove geometry column if not needed
  
  return(filtered_df)
}


m_b2000_twrf = filter_occurrences_by_grid(m_b2000_twr, grid_50km)
m_a2015_twrf = filter_occurrences_by_grid(m_a2015_twr, grid_50km)


cat("b2000 reduced by",
    uniqueN(m_b2000_twr$species)-uniqueN(m_b2000_twrf$species),
    "species (",
    (1-(uniqueN(m_b2000_twrf$species)/uniqueN(m_b2000_twr$species)))*100,
    "% ) through masking with EBBA 50km grid.", "\n")
cat("a2015 reduced by",
    uniqueN(m_a2015_twr$species)-uniqueN(m_a2015_twrf$species),
    "species (",
    (1-(uniqueN(m_a2015_twrf$species)/uniqueN(m_a2015_twr$species)))*100,
    "% ) through masking with EBBA 50km grid.", "\n")

cat("b2000 reduced by",
    (1-(nrow(m_b2000_twrf)/nrow(m_b2000_twr)))*100,
    "% of observations through masking with EBBA 50km grid.", "\n")
cat("a2015 reduced by",
    (1-(nrow(m_a2015_twrf)/nrow(m_a2015_twr)))*100,
    "% of observations through masking with EBBA 50km grid.", "\n")


species.list = function(df){
  species_list = df %>%
    filter(!is.na(species) & species != "") %>%  # Remove rows with missing or empty species
    group_by(class, order, family, species) %>%
    summarise(Count = n(), .groups = "drop")
  return(species_list)
}


m_species_list_b2000 <- species.list(m_b2000_twrf)
m_species_list_a2015 <- species.list(m_a2015_twrf)



# Combine datasets
m_combined_species_list <- bind_rows(m_species_list_b2000, m_species_list_a2015) %>%
  group_by(class, order, family, species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list) <- tools::toTitleCase(colnames(m_combined_species_list))

# View result
head(m_combined_species_list)

# Export file
write.csv(m_combined_species_list, "own datasets/Species_List_Mammals.csv", row.names = FALSE)


#___________________________________________________________________________,,,,2
### Conservative IUCN Species List  ####

m_b2000_fretwr = fread("own datasets/m_b2000_cleaned_shortened.csv")
m_a2015_fretwr = fread("own datasets/m_a2015_cleaned_shortened.csv")

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")

# masking with EBBA 50km grid
m_b2000_fretwr_filtered <- filter_occurrences_by_grid(m_b2000_fretwr, grid_50km)
m_a2015_fretwr_filtered <- filter_occurrences_by_grid(m_a2015_fretwr, grid_50km)


# species reduction through masking
cat("b2000 reduced by",
    uniqueN(m_b2000_fretwr$species)-uniqueN(m_b2000_fretwr_filtered$species),
    "species (",
    (1-(uniqueN(m_b2000_fretwr_filtered$species)/uniqueN(m_b2000_fretwr$species)))*100,
    "% ) through masking with EBBA 50km grid.", "\n")
cat("a2015 reduced by",
    uniqueN(m_a2015_fretwr$species)-uniqueN(m_a2015_fretwr_filtered$species),
    "species (",
    (1-(uniqueN(m_a2015_fretwr_filtered$species)/uniqueN(m_a2015_fretwr$species)))*100,
    "% ) through masking with EBBA 50km grid.", "\n")

cat("b2000 reduced by",
    (1-(nrow(m_b2000_fretwr_filtered)/nrow(m_b2000_fretwr)))*100,
    "% of observations through masking with EBBA 50km grid.", "\n")
cat("a2015 reduced by",
    (1-(nrow(m_a2015_fretwr_filtered)/nrow(m_a2015_fretwr)))*100,
    "% of observations through masking with EBBA 50km grid.", "\n")


# comparison with 500+ species GBIF data set
#cat("b2000 reduced by",
#    uniqueN(m_b2000_twrf$species)-uniqueN(m_b2000_fretwr_filtered$species),
#    "species (",
#    (1-(uniqueN(m_b2000_fretwr_filtered$species)/uniqueN(m_b2000_twrf$species)))*100,
#    "% ) compared with masked unreduced data set.", "\n")
#cat("a2015 reduced by",
#    uniqueN(m_a2015_twrf$species)-uniqueN(m_a2015_fretwr_filtered$species),
#    "species (",
#    (1-(uniqueN(m_a2015_fretwr_filtered$species)/uniqueN(m_a2015_twrf$species)))*100,
#    "% ) compared with masked unreduced data set.", "\n")

#cat("b2000 reduced by",
#    (1-(nrow(m_b2000_fretwr_filtered)/nrow(m_b2000_twrf)))*100,
#    "% of observations compared with masked unreduced data set.", "\n")
#cat("a2015 reduced by",
#    (1-(nrow(m_a2015_fretwr_filtered)/nrow(m_a2015_twrf)))*100,
#    "% of observations compared with masked unreduced data set.", "\n")

write.csv(m_b2000_fretwr_filtered, "own datasets/m_b2000_cleaned_shortened_masked.csv", row.names = FALSE)
write.csv(m_a2015_fretwr_filtered, "own datasets/m_a2015_cleaned_shortened_masked.csv", row.names = FALSE)


m_species_list_b2000_reduced <- species.list(m_b2000_fretwr_filtered)
m_species_list_a2015_reduced <- species.list(m_a2015_fretwr_filtered)


# Combine datasets
m_combined_species_list_reduced <- bind_rows(m_species_list_b2000_reduced, m_species_list_a2015_reduced) %>%
  group_by(class, order, family, species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list_reduced) <- tools::toTitleCase(colnames(m_combined_species_list_reduced))
head(m_combined_species_list_reduced)


write.csv(m_combined_species_list_reduced, "own datasets/Species_List_Mammals_Reduced.csv", row.names = FALSE)




#___________________________________________________________________________
## Visualization of Data  ####

world_data = world(path = ".")
ext_europe = c(-28, 64, 28, 81)


png("plots/occurence_mammals_b2000.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_b2000_fretwr_filtered$decimalLongitude, m_b2000_fretwr_filtered$decimalLatitude,
       col = "#2e6f4040", 
       cex = .2)
dev.off()


png("plots/occurence_mammals_a2015.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_a2015_fretwr_filtered$decimalLongitude, m_a2015_fretwr_filtered$decimalLatitude,
       col = "#e64f4040",
       cex = .2)
dev.off()




#___________________________________________________________________________
# EBBA BIRDS ####

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

uniqueN(ebba2$birdlife_scientific_name) #617 species


# add Class, Order and Family

taxa_info <- unique(ebba2$birdlife_scientific_name) %>%
  map_df(~ name_backbone(name = ., kingdom = "Animalia"))


taxa_info <- taxa_info %>%
  dplyr::select(canonicalName, order, family)

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
### correcting taxonomy to fit IUCN  ####


# Adjust column names to have uppercase first letters
colnames(ebba1) <- tools::toTitleCase(colnames(ebba1))

correct.taxonomy.b1 = function(df) {
  df1 = df %>% 
    mutate(Species = recode(Species, 
                            "Chionomys syriacus" = "Chionomys nivalis",
                            "Nothocricetulus migratorius" = "Cricetulus migratorius",
                            "Microtus liechtensteini" = "Microtus bavaricus",
                            "Microtus gerbii" = "Microtus gerbei",
                            "Stenocranius gregalis" = "Microtus gregalis",
                            "Microtus mystacinus" = "Microtus levis",
                            "Alexandromys oeconomus" = "Microtus oeconomus",
                            "Clethrionomys glareolus" = "Myodes glareolus",
                            "Craseomys rufocanus" = "Myodes rufocanus",
                            "Clethrionomys rutilus" = "Myodes rutilus",
                            "Eutamias sibiricus" = "Tamias sibiricus",
                            "Nannospalax leucodon" = "Spalax leucodon",
                            "Clanga clanga" = "Aquila clanga",
                            "Clanga pomarina" = "Aquila pomarina",
                           "Eudromias morinellus" = "Charadrius morinellus",
                            "Larus genei" = "Chroicocephalus genei",
                            "Larus ridibundus" = "Chroicocephalus ridibundus",
                            "Larus audouinii" = "Ichthyaetus audouinii",
                            "Larus melanocephalus" = "Ichthyaetus melanocephalus",
                            "Catharacta skua" = "Stercorarius skua",
                            "Tetrastes bonasia" = "Bonasa bonasia",
                            "Zapornia parva" = "Porzana parva",
                            "Zapornia pusilla" = "Porzana pusilla",
                            "Alaudala rufescens" = "Calandrella rufescens",
                            "Corvus monedula" = "Coloeus monedula",
                            "Acanthis flammea" = "Acanthis hornemanni",
                            "Luscinia svecica" = "Cyanecula svecica",
                            "Cercotrichas galactotes" = "Erythropygia galactotes",
                            "Poecile montanus" = "Parus montanus",
                            "Phylloscopus sibilatrix" = "Phylloscopus sibillatrix",
                            "Suthora alphonsiana" = "Sinosuthora alphonsiana",
                            "Suthora webbiana" = "Sinosuthora webbiana",
                            "Curruca cantillans" = "Sylvia cantillans",
                            "Curruca communis" = "Sylvia communis",
                            "Curruca conspicillata" = "Sylvia conspicillata",
                            "Curruca crassirostris" = "Sylvia crassirostris",
                            "Curruca curruca" = "Sylvia curruca",
                            "Curruca hortensis" = "Sylvia hortensis",
                            "Curruca melanocephala" = "Sylvia melanocephala",
                            "Curruca nisoria" = "Sylvia nisoria",
                            "Curruca ruppeli" = "Sylvia ruppeli",
                            "Curruca subalpina" = "Sylvia subalpina",
                            "Curruca undata" = "Sylvia undata",
                            "Leiopicus medius" = "Dendrocoptes medius",
                            "Hydrobates castro" = "Oceanodroma castro",
                            "Hydrobates leucorhous" = "Oceanodroma leucorhoa",
                            "Palaeornis eupatria" = "Psittacula eupatria",
                            "Alexandrinus krameri" = "Psittacula krameri",
                            "Microcarbo pygmaeus" = "Microcarbo pygmeus"
    ))
  return(df1)
}


ebba1.1 = correct.taxonomy.b1(ebba1)
ebba2.1 = correct.taxonomy.b1(ebba2)




#___________________________________________________________________________
## Extracting Species Lists  ####

# Extract unique species lists from both datasets

b1_species_list <- ebba1.1 %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")

b2_species_list <- ebba2.1 %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")


# View result
head(b1_species_list)
head(b2_species_list)

# combine files
b_species_list <- bind_rows(b1_species_list, b2_species_list) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Export file
write.csv(b_species_list, "own datasets/Species_List_Birds.csv", row.names = FALSE)




#___________________________________________________________________________
## Only species contained in 50km grid ####

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")


# Apply the function to both datasets
ebba1_filtered <- filter_occurrences_by_grid(ebba1, grid_50km)

cells = grid_50km$cell50x50
ebba2_filtered <- ebba2 %>% 
  filter(cell50x50 %in% cells)


(1-(uniqueN(ebba1_filtered$species)/uniqueN(ebba1$species)))*100
uniqueN(ebba1$Species)-uniqueN(ebba1_filtered$Species)

(1-(uniqueN(ebba2_filtered$Species)/uniqueN(ebba2$Species)))*100
uniqueN(ebba2$Species)-uniqueN(ebba2_filtered$Species)


b1_species_list_reduced <- ebba1_filtered %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")

b2_species_list_reduced <- ebba2_filtered %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")


# Adjust column names to have uppercase first letters
colnames(b1_species_list_reduced) <- tools::toTitleCase(colnames(b1_species_list_reduced))

# View result
head(b1_species_list_reduced)
head(b2_species_list_reduced)

# combine files
b_species_list_reduced <- bind_rows(b1_species_list_reduced, b2_species_list_reduced) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Export file
write.csv(b_species_list_reduced, "own datasets/Species_List_Birds_Reduced.csv", row.names = FALSE)



#___________________________________________________________________________
## Only species contained in change grid ####

change_grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")


# Apply the function to both datasets
ebba1_filtered_change <- filter_occurrences_by_grid(ebba1, change_grid)

cells_change = change_grid$cell50x50
ebba2_filtered_change <- ebba2 %>% 
  filter(cell50x50 %in% cells_change)


(1-(uniqueN(ebba1_filtered_change$Species)/uniqueN(ebba1$Species)))*100
uniqueN(ebba1$species)-uniqueN(ebba1_filtered_change$species)

(1-(uniqueN(ebba2_filtered$Species)/uniqueN(ebba2$Species)))*100
uniqueN(ebba2$Species)-uniqueN(ebba2_filtered_change$Species)


b1_species_list_reduced_change <- ebba1_filtered_change %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")

b2_species_list_reduced_change <- ebba2_filtered_change %>%
  filter(!is.na(Species) & Species != "") %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = n(), .groups = "drop")


# Adjust column names to have uppercase first letters
colnames(b1_species_list_reduced_change) <- tools::toTitleCase(colnames(b1_species_list_reduced_change))

# View result
head(b1_species_list_reduced_change)
head(b2_species_list_reduced_change)

# combine files
b_species_list_reduced_change <- bind_rows(b1_species_list_reduced_change, b2_species_list_reduced_change) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Export file
write.csv(b_species_list_reduced_change, "own datasets/Species_List_Birds_Reduced_Change.csv", row.names = FALSE)



#___________________________________________________________________________
## Exporting Data  ####

write.csv(ebba1, "own datasets/ebba1_cleaned.csv", row.names = FALSE)
write.csv(ebba2, "own datasets/ebba2_cleaned.csv", row.names = FALSE)



#___________________________________________________________________________
# Discrepancy of IUCN European Mammals ####

m_07 = read.csv("external datasets/IUCN Red List - European Mammals/European_Mammals_Red_List_09.11.07.csv")
m_25 = read.csv("external datasets/IUCN Red List - European Mammals/search_results_23.01.25.csv")

str(m_07)
m_07 = m_07[4:6]
str(m_07)

str(m_25)
m_25 = m_25[6:9]
str(m_25)


# Standardize species names in both data frames
m_07 <- m_07 %>%
  mutate(species = Scientific.name)

m_25 <- m_25 %>%
  mutate(species = paste(genusName, speciesName))

# Find species unique to each data frame
species_m07 <- setdiff(m_07$species, m_25$species)
species_m25 <- setdiff(m_25$species, m_07$species)

# Create a new data frame with differences
unique_species <- data.frame(
  species = c(species_m07, species_m25),
  source = c(rep("m_07", length(species_m07)), rep("m_25", length(species_m25)))
)

# View results
print(unique_species)


length(unique(m_07$Scientific.name)) # Unique species in m_07
length(unique(paste(m_25$genusName, m_25$speciesName)))

shared_species <- intersect(m_07$species, m_25$species)
length(shared_species)



taxa_info <- unique(unique_species$species) %>%
  map_df(~ name_backbone(name = ., kingdom = "Animalia"))


taxa_info <- taxa_info %>%
  dplyr::select(canonicalName, order, family)

taxa_info <- taxa_info %>%
  rename(species = canonicalName)

unique_species <- unique_species %>%
  left_join(taxa_info, by = "species") %>%
  mutate(Class = "Mammalia")

unique_species <- unique_species %>%
  rename(Order = order, Family = family)

unique_species

#c("now Vulpes lagopus",
#  "only European Russia",
#  "only Turkey",
#  "only Russia",
#  "only Russia",
#  
#  )
