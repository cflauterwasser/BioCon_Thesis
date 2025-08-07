# ___________________________________________________________________________
# Loading Packages ####

library(rgbif)
library(tidyverse)
library(geodata)
library(ggplot2)
library(sf)
library(data.table)
library(taxize)
library(rredlist)



# ___________________________________________________________________________
# GBIF MAMMALS ####

# ___________________________________________________________________________
## Preparation for Download Queries ####

### Import Species List ####

# european_species_habitats = read_csv("own datasets/Habitat_Information.csv")
# european_mammals = european_species_habitats |>
#  filter(Class == "Mammalia")
# species_list <- european_mammals$Binomial.Name
# taxon_keys <- sapply(species_list, function(x) name_backbone(name = x)$usageKey)



### Or simply all "Mammalia" ####

mammalia_key <- name_backbone(name = "Mammalia")$usageKey



# ___________________________________________________________________________
### Shapefile of Europe ####

# Read shapefile
shapefile_path <- "external datasets/Europe Shapefile/europe_simplified_coast.shp"

shape <- st_read(shapefile_path)

# Ensure the CRS is suitable for WKT
shape <- st_transform(shape, crs = 4326)

# Convert the geometry to WKT
wkt_region <- st_as_text(st_union(shape))



# ___________________________________________________________________________
## Downloading Datasets  ####

# ___________________________________________________________________________
### Initiating Download Process  ####

download_request_b2000 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_lte("year", 2000),
    pred("HAS_GEOSPATIAL_ISSUE", FALSE),
    pred("HAS_COORDINATE", TRUE),
    pred("OCCURRENCE_STATUS", "PRESENT"),
    pred_or(
      pred_not(pred_in("establishmentMeans", "MANAGED")),
      pred_isnull("establishmentMeans")
    )
  ),
  format = "SIMPLE_CSV"
)

download_request_a2015 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_gte("year", 2015),
    pred("HAS_GEOSPATIAL_ISSUE", FALSE),
    pred("HAS_COORDINATE", TRUE),
    pred("OCCURRENCE_STATUS", "PRESENT"),
    pred_or(
      pred_not(pred_in("establishmentMeans", "MANAGED")),
      pred_isnull("establishmentMeans")
    )
  ),
  format = "SIMPLE_CSV"
)


# ___________________________________________________________________________
### Status and Data Download  ####

# cancel request(s)
# downloads <- occ_download_list(limit = 100)
# downloads
# occ_download_cancel(downloads$results$key[1])
# occ_download_cancel(downloads$results$key[2])

# download_request_a2015 = downloads$results$key[1]
# download_request_b2000 = downloads$results$key[2]


# for checking status of download
occ_download_meta(download_request_b2000)
occ_download_meta(download_request_a2015)


# Download and save data
download_path <- "external datasets/GBIF/"
before_2000 <- occ_download_get(download_request_b2000, path = download_path, overwrite = TRUE)
after_2015 <- occ_download_get(download_request_a2015, path = download_path, overwrite = TRUE)
# options(timeout = 300)



## Importing Datasets ####

csv_before_2000 <- fread("external datasets/GBIF/0002113-250123221155621.csv")
csv_after_2015 <- fread("external datasets/GBIF/0002114-250123221155621.csv")



# ___________________________________________________________________________
## Investigating Datasets ####

# structure
str(csv_before_2000)
str(csv_after_2015)


# temporal distribution
min(csv_before_2000$year)
boxplot(csv_before_2000$year)

boxplot(csv_after_2015$year)



# checking levels of different variables
check.levels <- function(x) {
  x <- as.factor(x)
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

a <- csv_before_2000$decimalLongitude
head(a)

# ___________________________________________________________________________
## Cleaning Datasets  ####

# ___________________________________________________________________________
### Removing Useless Columns  ####

# str(csv_before_2000)
# summary(csv_before_2000)
sum(is.na(csv_before_2000$institutionCode))
check.levels(csv_before_2000$institutionCode)

reduce.columns <- function(df) {
  output <- df[, c(
    "class",
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
    "issue"
  )]
  return(output)
}

csv_before_2000_r <- reduce.columns(csv_before_2000)
head(csv_before_2000_r)

csv_after_2015_r <- reduce.columns(csv_after_2015)
head(csv_after_2015_r)


# clearing up memory
rm(list = c("csv_before_2000", "csv_after_2015"))
gc()



# ___________________________________________________________________________
### correcting taxonomy to fit IUCN  ####

correct.taxonomy.m <- function(df) {
  df1 <- df %>%
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
    )) %>%
    filter(!is.na(species) & species != "")

  cat(sprintf("%.2f%% of occurrences were merged/reduced.\n", round((1 - nrow(df1) / nrow(df)) * 100, 2)))
  return(df1)
}

csv_before_2000_r1 <- correct.taxonomy.m(csv_before_2000_r)
csv_after_2015_r1 <- correct.taxonomy.m(csv_after_2015_r)

uniqueN(csv_before_2000_r1$species) / uniqueN(csv_before_2000_r$species)
uniqueN(csv_after_2015_r1$species) / uniqueN(csv_after_2015_r$species)



# ___________________________________________________________________________
### Excluding Fossil and Extinct Species  ####

exclude.extinct <- function(df) {
  filtered <- df |>
    filter(basisOfRecord != "FOSSIL_SPECIMEN") |>
    filter(!species %in% c(
      "Mammuthus primigenius",
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
      "Ceratotherium neumayri"
    ))
  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000 <- exclude.extinct(csv_before_2000_r1)

m_a2015 <- exclude.extinct(csv_after_2015_r1)



# ___________________________________________________________________________
### Excluding Marine Families  ####

exclude.marine <- function(df) {
  filtered <- df |>
    filter(!family %in% c(
      "Balaenidae", # whale families
      "Balaenopteridae",
      "Delphinidae",
      "Monodontidae",
      "Phocoenidae",
      "Kogiidae",
      "Physeteridae",
      "Ziphiidae",
      "Phocidae", # seals
      "Odobenidae"
    )) |> # walruses
    filter(order != "Cetacea") # exclude outdated order "Cetacea"

  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_t <- exclude.marine(m_b2000)

m_a2015_t <- exclude.marine(m_a2015)



# ___________________________________________________________________________
### Excluding Domesticated Species  ####

exclude.domesticated <- function(df) {
  filtered <- df |>
    filter(!species %in% c(
      "Felis catus",
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
      "Bos grunniens"
    )) |>
    filter(infraspecificEpithet != "domesticus") |>
    filter(!(species == "Canis lupus" & infraspecificEpithet == "familiaris"))

  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_tw <- exclude.domesticated(m_b2000_t)

m_a2015_tw <- exclude.domesticated(m_a2015_t)



# ___________________________________________________________________________
### Excluding Captivity Anomalies ####

exclude.captive <- function(df) {
  filtered <- df |>
    filter(!species %in% c(
      "Ailurus fulgens",
      "Panthera tigris",
      "Panthera leo",
      "Phacochoerus africanus",
      "Giraffa camelopardalis",
      "Giraffa giraffa",
      "Setifer setosus",
      "Ailuropoda melanoleuca",
      "Puma concolor",
      "Brachyteles arachnoides",
      "Vicugna vicugna"
    )) |>
    filter(order != "Primates") |>
    filter(order != "Proboscidea") |>
    filter(family != "Rhinocerotidae") |>
    filter(family != "Rhinocerotidae")
  # TO BE CONTINUED!!!

  cat(sprintf("%.2f%% of occurrences were excluded.\n", round((1 - nrow(filtered) / nrow(df)) * 100, 2)))
  return(filtered)
}


m_b2000_twr <- exclude.captive(m_b2000_tw)

m_a2015_twr <- exclude.captive(m_a2015_tw)



# ___________________________________________________________________________
### Rasterizing Species Occurrences ####


grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp") # without Russia, Ukraine, Belarus, Moldavia, Turkey, Azerbaijan, Armenia, Georgia, west-Kazacstan

# EBBA1

rast_occ <- function(df, grid = grid_50km, chunk_size = 100000) {
  total_rows <- nrow(df)
  num_chunks <- ceiling(total_rows / chunk_size)
  pb <- txtProgressBar(min = 0, max = num_chunks, style = 3)

  results_list <- vector("list", num_chunks)

  for (i in seq_len(num_chunks)) {
    start_row <- ((i - 1) * chunk_size) + 1
    end_row <- min(i * chunk_size, total_rows)

    df_chunk <- df[start_row:end_row, ]

    df_sf <- st_as_sf(df_chunk, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
    df_sf <- st_transform(df_sf, st_crs(grid))

    joined <- st_join(df_sf, grid, left = FALSE)

    results_list[[i]] <- as.data.table(sf::st_drop_geometry(joined))

    setTxtProgressBar(pb, i)
  }

  close(pb)

  df_dt <- data.table::rbindlist(results_list)
  return(df_dt)
}


m_b2000_rast <- rast_occ(m_b2000_twr, grid_50km)
m_a2015_rast <- rast_occ(m_a2015_twr, grid_50km)

m_b2000_rast <- m_b2000_rast %>%
  rename(Species = species)

m_a2015_rast <- m_a2015_rast %>%
  rename(Species = species)



# cell = "26SLH4"

# test1.1 = m_b2000_dt |>
#  filter(cell50x50 == cell)
# check.levels(test1.1$species)
# length(unique(test1.1$species))

# test2.1 = delta_SR_m1 |>
#  filter(cell50x50 == cell)
# test2$SR_m_b2000

# test3.1 = data |>
#  filter(cell50x50 == cell)
# test3.1$SR_m_b2000


# test1.2 = m_a2015_dt |>
#  filter(cell50x50 == cell)
# check.levels(test1.2$species)
# length(unique(test1.2$species))

# test2.2 = delta_SR_m1 |>
#  filter(cell50x50 == cell)
# test2.2$SR_m_a2015

# test3.2 = data |>
#  filter(cell50x50 == cell)
# test3.2$SR_m_a2015



# ___________________________________________________________________________
### Exporting Data  ####


write_csv(m_b2000_rast, "own datasets/m_b2000_cleaned.csv")
write_csv(m_a2015_rast, "own datasets/m_a2015_cleaned.csv")


# clearing up memory
rm(list = c(
  "csv_before_2000_r", "csv_after_2015_r",
  "csv_before_2000_r1", "csv_after_2015_r1",
  "m_b2000", "m_a2015",
  "m_b2000_t", "m_a2015_t",
  "m_b2000_tw", "m_a2015_tw",
  "m_b2000_twr", "m_a2015_twr"
))
gc()



# ___________________________________________________________________________
## Reducing to Conservative IUCN Species List  ####

# m_b2000_twr_red = read_csv("own datasets/m_b2000_cleaned.csv")
# m_a2015_twr_red = read_csv("own datasets/m_a2015_cleaned.csv")

csv_before_2000 <- fread("external datasets/GBIF/0002113-250123221155621.csv")
csv_after_2015 <- fread("external datasets/GBIF/0002114-250123221155621.csv")

iucn_eur_mammals <- read_csv("external datasets/IUCN Red List - European Mammals/European_Mammals_Red_List_09.11.07.csv")
iucn_eur_mammals <- iucn_eur_mammals[3:6]
summary(iucn_eur_mammals)


iucn_eur_mammals <- iucn_eur_mammals %>%
  mutate(Scientific.name = recode(Scientific.name,
    "Alopex lagopus" = "Vulpes lagopus",
    # "Mustela sibirica" = "new_name",       # Russia, barely Europe
    # "Bos primigenius" = "new_name"),       # Aurochs, extinct
    # "Sousa chinensis" = "new_name",        # Indo-Pacific dolphin
    # "Eptesicus bottae" = "new_name",       # West Asia
    "Myotis aurascens" = "Myotis davidii",
    "Pipistrellus savii" = "Hypsugo savii",
    # "Plecotus sardus" = "new_name",        # only Sardinia 2001-2009
    # "Crocidura canariensis" = "new_name",  # Canarian shrew, only 2007
    # "Crocidura zimmermanni" = "new_name",  # Cretan shrew
    # "Sorex arunchi" = "new_name",          # GBIF no occ with coord
    # "Prolagus sardus" = "new_name",        # Sardinian pika, extinct
    # "Microtus felteni" = "new_name",       # no records in time window
    "Microtus middendorffii" = "Microtus middendorffi",
    # "Mus cypriacus" = "new_name",          # GBIF no occ with coord
    # "Spalax nehringi" = "new_name"         # West Asia, GBIF 2013
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


# clearing up memory
rm(list = c("csv_before_2000", "csv_after_2015"))
gc()


m_b2000_f <- m_b2000_f$filtered_df
m_a2015_f <- m_a2015_f$filtered_df

# comparison with unreduced data
# cat("b2000 reduced by", uniqueN(csv_before_2000$species)-uniqueN(m_b2000_f$species), "species (",(1-(uniqueN(m_b2000_f$species)/uniqueN(csv_before_2000$species)))*100, "% ) through application of IUCN species list.", "\n")
# cat("a2015 reduced by", uniqueN(csv_after_2015$species)-uniqueN(m_a2015_f$species), "species (",(1-(uniqueN(m_a2015_f$species)/uniqueN(csv_after_2015$species)))*100, "% ) through application of IUCN species list.", "\n")

# cat("b2000 reduced by", (1-(nrow(m_b2000_f)/nrow(csv_before_2000)))*100, "% of observations through application of IUCN species list.", "\n")
# cat("a2015 reduced by", (1-(nrow(m_a2015_f)/nrow(csv_after_2015)))*100, "% of observations through application of IUCN species list.", "\n")


m_b2000_fr <- reduce.columns(m_b2000_f)
m_a2015_fr <- reduce.columns(m_a2015_f)

m_b2000_fr <- correct.taxonomy.m(m_b2000_fr)
m_a2015_fr <- correct.taxonomy.m(m_a2015_fr)

m_b2000_fre <- exclude.extinct(m_b2000_fr)
m_a2015_fre <- exclude.extinct(m_a2015_fr)

m_b2000_fret <- exclude.marine(m_b2000_fre)
m_a2015_fret <- exclude.marine(m_a2015_fre)

m_b2000_fretw <- exclude.domesticated(m_b2000_fret)
m_a2015_fretw <- exclude.domesticated(m_a2015_fret)

m_b2000_fretwr <- exclude.captive(m_b2000_fretw)
m_a2015_fretwr <- exclude.captive(m_a2015_fretw)

# how many species remain after cleaning
cat("In b2000", uniqueN(m_b2000_f$species) - uniqueN(m_b2000_fretwr$species), "species (", (1 - (uniqueN(m_b2000_fretwr$species) / uniqueN(m_b2000_f$species))) * 100, "% )", "were removed through cleaning.", "\n")
cat("In b2000", uniqueN(m_a2015_f$species) - uniqueN(m_a2015_fretwr$species), "species (", (1 - (uniqueN(m_a2015_fretwr$species) / uniqueN(m_a2015_f$species))) * 100, "% )", "were removed through cleaning.", "\n")


# comparison with unreduced data
# cat("b2000 reduced by",
#    uniqueN(m_b2000_twr$species)-uniqueN(m_b2000_fretwr$species),
#    "species (",
#    (1-(uniqueN(m_b2000_fretwr$species)/uniqueN(m_b2000_twr$species)))*100,
#    "% ) through application of IUCN species list.", "\n")
# cat("a2015 reduced by",
#    uniqueN(m_a2015_twr$species)-uniqueN(m_a2015_fretwr$species),
#    "species (",
#    (1-(uniqueN(m_a2015_fretwr$species)/uniqueN(m_a2015_twr$species)))*100,
#    "% ) through application of IUCN species list.", "\n")

# cat("b2000 reduced by",
#    (1-(nrow(m_b2000_fretwr)/nrow(m_b2000_twr)))*100,
#    "% of observations through application of IUCN species list.", "\n")
# cat("a2015 reduced by",
#    (1-(nrow(m_a2015_fretwr)/nrow(m_a2015_twr)))*100,
#    "% of observations through application of IUCN species list.", "\n")

m_b2000_fretwr <- rast_occ(m_b2000_fretwr)
m_a2015_fretwr <- rast_occ(m_a2015_fretwr)


write_csv(m_b2000_fretwr, "own datasets/m_b2000_cleaned_shortened.csv")
write_csv(m_a2015_fretwr, "own datasets/m_a2015_cleaned_shortened.csv")

# clearing up memory
rm(list = c(
  "m_b2000_f", "m_a2015_f",
  "m_b2000_fr", "m_a2015_fr",
  "m_b2000_fre", "m_a2015_fre",
  "m_b2000_fret", "m_a2015_fret",
  "m_b2000_fretw", "m_a2015_fretw",
  "m_b2000_fretwr", "m_a2015_fretwr"
))
gc()



# ___________________________________________________________________________
## Extracting Species Lists  ####

# ___________________________________________________________________________
### 'Original' 500+ GBIF species list  ####

# Extract unique species lists from both datasets

m_b2000_twr <- fread("own datasets/m_b2000_cleaned.csv")
m_a2015_twr <- fread("own datasets/m_a2015_cleaned.csv")


grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")


# masking for ebba 50km grid
# Define the function
filter_occurrences_by_grid <- function(occ_data, grid_shapefile) {
  # Convert occurrences to an sf object (assuming WGS84 CRS)
  occ_sf <- st_as_sf(occ_data,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326
  ) # EPSG:4326 (WGS84)

  # Preserve original lat/lon before transformation
  occ_sf$decimalLongitude <- occ_data$decimalLongitude
  occ_sf$decimalLatitude <- occ_data$decimalLatitude

  # Transform occurrences to match the grid CRS
  occ_sf <- st_transform(occ_sf, crs = st_crs(grid_shapefile))

  # Perform spatial join to filter occurrences within grid
  filtered_sf <- st_join(occ_sf, grid_shapefile, left = FALSE)

  # Convert back to dataframe (removing geometry column but keeping lat/lon)
  filtered_df <- as.data.frame(filtered_sf) %>%
    dplyr::select(-geometry) # Remove geometry column if not needed

  return(filtered_df)
}


m_b2000_twrf <- filter_occurrences_by_grid(m_b2000_twr, grid_50km)
m_a2015_twrf <- filter_occurrences_by_grid(m_a2015_twr, grid_50km)


cat(
  "b2000 reduced by",
  uniqueN(m_b2000_twr$species) - uniqueN(m_b2000_twrf$species),
  "species (",
  (1 - (uniqueN(m_b2000_twrf$species) / uniqueN(m_b2000_twr$species))) * 100,
  "% ) through masking with EBBA 50km grid.", "\n"
)
cat(
  "a2015 reduced by",
  uniqueN(m_a2015_twr$species) - uniqueN(m_a2015_twrf$species),
  "species (",
  (1 - (uniqueN(m_a2015_twrf$species) / uniqueN(m_a2015_twr$species))) * 100,
  "% ) through masking with EBBA 50km grid.", "\n"
)

cat(
  "b2000 reduced by",
  (1 - (nrow(m_b2000_twrf) / nrow(m_b2000_twr))) * 100,
  "% of observations through masking with EBBA 50km grid.", "\n"
)
cat(
  "a2015 reduced by",
  (1 - (nrow(m_a2015_twrf) / nrow(m_a2015_twr))) * 100,
  "% of observations through masking with EBBA 50km grid.", "\n"
)


species.list.m <- function(df) {
  species_list <- df %>%
    filter(!is.na(Species) & Species != "") %>% # Remove rows with missing or empty species
    group_by(class, order, family, Species) %>%
    summarise(Count = n(), .groups = "drop")
  return(species_list)
}


m_species_list_b2000 <- species.list.m(m_b2000_twrf)
m_species_list_a2015 <- species.list.m(m_a2015_twrf)



# Combine datasets
m_combined_species_list <- bind_rows(m_species_list_b2000, m_species_list_a2015) %>%
  group_by(class, order, family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list) <- tools::toTitleCase(colnames(m_combined_species_list))

# View result
head(m_combined_species_list)

# Export file
write_csv(m_combined_species_list, "own datasets/Species_List_Mammals.csv")


# ___________________________________________________________________________,,,,2
### Conservative IUCN Species List  ####

m_b2000_fretwr <- fread("own datasets/m_b2000_cleaned_shortened.csv")
m_a2015_fretwr <- fread("own datasets/m_a2015_cleaned_shortened.csv")

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp")

# masking with EBBA 50km grid
m_b2000_fretwr_filtered <- filter_occurrences_by_grid(m_b2000_fretwr, grid_50km)
m_a2015_fretwr_filtered <- filter_occurrences_by_grid(m_a2015_fretwr, grid_50km)


# species reduction through masking
cat(
  "b2000 reduced by",
  uniqueN(m_b2000_fretwr$Species) - uniqueN(m_b2000_fretwr_filtered$Species),
  "Species (",
  (1 - (uniqueN(m_b2000_fretwr_filtered$Species) / uniqueN(m_b2000_fretwr$Species))) * 100,
  "% ) through masking with EBBA 50km grid.", "\n"
)
cat(
  "a2015 reduced by",
  uniqueN(m_a2015_fretwr$Species) - uniqueN(m_a2015_fretwr_filtered$Species),
  "Species (",
  (1 - (uniqueN(m_a2015_fretwr_filtered$Species) / uniqueN(m_a2015_fretwr$Species))) * 100,
  "% ) through masking with EBBA 50km grid.", "\n"
)

cat(
  "b2000 reduced by",
  (1 - (nrow(m_b2000_fretwr_filtered) / nrow(m_b2000_fretwr))) * 100,
  "% of observations through masking with EBBA 50km grid.", "\n"
)
cat(
  "a2015 reduced by",
  (1 - (nrow(m_a2015_fretwr_filtered) / nrow(m_a2015_fretwr))) * 100,
  "% of observations through masking with EBBA 50km grid.", "\n"
)


# comparison with 500+ Species GBIF data set
# cat("b2000 reduced by",
#    uniqueN(m_b2000_twrf$Species)-uniqueN(m_b2000_fretwr_filtered$Species),
#    "Species (",
#    (1-(uniqueN(m_b2000_fretwr_filtered$Species)/uniqueN(m_b2000_twrf$Species)))*100,
#    "% ) compared with masked unreduced data set.", "\n")
# cat("a2015 reduced by",
#    uniqueN(m_a2015_twrf$Species)-uniqueN(m_a2015_fretwr_filtered$Species),
#    "Species (",
#    (1-(uniqueN(m_a2015_fretwr_filtered$Species)/uniqueN(m_a2015_twrf$Species)))*100,
#    "% ) compared with masked unreduced data set.", "\n")

# cat("b2000 reduced by",
#    (1-(nrow(m_b2000_fretwr_filtered)/nrow(m_b2000_twrf)))*100,
#    "% of observations compared with masked unreduced data set.", "\n")
# cat("a2015 reduced by",
#    (1-(nrow(m_a2015_fretwr_filtered)/nrow(m_a2015_twrf)))*100,
#    "% of observations compared with masked unreduced data set.", "\n")

write_csv(m_b2000_fretwr_filtered, "own datasets/m_b2000_cleaned_shortened_masked.csv")
write_csv(m_a2015_fretwr_filtered, "own datasets/m_a2015_cleaned_shortened_masked.csv")

m_b2000_fretwr_filtered <- m_b2000_fretwr_filtered %>%
  rename(Species = species)
m_a2015_fretwr_filtered <- m_a2015_fretwr_filtered %>%
  rename(Species = species)

m_species_list_b2000_reduced <- species.list.m(m_b2000_fretwr_filtered)
m_species_list_a2015_reduced <- species.list.m(m_a2015_fretwr_filtered)


# Combine datasets
m_combined_species_list_reduced <- bind_rows(m_species_list_b2000_reduced, m_species_list_a2015_reduced) %>%
  group_by(class, order, family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list_reduced) <- tools::toTitleCase(colnames(m_combined_species_list_reduced))
head(m_combined_species_list_reduced)


write_csv(m_combined_species_list_reduced, "own datasets/Species_List_Mammals_Reduced.csv")




# ___________________________________________________________________________
## Visualization of Data  ####

world_data <- world(path = ".")
ext_europe <- c(-28, 64, 28, 81)


png("plots/occurrence frequency/occurence_mammals_b2000.png",
  width = 800,
  height = 600,
  res = 100
)
plot(world_data, ext = ext_europe)
points(m_b2000_fretwr_filtered$decimalLongitude, m_b2000_fretwr_filtered$decimalLatitude,
  col = "#2e6f4040",
  cex = .2
)
dev.off()


png("plots/occurrence frequency/occurence_mammals_a2015.png",
  width = 800,
  height = 600,
  res = 100
)
plot(world_data, ext = ext_europe)
points(m_a2015_fretwr_filtered$decimalLongitude, m_a2015_fretwr_filtered$decimalLatitude,
  col = "#e64f4040",
  cex = .2
)
dev.off()




# ___________________________________________________________________________
# EBBA BIRDS ####

ebba1 <- fread("external datasets/EBBA/ebba1_data_gbif_50km.csv")
ebba2 <- fread("external datasets/EBBA/ebba2_data_occurrence_50km.csv")
str(ebba1)
str(ebba2)



# ___________________________________________________________________________
## Cleaning Datasets ####

uniqueN(ebba1$species)
uniqueN(ebba2$birdlife_scientific_name) # 495 species

ebba1.1 <- reduce.columns(ebba1)

# Adjust column names to have uppercase first letters
colnames(ebba1.1) <- tools::toTitleCase(colnames(ebba1.1))

ebba2.1 <- ebba2 %>%
  rename(Species = birdlife_scientific_name)


# add Class, Order and Family

taxa_info <- unique(ebba2.1$Species) %>%
  map_df(~ name_backbone(name = ., kingdom = "Animalia"))


taxa_info <- taxa_info %>%
  dplyr::select(canonicalName, order, family)

taxa_info <- taxa_info %>%
  rename(Species = canonicalName)

ebba2.2 <- ebba2.1 %>%
  left_join(taxa_info, by = "Species") %>%
  mutate(Class = "Aves")

ebba2.2 <- ebba2.2 %>%
  rename(Order = order, Family = family)



# ___________________________________________________________________________
### Correcting taxonomy to IUCN terminology (for later habitat pull)  ####

correct_taxonomy_b <- function(df) {
  df_corrected <- df %>%
    mutate(Species = recode(Species,
      "Parus montanus" = "Poecile montanus",
      "Acanthis hornemanni" = "Acanthis flammea",
      "Cyanecula svecica" = "Luscinia svecica",
      "Microcarbo pygmeus" = "Microcarbo pygmaeus",
      "Clanga pomarina" = "Aquila pomarina",
      "Aquila clanga" = "Clanga clanga",
      "Dendrocoptes medius" = "Leiopicus medius",
      "Calandrella rufescens" = "Alaudala rufescens",
      "Luscinia calliope" = "Calliope calliope",
      "Turdus ruficollis" = "Turdus atrogularis",
      "Oceanodroma leucorhoa" = "Hydrobates leucorhous",
      "Oceanodroma castro" = "Hydrobates castro",
      "Melanocorypha leucoptera" = "Alauda leucoptera",
      "Aquila pomarina" = "Clanga pomarina",
      "Charadrius morinellus" = "Eudromias morinellus",
      "Chroicocephalus genei" = "Larus genei",
      "Chroicocephalus ridibundus" = "Larus ridibundus",
      "Ichthyaetus audouinii" = "Larus audouinii",
      "Ichthyaetus melanocephalus" = "Larus melanocephalus",
      "Stercorarius skua" = "Catharacta skua",
      "Coloeus monedula" = "Corvus monedula",
      "Erythropygia galactotes" = "Cercotrichas galactotes",
      "Phylloscopus sibillatrix" = "Phylloscopus sibilatrix",
      "Sylvia cantillans" = "Curruca cantillans",
      "Sylvia communis" = "Curruca communis",
      "Sylvia conspicillata" = "Curruca conspicillata",
      "Sylvia curruca" = "Curruca curruca",
      "Sylvia hortensis" = "Curruca hortensis",
      "Sylvia melanocephala" = "Curruca melanocephala",
      "Sylvia nisoria" = "Curruca nisoria",
      "Sylvia ruppeli" = "Curruca ruppeli",
      "Sylvia undata" = "Curruca undata",
      "Psittacula krameri" = "Alexandrinus krameri",
      "Ardea ibis" = "Bubulcus ibis",
      "Botaurus minutus" = "Ixobrychus minutus"
    ))


  return(df_corrected)
}

ebba1.2 <- correct_taxonomy_b(ebba1.1)
ebba2.3 <- correct_taxonomy_b(ebba2.2)



# ___________________________________________________________________________
### Only present species ####

check.levels(ebba1.2$occurrenceStatus)
check.levels(ebba2.3$occurrence)

ebba1_present <- ebba1.2 %>%
  filter(occurrenceStatus == "PRESENT") %>%
  dplyr::distinct(Species)
head(ebba1_present)

ebba2_present <- ebba2.3 %>%
  filter(occurrence == 1) %>%
  dplyr::distinct(Species)
head(ebba2_present)

ebba1_absent <- ebba1.2 %>%
  filter(occurrenceStatus == "ABSENT") %>%
  dplyr::distinct(Species)
head(ebba1_absent)

ebba2_absent <- ebba2.3 %>%
  filter(occurrence == 0) %>%
  dplyr::distinct(Species)
head(ebba2_absent)


# Species only ABSENT in EBBA1
ebba1_only_absent <- dplyr::setdiff(ebba1_absent, ebba1_present)

# Now intersect that with species PRESENT in EBBA2
t0_absent_t1_present <- dplyr::intersect(ebba1_only_absent, ebba2_present)

t0_absent_t1_present



ebba1.3 <- ebba1.2 |>
  filter(occurrenceStatus == "PRESENT")
check.levels(ebba1.3$occurrenceStatus)

uniqueN(ebba1.3$species) # 491 species


uniqueN(ebba2.3$Species) # 617 species

check.levels(ebba2.3$occurrence)
ebba2.4 <- ebba2.3 |>
  filter(occurrence == 1)
check.levels(ebba2.4$occurrence)

uniqueN(ebba2.4$Species) # 617 species


str(ebba2.4)
head(ebba2.4)



# ___________________________________________________________________________
### Reduce EBBA2 to EBBA1 species ####

uniqueN(ebba1.3$Species)
uniqueN(ebba2.4$Species)

species_only_in_ebba1 <- setdiff(ebba1.3$Species, ebba2.4$Species)
species_only_in_ebba1

ebba2_reduced <- ebba2.4 |> # not very elegant since created again below. Change if time
  filter(Species %in% ebba1.3$Species)

ebba1_reduced <- ebba1.3 |>
  filter(Species %in% ebba2_reduced$Species)

# Identify species lost during filtering (those that are only absent in EBBA1 but present in EBBA2)
lost_absent_species <- setdiff(t0_absent_t1_present$Species, ebba1_reduced$Species)

# Retrieve their full records from the original EBBA1 dataset
absent_species_retained <- ebba1.1 |>
  filter(Species %in% lost_absent_species)

# Add them back in
ebba1_reduced1 <- bind_rows(ebba1_reduced, absent_species_retained) |>
  dplyr::distinct()

ebba2_reduced1 <- ebba2.4 |>
  filter(Species %in% ebba1_reduced1$Species)

# Final test: should return character(0)
setdiff(t0_absent_t1_present$Species, ebba1_reduced1$Species)
t0_absent_t1_present$Species %in% ebba1_reduced1$Species
setdiff(t0_absent_t1_present$Species, ebba2_reduced1$Species)



# ___________________________________________________________________________
## Rasterizing Data  ####

grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba2_grid50x50_v1_EditNoIslandsNoEast.shp") # without Russia, Ukraine, Belarus, Moldavia, Turkey, Azerbaijan, Armenia, Georgia, west-Kazacstan


ebba1_rast <- ebba1_reduced1 %>%
  rast_occ()

uniqueN(ebba1_rast$cell50x50)



# ___________________________________________________________________________
## Standardizing Data Structure ####

head(ebba1_rast)
head(ebba2_reduced1)

ebba1_std <- ebba1_rast %>%
  dplyr::select(cell50x50, Class, Order, Family, Species)

ebba2_std <- ebba2_reduced1 %>%
  dplyr::select(cell50x50, Class, Order, Family, Species)

glimpse(ebba1_std)
glimpse(ebba2_std)



# ___________________________________________________________________________
## Reducing Data to EBBA Change Grid  ####

# Deriving EBBA Change Cells
change_grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")

uniqueN(change_grid_50km$cell50x50)

ebba_change_cells <- change_grid_50km$cell50x50
head(ebba_change_cells)
length(ebba_change_cells)

ebba_change <- function(df) {
  df_change <- df %>%
    filter(cell50x50 %in% ebba_change_cells)
  return(df_change)
}


ebba1_change_data <- ebba_change(ebba1_std)
ebba2_change_data <- ebba_change(ebba2_std)

uniqueN(ebba1_change_data$cell50x50)
uniqueN(ebba2_change_data$cell50x50)



# ___________________________________________________________________________
## Exporting Data  ####

head(ebba1_change_data)
head(ebba2_change_data)

write_csv(ebba1_change_data, "own datasets/ebba1_cleaned.csv")
write_csv(ebba2_change_data, "own datasets/ebba2_cleaned.csv")



# ___________________________________________________________________________
## Extracting Species Lists  ####

species.list.b <- function(df) {
  species_list <- df %>%
    filter(!is.na(Species) & Species != "") %>% # Remove rows with missing or empty species
    group_by(Class, Order, Family, Species) %>%
    summarise(Count = n(), .groups = "drop")
  return(species_list)
}

# Extract unique species lists from both datasets

b1_species_list <- species.list.b(ebba1_reduced1)
b2_species_list <- species.list.b(ebba2_reduced1)


# View result
head(b1_species_list)
head(b2_species_list)

# combine files
b_species_list <- bind_rows(b1_species_list, b2_species_list) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Export file
write_csv(b_species_list, "own datasets/Species_List_Birds.csv")




# ___________________________________________________________________________
### Only species contained in change grid ####

change_grid <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")


# Apply the function to both datasets
ebba1_filtered_change <- filter_occurrences_by_grid(ebba1_reduced1, change_grid)

cells_change <- change_grid$cell50x50
ebba2_filtered_change <- ebba2_reduced1 %>%
  filter(cell50x50 %in% cells_change)


b1_species_list_reduced_change <- species.list.b(ebba1_filtered_change)

b2_species_list_reduced_change <- species.list.b(ebba2_filtered_change)


# Adjust column names to have uppercase first letters
colnames(b1_species_list_reduced_change) <- tools::toTitleCase(colnames(b1_species_list_reduced_change))

# View result
head(b1_species_list_reduced_change)
head(b2_species_list_reduced_change)

uniqueN(b1_species_list_reduced_change$Species)
uniqueN(b2_species_list_reduced_change$Species)


# combine files
b_species_list_reduced_change <- bind_rows(b1_species_list_reduced_change, b2_species_list_reduced_change) %>%
  group_by(Class, Order, Family, Species) %>%
  summarise(Count = sum(Count), .groups = "drop")

uniqueN(b_species_list_reduced_change$Species)

# Export file
write_csv(b_species_list_reduced_change, "own datasets/Species_List_Birds_Reduced_Change.csv")



uniqueN(ebba1_change_data$Species)
uniqueN(ebba2_change_data$Species)

sp_list <- b_species_list_reduced_change$Species
sp_data <- bind_rows(
  dplyr::select(ebba1_change_data, Species),
  dplyr::select(ebba2_change_data, Species)
) %>%
  distinct(Species) %>%
  pull(Species)

uniqueN(sp_list)
uniqueN(sp_data)

setdiff(sp_list, sp_data) # in sp_list but not in sp_data
setdiff(sp_data, sp_list) # in sp_data but not in sp_list



# ___________________________________________________________________________
# Discrepancy of IUCN European Mammals ####

m_07 <- read_csv("external datasets/IUCN Red List - European Mammals/European_Mammals_Red_List_09.11.07.csv")
m_25 <- read_csv("external datasets/IUCN Red List - European Mammals/search_results_23.01.25.csv")

str(m_07)
m_07 <- m_07[4:6]
str(m_07)

str(m_25)
m_25 <- m_25[6:9]
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

# c("now Vulpes lagopus",
#  "only European Russia",
#  "only Turkey",
#  "only Russia",
#  "only Russia",
#
#  )
