
#___________________________________________________________________________
# Loading Packages ####

library(rgbif)
library(tidyverse)
library(sf)
library(ggspatial)
library(geodata)
library(ggplot2)



#___________________________________________________________________________
# GBIF (Mammals) ####

#___________________________________________________________________________
## Preparation for Download Queries ####

### Import Species List ####

european_species_habitats = read.csv("own datasets/Habitat_Information.csv")
european_mammals = european_species_habitats |>
  filter(Class == "Mammalia")
species_list <- european_mammals$Binomial.Name

taxon_keys <- sapply(species_list, function(x) name_backbone(name = x)$usageKey)



### Or simply all "Mammalia" ####

mammalia_key <- name_backbone(name = "Mammalia")$usageKey



#___________________________________________________________________________
## Shapefile of Europe ####

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
## Testing Queries ####

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
## Initiating Download Process  ####

download_request1 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_lte("year", 2000)
  )
)

download_request2 <- occ_download(
  pred_and(
    pred_in("taxonKey", mammalia_key),
    pred_within(wkt_region),
    pred_gte("year", 2015)
  )
)



#___________________________________________________________________________
## Status and Data Download  ####

#cancel request(s)
#downloads <- occ_download_list(limit = 100)
#downloads
#occ_download_cancel(downloads$results$key[1])
#occ_download_cancel(downloads$results$key[2])


# for checking status of download
occ_download_meta(download_request1)
occ_download_meta(download_request2)


# Download and save data
download_path <- "external datasets/GBIF/"
before_2000 = occ_download_get(download_request1, path = download_path, overwrite = TRUE)
after_2015 = occ_download_get(download_request2, path = download_path, overwrite = TRUE)
options(timeout = 600)


# import downloaded data into R
dwca_file_before_2000 <- occ_download_import(as.download("external datasets/GBIF/0003238-250121130708018.zip"))
dwca_file_after_2015 <- occ_download_import(as.download("external datasets/GBIF/0003239-250121130708018.zip"))


# Check the structure
str(dwca_file_before_2000)
str(dwca_file_after_2015)




#___________________________________________________________________________
## Filtering out Marine Families  ####

m_b2000_t = dwca_file_before_2000 |> 
  filter(!family %in% c("Balaenidae", # whale families
                        "Balaenopteridae",
                        "Delphinidae",
                        "Monodontidae",
                        "Phocoenidae",
                        "Kogiidae",
                        "Physeteridae",
                        "Ziphiidae",
                        "Phocidae", # seals
                        "Odobenidae" # walruses
  ))

nrow(m_b2000_t)/nrow(dwca_file_before_2000)


m_a2015_t = dwca_file_after_2015 |> 
  filter(!family %in% c("Balaenidae", # whale families
                        "Balaenopteridae",
                        "Delphinidae",
                        "Monodontidae",
                        "Phocoenidae",
                        "Kogiidae",
                        "Physeteridae",
                        "Ziphiidae",
                        "Phocidae", # seals
                        "Odobenidae" # walruses
  ))

nrow(m_a2015_t)/nrow(dwca_file_after_2015)



#___________________________________________________________________________
## Extracting Species Lists  ####

# Extract unique species lists from both datasets
m_species_list_b2000 <- m_b2000_t %>%
  select(class, order, family, species) %>%
  distinct()

m_species_list_a2015 <- m_a2015_t %>%
  select(class, order, family, species) %>%
  distinct()

# Combine datasets
m_combined_species_list <- bind_rows(m_species_list_b2000, m_species_list_a2015) %>%
  distinct()  # Remove duplicates

# Adjust column names to have uppercase first letters
colnames(m_combined_species_list) <- tools::toTitleCase(colnames(combined_species_list))

# View result
head(m_combined_species_list)

# Export file
write.csv(m_combined_species_list, "own datasets/mammal_species_list.csv", row.names = FALSE)



#___________________________________________________________________________
## Visualization of Data  ####

world_data = world(path = ".")
ext_europe = c(-25, 62, 30, 77)


png("plot_before_2000.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_b2000_t$decimalLongitude, m_b2000_t$decimalLatitude,
       col = "#2e6f4040", #last 2 digits are transparency
       cex = .2)
dev.off()


png("plot_after_2015.png",
    width = 800,
    height = 600,
    res = 100)
plot(world_data, ext = ext_europe)
points(m_a2015_t$decimalLongitude, m_a2015_t$decimalLatitude,
       col = "#e64f4040",
       cex = .2)
dev.off()



#___________________________________________________________________________
# EBBA (Birds) ####

