library(readxl)
library(sf)
library(terra)
library(dplyr)
library(purrr)



#___________________________________________________________________________
## Functions ####
#___________________________________________________________________________


create_squares <- function(points_sf, width) {
  # Ensure the input is a point sf object
  if (!inherits(points_sf, "sf") || !inherits(st_geometry(points_sf), "sfc_POINT")) {
    stop("Input must be an sf object with point geometries.")
  }
  # Calculate half-width (to shift the square corners)
  half_width <- width / 2
  
  squares_sf <- st_as_sf(st_buffer(points_sf, dist = half_width, endCapStyle = "SQUARE"))
  
  return(squares_sf)
}




#___________________________________________________________________________

filter_points_in_clusters <- function(points_sf, squares_sf, cluster_size_vector) 
  {
    npoints = nrow(points_sf)
    n_clusters_vector = npoints%/%cluster_size_vector
    n_clusters_vector[n_clusters_vector==0]=1  # when whole landscape, npoints<cluster_size 
    points_within_clusters<- list()
    for (n_clusters in n_clusters_vector) 
    {
      if(n_clusters==npoints)
      {
        points_in_clusters <- split(points_sf, 1:npoints)
        clusters_convex_hulls <- split(st_geometry(squares_sf), 1:npoints)
      }
      else
      {
      # Extract coordinates for k-means
      coords <- st_coordinates(points_sf)
      
      # Perform k-means clustering
      kmeans_result <- kmeans(coords, centers = n_clusters)
      
      # split the points sf into a list of clusters of points
      points_in_clusters <- split(points_sf,kmeans_result$cluster)
      squares_in_clusters <- split(squares_sf,kmeans_result$cluster)
      
      # Merge all squares into a single geometry
      merged_geom_within_clusters <- lapply(squares_in_clusters, st_union)
      
      # Compute convex hull
      clusters_convex_hulls <- lapply(merged_geom_within_clusters, st_convex_hull)
      
      #clusters_convex_hulls <- lapply(points_in_clusters, 
      #                                   function(points) {st_convex_hull(st_union(points))})
      }
      # Add the points to the list
      points_within_clusters[[paste0("size_", 
                                     cluster_size_vector[n_clusters_vector==n_clusters]) ]] <-
        list(points=points_in_clusters,chulls=clusters_convex_hulls)
    }
  # Return the list of points within clusters
  return(points_within_clusters)
}



#___________________________________________________________________________

extract_species_positions <- function(species_habitat_matrix, species_site_matrix) {
  # Get habitat names
  habitat_names <- colnames(species_habitat_matrix[,-1])
  
  # Initialize a list to store species positions for each habitat
  habitat_positions <- list()
  
  # Loop through each habitat
  for (habitat in habitat_names) {
    # Get species associated with this habitat
    species_in_habitat <- 
      rownames(species_habitat_matrix)[species_habitat_matrix[, habitat] == 1]
    
    # Find positions (indices) of these species in the species-site matrix
    species_positions <- which(rownames(species_site_matrix) %in% species_in_habitat)
    
    # Store results in the list
    habitat_positions[[habitat]] <- species_positions
  }
  
  return(habitat_positions)
}


#___________________________________________________________________________
# AI-suggested alternative for "extract_species_positions":

extract_species_indices <- function(species_habitat_matrix, species_site_matrix) {
  # Remove 'Species' column from habitat matrix if it exists
  habitat_names <- setdiff(colnames(species_habitat_matrix), "Species")
  
  # Set rownames to species names for easier lookup
  rownames(species_habitat_matrix) <- species_habitat_matrix$Species
  
  # Get species names used in the site matrix (i.e. points[,-1])
  site_species_names <- colnames(species_site_matrix)
  
  # Initialize list to store indices of species for each habitat group
  species_indices_by_habitat <- list()
  
  for (habitat in habitat_names) {
    # Species marked as 1 in this habitat
    species_in_habitat <- rownames(species_habitat_matrix)[species_habitat_matrix[[habitat]] == 1]
    
    # Only keep species that are actually present in the site matrix
    matching_species <- intersect(site_species_names, species_in_habitat)
    
    # Convert species names to indices (i.e. column positions in site matrix)
    indices <- match(matching_species, site_species_names)
    
    # Store under the habitat name
    species_indices_by_habitat[[habitat]] <- indices
  }
  
  return(species_indices_by_habitat)
}



#___________________________________________________________________________

summarize_samples<- function(samples, polygons, habitat_raster, habitat_names,
                             habitat_values, species_groups, species_group_names)
{
  # Initialize an empty data frame for the results
  results_df <- data.frame(matrix(ncol = length(habitat_names)+
                                    length(species_group_names)+2, 
                                  nrow = 0))
  colnames(results_df) <- c(habitat_names,"Area_Total",species_group_names,"Sp_Total")
  
  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(samples), style = 3)
  
  # Iterate over each area sample (i.e. group of sites)
  for (i in seq_along(samples)) {
    sample <- samples[[i]]
    polygon <- polygons[[i]]
    
    # Crop and mask the raster to the polygon extent
    habitat_cropped <- crop(habitat_raster, vect(polygon))
    habitat_masked <- mask(habitat_cropped, vect(polygon))
    
    # Calculate the area of each habitat type
    habitat_df <- freq(habitat_masked, bylayer=FALSE)
    habitat_df <- rbind(habitat_df,freq(habitat_masked, bylayer=FALSE,value=NA))
    
    # Ensure all possible values are included
    all_values_df <- data.frame(value = habitat_values, count = 0)
    
    # Merge with actual frequency data, replacing 0 where missing
    habitat_df <- merge(all_values_df, habitat_df, by = "value", all.x = TRUE)
    
    # Fill NA counts with 0
    habitat_df$count <- ifelse(is.na(habitat_df$count.y), 0, habitat_df$count.y)
    
    # Drop unnecessary column
    habitat_df <- habitat_df[, c("value", "count")]
    
    habitat_df$area <- habitat_df$count * res(habitat_raster)[1] * res(habitat_raster)[2]
    
    # Store the results
    results_df[i, seq_along(habitat_names)] <- habitat_df$area # store area of each habitat
    results_df[i, length(habitat_names)+1] <- 
      sum(results_df[i, seq_along(habitat_names)]) # store total area
    
    # Subset species occurrences for each group
    total_species=0
    for (k in seq_along(species_groups))
    {
      group_species <- st_drop_geometry(sample)[, species_groups[[k]]+1 ]
      species_present <- colSums(group_species > 0)  # Count species occurrences
      num_species <- sum(species_present > 0)       # Number of species in the group
      results_df[i, length(habitat_names)+1+k] <- num_species #store species number in results
      total_species <- total_species + num_species
    }
    
    #store the total number of species by summing across species groups
    results_df[i, length(habitat_names)+length(species_groups)+2] = total_species
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  close(pb) # Close progress bar
  cat("\n✔ Sample summary complete!\n")
  
  return(results_df)
}






#___________________________________________________________________________
# Execution ####
#___________________________________________________________________________

#___________________________________________________________________________
### Specify Dataset ####


#________________________
dataset       <- "ebba1"  # "ebba1" or "ebba2"

classif_type  <- "nhd"    # "fag" or "nhd"
# (forest, agriculture, generalist / natural, human-dominated)

landuse_type  <- "nhd"    # "faugo", "fao" or "nhd"
# (forest, agriculture, urban, grassland, other / forest, agriculture, other / natural, human-dominated)
#________________________


# AUTO-GENERATED PATHS
file_suffix    <- toupper(dataset)
year           <- if (dataset == "ebba1") "2000" else "2018"

input_csv      <- paste0("own datasets/", dataset, "_matrix_centroids.csv")
landuse_raster <- paste0("own datasets/simplified_land_use_", year, "_", toupper(landuse_type), ".tif")
output_prefix  <- paste0("own datasets/cSARgeom_", file_suffix, "_", classif_type, "_", landuse_type)
output_final   <- paste0(output_prefix, "_sum.csv")
classif_file   <- paste0("own datasets/aves_classification_", file_suffix, "_", classif_type, ".csv")
# SPECIES GROUP NAMES BASED ON CLASSIF
species_group_names <- if (classif_type == "fag") {
  c("forest_sp", "agriculture_sp", "generalist_sp")
} else {
  c("natural_sp", "human_dominated_sp")
}

if (landuse_type == "faugo") {
  habitat_names  <- c("Forest", "Agriculture", "Urban", "Grassland", "Other")
  habitat_values <- c(1, 2, 3, 4, 5)
  
} else if (landuse_type == "fao") {
  habitat_names  <- c("Forest", "Agriculture", "Other")
  habitat_values <- c(1, 2, 3)
  
} else if (landuse_type == "nhd") {
  habitat_names  <- c("Natural", "Human_dominated")
  habitat_values <- c(1, 2)
  
} else {
  stop("Invalid landuse_type. Use 'fao', 'faugo', or 'nhd'")
}

species_group_names <- if (classif_type == "fag") {
  c("forest_sp", "agriculture_sp", "generalist_sp")
} else {
  c("natural_sp", "human_dominated_sp")
}



#___________________________________________________________________________
### Independent of Species Classes ####

data <- read_csv(input_csv)
points = st_as_sf(data,coords=c("lon","lat"), crs=3035)

squares_sf <- create_squares(points, width = 50000) # changed to 50,000 to fit minimum size of EBBA grid cells
pt_in_clusters = filter_points_in_clusters(points,squares_sf,c(1,4,16,64,256,1024))


# Extract convex hulls for each scale (list of lists)
polygon_templates <- lapply(pt_in_clusters, function(cl) cl$chulls)


# Save to file for reproducibility
# saveRDS(polygon_templates, "own datasets/fixed_cluster_polygons_EBBA1.rds")



#___________________________________________________________________________
### Dependent on Species Classes ####

# pt_in_clusters <- read_csv(paste0(output_prefix, ".csv"))
lu <- terra::rast(landuse_raster)
classif <- read_csv(classif_file)

species_groups = extract_species_indices(classif,points[,-1])

samples=lapply(pt_in_clusters , \(x) x$points)
polygons=lapply(pt_in_clusters , \(x) x$chulls)
res=summarize_samples(purrr::flatten(samples), purrr::flatten(polygons),
                      lu, habitat_names, habitat_values, species_groups,
                      names(species_groups))
head(res)

write_csv(res, output_final)



#___________________________________________________________________________
# cSAR ####

res = read_csv(output_final)

library(sars)
help("sar_countryside")
plot(log(res$Area_Total),log(res$Sp_Total))
sar=lm(log(res$Sp_Total)~log(res$Area_Total))
summary(sar)

datacsar = res %>% 
  dplyr::select(-Area_Total, -Sp_Total)
head(datacsar)

which(apply(datacsar[, 1:2], 1, function(row) all(row == 0)))


cSAR <- sar_countryside(data = datacsar, modType = "power",
                      gridStart = "none", 
                      habNam = habitat_names, 
                      spNam = species_group_names)
cSAR



#___________________________________________________________________________
# deltaSR for delta_cSAR ####
#___________________________________________________________________________


library(sf)
library(terra)
library(dplyr)
library(purrr)

#___________________________________________________________________________
### Specify Dataset ####

#________________________
dataset       <- "ebba1"  # "ebba1" or "ebba2"

classif_type  <- "nhd"    # "fag" or "nhd"
# (forest, agriculture, generalist / natural, human-dominated)

landuse_type  <- "nhd"    # "faugo", "fao" or "nhd"
# (forest, agriculture, urban, grassland, other / forest, agriculture, other / natural, human-dominated)
#________________________



#___________________________________________________________________________
### Execution ####

# AUTO-GENERATED PATHS
file_suffix    <- toupper(dataset)
year           <- if (dataset == "ebba1") "2000" else "2018"

input_csv      <- paste0("own datasets/", dataset, "_matrix_centroids.csv")
landuse_raster <- paste0("own datasets/simplified_land_use_", year, "_", toupper(landuse_type), ".tif")
output_prefix  <- paste0("own datasets/cSARgeom_", file_suffix, "_", classif_type, "_", landuse_type)
output_final   <- paste0(output_prefix, "_sum.csv")
classif_file   <- paste0("own datasets/aves_classification_", file_suffix, "_", classif_type, ".csv")
# SPECIES GROUP NAMES BASED ON CLASSIF
species_group_names <- if (classif_type == "fag") {
  c("forest_sp", "agriculture_sp", "generalist_sp")
} else {
  c("natural_sp", "human_dominated_sp")
}

if (landuse_type == "faugo") {
  habitat_names  <- c("Forest", "Agriculture", "Urban", "Grassland", "Other")
  habitat_values <- c(1, 2, 3, 4, 5)
  
} else if (landuse_type == "fao") {
  habitat_names  <- c("Forest", "Agriculture", "Other")
  habitat_values <- c(1, 2, 3)
  
} else if (landuse_type == "nhd") {
  habitat_names  <- c("Natural", "Human_dominated")
  habitat_values <- c(1, 2)
  
} else {
  stop("Invalid landuse_type. Use 'fao', 'faugo', or 'nhd'")
}

species_group_names <- if (classif_type == "fag") {
  c("forest_sp", "agriculture_sp", "generalist_sp")
} else {
  c("natural_sp", "human_dominated_sp")
}



data <- read_csv(input_csv)
points = st_as_sf(data,coords=c("lon","lat"), crs=3035)


# modified function
filter_points_in_ready_clusters <- function(points_sf, given_chulls) 
{
  npoints <- nrow(points_sf)
  points_within_clusters <- list()
  
  for (key in names(given_chulls)) 
  {
    chulls_this_size <- given_chulls[[key]]
    n_clusters <- length(chulls_this_size)
    
    if (n_clusters == npoints) 
    {
      points_in_clusters <- split(points_sf, 1:npoints)
    } 
    else 
    {
      coords <- st_coordinates(points_sf)
      kmeans_result <- kmeans(coords, centers = n_clusters)
      points_in_clusters <- split(points_sf, kmeans_result$cluster)
    }
    
    points_within_clusters[[key]] <- list(
      points = points_in_clusters,
      chulls = chulls_this_size
    )
  }
  
  return(points_within_clusters)
}

polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")


pt_in_clusters = filter_points_in_ready_clusters(points, polygon_templates)


# rest of the pipeline

lu <- terra::rast(landuse_raster)
classif <- read_csv(classif_file)

species_groups = extract_species_indices(classif,points[,-1])


samples=lapply(pt_in_clusters , \(x) x$points)
polygons=lapply(pt_in_clusters , \(x) x$chulls)


# polygon_templates is your named list: size_1, size_4, etc.
named_polygons <- list()
polygon_ids <- c()

for (size_name in names(polygons)) {
  poly_list <- polygons[[size_name]]
  size_number <- sub("size_", "", size_name)  # get just the number part
  
  for (i in seq_along(poly_list)) {
    poly_id <- paste0(size_number, "_", sprintf("%03d", i))
    polygon_ids <- c(polygon_ids, poly_id)
    named_polygons <- append(named_polygons, list(poly_list[[i]]))
  }
}


res=summarize_samples(purrr::flatten(samples), named_polygons,
                      lu, habitat_names, habitat_values, species_groups,
                      names(species_groups))
head(res)

write_csv(res, paste0(output_final))



#___________________________________________________________________________

t0_df = read_csv("own datasets/cSARgeom_EBBA1_fag_faugo_fixed.csv")
t1_df = read_csv("own datasets/cSARgeom_EBBA2_fag_faugo_fixed.csv")

head(t0_df)
head(t1_df)



t0 <- cbind(polygon_id = polygon_ids, t0_df)
t1 <- cbind(polygon_id = polygon_ids, t1_df)

delta_df <- t0 %>%
  rename_with(~ paste0(.x, "_t0"), -polygon_id) %>%
  left_join(rename_with(t1, ~ paste0(.x, "_t1"), -polygon_id), by = "polygon_id") %>%
  mutate(
    delta_forest_sp = forest_sp_t1 - forest_sp_t0,
    delta_agriculture_sp = agriculture_sp_t1 - agriculture_sp_t0,
    delta_generalist_sp = generalist_sp_t1 - generalist_sp_t0
  )

head(delta_df)

write_csv(delta_df, "own datasets/cSARgeom_deltaEBBA_fixed.csv")



#___________________________________________________________________________
### Automated Pipeline ####

run_pipeline <- function(dataset, classif_type, landuse_type) {
  
  file_suffix    <- toupper(dataset)
  year           <- if (dataset == "ebba1") "2000" else if (dataset == "ebba2") "2018" else stop("Unknown dataset")
  
  input_csv      <- paste0("own datasets/", dataset, "_matrix_centroids.csv")
  landuse_raster <- paste0("own datasets/simplified_land_use_", year, "_", toupper(landuse_type), ".tif")
  output_prefix  <- paste0("own datasets/cSARgeom_", file_suffix, "_", classif_type, "_", landuse_type)
  output_final   <- paste0(output_prefix, "_sum.csv")
  classif_file   <- paste0("own datasets/aves_classification_", file_suffix, "_", classif_type, ".csv")
  
  # Define habitat
  habitat_map <- list(
    faugo = list(names = c("Forest", "Agriculture", "Urban", "Grassland", "Other"), values = 1:5),
    fao   = list(names = c("Forest", "Agriculture", "Other"), values = 1:3),
    nhd   = list(names = c("Natural", "Human_dominated"), values = 1:2)
  )
  if (!landuse_type %in% names(habitat_map)) stop("Invalid landuse_type.")
  
  habitat_names  <- habitat_map[[landuse_type]]$names
  habitat_values <- habitat_map[[landuse_type]]$values
  
  # Species group names
  species_group_names <- if (classif_type == "fag") {
    c("forest_sp", "agriculture_sp", "generalist_sp")
  } else {
    c("natural_sp", "human_dominated_sp")
  }
  
  data <- read_csv(input_csv)
  points <- st_as_sf(data, coords = c("lon", "lat"), crs = 3035)
  
  polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")
  pt_in_clusters <- filter_points_in_ready_clusters(points, polygon_templates)
  
  lu <- terra::rast(landuse_raster)
  classif <- read_csv(classif_file)
  species_groups <- extract_species_indices(classif, points[,-1])
  
  samples <- lapply(pt_in_clusters, \(x) x$points)
  polygons <- lapply(pt_in_clusters, \(x) x$chulls)
  
  # Generate polygon IDs
  named_polygons <- list()
  polygon_ids <- c()
  for (size_name in names(polygons)) {
    poly_list <- polygons[[size_name]]
    size_number <- sub("size_", "", size_name)
    for (i in seq_along(poly_list)) {
      poly_id <- paste0(size_number, "_", sprintf("%03d", i))
      polygon_ids <- c(polygon_ids, poly_id)
      named_polygons <- append(named_polygons, list(poly_list[[i]]))
    }
  }
  
  res <- summarize_samples(purrr::flatten(samples), named_polygons,
                           lu, habitat_names, habitat_values, species_groups,
                           names(species_groups))
  
  write_csv(res, output_final)
  
  return(list(data = res, polygon_ids = polygon_ids))
}



# Load libraries
library(sf)
library(terra)
library(dplyr)
library(purrr)

dataset_t0   = "ebba1"
dataset_t1   = "ebba2"
classif_type = "nhd"  # "fag" or "nhd"
landuse_type = "nhd"  # "faugo", "fao" or "nhd"

# Run pipeline separately for each dataset
res1 <- run_pipeline(dataset_t0, classif_type, landuse_type)
res2 <- run_pipeline(dataset_t1, classif_type, landuse_type)

# Prepare inputs
t0 <- cbind(polygon_id = res1$polygon_ids, res1$data)
t1 <- cbind(polygon_id = res2$polygon_ids, res2$data)

# Merge with renamed columns
t0_renamed <- t0 %>% rename_with(~ paste0(.x, "_t0"), -polygon_id)
t1_renamed <- t1 %>% rename_with(~ paste0(.x, "_t1"), -polygon_id)
delta_df <- left_join(t0_renamed, t1_renamed, by = "polygon_id")

# Identify species group columns
group_names <- setdiff(colnames(res1$data), "polygon_id")  # e.g. forest_sp, agriculture_sp, etc.

# Dynamically compute deltas
for (grp in group_names) {
  delta_col <- paste0("delta_", grp)
  t0_col <- paste0(grp, "_t0")
  t1_col <- paste0(grp, "_t1")
  
  delta_df[[delta_col]] <- delta_df[[t1_col]] - delta_df[[t0_col]]
}

head(delta_df)

delta_df %>% 
  dplyr::filter(Area_Total_t0 == 0)
delta_df %>% 
  dplyr::filter(Area_Total_t1 == 0)


# Save final output
output_file = paste0("own datasets/cSARgeom_deltaEBBA_fixed_", classif_type, "_", landuse_type, ".csv")
write_csv(delta_df, output_file)

boxplot(delta_df$delta_Sp_Total)
summary(delta_df$delta_Sp_Total)



#___________________________________________________________________________
# cSAR ####

res = read_csv(paste0("own datasets/cSARgeom_deltaEBBA_fixed_", classif_type, "_", landuse_type, ".csv"))

library(sars)
help("sar_countryside")
plot(log(res$Area_Total),log(res$Sp_Total))
sar=lm(log(res$Sp_Total)~log(res$Area_Total))
summary(sar)

datacsar = res %>% 
  dplyr::select(-Area_Total, -Sp_Total)
head(datacsar)

which(apply(datacsar[, 1:2], 1, function(row) all(row == 0)))


cSAR <- sar_countryside(data = datacsar, modType = "power",
                        gridStart = "none", 
                        habNam = habitat_names, 
                        spNam = species_group_names)
cSAR
