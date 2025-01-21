
#___________________________________________________________________________
## Loading Packages ####

library(tidyverse)
library(openxlsx)
library(rredlist)
library(rgbif)
library(httr)
library(jsonlite)




#___________________________________________________________________________
## ASM European Mammal Species ####

european_mammals = read.csv("external datasets/ASM Mammal Diversity Database/MDD_v1.13_6753species.csv")

european_mammals$continentDistribution = as.factor(european_mammals$continentDistribution)
summary(european_mammals$continentDistribution)


european_mammals <- european_mammals |> 
  filter(str_detect(continentDistribution, "Europe"))


european_mammals$continentDistribution = as.factor(european_mammals$continentDistribution)
summary(european_mammals$continentDistribution)


# reducing to Order, Family, Genus and Species
european_mammals <- european_mammals %>%
  dplyr::select(Order = 10, Family = 15, Genus = 18, Species = 20)
head(european_mammals)


# adding Class
european_mammals <- european_mammals %>%
  mutate(Class = "Mammalia") %>%
  relocate(Class, .before = Order)




#___________________________________________________________________________
## ERLoB European Bird Species ####

european_birds = read.xlsx("external datasets/European Red List of Birds 2021/ERLoB2021_categories.xlsx", sheet = 2, colNames = F)

european_birds <- european_birds %>%
  setNames(as.character(slice(., 2))) %>%
  slice(-1, -2)

head(european_birds)



# GBIF approach

# Extract species names
species <- european_birds$`Scientific Name`

# Query GBIF with error handling
taxonomy <- lapply(species, function(sp) {
  tryCatch({
    result <- name_backbone(name = sp, rank = "species")
    # Extract only the relevant columns (family and order)
    data.frame(scientificName = sp, 
               Family = result$family, 
               Order = result$order)
  }, error = function(e) {
    # Return a data frame with NAs for unmatched species
    data.frame(scientificName = sp, 
               Family = NA, 
               Order = NA)
  })
})

# Combine all results into a single data frame
taxonomy_df <- do.call(rbind, taxonomy)

# Add Family and Order to the original data
european_birds <- cbind(european_birds, taxonomy_df[, c("Family", "Order")])

# View the updated data frame
head(european_birds)


european_birds <- european_birds %>%
  mutate(Class = "Aves") %>%
  dplyr::select(Class, Order, Family, `Scientific Name`)


# View the updated data frame
head(european_birds)




#___________________________________________________________________________
## IUCN Habitat Data ####

api_key = "p6KsBkyMcDwkk4LDjt4gyUuNGPBwT4X8K9Jq"


#test = rl_species_latest("Gorilla", "gorilla", key = api_key)
#habitats = test$habitats
#habitats = habitats$description


### Splitting Binomial Names ####

split_species <- function(species_vector) {
  # Split species names into Genus and Species
  species_split <- strsplit(species_vector, " ")
  
  # Create a data frame
  species_df <- data.frame(
    Genus = sapply(species_split, `[`, 1),
    Species = sapply(species_split, `[`, 2)
  )
  
  return(species_df)
}

# Example usage
#species <- c("Gorilla gorilla", "Pan troglodytes", "Canis lupus")
#species = split_species(species)


### Automated Pull of Habitat Data ####

pull_habitats <- function(species_list, api_key) {
  habitats_column <- vector("character", length = nrow(species_list)) # To store habitats
  failed_species <- vector("character") # To store failed species
  
  # Loop over each row of the species list
  for (n in 1:nrow(species_list)) {
    tryCatch({
      # API request for habitats
      list <- rl_species_latest(species_list$Genus[n], species_list$Species[n], key = api_key)
      
      if ("habitats" %in% names(list) && !is.null(list$habitats)) {
        habitats <- list$habitats
        if ("description" %in% colnames(habitats)) {
          habitats <- habitats %>%
            filter(!is.na(description) & description != "") %>%
            mutate(description = unlist(description))
          
          if (nrow(habitats) > 0) {
            formatted_habitats <- paste(habitats$description, "[", habitats$suitability, "]", sep = " ", collapse = ", ")
            habitats_column[n] <- formatted_habitats
          } else {
            habitats_column[n] <- NA
          }
        }
      } else {
        habitats_column[n] <- NA
      }
    }, error = function(e) {
      # Log failed species
      failed_species <<- c(failed_species, paste(species_list$Genus[n], species_list$Species[n]))
      habitats_column[n] <- NA
      message("Error for species: ", species_list$Genus[n], " ", species_list$Species[n], " - ", e$message)
    })
    
    # Print progress
    print(paste(n, "/", nrow(species_list), "species processed"))
    Sys.sleep(0.5)
  }
  
  # Attach the failed species vector as an attribute to the result
  attr(habitats_column, "failed_species") <- failed_species
  
  # Return only the habitat data as a vector
  return(habitats_column)
}


#### Mammals ####
european_mammal_habitats = european_mammals |> 
  mutate(Habitats_IUCN = pull_habitats(european_mammals, api_key))


# 'failed species' (not found on IUCN)
failed_mammal_species <- attr(european_mammal_habitats$Habitats_IUCN, "failed_species")
failed_mammal_species


# info on ability to live in human-modified habitats
european_mammal_habitats <- european_mammal_habitats %>%
  mutate(Human_modified_IUCN = ifelse(grepl("Artificial[^\\[]*\\[ Suitable \\]", Habitats_IUCN), "Yes", "No"))


head(european_mammal_habitats)



#### Birds ####
bird_species = split_species(european_birds$`Scientific Name`)

european_bird_habitats = european_birds |> 
  mutate(Habitats_IUCN = pull_habitats(bird_species, api_key))


# 'failed species' (not found on IUCN)
failed_bird_species <- attr(european_bird_habitats$Habitats_IUCN, "failed_species")
failed_bird_species


# info on ability to live in human-modified habitats
european_bird_habitats <- european_bird_habitats %>%
  mutate(Human_modified_IUCN = ifelse(grepl("Artificial[^\\[]*\\[ Suitable \\]", Habitats_IUCN), "Yes", "No"))


head(european_bird_habitats)




#___________________________________________________________________________
## Other Habitat Data Sources ####

# Query EUNIS API
species_name <- "Vulpes vulpes"
url <- paste0("https://eunis.eea.europa.eu/api/species/", URLencode(species_name), "/habitats")

response <- GET(url)
if (status_code(response) == 200) {
  habitat_data <- content(response, as = "text", encoding = "UTF-8")
  habitat_df <- fromJSON(habitat_data, flatten = TRUE)
  print(habitat_df)
} else {
  print("Failed to retrieve data. Check species name or API status.")
}



species_name <- "Vulpes vulpes"
url <- paste0("https://eol.org/api/pages/1.0.json?q=", URLencode(species_name))

response <- GET(url)
if (status_code(response) == 200) {
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  habitat_info <- data$taxonConcept$details$habitats
  print(habitat_info)
} else {
  print("Failed to fetch data from EOL.")
}




#___________________________________________________________________________
## Merging Datasets ####

# Combine Genus and Species columns in the mammal dataset
european_mammal_habitats$`Binomial Name` <- paste(european_mammal_habitats$Genus, european_mammal_habitats$Species)

# Select and rename columns to match the bird dataset structure
european_mammals <- european_mammal_habitats[, c("Class", "Order", "Family", "Binomial Name", "Habitats_IUCN", "Human_modified_IUCN")]

# Ensure column names match the bird dataset
european_birds <- european_bird_habitats
colnames(european_birds)[colnames(european_birds) == "Scientific Name"] <- "Binomial Name"

# Combine the two datasets
european_species_habitats <- rbind(european_mammals, european_birds)

# View combined dataset
head(european_species_habitats)




#___________________________________________________________________________
## Exporting Dataset ####

write.csv(european_species_habitats, "own datasets/Habitat_Information.csv")
