
#___________________________________________________________________________
## Loading Packages ####

library(tidyverse)
library(rredlist)
library(rgbif)
library(httr)
library(jsonlite)




# Function Splitting Binomial Names ####

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




#___________________________________________________________________________
## European Mammal Species ####

european_mammals = read.csv("own datasets/Mammal_Species_List.csv")

european_mammals = european_mammals |> 
  mutate(Genus = split_species(european_mammals$Species)$Genus) |> 
  mutate(Species = split_species(european_mammals$Species)$Species)

european_mammals = european_mammals [, c("Class", "Order", "Family", "Genus", "Species")]
str(european_mammals)




#___________________________________________________________________________
## European Bird Species ####

european_birds = read.csv("own datasets/Bird_Species_List.csv")

european_birds = european_birds |> 
  mutate(Genus = split_species(european_birds$Species)$Genus) |> 
  mutate(Species = split_species(european_birds$Species)$Species)

european_birds = european_birds [, c("Class", "Order", "Family", "Genus", "Species")]
str(european_birds)




#___________________________________________________________________________
## IUCN Habitat Data ####

api_key = "p6KsBkyMcDwkk4LDjt4gyUuNGPBwT4X8K9Jq"


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


# Combine Genus and Species columns to Binomial Name
european_mammal_habitats$`Binomial Name` <- paste(european_mammal_habitats$Genus, european_mammal_habitats$Species)

european_mammal_habitats = european_mammal_habitats[, c("Class", "Order", "Family", "Binomial Name", "Habitats_IUCN", "Human_modified_IUCN")]


head(european_mammal_habitats)

write.csv(european_mammal_habitats, "own datasets/Mammal_Habitat_Information.csv")



#### Birds ####

european_bird_habitats = european_birds |> 
  mutate(Habitats_IUCN = pull_habitats(european_birds, api_key))


# 'failed species' (not found on IUCN)
failed_bird_species <- attr(european_bird_habitats$Habitats_IUCN, "failed_species")
failed_bird_species


# info on ability to live in human-modified habitats
european_bird_habitats <- european_bird_habitats %>%
  mutate(Human_modified_IUCN = ifelse(grepl("Artificial[^\\[]*\\[ Suitable \\]", Habitats_IUCN), "Yes", "No"))

head(european_bird_habitats)


# Combine Genus and Species columns to Binomial Name
european_bird_habitats$`Binomial Name` <- paste(european_bird_habitats$Genus, european_bird_habitats$Species)

european_bird_habitats = european_bird_habitats[, c("Class", "Order", "Family", "Binomial Name", "Habitats_IUCN", "Human_modified_IUCN")]


head(european_mammal_habitats)

write.csv(european_bird_habitats, "own datasets/Bird_Habitat_Information.csv")




#___________________________________________________________________________
## Merging Datasets ####

# Combine the two datasets
european_species_habitats <- rbind(european_mammal_habitats, european_bird_habitats)

# View combined dataset
head(european_species_habitats)




#___________________________________________________________________________
## Exporting Dataset ####

write.csv(european_species_habitats, "own datasets/Complete_Habitat_Information.csv")
