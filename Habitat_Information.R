
#___________________________________________________________________________
# Loading Packages ####

library(tidyverse)
library(rredlist)
library(rgbif)
library(httr)
library(jsonlite)
library(data.table)




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
# Preparing Species Lists ####


#___________________________________________________________________________
## European Mammal Species ####

# european_mammals = read.csv("own datasets/Species_List_Mammals.csv")
european_mammals = read.csv("own datasets/Species_List_Mammals_Reduced.csv") #conservative IUCN guess

head(european_mammals)


# correcting species names unknown to IUCN RL
european_mammals = european_mammals %>% 
  mutate(Species = recode(Species, 
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


european_mammals = european_mammals %>%
  distinct(Species, .keep_all = TRUE) # removing possible duplicates


european_mammals = european_mammals |> 
  mutate(Genus = split_species(european_mammals$Species)$Genus) |> 
  mutate(Species = split_species(european_mammals$Species)$Species)

european_mammals = european_mammals [, c("Class", "Order", "Family", "Genus", "Species")]
str(european_mammals)






#___________________________________________________________________________
## European Bird Species ####

# european_birds = read.csv("own datasets/Species_List_Birds.csv")
european_birds = read.csv("own datasets/Species_List_Birds_Reduced_Change.csv") # only in reduced extent


european_birds = european_birds %>% 
  mutate(Species = recode(Species, 
                          "Aquila clanga" = "Clanga clanga",
                          "Aquila pomarina" = "Clanga pomarina",
                          "Charadrius morinellus" = "Eudromias morinellus",
                          "Chroicocephalus genei" = "Larus genei",
                          "Chroicocephalus ridibundus" = "Larus ridibundus",
                          "Ichthyaetus audouinii" = "Larus audouinii",
                          "Ichthyaetus melanocephalus" = "Larus melanocephalus",
                          "Stercorarius skua" = "Catharacta skua",
                          "Bonasa bonasia" = "Tetrastes bonasia",
                          "Porzana parva" = "Zapornia parva",
                          "Porzana pusilla" = "Zapornia pusilla",
                          "Calandrella rufescens" = "Alaudala rufescens",
                          "Coloeus monedula" = "Corvus monedula",
                          "Acanthis hornemanni" = "Acanthis flammea",
                          "Cyanecula svecica" = "Luscinia svecica",
                          "Erythropygia galactotes" = "Cercotrichas galactotes",
                          "Parus montanus" = "Poecile montanus",
                          "Phylloscopus sibillatrix" = "Phylloscopus sibilatrix",
                          "Sinosuthora alphonsiana" = "Suthora alphonsiana",
                          "Sinosuthora webbiana" = "Suthora webbiana",
                          "Sylvia cantillans" = "Curruca cantillans",
                          "Sylvia communis" = "Curruca communis",
                          "Sylvia conspicillata" = "Curruca conspicillata",
                          "Sylvia crassirostris" = "Curruca crassirostris",
                          "Sylvia curruca" = "Curruca curruca",
                          "Sylvia hortensis" = "Curruca hortensis",
                          "Sylvia melanocephala" = "Curruca melanocephala",
                          "Sylvia nisoria" = "Curruca nisoria",
                          "Sylvia ruppeli" = "Curruca ruppeli",
                          "Sylvia subalpina" = "Curruca subalpina",
                          "Sylvia undata" = "Curruca undata",
                          "Dendrocoptes medius" = "Leiopicus medius",
                          "Oceanodroma castro" = "Hydrobates castro",
                          "Oceanodroma leucorhoa" = "Hydrobates leucorhous",
                          "Psittacula eupatria" = "Palaeornis eupatria",
                          "Psittacula krameri" = "Alexandrinus krameri",
                          "Microcarbo pygmeus" = "Microcarbo pygmaeus"
  ))


european_birds = european_birds %>%
  distinct(Species, .keep_all = TRUE) # removing possible duplicates


european_birds = european_birds |> 
  mutate(Genus = split_species(european_birds$Species)$Genus) |> 
  mutate(Species = split_species(european_birds$Species)$Species)

european_birds = european_birds [, c("Class", "Order", "Family", "Genus", "Species")]
str(european_birds)




#___________________________________________________________________________
# IUCN Habitat Data ####

api_key = "p6KsBkyMcDwkk4LDjt4gyUuNGPBwT4X8K9Jq"



#___________________________________________________________________________
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
            formatted_habitats <- paste0(habitats$description, " [", habitats$suitability, "]", sep = "", collapse = "; ")
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
    Sys.sleep(0.6)
  }
  
  # Attach the failed species vector as an attribute to the result
  attr(habitats_column, "failed_species") <- failed_species
  
  # Return only the habitat data as a vector
  return(habitats_column)
}



#___________________________________________________________________________
#### Birds ####

european_bird_habitats = european_birds |> 
  mutate(Habitats_IUCN = pull_habitats(european_birds, api_key))


# 'failed species' (not found on IUCN)
failed_bird_species <- attr(european_bird_habitats$Habitats_IUCN, "failed_species")
failed_bird_species


# Combine Genus and Species columns to Binomial Name
european_bird_habitats$`Binomial Name` <- paste(european_bird_habitats$Genus, european_bird_habitats$Species)

european_bird_habitats = european_bird_habitats[, c("Class", "Order", "Family", "Binomial Name", "Habitats_IUCN")]


write.csv(european_bird_habitats, "own datasets/Habitat_Information_Birds.csv")

write.csv(failed_bird_species, "own datasets/Habitat_Information_Failed_Birds.csv")



#___________________________________________________________________________
#### Mammals ####

european_mammal_habitats = european_mammals |> 
  mutate(Habitats_IUCN = pull_habitats(european_mammals, api_key))


# 'failed species' (not found on IUCN)
failed_mammal_species <- attr(european_mammal_habitats$Habitats_IUCN, "failed_species")
failed_mammal_species


# Combine Genus and Species columns to Binomial Name
european_mammal_habitats$`Binomial Name` <- paste(european_mammal_habitats$Genus, european_mammal_habitats$Species)

european_mammal_habitats = european_mammal_habitats[, c("Class", "Order", "Family", "Binomial Name", "Habitats_IUCN")]


head(european_mammal_habitats)

write.csv(european_mammal_habitats, "own datasets/Habitat_Information_Mammals.csv")

write.csv(failed_mammal_species, "own datasets/Habitat_Information_Failed_Mammals.csv")



#___________________________________________________________________________
# Merging Datasets ####

# Combine the two datasets
european_species_habitats <- rbind(european_mammal_habitats, european_bird_habitats)

# View combined dataset
head(european_species_habitats)



#___________________________________________________________________________
## Exporting Dataset ####

write.csv(european_species_habitats, "own datasets/Habitat_Information_Complete.csv", row.names = F)



#___________________________________________________________________________
# Categorizing by Specialization ####

hab_pref = read.csv("own datasets/Habitat_Information_Complete.csv")
str(hab_pref)
# Split the column into separate categories
habitats_list <- strsplit(hab_pref$Habitats_IUCN, "; ")

# Get unique habitat types
unique_habitats <- unique(unlist(habitats_list))
unique_habitats <- sort(unique_habitats)
head(unique_habitats)


# Remove text inside square brackets
clean_habitats <- gsub("\\s*\\[.*?\\]", "", unique_habitats)

# Get unique and sorted habitats
clean_habitats <- sort(unique(clean_habitats))

# Display first few results
head(clean_habitats)
length(clean_habitats)/length(unique_habitats)

# Count unique categories
#length(unique_habitats)


# Count how many categories each row has
#num_categories_per_row <- sapply(habitats_list, length)

# Summary statistics
#summary(num_categories_per_row)



hab_pref_long <- hab_pref %>%
  mutate(row_id = row_number()) %>%
  separate_rows(Habitats_IUCN, sep = ", ")



#___________________________________________________________________________
## Forest, Agriculture, Generalist ####

# Define habitat categories
forest_habitats <- c("Forest - Boreal", 
                     "Forest - Subantarctic", 
                     "Forest - Subarctic",
                     "Forest - Subtropical/Tropical Dry", 
                     "Forest - Subtropical/Tropical Mangrove Vegetation Above High Tide Level",
                     "Forest - Subtropical/Tropical Moist Lowland",
                     "Forest - Subtropical/Tropical Moist Montane",
                     "Forest - Subtropical/Tropical Swamp",
                     "Forest - Temperate",
                     "Artificial/Terrestrial - Plantations" # is this rather forest?
                     #"Artificial/Terrestrial - Subtropical/Tropical Heavily Degraded Former Forest" #?
                     )

agriculture_habitats <- c("Artificial/Terrestrial - Arable Land", 
                          "Artificial/Terrestrial - Pastureland",
                          "Artificial/Aquatic - Irrigated Land (includes irrigation channels)", #?
                          "Artificial/Aquatic - Seasonally Flooded Agricultural Land" #?
                          )



# Process the data
# more inclusive forest habitat
specialization <- hab_pref_long %>%
  group_by(Binomial.Name) %>%
  summarise(
    has_forest_suitable = any(str_detect(Habitats_IUCN, paste(forest_habitats, collapse = "|")) & str_detect(Habitats_IUCN, "\\[Suitable\\]")),
    has_forest_marginal = any(str_detect(Habitats_IUCN, paste(forest_habitats, collapse = "|")) & str_detect(Habitats_IUCN, "\\[Marginal\\]")),
    has_agriculture_suitable = any(str_detect(Habitats_IUCN, paste(agriculture_habitats, collapse = "|")) & str_detect(Habitats_IUCN, "\\[Suitable\\]")),
    has_other_suitable = any(!str_detect(Habitats_IUCN, paste(c(forest_habitats, agriculture_habitats), collapse = "|")) & str_detect(Habitats_IUCN, "\\[Suitable\\]"))
  ) %>%
  mutate(
    specialization_FAG = case_when(
      has_forest_suitable & !has_agriculture_suitable ~ "forest",
      has_agriculture_suitable & !has_forest_suitable ~ "agriculture",
      TRUE ~ "generalist"
    )
  ) %>%
  dplyr::select(Binomial.Name, specialization_FAG)


# Merge back with original data
#hab_pref_long <- hab_pref_long %>%
#  left_join(specialization, by = "Binomial.Name")

nrow(hab_pref)
nrow(specialization)

missing_species <- setdiff(hab_pref$Binomial.Name, specialization$Binomial.Name)
print(missing_species)

hab_pref %>%
  filter(Binomial.Name %in% missing_species) %>%
  dplyr::select(Binomial.Name, Habitats_IUCN) %>%
  print(n = Inf)
uniqueN(hab_pref$Binomial.Name)


m = read.csv("own datasets/Habitat_Information_Mammals.csv")
b = read.csv("own datasets/Habitat_Information_Birds.csv")
nrow(m) + nrow(b)
uniqueN(m$Binomial.Name) + uniqueN(b$Binomial.Name)
uniqueN(m$Binomial.Name)/nrow(m)
uniqueN(b$Binomial.Name)/nrow(b)

# possibly old taxonomic names in first period, therefore species double!!


# get list with rest of taxonomic info, merge, divide by birds/mammals, get separate stats

specialization1 = specialization %>% 
  left_join(hab_pref, specialization, by = "Binomial.Name")

specialization1$specialization = as.factor(specialization1$specialization_FAG)
summary(specialization1$specialization_FAG)

str(specialization1)

specialization1


# Function to count and calculate percentage
count_percent <- function(data, class_name) {
  data %>%
    filter(Class == class_name) %>%
    count(specialization_FAG) %>%
    mutate(percentage = (n / sum(n)) * 100)
}

# Count and percentage for Aves
aves_stats <- count_percent(specialization1, "Aves")

# Count and percentage for Mammalia
mammalia_stats <- count_percent(specialization1, "Mammalia")

# results
aves_stats
mammalia_stats


write.csv(specialization1, "own datasets/Habitat_Classes_FAG.csv", row.names = F)



#___________________________________________________________________________
## Natural vs. Human Dominated ####

clean_habitats

natural_habitats <- c("Back Slope",
                      "Caves and Subterranean Habitats (non-aquatic) - Caves",
                      "Caves and Subterranean Habitats (non-aquatic) - Other Subterranean Habitats",
                      "Desert",
                      "Desert - Cold",
                      "Desert - Hot",
                      "Desert - Temperate",
                      "Foreslope (Outer Reef Slope)",
                      "Forest - Boreal",
                      "Forest - Subarctic",
                      "Forest - Subtropical/Tropical Dry",
                      "Forest - Subtropical/Tropical Mangrove Vegetation Above High Tide Level",
                      "Forest - Subtropical/Tropical Moist Lowland",
                      "Forest - Subtropical/Tropical Moist Montane",
                      "Forest - Subtropical/Tropical Swamp",
                      "Forest - Temperate",
                      "Grassland",
                      "Grassland - Subarctic",
                      "Grassland - Subtropical/Tropical Dry",
                      "Grassland - Subtropical/Tropical High Altitude",
                      "Grassland - Subtropical/Tropical Seasonally Wet/Flooded",
                      "Grassland - Temperate",
                      "Grassland - Tundra",
                      "Inter-Reef Rubble Substrate",
                      "Inter-Reef Soft Substrate",
                      "Introduced vegetation",
                      "Lagoon",
                      "Marine Coastal/Supratidal - Coastal Brackish/Saline Lagoons/Marine Lakes",
                      "Marine Coastal/Supratidal - Coastal Freshwater Lakes",
                      "Marine Coastal/Supratidal - Coastal Sand Dunes",
                      "Marine Coastal/Supratidal - Sea Cliffs and Rocky Offshore Islands",
                      "Marine Intertidal",
                      "Marine Intertidal - Mud Flats and Salt Flats",
                      "Marine Intertidal - Rocky Shoreline",
                      "Marine Intertidal - Salt Marshes (Emergent Grasses)",
                      "Marine Intertidal - Sandy Shoreline and/or Beaches, Sand Bars, Spits, Etc",
                      "Marine Intertidal - Shingle and/or Pebble Shoreline and/or Beaches",
                      "Marine Intertidal - Tidepools",
                      "Marine Neritic - Estuaries",
                      "Marine Neritic - Macroalgal/Kelp",
                      "Marine Neritic - Pelagic",
                      "Marine Neritic - Seagrass (Submerged)",
                      "Marine Neritic - Subtidal Loose Rock/pebble/gravel",
                      "Marine Neritic - Subtidal Rock and Rocky Reefs",
                      "Marine Neritic - Subtidal Sandy",
                      "Marine Neritic - Subtidal Sandy-Mud",
                      "Marine Oceanic",
                      "Marine Oceanic - Epipelagic (0-200m)",
                      "Marine Oceanic - Mesopelagic (200-1000m)",
                      "Other",
                      "Outer Reef Channel",
                      "Rocky areas (eg. inland cliffs, mountain peaks)",
                      "Savanna - Dry",
                      "Savanna - Moist",
                      "Shrubland",
                      "Shrubland - Boreal",
                      "Shrubland - Mediterranean-type Shrubby Vegetation",
                      "Shrubland - Subantarctic",
                      "Shrubland - Subarctic",
                      "Shrubland - Subtropical/Tropical Dry",
                      "Shrubland - Subtropical/Tropical High Altitude",
                      "Shrubland - Subtropical/Tropical Moist",
                      "Shrubland - Temperate",
                      "Unknown",
                      "Wetlands (inland)",
                      "Wetlands (inland) - Alpine Wetlands (includes temporary waters from snowmelt)",
                      "Wetlands (inland) - Bogs, Marshes, Swamps, Fens, Peatlands",
                      "Wetlands (inland) - Freshwater Springs and Oases",
                      "Wetlands (inland) - Permanent Freshwater Lakes (over 8ha)",
                      "Wetlands (inland) - Permanent Freshwater Marshes/Pools (under 8ha)",
                      "Wetlands (inland) - Permanent Inland Deltas",
                      "Wetlands (inland) - Permanent Rivers/Streams/Creeks (includes waterfalls)",
                      "Wetlands (inland) - Permanent Saline, Brackish or Alkaline Lakes",
                      "Wetlands (inland) - Permanent Saline, Brackish or Alkaline Marshes/Pools",
                      "Wetlands (inland) - Seasonal/Intermittent Freshwater Lakes (over 8ha)",
                      "Wetlands (inland) - Seasonal/Intermittent Freshwater Marshes/Pools (under 8ha)",
                      "Wetlands (inland) - Seasonal/Intermittent Saline, Brackish or Alkaline Lakes and Flats",
                      "Wetlands (inland) - Seasonal/Intermittent Saline, Brackish or Alkaline Marshes/Pools",
                      "Wetlands (inland) - Seasonal/Intermittent/Irregular Rivers/Streams/Creeks",
                      "Wetlands (inland) - Shrub Dominated Wetlands",
                      "Wetlands (inland) - Tundra Wetlands (incl. pools and temporary waters from snowmelt)"
)

human_dominated_habitats <- c("Artificial/Aquatic - Aquaculture Ponds",
                              "Artificial/Aquatic - Canals and Drainage Channels, Ditches",
                              "Artificial/Aquatic - Excavations (open)",
                              "Artificial/Aquatic - Irrigated Land (includes irrigation channels)",
                              "Artificial/Aquatic - Karst and Other Subterranean Hydrological Systems (human-made)",
                              "Artificial/Aquatic - Ponds (below 8ha)",
                              "Artificial/Aquatic - Salt Exploitation Sites",
                              "Artificial/Aquatic - Seasonally Flooded Agricultural Land",
                              "Artificial/Aquatic - Wastewater Treatment Areas",
                              "Artificial/Aquatic - Water Storage Areas (over 8ha)",
                              "Artificial/Marine - Mari/Brackishculture Ponds",
                              "Artificial/Marine - Mariculture Cages",
                              "Artificial/Marine - Marine Anthropogenic Structures",
                              "Artificial/Terrestrial - Arable Land",
                              "Artificial/Terrestrial - Pastureland",
                              "Artificial/Terrestrial - Plantations",
                              "Artificial/Terrestrial - Rural Gardens",
                              "Artificial/Terrestrial - Subtropical/Tropical Heavily Degraded Former Forest",
                              "Artificial/Terrestrial - Urban Areas"
)



specialization_AN <- hab_pref_long %>%
  group_by(Binomial.Name) %>%
  summarise(
    has_artificial_suitable = any(map_lgl(Habitats_IUCN, function(hab) {
      any(str_detect(hab, fixed(human_dominated_habitats))) &&
        str_detect(hab, fixed("[Suitable]"))
    }), na.rm = TRUE),
    
    has_natural_suitable = any(map_lgl(Habitats_IUCN, function(hab) {
      any(str_detect(hab, fixed(natural_habitats))) &&
        str_detect(hab, fixed("[Suitable]"))
    }), na.rm = TRUE)
  ) %>%
  mutate(
    specialization_AN = case_when(
      has_artificial_suitable ~ "human_dominated",
      !has_artificial_suitable & has_natural_suitable ~ "natural",
      TRUE ~ "unknown"  # No suitable habitat at all
    )
  ) %>%
  dplyr::select(Binomial.Name, specialization_AN)


hab_pref1 = read.csv("own datasets/Habitat_Classes_FAG.csv")

  
specialization_total = specialization_AN %>% 
  left_join(hab_pref1, specialization_AN, by = "Binomial.Name")

specialization_total$specialization = as.factor(specialization_total$specialization_AN)
summary(specialization_total$specialization_AN)

str(specialization_total)

specialization_total



# Function to count and calculate percentage
count_percent1 <- function(data, class_name) {
  data %>%
    filter(Class == class_name) %>%
    count(specialization_AN) %>%
    mutate(percentage = (n / sum(n)) * 100)
}

test = specialization_total %>% 
  filter(specialization_AN == "unknown") %>% 
  print()


# not an elegant solution, but necessary
specialization_AN2 <- specialization_total %>%
  mutate(specialization_AN = as.character(specialization_AN)) %>%
  mutate(specialization_AN = ifelse(specialization_AN == "unknown", "natural", specialization_AN)) %>%
  mutate(specialization_AN = as.factor(specialization_AN))

summary(specialization_AN2$specialization_AN)


# Count and percentage for Aves
aves_stats <- count_percent1(specialization_AN2, "Aves")

# Count and percentage for Mammalia
mammalia_stats <- count_percent1(specialization_AN2, "Mammalia")

aves_stats
mammalia_stats


write.csv(specialization_AN2, "own datasets/Habitat_Classes_Complete.csv", row.names = F)
