#___________________________________________________________________________
#1. LOADING PACKAGES ####

library(tidyverse)
library(BioTIMEr)
library(geodata)
library(terra)
library(predictsFunctions)



#___________________________________________________________________________
#2. ACCESSING DATABASES ####


#___________________________________________________________________________
####  BioTIME ####

biotime = read.csv("datasets/BioTIMEQuery_24_06_2021.csv")

str(biotime)
summary(biotime)

max(biotime$YEAR)
hist(biotime$YEAR)

resurvey_2020 <- biotime %>%
  filter(YEAR >= 2010)

hist(resurvey_2020$YEAR)

# read more about structure!



#___________________________________________________________________________
#### WorldClim ####

wc_data <- worldclim_global(var = "bio", res = 10, path = "./datasets") # downloading BioClim variables at 10-minute resolution

bio1 <- wc_data[[1]] # Loading specific variable (e.g., Bio1 - Annual Mean Temperature)

plot(bio1)



#___________________________________________________________________________
#### PREDICTS ####

webFile <- url("https://timnewbold.github.io/predicts_database.rds?dl=1")
predicts <- readRDS(webFile)

str(predicts)
summary(predicts$Diversity_metric)


predicts_SR <- predicts %>%
  filter(Diversity_metric == "species richness")

ggplot(predicts_SR, aes(x = Measurement)) +
  geom_histogram(fill = "darkgreen",
                 color = "black") +
  labs(x = "Measurement",
       y = "Frequency") +
  theme_minimal()


predicts_abundance <- predicts %>%
  filter(Diversity_metric == "abundance")

ggplot(predicts_abundance, aes(x = Measurement)) +
  geom_histogram(fill = "darkgreen",
                 color = "black") +
  labs(x = "Measurement",
       y = "Frequency") +
  theme_minimal()



#___________________________________________________________________________
#### sREplot ####




#___________________________________________________________________________
#### Breeding Bird Survey ####


