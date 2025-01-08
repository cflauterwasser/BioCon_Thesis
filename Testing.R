#_______________________________________________________________________
####  Loading Packages ####

library(tidyverse)
library(BioTIMEr)
library(geodata)
library(terra)
library(predictsFunctions)


#_______________________________________________________________________
####  Importing datasets ####

#BioTIME
biotime = read.csv("datasets/BioTIMEQuery_24_06_2021.csv")

str(biotime)
summary(biotime)


#worldclim
wc_data <- worldclim_global(var = "bio", res = 10, path = "./datasets") # downloading BioClim variables at 10-minute resolution

bio1 <- wc_data[[1]] # Loading specific variable (e.g., Bio1 - Annual Mean Temperature)

plot(bio1)


#PREDICTS
webFile <- url("https://timnewbold.github.io/predicts_database.rds?dl=1")
predicts <- readRDS(webFile)
