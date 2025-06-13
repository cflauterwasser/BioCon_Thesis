#___________________________________________________________________________
# Load Packages ####
#___________________________________________________________________________

library(tidyverse)



#___________________________________________________________________________
# Prepare data set ####
#___________________________________________________________________________

FAG_b_2000 = read.csv("own datasets/cSAR_FAG_b_2000.csv")
FAG_b_2018 = read.csv("own datasets/cSAR_FAG_b_2018.csv")

deltaT = read.csv("own datasets/deltaT_50km_20y.csv")

beta_deltaT = read.csv("own datasets/temperature_coefficients_EBBA_change.csv")


names(FAG_b_2000)[-1] <- paste0(names(FAG_b_2000)[-1], "_t0")
names(FAG_b_2018)[-1] <- paste0(names(FAG_b_2018)[-1], "_t1")
FAG_b_merged <- merge(FAG_b_2000, FAG_b_2018, by = "cell50x50")
FAG_b <- merge(FAG_b_merged, deltaT, by = "cell50x50", all.x = TRUE)

# Delta species richness (can do it per group, or total)
FAG_b$deltaS_F <- FAG_b$Spcs_F_t1 - FAG_b$Spcs_F_t0
FAG_b$deltaS_A <- FAG_b$Spcs_A_t1 - FAG_b$Spcs_A_t0
FAG_b$deltaS_O <- FAG_b$Spcs_O_t1 - FAG_b$Spcs_O_t0

head(FAG_b)



#___________________________________________________________________________
# Experimental New Function ####
#___________________________________________________________________________






#___________________________________________________________________________
# Evaluate Fit ####
#___________________________________________________________________________

sumofsquares<-function(x)
{
  y<-sum(residuals(x)^2)
  y	
}


summary()
sumofsquares()
AIC()