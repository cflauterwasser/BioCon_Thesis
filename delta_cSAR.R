# ___________________________________________________________________________
# Load Packages ####
# ___________________________________________________________________________

library(tidyverse)



# ___________________________________________________________________________
# Import fixed geom data set ####
# ___________________________________________________________________________

beta_deltaT <- read_csv("own datasets/temperature_coefficients_EBBA_change.csv")



# ___________________________________________________________________________
# NHD ####
# ___________________________________________________________________________

NHD_b <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_nhd_nhd.csv")
head(NHD_b)

NHD_b <- NHD_b %>%
  rename(
    Area_N_t0  = Natural_t0,
    Area_HD_t0 = Human_dominated_t0,
    Area_N_t1  = Natural_t1,
    Area_HD_t1 = Human_dominated_t1,
    Sp_N_t0    = natural_sp_t0,
    Sp_HD_t0   = human_dominated_sp_t0,
    S_t0       = Sp_Total_t0,
    Sp_N_t1    = natural_sp_t1,
    Sp_HD_t1   = human_dominated_sp_t1,
    S_t1       = Sp_Total_t1,
    deltaS_N   = delta_natural_sp,
    deltaS_HD  = delta_human_dominated_sp
  ) %>% 
  dplyr::select(Area_N_t0, Area_HD_t0,
                Area_N_t1, Area_HD_t1,
                Sp_N_t0, Sp_HD_t0,
                S_t0,
                Sp_N_t1, Sp_HD_t1,
                S_t1,
                deltaS_N, deltaS_HD
                )

head(NHD_b)



# ___________________________________________________________________________
### Without deltaT ####

# Fixed version of the cSAR model function, using hardcoded betaT
delta_cSAR_NHD <- function(Area_N_t0, Area_HD_t0,
                           Area_N_t1, Area_HD_t1,
                           h_N, h_HD, z) {
  S_t0 <- (h_N * Area_N_t0 + h_HD * Area_HD_t0)^z
  S_t1 <- (h_N * Area_N_t1 + h_HD * Area_HD_t1)^z
  
  deltaS_hat <- S_t1 - S_t0
  return(deltaS_hat)
}

# Shared starting values
start_vals_N <- list(
  h_N = 1.0, # strong natural landscape affinity
  h_HD = 0.1,
  z   = 0.25
)

start_vals_HD <- list(
  h_N = 0.1,
  h_HD = 1.0, # strong human-dominated landscape affinity
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_N = 1e-12,
  h_HD = 1e-12,
  z   = 0.01
)


# Fit for Natural birds
fit_cSAR_N <- nls(
  deltaS_N ~ delta_cSAR_NHD(
    Area_N_t0, Area_HD_t0,
    Area_N_t1, Area_HD_t1,
    h_N, h_HD,
    z
  ),
  data = NHD_b,
  start = start_vals_N,
  lower = lower_bounds,
  algorithm = "port", # needed for lower bounds
  trace = TRUE,
  control = nls.control(maxiter = 1000#, warnOnly = TRUE # suppresses stoping on error
  )
)

# Fit for Human-Dominated birds
fit_cSAR_HD <- nls(
  deltaS_HD ~ delta_cSAR_NHD(
    Area_N_t0, Area_HD_t0,
    Area_N_t1, Area_HD_t1,
    h_N, h_HD,
    z
  ),
  data = NHD_b,
  start = start_vals_HD,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_N)
summary(fit_cSAR_HD)



# ___________________________________________________________________________
### With deltaT / logCCV ####

# Use beta coefficient for deltaT (pre-extracted)
betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "deltaT"]

# Fixed version of the cSAR model function, using hardcoded betaT
delta_cSAR_deltaT_NHD <- function(Area_N_t0, Area_HD_t0,
                                  Area_N_t1, Area_HD_t1,
                                  h_N, h_HD,
                                  z,
                                  deltaT) {
  betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "log_ccv"] # fixed effect from external model
  
  S_t0 <- (h_N * Area_N_t0 + h_HD * Area_HD_t0)^z
  S_t1 <- (h_N * Area_N_t1 + h_HD * Area_HD_t1)^z
  
  deltaS_hat <- S_t1 - S_t0 + betaT * deltaT
  return(deltaS_hat)
}

# Shared starting values
start_vals_N <- list(
  h_N = 1.0,
  h_HD = 0.1,
  z   = 0.25
)

start_vals_HD <- list(
  h_N = 0.1,
  h_HD = 1.0,
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_N = 1e-12,
  h_HD = 1e-12,
  z   = 0.01
)


# Fit for Natural birds
fit_cSAR_N <- nls(
  deltaS_N ~ delta_cSAR_deltaT_NHD(
    Area_N_t0, Area_HD_t0,
    Area_N_t1, Area_HD_t1,
    h_N, h_HD,
    z,
    deltaT
  ),
  data = NHD_b,
  start = start_vals_N,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Human-Dominated birds
fit_cSAR_HD <- nls(
  deltaS_HD ~ delta_cSAR_deltaT_NHD(
    Area_N_t0, Area_HD_t0,
    Area_N_t1, Area_HD_t1,
    h_N, h_HD,
    z,
    deltaT
  ),
  data = NHD_HD,
  start = start_vals_A,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_N)
summary(fit_cSAR_HD)



# ___________________________________________________________________________
# FAO ####
# ___________________________________________________________________________

FAO_b <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_fag_fao.csv")
head(FAO_b)

FAO_b <- FAO_b %>%
  rename(
    Area_F_t0 = Forest_t0,
    Area_A_t0 = Agriculture_t0,
    Area_O_t0 = Other_t0,
    Area_F_t1 = Forest_t1,
    Area_A_t1 = Agriculture_t1,
    Area_O_t1 = Other_t1,
    Sp_F_t0 = forest_sp_t0,
    Sp_A_t0 = agriculture_sp_t0,
    Sp_G_t0 = generalist_sp_t0,
    S_t0 = Sp_Total_t0,
    Sp_F_t1 = forest_sp_t1,
    Sp_A_t1 = agriculture_sp_t1,
    Sp_G_t1 = generalist_sp_t1,
    S_t1 = Sp_Total_t1,
    deltaS_F = delta_forest_sp,
    deltaS_A = delta_agriculture_sp,
    deltaS_G = delta_generalist_sp
  )%>% 
  dplyr::select(
    Area_F_t0, Area_A_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_O_t1,
    Sp_F_t0, Sp_A_t0, Sp_G_t0,
    S_t0,
    Sp_F_t1, Sp_A_t1, Sp_G_t1,
    S_t1,
    deltaS_F, deltaS_A, deltaS_G
  )

head(FAO_b)



# ___________________________________________________________________________
### Without deltaT ####

delta_cSAR_FAO <- function(Area_F_t0, Area_A_t0, Area_O_t0,
                           Area_F_t1, Area_A_t1, Area_O_t1,
                           h_F, h_A, h_O, z) {
  S_t0 <- (h_F * Area_F_t0 + h_A * Area_A_t0 + h_O * Area_O_t0)^z
  S_t1 <- (h_F * Area_F_t1 + h_A * Area_A_t1 + h_O * Area_O_t1)^z
  
  deltaS_hat <- S_t1 - S_t0
  return(deltaS_hat)
}

# Shared starting values
start_vals_F <- list(
  h_F = 1.0, # Strong forest affinity
  h_A = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_A <- list(
  h_F = 0.1,
  h_A = 1.0, # Strong agriculture affinity
  h_O = 0.1,
  z   = 0.25
)

start_vals_O <- list(
  h_F = 0.5,
  h_A = 0.5,
  h_O = 0.5,
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_F = 1e-12,
  h_A = 1e-12,
  h_O = 1e-12,
  z   = 0.01
)


# Fit for Forest birds
fit_cSAR_F <- nls(
  deltaS_F ~ delta_cSAR_FAO(
    Area_F_t0, Area_A_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_O_t1,
    h_F, h_A, h_O,
    z
  ),
  data = FAO_b,
  start = start_vals_F,
  lower = lower_bounds,
  algorithm = "port", # needed for lower bounds
  trace = TRUE,
  control = nls.control(maxiter = 1000#, warnOnly = TRUE # suppresses stop-on-warning
  )
)

# Fit for Agriculture birds
fit_cSAR_A <- nls(
  deltaS_A ~ delta_cSAR_FAO(
    Area_F_t0, Area_A_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_O_t1,
    h_F, h_A, h_O, z
  ),
  data = FAO_b,
  start = start_vals_A,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Generalist/Other birds
fit_cSAR_G <- nls(
  deltaS_G ~ delta_cSAR_FAO(
    Area_F_t0, Area_A_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_O_t1,
    h_F, h_A, h_O, z
  ),
  data = FAO_b,
  start = start_vals_O,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_F)
summary(fit_cSAR_A)
summary(fit_cSAR_G)



# ___________________________________________________________________________
### With deltaT / logCCV ####

# Use beta coefficient for deltaT (pre-extracted)
betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "deltaT"]

# Fixed version of the cSAR model function, using hardcoded betaT
delta_cSAR_deltaT_FAO <- function(A_F_t0, A_A_t0, A_G_t0, A_U_t0, A_O_t0,
                              A_F_t1, A_A_t1, A_G_t1, A_U_t1, A_O_t1,
                              h_F, h_A, h_G, h_U, h_O, z, deltaT) {
  betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "log_ccv"] # fixed effect from external model
  
  S_t0 <- (h_F * A_F_t0 + h_A * A_A_t0 + h_G * A_G_t0 + h_U * A_U_t0 + h_O * A_O_t0)^z
  S_t1 <- (h_F * A_F_t1 + h_A * A_A_t1 + h_G * A_G_t1 + h_U * A_U_t1 + h_O * A_O_t1)^z
  
  deltaS_hat <- S_t1 - S_t0 + betaT * deltaT
  return(deltaS_hat)
}

# Shared starting values
start_vals_F <- list(
  h_F = 1.0, # Strong forest affinity
  h_A = 0.1,
  h_G = 0.1,
  h_U = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_A <- list(
  h_F = 0.1,
  h_A = 1.0, # Strong agriculture affinity
  h_G = 0.1,
  h_U = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_O <- list(
  h_F = 0.5,
  h_A = 0.5,
  h_G = 0.5,
  h_U = 0.5,
  h_O = 0.5,
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_F = 1e-12,
  h_A = 1e-12,
  h_G = 1e-12,
  h_U = 1e-12,
  h_O = 1e-12,
  z   = 0.01
)


# Fit for Forest birds
fit_cSAR_F <- nls(
  deltaS_F ~ delta_cSAR_deltaT_FAO(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAO_b,
  start = start_vals_F,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Agriculture birds
fit_cSAR_A <- nls(
  deltaS_A ~ delta_cSAR_deltaT_FAO(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAO_b,
  start = start_vals_A,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Generalist/Other birds
fit_cSAR_O <- nls(
  deltaS_O ~ delta_cSAR_deltaT_FAO(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAO_b,
  start = start_vals_O,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_F)
summary(fit_cSAR_A)
summary(fit_cSAR_O)



# ___________________________________________________________________________
# FAUGO ####
# ___________________________________________________________________________

FAUGO_b <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_fag_faugo.csv")
head(FAUGO_b)

FAUGO_b <- FAUGO_b %>%
  rename(
    Area_F_t0 = Forest_t0,
    Area_A_t0 = Agriculture_t0,
    Area_U_t0 = Urban_t0,
    Area_G_t0 = Grassland_t0,
    Area_O_t0 = Other_t0,
    Area_F_t1 = Forest_t1,
    Area_A_t1 = Agriculture_t1,
    Area_U_t1 = Urban_t1,
    Area_G_t1 = Grassland_t1,
    Area_O_t1 = Other_t1,
    Sp_F_t0 = forest_sp_t0,
    Sp_A_t0 = agriculture_sp_t0,
    Sp_G_t0 = generalist_sp_t0,
    S_t0 = Sp_Total_t0,
    Sp_F_t1 = forest_sp_t1,
    Sp_A_t1 = agriculture_sp_t1,
    Sp_G_t1 = generalist_sp_t1,
    S_t1 = Sp_Total_t1,
    deltaS_F = delta_forest_sp,
    deltaS_A = delta_agriculture_sp,
    deltaS_G = delta_generalist_sp
  )

head(FAUGO_b)



# ___________________________________________________________________________
### Without deltaT ####

# Use beta coefficient for deltaT (pre-extracted)
betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "deltaT"]

# Fixed version of the cSAR model function, using hardcoded betaT
delta_cSAR_FAUGO <- function(Area_F_t0, Area_A_t0, Area_U_t0, Area_G_t0, Area_O_t0,
                             Area_F_t1, Area_A_t1, Area_U_t1, Area_G_t1, Area_O_t1,
                       h_F, h_A, h_U, h_G, h_O, z) {
  S_t0 <- (h_F * Area_F_t0 + h_A * Area_A_t0 + h_U * Area_U_t0 + h_G * Area_G_t0 + h_O * Area_O_t0)^z
  S_t1 <- (h_F * Area_F_t1 + h_A * Area_A_t1 + h_U * Area_U_t1 + h_G * Area_G_t1 + h_O * Area_O_t1)^z

  deltaS_hat <- S_t1 - S_t0
  return(deltaS_hat)
}

# Shared starting values
start_vals_F <- list(
  h_F = 1.0, # Strong forest affinity
  h_A = 0.1,
  h_U = 0.1,
  h_G = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_A <- list(
  h_F = 0.1,
  h_A = 1.0, # Strong agriculture affinity
  h_U = 0.1,
  h_G = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_O <- list(
  h_F = 0.5,
  h_A = 0.5,
  h_U = 0.5,
  h_G = 0.5,
  h_O = 0.5,
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_F = 1e-12,
  h_A = 1e-12,
  h_U = 1e-12,
  h_G = 1e-12,
  h_O = 1e-12,
  z   = 0.01
)


# Fit for Forest birds
fit_cSAR_F <- nls(
  deltaS_F ~ delta_cSAR_FAUGO(
    Area_F_t0, Area_A_t0, Area_U_t0, Area_G_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_U_t1, Area_G_t1, Area_O_t1,
    h_F, h_A, h_U, h_G, h_O, z
  ),
  data = FAUGO_b,
  start = start_vals_F,
  lower = lower_bounds,
  algorithm = "port", # needed for lower bounds
  trace = TRUE,
  control = nls.control(maxiter = 1000#, warnOnly = TRUE # suppresses stop-on-warning
  )
)

# Fit for Agriculture birds
fit_cSAR_A <- nls(
  deltaS_A ~ delta_cSAR_FAUGO(
    Area_F_t0, Area_A_t0, Area_U_t0, Area_G_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_U_t1, Area_G_t1, Area_O_t1,
    h_F, h_A, h_U, h_G, h_O, z
  ),
  data = FAUGO_b,
  start = start_vals_A,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Generalist/Other birds
fit_cSAR_G <- nls(
  deltaS_G ~ delta_cSAR_FAUGO(
    Area_F_t0, Area_A_t0, Area_U_t0, Area_G_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_U_t1, Area_G_t1, Area_O_t1,
    h_F, h_A, h_U, h_G, h_O, z
  ),
  data = FAUGO_b,
  start = start_vals_O,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_F)
summary(fit_cSAR_A)
summary(fit_cSAR_G)



# ___________________________________________________________________________
### With deltaT / logCCV ####

# Use beta coefficient for deltaT (pre-extracted)
betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "deltaT"]

# Fixed version of the cSAR model function, using hardcoded betaT
delta_cSAR_deltaT_FAUGO <- function(A_F_t0, A_A_t0, A_G_t0, A_U_t0, A_O_t0,
                              A_F_t1, A_A_t1, A_G_t1, A_U_t1, A_O_t1,
                              h_F, h_A, h_G, h_U, h_O, z, deltaT) {
  betaT <- beta_deltaT$Coefficient[beta_deltaT$Variable == "log_ccv"] # fixed effect from external model

  S_t0 <- (h_F * A_F_t0 + h_A * A_A_t0 + h_G * A_G_t0 + h_U * A_U_t0 + h_O * A_O_t0)^z
  S_t1 <- (h_F * A_F_t1 + h_A * A_A_t1 + h_G * A_G_t1 + h_U * A_U_t1 + h_O * A_O_t1)^z

  deltaS_hat <- S_t1 - S_t0 + betaT * deltaT
  return(deltaS_hat)
}

# Shared starting values
start_vals_F <- list(
  h_F = 1.0, # Strong forest affinity
  h_A = 0.1,
  h_G = 0.1,
  h_U = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_A <- list(
  h_F = 0.1,
  h_A = 1.0, # Strong agriculture affinity
  h_G = 0.1,
  h_U = 0.1,
  h_O = 0.1,
  z   = 0.25
)

start_vals_O <- list(
  h_F = 0.5,
  h_A = 0.5,
  h_G = 0.5,
  h_U = 0.5,
  h_O = 0.5,
  z   = 0.25
)


# Set lower bounds
lower_bounds <- list(
  h_F = 1e-12,
  h_A = 1e-12,
  h_G = 1e-12,
  h_U = 1e-12,
  h_O = 1e-12,
  z   = 0.01
)


# Fit for Forest birds
fit_cSAR_F <- nls(
  deltaS_F ~ delta_cSAR_deltaT(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAUGO_b,
  start = start_vals_F,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Agriculture birds
fit_cSAR_A <- nls(
  deltaS_A ~ delta_cSAR_deltaT(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAUGO_b,
  start = start_vals_A,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)

# Fit for Generalist/Other birds
fit_cSAR_O <- nls(
  deltaS_O ~ delta_cSAR_deltaT(
    Area_F_t0, Area_A_t0, Area_G_t0, Area_U_t0, Area_O_t0,
    Area_F_t1, Area_A_t1, Area_G_t1, Area_U_t1, Area_O_t1,
    h_F, h_A, h_G, h_U, h_O, z, deltaT
  ),
  data = FAUGO_b,
  start = start_vals_O,
  lower = lower_bounds,
  algorithm = "port",
  trace = TRUE,
  control = nls.control(maxiter = 1000)
)


summary(fit_cSAR_F)
summary(fit_cSAR_A)
summary(fit_cSAR_O)



# ___________________________________________________________________________
# Evaluate Fit ####
# ___________________________________________________________________________

evaluate_cSAR <- function(model) {
  ssr <- sum(residuals(model)^2)
  aic_val <- AIC(model)
  model_summary <- summary(model)
  r_squared <- if ("r.squared" %in% names(model_summary)) model_summary$r.squared else NA
  
  results <- list(
    Sum_of_Squares = ssr,
    AIC = aic_val,
    R_Squared = r_squared,
    Model_Summary = model_summary
  )
  
  return(results)
}


evaluate_cSAR(fit_cSAR_F)
evaluate_cSAR(fit_cSAR_A)
evaluate_cSAR(fit_cSAR_O)





t0_geom <- read_csv("own datasets/cSARgeom_EBBA1_fag_faugo_sum.csv")
t1_geom <- read_csv("own datasets/cSARgeom_EBBA2_fag_faugo_sum.csv")

head(t0_geom)
head(t1_geom)

t0_long <- t0_geom %>%
  select(forest_sp, agriculture_sp, generalist_sp, Sp_Total) %>%
  mutate(Time = "T0")

t1_long <- t1_geom %>%
  select(forest_sp, agriculture_sp, generalist_sp, Sp_Total) %>%
  mutate(Time = "T1")

# Combine and pivot to long format
combined_long <- bind_rows(t0_long, t1_long) %>%
  pivot_longer(cols = -Time, names_to = "Species_Group", values_to = "Count")

# Create the boxplot
ggplot(combined_long, aes(x = Species_Group, y = Count, fill = Time)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparison of Species Groups Between T0 and T1",
    x = "Species Group",
    y = "Species Count"
  ) +
  scale_fill_brewer(palette = "Set2")



t0_geom <- read_csv("own datasets/cSARgeom_EBBA1_nhd_faugo_sum.csv")
t1_geom <- read_csv("own datasets/cSARgeom_EBBA2_nhd_faugo_sum.csv")

head(t0_geom)
head(t1_geom)

t0_long <- t0_geom %>%
  select(natural_sp, human_dominated_sp, Sp_Total) %>%
  mutate(Time = "T0")

t1_long <- t1_geom %>%
  select(natural_sp, human_dominated_sp, Sp_Total) %>%
  mutate(Time = "T1")

# Combine and pivot to long format
combined_long <- bind_rows(t0_long, t1_long) %>%
  pivot_longer(cols = -Time, names_to = "Species_Group", values_to = "Count")

# Create the boxplot
ggplot(combined_long, aes(x = Species_Group, y = Count, fill = Time)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparison of Species Groups Between T0 and T1",
    x = "Species Group",
    y = "Species Count"
  ) +
  scale_fill_brewer(palette = "Set2")


