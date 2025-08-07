# Debugging geom ebba SR mismatch.R
# Purpose: Debugging Isse of missmatching SR numbers from EBBA and Geom Calculations
# Inputs:
# Outputs:
# Author: Caspar Lauterwasser
# Date: 2025-08-01



library(tidyverse)
library(data.table)
library(sf)



# ___________________________________________________________________________
# Visual Comparison of SR of EBBA Grid and Geom Methods ####
# ___________________________________________________________________________


# ___________________________________________________________________________
## Importing data ####

nhd_nhd <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_nhd_nhd.csv")
fag_fao <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_fag_fao.csv")
fag_faugo <- read_csv("own datasets/cSARgeom_deltaEBBA_fixed_fag_faugo.csv")


SR_nhd <- read_csv("own datasets/SR_classes_NHD.csv")
SR_fag <- read_csv("own datasets/SR_classes_FAG.csv")

# geom_shp =
# ebba_grid =



# ___________________________________________________________________________
## Subsetting data ####

### NHD ####

# Subset the dataframe
# Define the target area and tolerance
# target_area <- 2500000000  # 50km x 50km in m²
# tolerance <- 0.1           # 10% tolerance

# nhd_subset <- nhd_nhd %>%
#  dplyr::filter(
#    Area_Total_t0 >= target_area * (1 - tolerance) &
#      Area_Total_t0 <= target_area * (1 + tolerance)
#  )

nhd_subset <- nhd_nhd[1:1804, ]
head(nhd_subset)
boxplot(nhd_subset$Area_Total_t0)
boxplot(nhd_subset$Area_Total_t1)

head(nhd_subset)


SR_nhd <- SR_nhd[, 1:7]
head(SR_nhd)

uniqueN(SR_nhd$cell50x50)
SR_nhd <- SR_nhd %>%
  filter(!if_all(-1, is.na))
uniqueN(SR_nhd$cell50x50)


### FAG ####

fag_subset <- fag_fao[1:1804, ]
head(fag_subset)
boxplot(fag_subset$Area_Total_t0)
boxplot(fag_subset$Area_Total_t1)

head(fag_subset)


SR_fag <- SR_fag[, 1:10]
head(SR_fag)

uniqueN(SR_fag$cell50x50)
SR_fag <- SR_fag %>%
  filter(!if_all(-1, is.na))
uniqueN(SR_fag$cell50x50)



# ___________________________________________________________________________
## Plotting ####

compare_boxplots <- function(vec1, vec2,
                             names = c("Group 1", "Group 2"),
                             colors = c("#1b9e77", "#d95f02"),
                             ylab = "Value",
                             plot_title = "Comparison of Distributions") {
  df <- data.frame(
    value = c(vec1, vec2),
    group = factor(rep(names, times = c(length(vec1), length(vec2))))
  )

  ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.colour = "black") +
    scale_fill_manual(values = colors) +
    labs(title = plot_title, x = "", y = ylab) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14)
    ) +
    theme_minimal(base_size = 20)
}



compare_histograms <- function(vec1, vec2,
                               names = c("Group 1", "Group 2"),
                               colors = c("#1b9e77", "#d95f02"),
                               xlab = "Value",
                               bins = 30,
                               plot_title = "Comparison of Distributions") {
  df <- data.frame(
    value = c(vec1, vec2),
    group = factor(rep(names, times = c(length(vec1), length(vec2))))
  )

  stats <- df %>%
    group_by(group) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE)
    )

  ggplot(df, aes(x = value, fill = group)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = bins, color = "black") +
    geom_vline(data = stats, aes(xintercept = mean, color = group), linetype = "dashed", size = 1) +
    geom_vline(data = stats, aes(xintercept = median, color = group), linetype = "solid", size = 1) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(title = plot_title, x = xlab, y = "Count", fill = "", color = "Stat lines:") +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 14)
    ) +
    theme_minimal(base_size = 20)
}



# ___________________________________________________________________________
### NHD ####

#### Species Richness ####

plot <- compare_boxplots(
  nhd_subset$natural_sp_t0,
  SR_nhd$SR_natural_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR natural t0"
)
plot
ggsave("plots/species richness/debug_SR_boxp natural t0.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_boxplots(
  nhd_subset$natural_sp_t1,
  SR_nhd$SR_natural_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR natural t1"
)
plot
ggsave("plots/species richness/debug_SR_boxp natural t1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_boxplots(
  nhd_subset$human_dominated_sp_t0,
  SR_nhd$SR_human_dominated_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR human dominated t0"
)
plot
ggsave("plots/species richness/debug_SR_boxp human dominated t0.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_boxplots(
  nhd_subset$human_dominated_sp_t1,
  SR_nhd$SR_human_dominated_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR human dominated t1"
)
plot
ggsave("plots/species richness/debug_SR_boxp human dominated t1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot <- plot <- compare_histograms(
  nhd_subset$natural_sp_t0,
  SR_nhd$SR_natural_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR natural t0"
)
plot
ggsave("plots/species richness/debug_SR_hist natural t0.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_histograms(
  nhd_subset$natural_sp_t1,
  SR_nhd$SR_natural_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR natural t1"
)
plot
ggsave("plots/species richness/debug_SR_hist natural t1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_histograms(
  nhd_subset$human_dominated_sp_t0,
  SR_nhd$SR_human_dominated_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR human dominated t0"
)
plot
ggsave("plots/species richness/debug_SR_hist human dominated t0.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_histograms(
  nhd_subset$human_dominated_sp_t1,
  SR_nhd$SR_human_dominated_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR human dominated t1"
)
plot
ggsave("plots/species richness/debug_SR_hist human dominated t1.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



#### Delta Species Richness ####

plot <- compare_boxplots(
  nhd_subset$delta_natural_sp,
  SR_nhd$deltaSR_bird_natural,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Δ Species Richness",
  plot_title = "ΔSR natural"
)
plot
ggsave("plots/species richness/debug_deltaSR_boxp natural.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_boxplots(
  nhd_subset$delta_human_dominated_sp,
  SR_nhd$deltaSR_bird_human_dominated,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Δ Species Richness",
  plot_title = "ΔSR human dominated"
)
plot
ggsave("plots/species richness/debug_deltaSR_boxp human dominated.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")


plot <- compare_histograms(
  nhd_subset$delta_natural_sp,
  SR_nhd$deltaSR_bird_natural,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Δ Species Richness",
  plot_title = "ΔSR natural"
)
plot
ggsave("plots/species richness/debug_deltaSR_hist natural.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")

plot <- compare_histograms(
  nhd_subset$delta_human_dominated_sp,
  SR_nhd$deltaSR_bird_human_dominated,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Δ Species Richness",
  plot_title = "ΔSR human dominated"
)
plot
ggsave("plots/species richness/debug_deltaSR_hist human dominated.png", plot = plot, width = 10, height = 8, dpi = 300, bg = "white")



# ___________________________________________________________________________
### FAG ####

#### Species Richness ####

compare_boxplots(
  fag_subset$forest_sp_t0,
  SR_fag$SR_forest_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR forest t0"
)
compare_boxplots(
  fag_subset$forest_sp_t1,
  SR_fag$SR_forest_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR forest t1"
)

compare_boxplots(
  fag_subset$agriculture_sp_t0,
  SR_fag$SR_agriculture_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR agriculture t0"
)
compare_boxplots(
  fag_subset$agriculture_sp_t1,
  SR_fag$SR_agriculture_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR agriculture t1"
)

compare_boxplots(
  fag_subset$generalist_sp_t0,
  SR_fag$SR_generalist_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR generalist t0"
)
compare_boxplots(
  fag_subset$generalist_sp_t1,
  SR_fag$SR_generalist_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Species Richness",
  plot_title = "SR generalist t1"
)


compare_histograms(
  fag_subset$forest_sp_t0,
  SR_fag$SR_forest_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR forest t0"
)
compare_histograms(
  fag_subset$forest_sp_t1,
  SR_fag$SR_forest_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR forest t1"
)

compare_histograms(
  fag_subset$agriculture_sp_t0,
  SR_fag$SR_agriculture_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR agriculture t0"
)
compare_histograms(
  fag_subset$agriculture_sp_t1,
  SR_fag$SR_agriculture_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR agriculture t1"
)

compare_histograms(
  fag_subset$generalist_sp_t0,
  SR_fag$SR_generalist_ebba1,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR generalist t0"
)
compare_histograms(
  fag_subset$generalist_sp_t1,
  SR_fag$SR_generalist_ebba2,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR generalist t1"
)



#### Delta Species Richness ####

compare_boxplots(
  fag_subset$delta_forest_sp,
  SR_fag$deltaSR_bird_forest,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Δ Species Richness",
  plot_title = "ΔSR forest"
)
compare_boxplots(
  fag_subset$delta_agriculture_sp,
  SR_fag$deltaSR_bird_agriculture,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Δ Species Richness",
  plot_title = "ΔSR agriculture"
)
compare_boxplots(
  fag_subset$delta_generalist_sp,
  SR_fag$deltaSR_bird_generalist,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  ylab = "Δ Species Richness",
  plot_title = "ΔSR generalist"
)


compare_histograms(
  fag_subset$delta_forest_sp,
  SR_fag$deltaSR_bird_forest,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Δ Species Richness",
  plot_title = "ΔSR forest"
)
compare_histograms(
  fag_subset$delta_agriculture_sp,
  SR_fag$deltaSR_bird_agriculture,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Δ Species Richness",
  plot_title = "ΔSR agriculture"
)
compare_histograms(
  fag_subset$delta_generalist_sp,
  SR_fag$deltaSR_bird_generalist,
  names = c("geom SR", "EBBA Grid SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Δ Species Richness",
  plot_title = "ΔSR generalist"
)

# CONCLUSION: Heavy misalignment! Investigate how both methods of calculating richness by subgroup deviate from each other!



# ___________________________________________________________________________
# Step-by-Step Comparison of both Methods ####
# ___________________________________________________________________________

ebba1 <- read_csv("own datasets/ebba1_cleaned.csv")
ebba2 <- read_csv("own datasets/ebba2_cleaned.csv")

ebba1_matrix_change <- read_csv("own datasets/ebba1_matrix_centroids.csv")
ebba2_matrix_change <- read_csv("own datasets/ebba2_matrix_centroids.csv")


# ___________________________________________________________________________
## Manual SR calculation from both raw data sets ####

ncol(ebba1_matrix_change) - 3
ncol(ebba2_matrix_change) - 3

nrow(ebba1_matrix_change)
nrow(ebba2_matrix_change)


testSR1 <- cbind(
  ebba1_matrix_change[, 1:3],
  SR = rowSums(ebba1_matrix_change[, 4:ncol(ebba1_matrix_change)])
)
testSR2 <- cbind(
  ebba2_matrix_change[, 1:3],
  SR = rowSums(ebba2_matrix_change[, 4:ncol(ebba1_matrix_change)])
)


head(testSR1)
head(testSR2)

testSR_combined <- testSR1 %>%
  rename(SR1 = SR) %>%
  left_join(testSR2 %>% rename(SR2 = SR),
    by = c("location", "lon", "lat")
  ) %>%
  mutate(deltaSR = SR2 - SR1)
head(testSR_combined)

oldSR <- read_csv("own datasets/deltaSR_birds_50km_change.csv")
head(oldSR)


# First, make sure the join key has the same name
oldSR <- oldSR %>% rename(location = cell50x50)

# Merge both dataframes by 'location'
merged_df <- left_join(oldSR, testSR_combined, by = "location")

# Rename the testSR_combined columns to have "matrix_" prefix
colnames(merged_df)[colnames(merged_df) %in% c("lon", "lat", "SR1", "SR2", "deltaSR")] <-
  paste0("matrix_", colnames(merged_df)[colnames(merged_df) %in% c("lon", "lat", "SR1", "SR2", "deltaSR")])

# Reorder columns: stick matrix_ columns right after their counterparts
merged_df <- merged_df %>%
  relocate(matrix_lon, .after = location) %>%
  relocate(matrix_lat, .after = matrix_lon) %>%
  relocate(matrix_SR1, .after = SR_ebba1) %>%
  relocate(matrix_SR2, .after = SR_ebba2) %>%
  relocate(matrix_deltaSR, .after = delta_SR) %>%
  dplyr::select(-occ_ebba1, -occ_ebba2)


change_grid_50km <- st_read("external datasets/EBBA/ebba2_grid50x50_v1/ebba_grid50x50_change_selfmade_NoIslandsNoEast.shp")
ebba_change_cells <- change_grid_50km$cell50x50

ebba_change <- function(df, cell_column) {
  df_change <- df %>%
    filter({{ cell_column }} %in% ebba_change_cells)
}


a <- ebba_change(merged_df, location)
head(a)

nrow(merged_df)
nrow(a)

a_diff <- a %>%
  mutate(
    diff_SR1       = SR_ebba1 - matrix_SR1,
    diff_SR2       = SR_ebba2 - matrix_SR2,
    diff_deltaSR   = delta_SR - matrix_deltaSR
  )

a_diff_nonzero <- a_diff %>%
  filter(diff_SR1 != 0 | diff_SR2 != 0 | diff_deltaSR != 0)
a_diff_nonzero

a_long <- a_diff %>%
  dplyr::select(location, starts_with("diff_")) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "comparison",
    values_to = "difference"
  )

ggplot(a_long, aes(x = comparison, y = difference)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Difference between EBBA and Matrix Estimates",
    x = "Comparison",
    y = "Difference in Species Richness"
  ) +
  theme_minimal()

compare_histograms(
  a$SR_ebba1,
  a$matrix_SR1,
  names = c("geom SR", "matrix SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR ebba1"
)

compare_histograms(
  a$SR_ebba2,
  a$matrix_SR2,
  names = c("geom SR", "matrix SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "SR ebba2"
)

compare_histograms(
  a$delta_SR,
  a$matrix_deltaSR,
  names = c("geom SR", "matrix SR"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = "delta SR"
)

# CONCLUSION: Slightly misaligned. Flaws in the transformation into the matrix format, or in my SR calculations?



# ___________________________________________________________________________
## comparing total species number in both OUTPUT datasets ####

head(nhd_subset) # output of summarize_samples()
head(SR_nhd) # output of SR()

geom_total_sp <- nhd_subset %>%
  dplyr::select(Sp_Total_t0, Sp_Total_t1)

ebba_total_sp <- SR_nhd %>%
  mutate(Sp_Total_t0 = SR_natural_ebba1 + SR_human_dominated_ebba1) %>%
  mutate(Sp_Total_t1 = SR_natural_ebba2 + SR_human_dominated_ebba2)

head(geom_total_sp)
head(ebba_total_sp)

summary(geom_total_sp$Sp_Total_t0)
summary(ebba_total_sp$Sp_Total_t0)

compare_histograms(
  ebba_total_sp$Sp_Total_t0,
  geom_total_sp$Sp_Total_t0,
  names = c("ebba total sp", "geom total sp"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = ""
)

compare_histograms(
  ebba_total_sp$Sp_Total_t1,
  geom_total_sp$Sp_Total_t1,
  names = c("ebba total sp", "geom total sp"),
  colors = c("#4daf4a", "#377eb8"),
  xlab = "Species Richness",
  plot_title = ""
)


# CONCLUSION: Identical! That implies that the flaw is within species classification! Possibly in the summarize_samples() function of the geom script?
# Addition: But how does this differ from SR EBBA1 and SR EBBA2 comparison?? Classification again? Does the matrix conversion have a flaw, or my SR() function?



# ___________________________________________________________________________
## Specialization Classification ####

### Original Dataset ####

specializations <- read_csv("own datasets/Habitat_Classes_Complete.csv")

# Read specialization list
specializations_nhd <- specializations %>%
  dplyr::select(1:4) %>%
  filter(Class == "Aves") %>%
  rename(Specialization = specialization_NHD) %>%
  dplyr::select(Species, Specialization)

head(specializations_nhd)
specializations_nhd$Specialization %>%
  as.factor() %>%
  summary()


specializations_fag <- specializations %>%
  dplyr::select(1:4) %>%
  filter(Class == "Aves") %>%
  rename(Specialization = specialization_FAG) %>%
  dplyr::select(Species, Specialization)

head(specializations_fag)
specializations_fag$Specialization %>%
  as.factor() %>%
  summary()



### Geom ####

binary_classification <- function(matrix_df) {
  # Clean column names
  colnames(matrix_df) <- str_replace_all(colnames(matrix_df), "\\.", " ")

  # Load specialization data
  specializations <- read_csv("own datasets/Habitat_Classes_Complete.csv") %>%
    dplyr::select(1:4) %>% # Make sure this includes Species, specialization_NHD, specialization_FAG
    filter(Class == "Aves") %>%
    dplyr::select(Species, specialization_NHD, specialization_FAG)

  # Pivot both specialization columns into one tidy column
  long <- specializations %>%
    pivot_longer(
      cols = starts_with("specialization"),
      names_to = "type",
      values_to = "specialization"
    ) %>%
    filter(!is.na(specialization))

  # Convert to wide binary classification matrix
  classif <- long %>%
    mutate(value = 1) %>%
    pivot_wider(
      names_from = specialization,
      values_from = value,
      values_fill = 0
    )

  # Add grouped binary categories
  classif <- classif %>%
    group_by(Species) %>%
    summarise(
      forest_sp = max(forest, na.rm = TRUE),
      agriculture_sp = max(agriculture, na.rm = TRUE),
      generalist_sp = max(generalist, na.rm = TRUE),
      natural_sp = max(natural, na.rm = TRUE),
      human_dominated_sp = max(human_dominated, na.rm = TRUE)
    )

  # Filter only species that are in your matrix
  filtered_classif <- classif %>%
    filter(Species %in% colnames(matrix_df)[4:ncol(matrix_df)]) %>%
    dplyr::distinct(Species, .keep_all = TRUE)

  # Create separate output tables
  classif_fag <- filtered_classif %>%
    dplyr::select(Species, forest_sp, agriculture_sp, generalist_sp)

  classif_nhd <- filtered_classif %>%
    dplyr::select(Species, natural_sp, human_dominated_sp)

  return(list(fag = classif_fag, nhd = classif_nhd))
}

ebba1_classif <- binary_classification(ebba1_matrix_change)
ebba2_classif <- binary_classification(ebba2_matrix_change)

ebba1_classif$fag %>%
  mutate(test_sum = forest_sp + agriculture_sp + generalist_sp) %>%
  filter(test_sum == 0)

ebba1_classif$nhd %>%
  mutate(test_sum = natural_sp + human_dominated_sp) %>%
  filter(test_sum == 0)



ncol(ebba1_matrix_change) - 3
uniqueN(ebba1_classif$fag$Species)
uniqueN(ebba1_classif$nhd$Species)

ncol(ebba2_matrix_change) - 3
uniqueN(ebba2_classif$fag$Species)
uniqueN(ebba2_classif$nhd$Species)



# __________________________________________
# debugging
matrix_sp <- ebba1_matrix_change %>%
  dplyr::select(-location, -lon, -lat) %>%
  colnames()
classif_sp <- ebba1_classif$nhd$Species

intersect(matrix_sp, classif_sp) %>%
  length()

setdiff(matrix_sp, classif_sp) # in matrix_sp but not in classif_sp
setdiff(classif_sp, matrix_sp) # in classif_sp but not in matrix_sp

union(
  setdiff(matrix_sp, classif_sp),
  setdiff(classif_sp, matrix_sp)
)
# __________________________________________



# write_csv(ebba1_classif$fag, "own datasets/aves_classification_EBBA1_fag.csv")
# write_csv(ebba1_classif$nhd, "own datasets/aves_classification_EBBA1_nhd.csv")

# write_csv(ebba2_classif$fag, "own datasets/aves_classification_EBBA2_fag.csv")
# write_csv(ebba2_classif$nhd, "own datasets/aves_classification_EBBA2_nhd.csv")



### Comparison ####

head(ebba1_classif$nhd)
head(ebba2_classif$nhd)

head(specializations_nhd)


head(ebba1_classif$fag)
head(ebba2_classif$fag)

head(specializations_fag)


combined_nhd <- bind_rows(ebba1_classif$nhd, ebba2_classif$nhd) %>%
  distinct(Species, .keep_all = TRUE)

combined_fag <- bind_rows(ebba1_classif$fag, ebba2_classif$fag) %>%
  distinct(Species, .keep_all = TRUE)


nhd_long <- combined_nhd %>%
  pivot_longer(cols = -Species, names_to = "Specialization", values_to = "value") %>%
  filter(value == 1) %>%
  mutate(Specialization = gsub("_sp$", "", Specialization)) %>%
  dplyr::select(Species, Specialization)

fag_long <- combined_fag %>%
  pivot_longer(cols = -Species, names_to = "Specialization", values_to = "value") %>%
  filter(value == 1) %>%
  mutate(Specialization = gsub("_sp$", "", Specialization)) %>%
  dplyr::select(Species, Specialization)


# Check matches
all_equal(nhd_long, specializations_nhd) # for nhd
all_equal(fag_long, specializations_fag) # for fag

# Or find the mismatches
anti_join(nhd_long, specializations_nhd)
anti_join(specializations_nhd, nhd_long)

anti_join(fag_long, specializations_fag)
anti_join(specializations_fag, fag_long)


duplicated_species <- combined_nhd %>%
  count(Species) %>%
  filter(n > 1)

print(duplicated_species)

# CONCLUSION: Looks good now, after the changes in the function. But this is only comparing the original classification file with the result of binary_classification() function. Should also compare that with the filter_by_specialization() function output and extract_species_positions() output.



### EBBA Grid ####

specializations <- read_csv("own datasets/Habitat_Classes_Complete.csv")

filter_by_specialization <- function(df, df_name, specialization_list, category) {
  df_filtered <- df %>%
    inner_join(filter(specialization_list, specialization == category), by = "Species") # Keep only matching species
  assign(paste0(category, "_", df_name), df_filtered, envir = .GlobalEnv) # Save dataset with new name
}

# Read specialization list
specializations <- specializations %>%
  dplyr::select(Species, specialization_NHD) %>%
  rename(specialization = specialization_NHD)
head(specializations)


# Process birds
bird_datasets <- list(ebba1 = ebba1, ebba2 = ebba2)
for (df_name in names(bird_datasets)) {
  df <- bird_datasets[[df_name]]
  filter_by_specialization(df, df_name, specializations, "natural")
  filter_by_specialization(df, df_name, specializations, "human_dominated")
}

# Check results
ls(pattern = "^(natural|human_dominated)_")

head(human_dominated_ebba1)
head(human_dominated_ebba2)
head(natural_ebba1)
head(natural_ebba2)

classif_ebba_grid <- bind_rows(
  human_dominated_ebba1,
  human_dominated_ebba2,
  natural_ebba1,
  natural_ebba2
) %>%
  distinct(Species, specialization) %>%
  rename(Specialization = specialization)
classif_ebba_grid

# Check matches
all_equal(classif_ebba_grid, specializations_nhd) # with original file
all_equal(nhd_long, classif_ebba_grid) # with geom file

# find mismatches
anti_join(classif_ebba_grid, specializations_nhd) # with original file
anti_join(nhd_long, classif_ebba_grid) # with geom file


# CONCLUSION: Until here everything matches perfectly! Flaw has to be either in SR() and/or delta_SR(), or in extract_species_positions() and/or summarize_samples().



### Testing extract_species_positions() output ####

filter_points_in_ready_clusters <- function(points_sf, given_chulls) {
  npoints <- nrow(points_sf)
  points_within_clusters <- list()

  for (key in names(given_chulls))
  {
    chulls_this_size <- given_chulls[[key]]
    n_clusters <- length(chulls_this_size)

    if (n_clusters == npoints) {
      points_in_clusters <- split(points_sf, 1:npoints)
    } else {
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


extract_species_positions <- function(species_habitat_matrix, species_site_matrix) {
  # Get habitat names
  habitat_names <- colnames(species_habitat_matrix[, -1])

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



# for EBBA1

data <- read_csv("own datasets/ebba1_matrix_centroids.csv")
points <- st_as_sf(data, coords = c("lon", "lat"), crs = 3035)

polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")

pt_in_clusters <- filter_points_in_ready_clusters(points, polygon_templates)

classif <- read_csv("own datasets/aves_classification_EBBA1_nhd.csv")

species_groups <- extract_species_positions(classif, points[, -1])

classif
points
species_groups

sp_positions <- points %>%
  dplyr::select(-location) %>%
  colnames()
# sp_positions
# species_groups

# Create a vector of NA values the same length as sp_positions
classification <- rep(NA, length(sp_positions))

# Assign classifications based on the positions
classification[species_groups$natural_sp] <- "natural"
classification[species_groups$human_dominated_sp] <- "human_dominated"

# Combine into a data frame
classified_species_ebba1 <- data.frame(
  species = sp_positions,
  classification = classification
)

classified_species_ebba1 <- classified_species_ebba1 %>%
  rename(Species = species) %>%
  rename(Specialization = classification)


# for EBBA2

data <- read_csv("own datasets/ebba2_matrix_centroids.csv")
points <- st_as_sf(data, coords = c("lon", "lat"), crs = 3035)

polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")

pt_in_clusters <- filter_points_in_ready_clusters(points, polygon_templates)

classif <- read_csv("own datasets/aves_classification_EBBA2_nhd.csv")

species_groups <- extract_species_positions(classif, points[, -1])

sp_positions <- points %>%
  dplyr::select(-location) %>%
  colnames()
sp_positions
species_groups

# Create a vector of NA values the same length as sp_positions
classification <- rep(NA, length(sp_positions))

# Assign classifications based on the positions
classification[species_groups$natural_sp] <- "natural"
classification[species_groups$human_dominated_sp] <- "human_dominated"

# Combine into a data frame
classified_species_ebba2 <- data.frame(
  species = sp_positions,
  classification = classification
)

classified_species_ebba2 <- classified_species_ebba2 %>%
  rename(Species = species) %>%
  rename(Specialization = classification)


head(classified_species_ebba1)
nrow(classified_species_ebba1)
head(classified_species_ebba2)
nrow(classified_species_ebba2)


# merge
classified_species <- bind_rows(
  classified_species_ebba1,
  classified_species_ebba2
) %>%
  distinct(Species, .keep_all = TRUE) %>%
  filter(Species != "geometry")

head(classified_species)
nrow(classified_species)
uniqueN(classified_species$Species)


# comparison

# Check matches
all_equal(specializations_nhd, classified_species) # with original file
all_equal(nhd_long, classified_species) # with former geom file
all_equal(classif_ebba_grid, classified_species) # with ebba grid file

# find mismatches
anti_join(specializations_nhd, classified_species) # with original file
anti_join(nhd_long, classified_species) # with former geom file
anti_join(classif_ebba_grid, classified_species) # with ebba grid file

anti_join(specializations_nhd, classified_species) %>%
  nrow()
anti_join(nhd_long, classified_species) %>%
  nrow()
anti_join(classif_ebba_grid, classified_species) %>%
  nrow()

# CONCLUSION: Chaos! Problem seems to occur within this function! Could it be related to fixing/saving the geometry once generated with ebba1? Be sure to double check that "back-translation" of position-classification information (of "species_groups") is flawless, otherwise test itself nonsensical.





#### AI-suggested Alternative ####

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



# for EBBA1

data <- read_csv("own datasets/ebba1_matrix_centroids.csv")
points <- st_as_sf(data, coords = c("lon", "lat"), crs = 3035)

polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")

pt_in_clusters <- filter_points_in_ready_clusters(points, polygon_templates)

classif <- read_csv("own datasets/aves_classification_EBBA1_nhd.csv")

species_groups <- extract_species_indices(classif, points[, -1])

classif
points
species_groups

sp_positions <- points %>%
  dplyr::select(-location) %>%
  colnames()
# sp_positions
# species_groups

# Create a vector of NA values the same length as sp_positions
classification <- rep(NA, length(sp_positions))

# Assign classifications based on the positions
classification[species_groups$natural_sp] <- "natural"
classification[species_groups$human_dominated_sp] <- "human_dominated"

# Combine into a data frame
classified_species_ebba1 <- data.frame(
  species = sp_positions,
  classification = classification
)

classified_species_ebba1 <- classified_species_ebba1 %>%
  rename(Species = species, Specialization = classification)


# for EBBA2

data <- read_csv("own datasets/ebba2_matrix_centroids.csv")
points <- st_as_sf(data, coords = c("lon", "lat"), crs = 3035)

polygon_templates <- readRDS("own datasets/fixed_cluster_polygons_EBBA1.rds")

pt_in_clusters <- filter_points_in_ready_clusters(points, polygon_templates)

classif <- read_csv("own datasets/aves_classification_EBBA2_nhd.csv")

species_groups <- extract_species_indices(classif, points[, -1])

sp_positions <- points %>%
  dplyr::select(-location) %>%
  colnames()
# sp_positions
# species_groups

# Create a vector of NA values the same length as sp_positions
classification <- rep(NA, length(sp_positions))

# Assign classifications based on the positions
classification[species_groups$natural_sp] <- "natural"
classification[species_groups$human_dominated_sp] <- "human_dominated"

# Combine into a data frame
classified_species_ebba2 <- data.frame(
  species = sp_positions,
  classification = classification
)

classified_species_ebba2 <- classified_species_ebba2 %>%
  rename(Species = species, Specialization = classification)


head(classified_species_ebba1)
nrow(classified_species_ebba1)
head(classified_species_ebba2)
nrow(classified_species_ebba2)


# merge
classified_species <- bind_rows(
  classified_species_ebba1,
  classified_species_ebba2
) %>%
  distinct(Species, .keep_all = TRUE) %>%
  filter(Species != "geometry")



# List of reference datasets to compare against
references <- list(
  original = specializations_nhd,
  former_geom = nhd_long,
  ebba_grid = classif_ebba_grid
)

# Check for full equality
lapply(references, function(ref) all_equal(ref, classified_species))

# Show mismatches
lapply(references, function(ref) anti_join(ref, classified_species))

# Optional: Count mismatches
sapply(references, function(ref) nrow(anti_join(ref, classified_species)))


install.packages("styler")

# Load styler
library(styler)

# Style a script file and overwrite it (be careful!)
styler::style_dir()
# Or style your current script interactively in RStudio with:
styler::style_active_file()
