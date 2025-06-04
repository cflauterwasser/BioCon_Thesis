# Master's Thesis Repository

## Table of Contents:

#### Habitat_Information.R

Processing species lists of European mammals and birds, adding information on suitable habitat types + ability to live in human-modified habitats  (Source: IUCN Red List).
Also classifying species into two sets of classes (forest/agriculture/generalist and natural/human-modified).

#### Occurence_Data.R

Pulling, processing and cleaning occurence data for mammals (GBIF) and birds (EBBA) in Europe.

#### Climate_Data.R

Processing WorldClim data for required time window to derive deltaT and CCV.

#### Species_Richness.R

Calculating SR and deltaSR for birds and mammals, also for forest/agriculture/generalist and natural/human-modified classes.
Also simple linear modelling.

#### Land_Use.R

Extracting land use data from CLC rasters, fitting it to EBBA grid used for SR- and climate analysis.
Testing cSAR application with sars package (currently not converging).
Quantifying land use change (unfinished).
Transforming data to fit Henrique's "cSAR geometry" script.

#### own datasets (folder)

Folder for output datasets of the different R-scripts in the repository.
Note that some files are not included tue to large size.

#### external datasets (folder) [hidden]

Folder for externally sourced input datasets for different analyses. Generally too large for the repository and therefore not uploaded.
