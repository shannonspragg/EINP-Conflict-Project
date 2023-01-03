
# Producing Generalist Focal Species Layer --------------------------------
  ## Combine land cover of alpine, forest, grassland, shrubland as ideal habitat types for generalist focal species


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
alpine <- rast("data/processed/bhb_alpine_mix.tif")
conifer <- rast("data/processed/bhb_conifer_mix.tif")
broadleaf <- rast("data/processed/bhb_broadleaf_mix.tif")
shrub <- rast("data/processed/bhb_shrubland.tif")

# Double check these are correct:
plot(alpine)
plot(conifer)
plot(broadleaf)
plot(shrub)


# Condense these into one raster: -----------------------------------------
general.focal <- c(alpine, conifer, broadleaf, shrub)

general.focal.hab.rast <- sum(general.focal, na.rm=TRUE)
plot(general.focal.hab.rast)

# Save:
writeRaster(general.focal.hab.rast, "data/processed/general_focal_species_habitat.tif")
