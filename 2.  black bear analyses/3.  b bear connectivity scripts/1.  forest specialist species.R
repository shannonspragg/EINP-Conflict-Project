
# Producing Specialist Species Layer - Bears --------------------------------
## Combine land cover of alpine, forest, and broadleaf as ideal habitat types for a forest specialist species (bears)


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
alpine <- rast("data/processed/bhb_alpine_mix.tif")
conifer <- rast("data/processed/bhb_conifer_mix.tif")
broadleaf <- rast("data/processed/bhb_broadleaf_mix.tif")

# Double check these are correct:
plot(alpine)
plot(conifer)
plot(broadleaf)


# Condense these into one raster: -----------------------------------------
specialist.bear <- c(alpine, conifer, broadleaf)

specialist.hab.rast <- sum(specialist.bear, na.rm=TRUE)
plot(specialist.hab.rast)


# Assign values for specialist selection: ---------------------------------
specialist.hab.rast[specialist.hab.rast == 1] <- 900 
specialist.hab.rast[specialist.hab.rast == 0] <- 0.9 # increase this to represent specialist selection
specialist.hab.rast[specialist.hab.rast == 900] <- 0 

# Save:
# NOTE: this is inverted so it is now a resistance layer, with 0 = no resistance and 0.9 = most resistance
writeRaster(specialist.hab.rast, "data/processed/forest_specialist_species_habitat.tif")
