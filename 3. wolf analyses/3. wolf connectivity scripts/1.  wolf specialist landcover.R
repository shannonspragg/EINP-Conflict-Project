
# Producing Specialist Species Layer - Wolves --------------------------------
## Combine land cover of alpine, forest, and broadleaf with XXX for ideal wolf habitat types 


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
forest  <- rast("data/processed/bhb_forest_land.tif")
shrub <- rast("data/processed/bhb_shrubland.tif")

# Double check these are correct:
plot(forest)
plot(shrub)


# Condense these into one raster: -----------------------------------------
specialist.wolf <- c(forest, shrub)

specialist.wolf.rast <- sum(specialist.wolf, na.rm=TRUE)
plot(specialist.wolf.rast)



# Assign values for resistance to specialist selection: ---------------------------------
specialist.wolf.rast[specialist.wolf.rast == 1] <- 900 
specialist.wolf.rast[specialist.wolf.rast == 0] <- 0.7 # increase this to represent wolf selection, not crazy specific but still high
specialist.wolf.rast[specialist.wolf.rast == 900] <- 0 

plot(specialist.wolf.rast)

# Save:
# NOTE: this is inverted so it is now a resistance layer, with 0 = no resistance and 0.7 = most resistance
writeRaster(specialist.wolf.rast, "data/processed/wolf_specialist_habitat.tif")
