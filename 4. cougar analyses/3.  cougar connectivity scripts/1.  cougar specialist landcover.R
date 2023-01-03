
# Producing Specialist Species Layer - Cougars --------------------------------
## Combine land cover of forests, edge habitats, and wetland areas for ideal cougar habitat types 


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
forest  <- rast("data/processed/bhb_forest_land.tif")
edge.habitat <- rast("data/processed/bhb_shrubland.tif")
wetland <- rast("data/processed/bhb_water_areas.tif")

# Double check these are correct:
plot(forest)
plot(edge.habitat)
plot(wetland)

# Condense these into one raster: -----------------------------------------
specialist.cougar <- c(forest, edge.habitat, wetland)

specialist.cougar.rast <- sum(specialist.cougar, na.rm=TRUE)
plot(specialist.cougar.rast)



# Assign values for specialist selection: ---------------------------------
specialist.cougar.rast[specialist.cougar.rast == 2] <- 1 
specialist.cougar.rast[specialist.cougar.rast == 1] <- 900 
specialist.cougar.rast[specialist.cougar.rast == 0] <- 0.7 # increase this to represent cougar selection, not crazy specific but still high
specialist.cougar.rast[specialist.cougar.rast == 900] <- 0 

# Save resistance input:
writeRaster(specialist.cougar.rast, "data/processed/cougar_specialist_habitat_resist.tif")
