
# Calculating edge habitats -------------------------------------------------------
  # Here we make a predictor raster representing forest edge habitats across our study area


# Load Packages -----------------------------------------------------------
library(sf)
library(terra)
library(raster)
library(dplyr)
library(tidyverse)


# Bring in data: ----------------------------------------------------------
forests <- rast("data/processed/bhb_forest_land.tif")


# Calculate edge habitat: -------------------------------------------------

forest.edge <- terra::boundaries(forests, classes = TRUE)



# Save --------------------------------------------------------------------

terra::writeRaster(forest.edge, "data/processed/forest_edge_habitats.tif")
