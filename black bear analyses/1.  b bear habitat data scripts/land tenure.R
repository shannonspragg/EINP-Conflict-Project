
# Prep Land Tenure Covariate ----------------------------------------------------
# We need to add together our protected area and crown lands (and private lands?) for our area


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)
library(raster)

# Load tenure data -------------------------------------------------------------

park.pas <- rast("data/processed/parkland_protected_areas.tif")

park.crowns <- rast("data/processed/parkland_crownlands.tif")

# Stack these into one raster:
#pa.crown.stack <- raster::stack(park.pas, park.crowns)

pa.crown.stack <- terra::merge(park.pas, park.crowns) #Need these to be in categories somehow
