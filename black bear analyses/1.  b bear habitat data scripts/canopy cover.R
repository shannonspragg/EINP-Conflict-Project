# Prep Canopy Cover Data ----------------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

global_canopy <- rast("data/original/p001r052_TC_2015_err.tif") #   NEED TO FIND RIGHT PORTION FOR AB

# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab_landcover))

st_crs(ab_landcover) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

temp.rast <- project(global_canopy, template.rast)

parkland.canopy.rsmple <- resample(global_canopy, temp.rast)
parkland.canopy.crop <- crop(parkland.canopy.rsmple, temp.rast)

parkland.v <- vect(parkland.reproj)

parkland.ghm.rast <- terra::mask(parkland.ghm.crop, parkland.v)

terra::writeRaster(parkland.ghm.rast, "data/processed/parkland_ghm.tif", overwrite=TRUE)

