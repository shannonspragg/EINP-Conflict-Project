
# Prep gHM Data ----------------------------------------------------


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

gHM <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif")

# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab_landcover))

st_crs(ab_landcover) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

temp.rast <- project(gHM, template.rast)

parkland.ghm.rsmple <- resample(gHM, template.rast)
parkland.ghm.crop <- crop(parkland.ghm.rsmple, template.rast)

parkland.v <- vect(parkland.reproj)

parkland.ghm.rast <- terra::mask(parkland.ghm.crop, parkland.v)

terra::writeRaster(parkland.ghm.rast, "data/processed/parkland_ghm.tif")

