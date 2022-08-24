# Prep Crown Land Data ----------------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

ab_crownlands <- st_read("data/original/Crown_Reservations_2018Dec.shp")


# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab_crownlands))

st_crs(ab_crownlands) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

parkland.v <- vect(parkland.reproj)
crownland.v <- vect(ab_crownlands)

park.crownland.crop <- crop(crownland.v, template.rast)

parkland.crownland.rast <- terra::rasterize(park.crownland.crop, template.rast, field = "SUBTYPE")
parkland.crown.rast <- terra::mask(parkland.crownland.rast, parkland.v)

terra::writeRaster(parkland.crown.rast, "data/processed/parkland_crownlands.tif", overwrite=TRUE)


