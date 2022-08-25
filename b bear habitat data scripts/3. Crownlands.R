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
bio.buf <- st_read("data/processed/biosphere_50km.shp")

# bio.reproj<- st_transform(bio.buf, st_crs(ab_crownlands))
# 
# st_crs(ab_crownlands) == st_crs(bio.reproj)
# st_make_valid(bio.reproj)
st_make_valid(ab_crownlands)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_biosphere.tif")

bio.v <- vect(bio.buf)
crownland.v <- vect(ab_crownlands)

bio.crownland.crop <- crop(crownland.v, template.rast)

bio.crownland.rast <- terra::rasterize(bio.crownland.crop, template.rast, field = "SUBTYPE")
bio.crown.rast <- terra::mask(bio.crownland.rast, bio.v)

terra::writeRaster(bio.crown.rast, "data/processed/biosphere_crownlands.tif", overwrite=TRUE)


