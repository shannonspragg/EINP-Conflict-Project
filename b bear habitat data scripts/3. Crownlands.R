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
bhb.buf <- st_read("data/processed/bhb_10km.shp")

crown.reproj<- st_transform(ab_crownlands, st_crs(bhb.buf))

st_crs(crown.reproj) == st_crs(bhb.buf)

st_is_valid(bhb.buf)
st_is_valid(crown.reproj)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.v <- vect(bhb.buf)
crownland.v <- vect(crown.reproj)

bhb.crownland.crop <- crop(crownland.v, template.rast)

bhb.crownland.rast <- terra::rasterize(bhb.crownland.crop, template.rast, field = "NAME")
bhb.crown.rast <- terra::mask(bhb.crownland.rast, bhb.v)

terra::writeRaster(bhb.crown.rast, "data/processed/bhb_crownlands.tif", overwrite=TRUE)


