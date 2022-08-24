# Prep NDVI Data ----------------------------------------------------


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

ndvi_can <- rast("data/original/AVHRRCOMP7d_2022.tif")

# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ndvi_can))

st_crs(ndvi_can) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

temp.rast <- project(ndvi_can, template.rast)

parkland.ndvi.rsmple <- resample(ndvi_can, template.rast)
parkland.ndvi.crop <- crop(parkland.ndvi.rsmple, template.rast)

parkland.v <- vect(parkland.reproj)

parkland.ndvi.rast <- terra::mask(parkland.ndvi.crop, parkland.v)

parkland.ndvi.rast <- parkland.ndvi.rast["AVH_9"] #select for the one with 24 julian weeks (highest), see: https://open.canada.ca/data/en/dataset/44ced2fa-afcc-47bd-b46e-8596a25e446e/resource/d1ea0134-4636-4f03-b723-eb1f4304c01c?inner_span=True#additional-info

terra::writeRaster(parkland.ndvi.rast, "data/processed/parkland_ndvi.tif", overwrite=TRUE)

