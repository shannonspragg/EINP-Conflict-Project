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
bhb.buf <- st_read("data/processed/bhb_50km.shp")

bhb.reproj<- st_transform(bhb.buf, st_crs(ndvi_can))

st_crs(ndvi_can) == st_crs(bhb.reproj)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

temp.rast <- project(ndvi_can, template.rast)

bhb.ndvi.rsmple <- resample(ndvi_can, template.rast)
bhb.ndvi.crop <- crop(bhb.ndvi.rsmple, template.rast)

bhb.v <- vect(bhb.reproj)

bhb.ndvi.rast <- terra::mask(bhb.ndvi.crop,bhb.v)

bhb.ndvi.rast <- bhb.ndvi.rast["AVH_9"] #select for the one with 24 julian weeks (highest), see: https://open.canada.ca/data/en/dataset/44ced2fa-afcc-47bd-b46e-8596a25e446e/resource/d1ea0134-4636-4f03-b723-eb1f4304c01c?inner_span=True#additional-info

terra::writeRaster(bhb.ndvi.rast, "data/processed/bhb_ndvi.tif", overwrite=TRUE)

