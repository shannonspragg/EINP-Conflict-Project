
# Prep Road Network Variable ----------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load Data -------------------------------------------------------------
ab.roads <- st_read("data/original/grnf048r10a_e.shp")
bhb.bound <- st_read("data/processed/bhb_10km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

ab.roads.reproj <- st_transform(ab.roads, st_crs(bhb.bound))

# Make Roads Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.roads.v <- vect(ab.roads.reproj)

bhb.roads.crop <- terra::crop(ab.roads.v, temp.rast)

# Dist to PA raster:
dist2roads <- terra::distance(temp.rast, ab.roads.v)
dist2roads.km <- measurements::conv_unit(dist2roads, "m", "km")
writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
#writeRaster(bhb.roads.crop, "data/processed/bhb_roads.tif", overwrite=TRUE)

