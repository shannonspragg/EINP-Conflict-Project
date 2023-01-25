
# Prep Road Network Variable ----------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(raster)

# Load Data -------------------------------------------------------------
ab.roads <- st_read("data/original/grnf048r10a_e.shp")
bhb.bound <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
temp.raster <- rast("data/processed/bhb_50km_template_rast.tif")
ab.roads.reproj <- st_transform(ab.roads, st_crs(bhb.bound))

# Filter roads by type for dist calc --------------------------------------
ab.roads.filt <- ab.roads.reproj %>% filter(ab.roads.reproj$TYPE == "FWY" | ab.roads.reproj$TYPE == "PASS" | ab.roads.reproj$TYPE == "HWY")

# Make Roads Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.roads.v <- vect(ab.roads.reproj)
ab.roads.filt.v <- vect(ab.roads.filt)

bhb.roads.crop <- terra::crop(ab.roads.v, temp.rast)
bhb.major.roads <- terra::crop(ab.roads.filt.v, temp.rast)

# Dist to roads raster:
dist2roads <- terra::distance(temp.rast, ab.roads.filt.v)
dist2roads.km <- measurements::conv_unit(dist2roads, "m", "km")

# Roads raster:
bhb.roads <- terra::rasterize(bhb.roads.crop, temp.rast, field = "RB_UID")
bhb.roads[bhb.roads >= 1] <- 1

bhb.roads.raster <- raster(bhb.roads)
bhb.roads.raster[is.na(bhb.roads.raster[])] <- 0 


writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.roads.raster, "data/processed/bhb_roads.tif", overwrite=TRUE)

roads.adjust <- bhb.roads.raster / 1
writeRaster(roads.adjust, "data/processed/bhb_roads_adjusted.tif")
