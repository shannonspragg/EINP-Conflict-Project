
# Prep elevation,  slope,  aspect,  and terrain ruggedness ----------------


# Load Packages -----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Bring in Data -----------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
bhb.bound.v <- vect(bhb.50km.boundary)
temp.raster <- raster("data/processed/dist2pa_km_bhb.tif")

# Prep elevation data: ----------------------------------------------------
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.bhb.crop <- crop(elev.can, project(bhb.bound.v, elev.can)) #crop to bhb

elev.bhb.raster <- raster(elev.bhb.crop)
slope.bhb <- raster::terrain(elev.bhb.raster, opt = "slope", unit='degrees')
#aspect.bhb <- raster::terrain(elev.bhb.raster, opt = "aspect", unit='degrees')

elev.bhb.rast <- rast(elev.bhb.raster)
slope.bhb.rast <- rast(slope.bhb)

elev.rsmpl <- terra::resample(elev.bhb.rast, temp.rast)
slope.rsmpl  <- terra::resample(slope.bhb.rast, temp.rast)

elev.km <- measurements::conv_unit(elev.rsmpl, "m", "km")

# Prep terrain ruggedness: --------------------------------------------
rough <- terrain(elev.bhb.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- project(rough.rescale, temp.rast)


writeRaster(rough.proj, "data/processed/terrain_ruggedness_bhb.tif", overwrite=TRUE)
writeRaster(slope.rsmpl, "data/processed/slope_bhb.tif", overwrite=TRUE)
writeRaster(elev.rsmpl, "data/processed/elevation_bhb.tif", overwrite=TRUE)
writeRaster(elev.km, "data/processed/elevation_km_bhb.tif", overwrite=TRUE)


