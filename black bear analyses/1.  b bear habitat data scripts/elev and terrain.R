
# Prep elevation,  slope,  aspect,  and terrain ruggedness ----------------


# Load Packages -----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Bring in Data -----------------------------------------------------------
bhb.10km.boundary <- st_read("data/processed/bhb_10km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
bhb.bound.v <- vect(bhb.10km.boundary)

# Prep elevation data: ----------------------------------------------------
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.bhb.crop <- crop(elev.can, project(bhb.bound.v, elev.can)) #crop to bhb

elev.bhb.raster <- raster(elev.bhb.crop)
slope.bhb <- raster::terrain(elev.bhb.raster, opt = "slope", unit='degrees')
aspect.bhb <- raster::terrain(elev.bhb.raster, opt = "aspect", unit='degrees')

# Code aspect into categories: flat, N, NE, E, SE, S, SW, W, NW;
aspect.flat <- (aspect.bhb == -1)
aspect.N <- (aspect.bhb < 22.5)

# is.factor(aspect.bhb)
# direction <- data.frame(ID=1:9, cover=c("flat", "N", "NE", "E", "SE", "S", "SW", "W", "NW"))
# levels(aspect.bhb) <- direction
# aspect.f <- as.factor(aspect.bhb)
# labels(aspect.f) <- letters[1:length(labels(aspect.f))]


# Prep terrain ruggedness: --------------------------------------------
rough <- terrain(elev.can.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- project(rough.rescale, bhs)

