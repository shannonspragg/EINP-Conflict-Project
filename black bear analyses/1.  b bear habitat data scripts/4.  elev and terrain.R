
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

# Code aspect into categories: flat, N, NE, E, SE, S, SW, W, NW;
# 
# aspect.flat <- (aspect.bhb == -1)
# aspect.NW <- (aspect.bhb <= 22.5)
# 
# aspect.N <- aspect.bhb[0,22.5]
# aspect.NE <- (aspect.bhb == 22.5:67.5 )
# 
# #Reclassify the values into 7 groups
# #all values between 0 and 20 equal 1, etc.
# m <- c(-1, 0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
# 
# r <- as.factor(aspect.bhb)
# 
# ## Add a landcover column to the Raster Attribute Table
# rat <- levels(r)[[1]]
# rat[["aspect direction"]] <- c("flat","N", "NE","E", "SE", "S", "SW", "W", "NW")
# levels(r) <- rat
# 

# is.factor(aspect.bhb)
# direction <- data.frame(ID=1:9, cover=c("flat", "N", "NE", "E", "SE", "S", "SW", "W", "NW"))
# levels(aspect.bhb) <- direction
# aspect.f <- as.factor(aspect.bhb)
# labels(aspect.f) <- letters[1:length(labels(aspect.f))]


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


