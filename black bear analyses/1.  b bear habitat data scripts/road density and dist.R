
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

ab.roads.reproj <- st_transform(ab.roads, st_crs(bhb.bound))

# Make Roads Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.roads.v <- vect(ab.roads.reproj)

bhb.roads.crop <- terra::crop(ab.roads.v, temp.rast)

# Dist to roads raster:
dist2roads <- terra::distance(temp.rast, ab.roads.v)
dist2roads.km <- measurements::conv_unit(dist2roads, "m", "km")

# Road density at 4km raster:
# bhb.roads <- intersect(ab.roads.reproj, bhb.bound)
# bhb.road.v <- vect(bhb.roads)
# 
# bhb.roads.crop$length <- perim(bhb.road.v) / 4000 #km
# road.dens <- tapply(bhb.roads.crop$length, bhb.roads.crop$RB_UID, sum)
# 
# road.dens.rast <- rast(road.dens)
# bhb.roads.crop[as.integer(names(bhb.roads.crop))] <- as.vector(road.dens)

# OR THIS:
temp.raster <- raster(temp.rast)

roads.crop <- st_crop(ab.roads.reproj, c(xmin=295652.2, xmax=439902.2, ymin=5846234, ymax=6010984))
road.density <- rasterize(roads.crop, temp.raster, fun='count', background=0)

road.dens.sqkm <- road.density / raster::area(road.density) # road density at 1km
road.dens.4km <- aggregate(road.density, 16) # make this a 4km resolution?

writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.roads.crop, "data/processed/bhb_roads.tif", overwrite=TRUE)

