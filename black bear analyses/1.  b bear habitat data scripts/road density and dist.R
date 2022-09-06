
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
temp.raster <- aggregate(temp.raster, 4) # make this 1km

roads.crop <- st_crop(ab.roads.reproj, c(xmin=295652.2, xmax=439902.2, ymin=5846234, ymax=6010984))
road.density.1km <- rasterize(roads.crop, temp.raster, fun='count', background=0)

# road.density.rsmpl <- resample(road.density.1km, temp.raster) # correct the extent
# road.dens.1km <- aggregate(road.density.rsmpl, 4)

# Need to have road density at 4km and 500m:
road.dens.4km <- aggregate(road.density.1km, 4) # make this a 4km resolution?
road.dens.500m <- disaggregate(road.density.1km, 2) # make this a 4km resolution?

# road.dens.4sqkm <- road.dens.4km / raster::area(road.dens.4km) # road density 1km
# road.dens.500sqkm <- road.dens.500m / raster::area(road.dens.500m) # road density 1km

# check for NA's:



writeRaster(road.dens.4km, "data/processed/bhb_road_density_4km.tif", overwrite=TRUE)
writeRaster(road.dens.500m, "data/processed/bhb_road_density_500m.tif", overwrite=TRUE)
writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
#writeRaster(bhb.roads.crop, "data/processed/bhb_roads.tif", overwrite=TRUE)

