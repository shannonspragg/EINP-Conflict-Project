
# Crop & Save Individual Habitat Variables --------------------------------
  # Here we crop our variable rasters to the Beaver Hills Watershed boundary and 


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")

# model 1 Beckman et al., 2015:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
roads <- rast("data/processed/bhb_roads.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
coniferous.forest <- rast("data/processed/bhb_conifer_mix.tif")
broadleaf.forest <- rast("data/processed/bhb_broadleaf_mix.tif")
alpine.mixed.forest <- rast("data/processed/bhb_alpine_mix.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")


bhb.buf.vect <- vect(bhb.50km.boundary)

# Crop our rasters to the BHB buffer shape:
#bhb.50km.v <- vect(bhb.50km.boundary)

# private.land.bhb <- terra::mask(private.land.rast, bhb.50km.v)
# elevation.bhb <- terra::mask(elevation, bhb.50km.v)
# dist2roads.bhb <- terra::mask(dist2roads, bhb.50km.v)
# pop.dens.bhb <- terra::mask(pop.dens, bhb.50km.v)
# road.dens.4km.bhb <- terra::mask(road.dens.4km, bhb.50km.v)
# road.dens.500m.bhb <- terra::mask(road.dens.500m, bhb.50km.v)
# evergreen.bhb <- terra::mask(evergreen.forest, bhb.50km.v)
# 
# pop.road.dens <- pop.dens.bhb * road.dens.500m.bhb
# pop.road.dens

