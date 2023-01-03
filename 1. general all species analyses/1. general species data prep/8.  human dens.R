# Prep Human Pop Dens --------------------------------------


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)

# Load Data ---------------------------------------------------------------
world.hum.dens <- terra::rast("data/original/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")

bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast(("data/processed/dist2pa_km_bhb.tif"))

# Crop Human Dens to BHB --------------------------------------------------
bhb.buf.vect <- vect(bhb.50km.boundary)

bhb.buf.reproj <- project(bhb.buf.vect, world.hum.dens)
#world.dens.crop <- crop(world.hum.dens, bhb.buf.reproj)

world.dens.resampl <- resample(world.hum.dens, temp.rast)

# CHECK FOR NA'S:
hum.dens.raster <- raster(world.dens.resampl)
hum.dens.raster[is.na(hum.dens.raster[])] <- 0 


#writeRaster(world.dens.crop, "data/processed/human_dens_crop.tif", overwrite=TRUE)
writeRaster(hum.dens.raster, "data/processed/human_dens_bhb.tif", overwrite=TRUE)
