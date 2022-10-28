
# Prep Predictor Raster Plots ---------------------------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)


dist.2.pa <- rast("Data/processed/dist2pa_km_bhb.tif") 
hum.dens.bhw <- rast("data/processed/bhw_popdens.tif")
animal.dens <- rast("data/processed/animal_production_density_raster.tif")
ground.dens <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.r <- rast("data/processed/bhb_ndvi.tif")
ghm.bhw <- rast("data/processed/bhw_human_mod.tif")
bhs.bhw <- rast("data/processed/bbear_habitat_bhw.tif")
biophys.bhw <- rast("data/processed/bhw_forest_sp_cumcurr.tif")
conflict.bhw <- rast("Data/processed/prob_conflict_all_bhw.tif")
agnostic_bio_cumcurr_bhw <- rast("data/processed/bhw_agno_cumcurr.tif")


# Crop some of these: -----------------------------------------------------
dist2pa.bhw <- mask(dist.2.pa, bhw.v)
animal.dens.bhw <- mask(animal.dens, bhw.v)
ground.dens.bhw <- mask(ground.dens, bhw.v)
ndvi.bhw <- mask(ndvi.r, bhw.v)

# General Wildlife Conflict Stack: ----------------------------------------
gen.conf.pred.stack <- c(dist2pa.bhw, hum.dens.bhw, animal.dens.bhw, ground.dens.bhw, ndvi.bhw, ghm.bhw, agnostic_bio_cumcurr_bhw)


# Bear conflict stack: ----------------------------------------------------
bbear.conf.pred.stack <- c(dist2pa.bhw, hum.dens.bhw, animal.dens.bhw, ground.dens.bhw, ndvi.bhw, ghm.bhw, bhs.bhw, biophys.bhw, conflict.bhw)

plot(gen.conf.pred.stack)
plot(bbear.conf.pred.stack)


