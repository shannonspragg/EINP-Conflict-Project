
# Prep inputs to run omniscape for just EINP ------------------------------


# Load packages -----------------------------------------------------------

library(terra)
library(sf)
library(tidyverse)
library(raster)


# Bring in data: ----------------------------------------------------------
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
einp.v <- st_read("data/processed/einp.shp") %>% st_transform(., crs=crs(temp.rast)) %>% st_buffer(., 50000) %>%
  as(., "SpatVector") 
bhs <- rast("data/processed/bbear_habitat_suitability.tif")
biosocial.resist <- rast("data/processed/biosocial_resist.tif")

einp.bound.v <- st_read("data/processed/einp.shp") %>% st_transform(., crs=crs(temp.rast)) %>%
  as(., "SpatVector")

# Crop the inputs to EINP boundary ----------------------------------------
bhs.einp <- crop(bhs, einp.v)
biosoc.resist.einp <- crop(biosocial.resist, einp.v)



# Save these: -------------------------------------------------------------
writeRaster(bhs.einp, "data/processed/einp_bhs_50km.tif")
writeRaster(biosoc.resist.einp, "data/processed/einp_biosocial_resist_50km.tif")



# Bring in outputs: -------------------------------------------------------

b.conf.einp.cumcurr <- rast("/Users/shannonspragg/analyses/einp_omniscape/bbears/EINP Bbear Conflict/cum_currmap.tif")
b.conf.einp.norm <- rast("/Users/shannonspragg/analyses/einp_omniscape/bbears/EINP Bbear Conflict/cum_currmap.tif")


# Crop to EINP ------------------------------------------------------------

# b.conf.cumcurr.einp <- terra::mask(b.conf.einp.cumcurr, einp.bound.v)
# b.conf.cumcurr.einp <- terra::mask(b.conf.einp.cumcurr, einp.bound.v)



