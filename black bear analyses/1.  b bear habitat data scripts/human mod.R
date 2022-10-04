
# Prep gHM Data ----------------------------------------------------


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

gHM <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif")

# Crop to our Region --------------------------------------------------------
bhw.buf <- st_read("data/processed/bhb_50km.shp")

bhw.reproj<- st_transform(bhw.buf, st_crs(gHM))

st_crs(gHM) == st_crs(bhw.reproj)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

temp.rast <- project(gHM, template.rast)

bhw.ghm.rsmple <- resample(gHM, template.rast)
#bhw.ghm.crop <- crop(bhw.ghm.rsmple, template.rast)

# bhw.reproj<- st_transform(bhw.reproj, st_crs(bhw.ghm.crop))
# bhw.v <- vect(bhw.reproj) 

#bhw.ghm.rast <- terra::mask(bhw.ghm.crop, bhw.v)

terra::writeRaster(bhw.ghm.rsmple, "data/processed/bhw_ghm.tif", overwrite=TRUE)

