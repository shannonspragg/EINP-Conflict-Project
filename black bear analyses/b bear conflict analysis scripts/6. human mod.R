
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
ghm.conv <- gHM/65536
# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_10km.shp")

bhb.reproj<- st_transform(bhb.buf, st_crs(ghm.conv))

st_crs(ghm.conv) == st_crs(bhb.reproj)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

temp.rast <- project(ghm.conv, template.rast)

bhb.ghm.rsmple <- resample(ghm.conv, template.rast)
bhb.ghm.crop <- crop(bhb.ghm.rsmple, template.rast)

bhb.reproj<- st_transform(bhb.reproj, st_crs(bhb.ghm.crop))
bhb.v <- vect(bhb.reproj) 

bhb.ghm.rast <- terra::mask(bhb.ghm.crop, bhb.v)

terra::writeRaster(bhb.ghm.rast, "data/processed/bhb_ghm.tif", overwrite=TRUE)

