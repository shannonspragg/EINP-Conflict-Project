
# Prep PA Data ----------------------------------------------------
  # Filter to AB and IUCN status


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load BC PAs -------------------------------------------------------------
fgdb <- "data/original/CPCAD-BDCAPC_Dec2020.gdb"
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf")
ab.PAs <- fc.sf %>% 
  filter(., LOC_E == "Alberta") 

# Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
ab.PAs.iucn.filtered <- ab.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV") %>% 
  st_make_valid()
ab.PAs.iucn.filtered$areaha <- st_area(ab.PAs.iucn.filtered) 
units(ab.PAs.iucn.filtered$areaha) <- units::make_units(ha)
ab.PAs.iucn.filtered$areaha <- as.numeric(ab.PAs.iucn.filtered$areaha) 

# Filter by PA's larger than 100 ha:
ab.PAs.fin <- filter(ab.PAs.iucn.filtered, areaha > 100) 


# create template raster --------------------------------------------------

  # Bring in land use regions:
ab.regions <- st_read("data/original/LUF Integrated Regional Plan Boundaries.shp")
ab.crs <- st_crs
n.sask <- ab.regions %>%
  filter(., LUF_NAME == "North Saskatchewan")

n.sask.proj <- n.sask %>% st_set_crs(3400) %>% st_transform(3400)

n.sask.bound <- n.sask.proj %>%
  st_buffer(., 25000) 

n.sask.bound.v <- n.sask.bound %>% as(., "SpatVector")

st_write(n.sask, "data/processed/north_saskatchewan.shp", append= FALSE)
st_write(n.sask.bound, "data/processed/north_saskatchewan_25km.shp", append=FALSE)

temp.rast <- rast(res=c(1000,1000), ext=ext(n.sask.bound))
crs(temp.rast) <- crs(n.sask.bound)
values(temp.rast) <- rep(1, ncell(temp.rast))

ab.pa.proj <- ab.PAs.fin %>% 
  st_transform(., crs=crs(temp.rast)) %>%
  as(., "SpatVector")

  # Dist to PA raster:
dist2pa <- terra::distance(temp.rast, ab.pa.proj)
dist2pa.km <- measurements::conv_unit(dist2pa, "m", "km")
writeRaster(dist2pa.km, "data/processed/dist2pa_km_n_sasck.tif", overwrite=TRUE)

