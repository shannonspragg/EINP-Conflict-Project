# Prep Private Land Data ----------------------------------------------------
    # We will create a private lands raster by taking the inverse of public (crown), protected, and 
    # first nation lands in AB

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load First Nation data -------------------------------------------------------------

ab_firstnations <- st_read("data/original/AL_TA_AB_2_146_CONFIRMED_eng.shp")


# Crop to our Region --------------------------------------------------------
bhw.buf <- st_read("data/processed/biosphere_50km.shp") # change to bhw

firstnation.reproj<- st_transform(ab_firstnations, st_crs(bhw.buf))

st_crs(ab_firstnations) == st_crs(bhw.reproj)

st_is_valid(ab_firstnations)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_biosphere.tif") # should be bhw

bhw.v <- vect(bhw.buf)
firstnation.v <- vect(firstnation.reproj)

bhw.firstnation.crop <- crop(firstnation.v, template.rast)

bhw.firstnation.rast <- terra::rasterize(bhw.firstnation.crop, template.rast, field = "NAME1")
bhw.firstnation.rast <- terra::mask(bhw.firstnation.rast, bhw.v)

# Stack our public,  protected & native lands: ----------------------------

  # Load PA and crown rasters:
bhw.pas <- rast("data/processed/biosphere_protected_areas.tif")

bhw.crowns <- rast("data/processed/biosphere_crownlands.tif")

# Stack these into one raster:
land.tenure.stack <- terra::merge(bhw.pas, bhw.crowns, bhw.firstnation.rast) #Need these to be in categories somehow

# land.tenure.stack[land.tenure.stack > -1] <- 1 # make our public, native, and protected lands 1

# Make private lands from the inverse of our other land tenures
private.lands <- terra::mask(template.rast, land.tenure.stack, updatevalue=100)
private.lands[private.lands < 100] <- 0
private.lands[private.lands == 100] <- 1


terra::writeRaster(private.lands, "data/processed/bhw_privatelands.tif", overwrite=TRUE)
