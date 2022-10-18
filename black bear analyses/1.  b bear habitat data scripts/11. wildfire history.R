# Prep Wildfire Data ----------------------------------------------------
# Filter to burned areas within the last 20 years


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)


# Filter wildfires by year ------------------------------------------------
historic_wildfires <- st_read(st_make_valid("data/original/WildfirePerimeters1931to2021v2.shp"))

recent_wildfires <- historic_wildfires %>% filter(historic_wildfires$YEAR >= 2003) # Filter to last 20 years
#plot(recent_wildfires['FIRE_NUMBE'])

# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_50km.shp")

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.v <- vect(bhb.buf)
wildfires.v <- vect(recent_wildfires)

wildfires.crop <- crop(wildfires.v, template.rast)

bhw.recent.wildfires.rast <- terra::rasterize(wildfires.crop, template.rast, field = "YEAR")

# Make a continuous raster:
bhw.recent.wildfires.rast[bhw.recent.wildfires.rast >= 2000] <- 1
bhw.recent.wildfires.raster <- raster(bhw.recent.wildfires.rast)
bhw.recent.wildfires.raster[is.na(bhw.recent.wildfires.raster[])] <- 0 

names(bhw.recent.wildfires.raster)[names(bhw.recent.wildfires.raster) == "YEAR"] <- "recent_wildfires"
#bhb.fire.rast <- terra::mask(recent.burns.r, bhb.v)

terra::writeRaster(bhw.recent.wildfires.raster, "data/processed/bhb_fire_history.tif", overwrite=TRUE)

