# Prep Wildfire Data ----------------------------------------------------
# Filter to burned areas within the last 20 years


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)


# Filter wildfires by year ------------------------------------------------
historic_wildfires <- st_read("data/original/WildfirePerimeters1931to2021v2.shp")

recent_wildfires <- historic_wildfires %>% filter(historic_wildfires$YEAR >= 2003) # Filter to last 20 years
plot(recent_wildfires['FIRE_NUMBE'])


# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(recent_wildfires))

st_crs(recent_wildfires) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(recent_wildfires)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

parkland.v <- vect(parkland.reproj)
wildfires.v <- vect(recent_wildfires)

wildfires.crop <- crop(wildfires.v, template.rast)

parkland.recent.wildfires.rast <- terra::rasterize(wildfires.crop, template.rast, field = "YEAR")

terra::writeRaster(parkland.recent.wildfires.rast, "data/processed/parkland_fire_history.tif")

