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
north.sask.bound <- st_read("data/processed/north_saskatchewan_25km.shp")

n.sask.reproj<- st_transform(north.sask.bound, st_crs(recent_wildfires))

st_crs(recent_wildfires) == st_crs(n.sask.reproj)
st_make_valid(recent_wildfires)
st_make_valid(n.sask.reproj)

n.sask.b.v <- vect(north.sask.bound)
wildfires.v <- vect(recent_wildfires)

ns.fires <- terra::crop(wildfires.v, n.sask.b.v)
