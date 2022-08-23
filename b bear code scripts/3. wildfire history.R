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

parkland.fire.hist <- st_intersection(parkland.reproj, recent_wildfires) #need to figure out the issue with the self-intersection


# parkland.b.v <- vect(parkland.reproj)
# wildfires.v <- vect(recent_wildfires)
# 
# parkland.fires <- terra::crop(wildfires.v, n.sask.b.v)
