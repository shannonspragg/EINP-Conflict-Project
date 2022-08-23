
# Prep Land Use Data ----------------------------------------------------
# Filter to different land cover types


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load Land cover data -------------------------------------------------------------

ab_landcover <- st_read("data/original/Lancover_Polygons_2010.shp")


# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab_landcover))

st_crs(ab.forest.inventory) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

parkland.landcover <- st_intersection(parkland.reproj, ab_landcover) #need to figure out the issue with the self-intersection


