
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
north.sask.buf <- st_read("data/processed/north_saskatchewan_25km.shp")

st_crs(ab_landcover) == st_crs(north.sask.buf)
st_make_valid(north.sask.buf)
st_make_valid(ab_landcover)

n.sask.landcover <- st_intersection(ab_landcover, north.sask.buf) #need to figure out the issue with the self-intersection

