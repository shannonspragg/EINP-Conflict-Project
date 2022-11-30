
# Prep Bear collar data for analysis --------------------------------------

library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in our Original Data --------------------------------------------

bear.collar <- read.csv("data/original/Bear_Clean.csv")

xy<-bear.collar[,c(8,7)]
collar.spdf<-SpatialPointsDataFrame(coords = xy,data = bear.collar,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
collar.data.sf <- st_as_sf(collar.spdf)

head(collar.data.sf)
str(collar.data.sf)
st_write(collar.data.sf, "data/processed/bear_collar_data.shp")

