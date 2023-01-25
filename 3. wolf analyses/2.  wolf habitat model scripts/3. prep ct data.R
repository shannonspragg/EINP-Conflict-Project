# Prep wolf CT data for analysis --------------------------------------

library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in our Original Data --------------------------------------------

wolf.ct <- read.csv("data/original/camera_trap_sightings.csv")
bhw.bound.50k <- st_read("data/processed/bhb_50km.shp")
ld <- rast("data/processed/bhb_landcover.tif") 
head(wolf.ct)

xy<-wolf.ct[,c(8,7)]
 ct.spdf<-SpatialPointsDataFrame(coords = xy,data = wolf.ct,
                                       proj4string = CRS("+proj=longlat +zone=12 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#ct.spdf<-SpatialPointsDataFrame(coords = xy,data = wolf.ct,
#                                    proj4string = CRS("EPSG:26912"))

# Ensure this is a sf data frame:
ct.data.sf <- st_as_sf(ct.spdf)

head(ct.data.sf)
str(ct.data.sf)
plot(ct.data.sf)

ct.data.reproj <- st_transform(ct.data.sf, crs(ld))
# Plot it:
plot(st_geometry(bhw.bound.50k))
plot(ld)
plot(st_geometry(ct.data.reproj), add=T)

st_write(ct.data.reproj, "data/processed/wolf_ct_data.shp", append= FALSE)

