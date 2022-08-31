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
bhb.buf <- st_read("data/processed/bhb_10km.shp")

bhb.reproj<- st_transform(bhb.buf, st_crs(recent_wildfires))

st_crs(recent_wildfires) == st_crs(bhb.reproj)
st_is_valid(bhb.reproj)
st_is_valid(recent_wildfires)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.v <- vect(bhb.reproj)
wildfires.v <- vect(recent_wildfires)

wildfires.crop <- crop(wildfires.v, template.rast)

bhb.recent.wildfires.rast <- terra::rasterize(wildfires.crop, template.rast, field = "YEAR")

# Make a continuous raster:
bhb.recent.wildfires.rast[bhb.recent.wildfires.rast > 2000] <- 1
bhb.rast <- terra::rasterize(bhb.v, template.rast, field = "OBJECTID")

recent.burns.r <- terra::mask(bhb.rast, bhb.recent.wildfires.rast, updatevalue=0)
names(recent.burns.r)[names(recent.burns.r) == "OBJECTID"] <- "recent_wildfires"
bhb.fire.rast <- terra::mask(recent.burns.r, bhb.v)

terra::writeRaster(bhb.fire.rast, "data/processed/bhb_fire_history.tif", overwrite=TRUE)

