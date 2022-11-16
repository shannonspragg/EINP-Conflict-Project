
# Prepping Ungulate density data ------------------------------------------


# Load packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(raster)


# Bring in data: ----------------------------------------------------------
wmu <- st_read("data/original/bf_wmu_polygons-polygon.shp")
bhw <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ungulate.harvest <- read_csv("data/original/ungulate_harvest_counts.csv") # remove notes col

wmu.reproj <- st_transform(wmu, st_crs(crs(temp.rast)))

# Crop down to BHW buffer: ------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
wmu.v <- vect(wmu.reproj)
crs(wmu.v) == crs(temp.rast)

wmu.crop <- terra::crop(wmu.v, temp.rast)

# Make sf again:
wmu.bhw <- as(wmu.crop, "Spatial")
wmu.bhw.sf <- st_as_sf(wmu.bhw)

str(ungulate.harvest)
ungulate.harvest <- ungulate.harvest[ -c(7) ]
ungulate.harvest$WMU <- as.character(ungulate.harvest$WMU)

# Join WMU with harvest data ----------------------------------------------
wmu.bhw.sf$WMU <- str_sub(wmu.bhw.sf$WMU, -3, -1)

wmu.harvest.join <- wmu.bhw.sf %>% 
  left_join(., ungulate.harvest, by = c("WMU" = "WMU"))


# Make rasters for ungulate count ------------------------------------------
harvest.v <- vect(wmu.harvest.join)

total.ungulate.rast <- terra::rasterize(harvest.v, temp.rast, field = "total_ungulate_harvest")
total.elk.rast <- terra::rasterize(harvest.v, temp.rast, field = "elk_harvest_count")
total.muledeer.rast <- terra::rasterize(harvest.v, temp.rast, field = "mule_deer_harvest_count")
total.whitetailed.deer.rast <- terra::rasterize(harvest.v, temp.rast, field = "wt_deer_harvest_count")


# Save these: -------------------------------------------------------------
writeRaster(total.ungulate.rast, "data/processed/total_ungulate_density.tif")
writeRaster(total.elk.rast, "data/processed/total_elk_density.tif")
writeRaster(total.muledeer.rast, "data/processed/total_muledeer_density.tif")
writeRaster(total.whitetailed.deer.rast, "data/processed/total_white_tailed_deer_density.tif")
st_write(wmu.harvest.join, "data/processed/ungulate_harvest_per_wmu.shp")

