
# Prep Road Network Variable ----------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(raster)

# Load Data -------------------------------------------------------------
ab.roads <- st_read("data/original/grnf048r10a_e.shp")
bhb.bound <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
temp.raster <- rast("data/processed/bhb_50km_template_rast.tif")
ab.roads.reproj <- st_transform(ab.roads, st_crs(bhb.bound))


# Filter roads by type for dist calc --------------------------------------
ab.roads.filt <- ab.roads.reproj %>% filter(ab.roads.reproj$TYPE == "FWY" | ab.roads.reproj$TYPE == "PASS" | ab.roads.reproj$TYPE == "HWY")

# Make Roads Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.roads.v <- vect(ab.roads.reproj)
ab.roads.filt.v <- vect(ab.roads.filt)

bhb.roads.crop <- terra::crop(ab.roads.v, temp.rast)
bhb.major.roads <- terra::crop(ab.roads.filt.v, temp.rast)

# Dist to roads raster:
dist2roads <- terra::distance(temp.rast, ab.roads.filt.v)
dist2roads.km <- measurements::conv_unit(dist2roads, "m", "km")

# Roads raster:
bhb.roads <- terra::rasterize(bhb.roads.crop, temp.rast, field = "RB_UID")
bhb.roads[bhb.roads >= 1] <- 1

bhb.roads.raster <- raster(bhb.roads)
bhb.roads.raster[is.na(bhb.roads.raster[])] <- 0 

# Calculate road density:

template.rast.1km <- rast(res=c(1000,1000), ext=ext(temp.raster)) # Let's do a 30x30 res to match land cover
crs(template.rast.1km) <- "epsg:32612" # UTM zone 12N for AB
values(template.rast.1km) <- rep(1, ncell(template.rast.1km))
temp.raster.1km <- raster(template.rast.1km)

roads.crop <- st_crop(ab.roads.reproj, c(xmin=257856.3, xmax=488106.3, ymin=5837774, ymax=6087024))
road.density.1km <- rasterize(roads.crop, temp.raster.1km, fun='count', background=0)

rd.disagg <- raster::disaggregate(road.density.1km, 4) # change to 250m resolution
rd.rast <- rast(rd.disagg)
rd.crop <- terra::project(rd.rast, temp.rast)

writeRaster(road.density.1km, "data/processed/bhb_road_density_1km.tif", overwrite=TRUE)
writeRaster(rd.crop, "data/processed/bhb_road_density_250m.tif", overwrite=TRUE)
writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.roads.raster, "data/processed/bhb_roads.tif", overwrite=TRUE)

