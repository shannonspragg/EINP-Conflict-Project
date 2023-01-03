
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


# Calculate Road Density --------------------------------------------------

  # Start with template raster
template.rast.1km <- rast(res=c(1000,1000), ext=ext(temp.raster)) # Let's do a 30x30 res to match land cover
crs(template.rast.1km) <- "epsg:32612" # UTM zone 12N for AB
values(template.rast.1km) <- rep(1, ncell(template.rast.1km))
temp.raster.1km <- raster(template.rast.1km)

  # Crop roads to our extent
roads.crop <- st_crop(ab.roads.reproj, c(xmin=257856.3, xmax=488106.3, ymin=5837774, ymax=6087024))

  # Calc density
road.density.1km <- rasterize(roads.crop, temp.raster.1km, fun='count', background=0)

rd.disagg <- raster::disaggregate(road.density.1km, 4) # change to 250m resolution
rd.rast <- rast(rd.disagg)
rd.crop <- terra::project(rd.rast, temp.rast)

writeRaster(road.density.1km, "data/processed/bhb_road_density_1km.tif", overwrite=TRUE)
writeRaster(rd.crop, "data/processed/bhb_road_density_250m.tif", overwrite=TRUE)

