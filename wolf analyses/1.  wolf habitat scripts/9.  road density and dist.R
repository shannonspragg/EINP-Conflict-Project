
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

# Make Roads Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.roads.v <- vect(ab.roads.reproj)

bhb.roads.crop <- terra::crop(ab.roads.v, temp.rast)

# Dist to roads raster:
dist2roads <- terra::distance(temp.rast, ab.roads.v)
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

roads.crop <- st_crop(ab.roads.reproj, c(xmin=257856.3, xmax=488214.7, ymin=5837774, ymax=6087088))
road.density.1km <- rasterize(roads.crop, temp.raster.1km, fun='count', background=0)

# e <- extent(295652.2, 439832.2, 5846234, 6011054)
# 
# road.density.1 <- crop(road.density.1km, e)
#   #setExtent(road.density.1km, e, keepres=TRUE)


# road.density.rsmpl <- resample(road.density.1km, temp.raster) # correct the extent
# road.dens.1km <- aggregate(road.density.rsmpl, 4)

# Need to have road density at 4km and 500m:
# road.dens.4km <- aggregate(road.density.1km, 4) # make this a 4km resolution?
# road.dens.500m <- disaggregate(road.density.1km, 2) # make this a 4km resolution?
# 
# # road.dens.4sqkm <- road.dens.4km / raster::area(road.dens.4km) # road density 1km
# # road.dens.500sqkm <- road.dens.500m / raster::area(road.dens.500m) # road density 1km
# 
# # check for NA's:
# table(is.na(road.dens.4km[])) #FALSE, no NA's
# table(is.na(road.dens.500m[])) #FALSE, no NA's

# road.dens.4km.rast <- rast(road.dens.4km)
# road.dens.4km.crop <- crop(road.dens.4km.rast, temp.rast)

# writeRaster(road.density.1km, "data/processed/bhb_road_density_1km.tif", overwrite=TRUE)
# writeRaster(road.dens.4km, "data/processed/bhb_road_density_4km.tif", overwrite=TRUE)
# writeRaster(road.dens.500m, "data/processed/bhb_road_density_500m.tif", overwrite=TRUE)
writeRaster(dist2roads.km, "data/processed/dist2roads_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.roads.raster, "data/processed/bhb_roads.tif", overwrite=TRUE)

