
# Prepping Waterbodies Data -----------------------------------------------

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in  data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.watershed <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

waterbodies <- st_read("data/original/waterbody_2.shp")

bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

  # Get waterbodies in the BHW buffer zone:
water.proj <- waterbodies %>% 
  st_transform(., crs=crs(bhb.watershed))
waterbhb <- st_intersection(water.proj, bhb.watershed)
st_write(waterbhb, "data/processed/bhw_waterbodies.shp")

# Make this a raster: -----------------------------------------------------
waterbodies.v <- vect(waterbhb)

waterbodies.rast <- terra::rasterize(waterbodies.v, temp.rast, field = "definit") 
waterbodies.rast[waterbodies.rast == 83] <- 1
waterbodies.raster <- raster(waterbodies.rast)
waterbodies.raster[is.na(waterbodies.raster[])] <- 0 


# Pull out lakes ----------------------------------------------
beaverhill.lake <- waterbhb %>% dplyr::filter(waterbhb$name_en == "Beaverhill Lake")

b.lake.v <- vect(beaverhill.lake)


st_write(beaverhill.lake, "data/processed/beaverhill_lake.shp")

blake.rast <- terra::rasterize(b.lake.v, temp.rast, field = "definit") 
blake.rast[blake.rast == 83] <- 1
blake.raster <- raster(blake.rast)
blake.raster[is.na(blake.raster[])] <- 0 

writeRaster(waterbodies.raster, "data/processed/bhb_50km_waterbodies.tif", overwrite=TRUE)
writeRaster(blake.raster, "data/processed/beaverhills_lake.tif", overwrite=TRUE)
