# Prep Forest Inventory Data ----------------------------------------------------
# Look at to forest area categories


library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load AB Forest Inventory -------------------------------------------------------------
figdb <- "data/original/ForestInventory.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", figdb))
fc_list <- ogrListLayers(figdb)
print(fc_list)

fic <- readOGR(dsn=figdb,layer="PH1_GENERALIZED")
fic.sf <- as(fic, "sf")
ab.forest.inventory <- fic.sf

# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab.forest.inventory))

st_crs(ab.forest.inventory) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab.forest.inventory)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

parkland.v <- vect(parkland.reproj)
forest.inv.v <- vect(ab.forest.inventory)

park.forest.crop <- crop(forest.inv.v, template.rast)

parkland.forest.inv.rast <- terra::rasterize(park.forest.crop, template.rast, field = "DESCRIPTION")
parkland.forest.inv.rast <- terra::mask(parkland.forest.inv.rast, parkland.v)

terra::writeRaster(parkland.forest.inv.rast, "data/processed/parkland_forest_inventory.tif", overwrite=TRUE)
