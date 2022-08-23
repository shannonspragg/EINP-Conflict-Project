
# Prep Land Use Data ----------------------------------------------------
# Filter to different land cover types


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load Land cover data -------------------------------------------------------------

ab_landcover <- st_read("data/original/Lancover_Polygons_2010.shp")


# Add column with classification descriptions -----------------------------
  # Following: https://ftp-public.abmi.ca//GISData/LandCover/W2W2010/LandcoverMapABMIGuide2010v1.0.pdf

ab_landcover <- ab_landcover %>%
  dplyr::mutate(LC_DESCRIPTION = 
           case_when(ab_landcover$LC_class == 20 ~ "Water",
                     ab_landcover$LC_class == 31 ~ "Snow/Ice",
                     ab_landcover$LC_class == 32 ~ "Rock/Rubble",
                     ab_landcover$LC_class == 33 ~ "Exposed Land",
                     ab_landcover$LC_class == 34 ~ "Developed",
                     ab_landcover$LC_class == 50 ~ "Shrubland",
                     ab_landcover$LC_class == 110 ~ "Grassland",
                     ab_landcover$LC_class == 120 ~ "Agriculture",
                     ab_landcover$LC_class == 210 ~ "Coniferous Forest",
                     ab_landcover$LC_class == 220 ~ "Broadleaf Forest",
                     ab_landcover$LC_class == 230 ~ "Mixed Forest",
  ))

# Crop to our Region --------------------------------------------------------
parkland.buf <- st_read("data/processed/parkland_county_10km.shp")

parkland.reproj<- st_transform(parkland.buf, st_crs(ab_landcover))

st_crs(ab_landcover) == st_crs(parkland.reproj)
st_make_valid(parkland.reproj)
st_make_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_parkland.tif")

parkland.v <- vect(parkland.reproj)
landcover.v <- vect(ab_landcover)

park.landcover.crop <- crop(landcover.v, template.rast)

parkland.landcover.rast <- terra::rasterize(park.landcover.crop, template.rast, field = "LC_DESCRIPTION")

terra::writeRaster(parkland.landcover.rast, "data/processed/parkland_landcover.tif")


