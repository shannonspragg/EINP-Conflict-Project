
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

# Subset out shrubland:
shrubland <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Shrubland")

# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_10km.shp") # Beaver Hills Watershed

bhb.reproj<- st_transform(bhb.buf, st_crs(ab_landcover))

st_crs(ab_landcover) == st_crs(bhb.reproj)
st_is_valid(bhw.reproj)
st_is_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.v <- vect(bhb.reproj)
landcover.v <- vect(ab_landcover)
shrubland.v <- vect(shrubland)

bhb.landcover.crop <- crop(landcover.v, template.rast)
bhb.shrub.crop <- crop(shrubland.v, template.rast)

bhb.landcover.rast <- terra::rasterize(bhb.landcover.crop, template.rast, field = "LC_DESCRIPTION")
bhb.lc.rast <- terra::mask(bhb.landcover.rast, bhb.v)

bhb.shrubland.rast <- terra::rasterize(bhb.shrub.crop, template.rast, field = "LC_DESCRIPTION")

# Make shrubland a continuous raster:
bhb.shrubland.rast[0] <- 1
bhb.shrubland.rast[bhb.shrubland.rast == 0] <- 1

bhb.rast <- terra::rasterize(bhb.v, template.rast, field = "OBJECTID")

shrubland.r <- terra::mask(bhb.rast, bhb.shrubland.rast, updatevalue=0)
names(shrubland.r)[names(shrubland.r) == "OBJECTID"] <- "shrubland"
bhb.shrub.rast <- terra::mask(shrubland.r, bhb.v)


terra::writeRaster(bhb.lc.rast, "data/processed/bhb_landcover.tif", overwrite=TRUE)
terra::writeRaster(bhb.shrub.rast, "data/processed/bhb_shrubland.tif", overwrite=TRUE)


