
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
                     ab_landcover$LC_class == 110 ~ "Grassland", # meadow / grassland
                     ab_landcover$LC_class == 120 ~ "Agriculture",
                     ab_landcover$LC_class == 210 ~ "Coniferous Forest", # conifer mixed forest
                     ab_landcover$LC_class == 220 ~ "Broadleaf Forest", # birch and aspen
                     ab_landcover$LC_class == 230 ~ "Mixed Forest", # alpine mixed forest
  ))

# Subset out shrubland:
shrubland <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Shrubland")
grassland <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Grassland")
conifer.mix <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Coniferous Forest")
broadleaf <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Broadleaf Forest")
alpine.mixed <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Mixed Forest")
water <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Water")


# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_50km.shp") # Beaver Hills Watershed

#bhb.reproj<- st_transform(bhb.buf, st_crs(ab_landcover))

#st_crs(ab_landcover) == st_crs(bhb.reproj)
st_is_valid(bhw.reproj)
st_is_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.v <- vect(bhb.buf)
#landcover.v <- vect(ab_landcover)
shrubland.v <- vect(shrubland)
grassland.v <- vect(grassland)
conifer.v <- vect(conifer.mix)
broadleaf.v <- vect(broadleaf)
alpinemix.v <- vect(alpine.mixed)
water.v <- vect(water)

#bhb.landcover.crop <- crop(landcover.v, template.rast)
bhb.shrub.crop <- crop(shrubland.v, template.rast)
bhb.grass.crop <- crop(grassland.v, template.rast)
bhb.conifer.crop <- crop(conifer.v, template.rast)
bhb.broadleaf.crop <- crop(broadleaf.v, template.rast)
bhb.alpine.crop <- crop(alpinemix.v, template.rast)
bhb.water.crop <- crop(water.v, template.rast)

#bhb.landcover.rast <- terra::rasterize(bhb.landcover.crop, template.rast, field = "LC_DESCRIPTION")
bhb.shrubland.rast <- terra::rasterize(bhb.shrub.crop, template.rast, field = "LC_DESCRIPTION")
bhb.grassland.rast <- terra::rasterize(bhb.grass.crop, template.rast, field = "LC_DESCRIPTION")
bhb.conifer.rast <- terra::rasterize(bhb.conifer.crop, template.rast, field = "LC_DESCRIPTION")
bhb.broadleaf.rast <- terra::rasterize(bhb.broadleaf.crop, template.rast, field = "LC_DESCRIPTION")
bhb.alpinemix.rast <- terra::rasterize(bhb.alpine.crop, template.rast, field = "LC_DESCRIPTION")
#bhb.water.rast <- terra::rasterize(bhb.water.crop, template.rast, field = "LC_DESCRIPTION")


# Make shrubland a continuous raster:
bhb.shrubland.rast[0] <- 1
bhb.shrubland.rast[bhb.shrubland.rast == 0] <- 1

bhb.rast <- terra::rasterize(bhb.v, template.rast, field = "OBJECTID")

shrubland.r <- terra::mask(bhb.rast, bhb.shrubland.rast, updatevalue=0)
names(shrubland.r)[names(shrubland.r) == "OBJECTID"] <- "shrubland"
bhb.shrub.rast <- terra::mask(shrubland.r, bhb.v)

# Make grassland a continuous raster:
bhb.grassland.rast[0] <- 1
bhb.grassland.rast[bhb.grassland.rast == 0] <- 1

grassland.r <- terra::mask(bhb.rast, bhb.grassland.rast, updatevalue=0)
names(grassland.r)[names(grassland.r) == "OBJECTID"] <- "grassland"
bhb.grassland.rast <- terra::mask(grassland.r, bhb.v)

# Make conifer a continuous raster:
bhb.conifer.rast[0] <- 1
bhb.conifer.rast[bhb.conifer.rast == 0] <- 1

conifer.r <- terra::mask(bhb.rast, bhb.conifer.rast, updatevalue=0)
names(conifer.r)[names(conifer.r) == "OBJECTID"] <- "coninfer mix"
bhb.conifer.rast <- terra::mask(conifer.r, bhb.v)

  # Make Evergreen forest at 500m:
evergreen.500m <- aggregate(bhb.conifer.rast, 2) #This gives us a "buffer" zone of edge forest at the new resolution

# Make broadleaf a continuous raster:
bhb.broadleaf.rast[0] <- 1
bhb.broadleaf.rast[bhb.broadleaf.rast == 0] <- 1

broadleaf.r <- terra::mask(bhb.rast, bhb.broadleaf.rast, updatevalue=0)
names(broadleaf.r)[names(broadleaf.r) == "OBJECTID"] <- "broadleaf forest"
bhb.broadleaf.rast <- terra::mask(broadleaf.r, bhb.v)

# Make alpine mix a continuous raster:
bhb.alpinemix.rast[0] <- 1
bhb.alpinemix.rast[bhb.alpinemix.rast == 0] <- 1

alpine.r <- terra::mask(bhb.rast, bhb.alpinemix.rast, updatevalue=0)
names(alpine.r)[names(alpine.r) == "OBJECTID"] <- "coninfer mix"
bhb.alpinemix.rast <- terra::mask(alpine.r, bhb.v)

# Make dist to drainage raster:
dist2drainage <- terra::distance(template.rast, bhb.water.crop)
dist2drainage.km <- measurements::conv_unit(dist2drainage, "m", "km")

# CHECK FOR NA'S:


# Save rasters
#terra::writeRaster(bhb.landcover.rast, "data/processed/bhb_landcover.tif", overwrite=TRUE)
terra::writeRaster(bhb.shrub.rast, "data/processed/bhb_shrubland.tif", overwrite=TRUE)
terra::writeRaster(bhb.grassland.rast, "data/processed/bhb_grassland.tif", overwrite=TRUE)
terra::writeRaster(bhb.conifer.rast, "data/processed/bhb_conifer_mix.tif", overwrite=TRUE)
terra::writeRaster(bhb.broadleaf.rast, "data/processed/bhb_broadleaf_mix.tif", overwrite=TRUE)
terra::writeRaster(bhb.alpinemix.rast, "data/processed/bhb_alpine_mix.tif", overwrite=TRUE)
writeRaster(dist2drainage.km, "data/processed/dist2drainage_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.water.rast, "data/processed/bhb_drainage_areas.tif", overwrite=TRUE)
writeRaster(evergreen.500m, "data/processed/bhb_evergreen_500m.tif", overwrite = TRUE)
