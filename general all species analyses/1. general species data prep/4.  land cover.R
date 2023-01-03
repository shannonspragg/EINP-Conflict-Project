
# Prep Land Use Data ----------------------------------------------------
# Filter to different land cover types


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)
library(raster)

# Load Land cover data -------------------------------------------------------------

ab_landcover <- st_read("data/original/Lancover_Polygons_2010.shp")
wb <- raster("data/processed/bhb_50km_waterbodies.tif")
wb.bhb <- st_read("data/original/waterbody_2.shp")

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
                     ab_landcover$LC_class == 220 ~ "Broadleaf Forest", # birch, oak and aspen
                     ab_landcover$LC_class == 230 ~ "Mixed Forest", # alpine mixed forest
  ))

# Subset out shrubland and other types:
shrubland <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Shrubland")
grassland <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Grassland")
conifer.mix <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Coniferous Forest")
broadleaf <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Broadleaf Forest")
alpine.mixed <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Mixed Forest")
water <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Water")
agriculture <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Agriculture")
forest.cover <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Coniferous Forest" | ab_landcover$LC_DESCRIPTION == "Broadleaf Forest" | ab_landcover$LC_DESCRIPTION == "Mixed Forest")
exposed <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Exposed Land")
glacial <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Snow/Ice")
rocky <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Rock/Rubble")
developed <- ab_landcover %>% filter(ab_landcover$LC_DESCRIPTION == "Developed")

# Subset forest, alpine, shrub, grassland for generalist species resistance:
generalist_habitat <- dplyr::filter(ab_landcover, LC_DESCRIPTION == "Shrubland" | LC_DESCRIPTION == "Grassland" | LC_DESCRIPTION == "Coniferous Forest"
                                    | LC_DESCRIPTION == "Broadleaf Forest" | LC_DESCRIPTION == "Mixed Forest")

# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_50km.shp") # Beaver Hills Watershed

bhb.reproj<- st_transform(bhb.buf, st_crs(ab_landcover))

#st_crs(ab_landcover) == st_crs(bhb.reproj)
# st_is_valid(bhw.reproj)
# st_is_valid(ab_landcover)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif")

## Set up a raster "template" for alberta
temp.rast.ab <- rast(res=c(250,250), ext=ext(landcover.v)) # Let's do a 250x250 res for computational purposes
crs(temp.rast.ab) <- "epsg:32612" # UTM zone 12N for AB
values(temp.rast.ab) <- rep(1, ncell(temp.rast.ab))

# Make spat vectors
bhb.v <- vect(bhb.buf)
landcover.v <- vect(ab_landcover)
shrubland.v <- vect(shrubland)
grassland.v <- vect(grassland)
conifer.v <- vect(conifer.mix)
broadleaf.v <- vect(broadleaf)
alpinemix.v <- vect(alpine.mixed)
water.v <- vect(water)
agriculture.v <- vect(agriculture)
generalist.v <- vect(generalist_habitat)
wb.v <- vect(wb.bhb)
forest.v <- vect(forest.cover)
exposed.v <- vect(exposed)
glacial.v <- vect(glacial)
rocky.v <- vect(rocky)
developed.v <- vect(developed) 

bhb.landcover.crop <- crop(landcover.v, template.rast)
bhb.shrub.crop <- crop(shrubland.v, template.rast)
bhb.grass.crop <- crop(grassland.v, template.rast)
bhb.conifer.crop <- crop(conifer.v, template.rast)
bhb.broadleaf.crop <- crop(broadleaf.v, template.rast)
bhb.alpine.crop <- crop(alpinemix.v, template.rast)
bhb.water.crop <- crop(water.v, template.rast)
bhb.ag.crop <- crop(agriculture.v, template.rast)
bhb.generalist.crop <- crop(generalist.v, template.rast)
bhb.forest.crop <- crop(forest.v, template.rast)
bhb.exposed.crop <- crop(exposed.v, template.rast)
bhb.glacial.crop <- crop(glacial.v, template.rast)
bhb.rocky.crop <- crop(rocky.v, template.rast)
bhb.dev.crop <- crop(developed.v, template.rast)

# Rasterize each cover type
ab.landcover.rast <- terra::rasterize(landcover.v, temp.rast.ab, field = "LC_DESCRIPTION")
bhb.landcover.type.rast <- terra::rasterize(bhb.landcover.crop, template.rast, field = "LC_DESCRIPTION")
bhb.shrubland.rast <- terra::rasterize(bhb.shrub.crop, template.rast, field = "LC_class")
bhb.grassland.rast <- terra::rasterize(bhb.grass.crop, template.rast, field = "LC_class")
bhb.conifer.rast <- terra::rasterize(bhb.conifer.crop, template.rast, field = "LC_class")
bhb.broadleaf.rast <- terra::rasterize(bhb.broadleaf.crop, template.rast, field = "LC_class")
bhb.alpinemix.rast <- terra::rasterize(bhb.alpine.crop, template.rast, field = "LC_class")
bhb.water.rast <- terra::rasterize(bhb.water.crop, template.rast, field = "LC_class")
bhb.ag.rast <- terra::rasterize(bhb.ag.crop, template.rast, field = "LC_class")
bhb.generalist.rast <- terra::rasterize(bhb.generalist.crop, template.rast, field = "LC_class")
bhb.forest.rast <- terra::rasterize(bhb.forest.crop, template.rast, field = "LC_class")
bhb.exposed.rast <- terra::rasterize(bhb.exposed.crop, template.rast, field = "LC_class")
bhb.glacial.rast <- terra::rasterize(bhb.glacial.crop, template.rast, field = "LC_class")
bhb.rocky.rast <- terra::rasterize(bhb.rocky.crop, template.rast, field = "LC_class")
bhb.dev.rast <- terra::rasterize(bhb.dev.crop, template.rast, field = "LC_class")

# Make shrubland a continuous raster:
bhb.shrubland.rast[bhb.shrubland.rast == 50] <- 1
bhb.shrubland.raster <- raster(bhb.shrubland.rast)
bhb.shrubland.raster[is.na(bhb.shrubland.raster[])] <- 0 
names(bhb.shrubland.raster)[names(bhb.shrubland.raster) == "LC_class"] <- "shrubland"

# Make grassland a continuous raster:
bhb.grassland.rast[bhb.grassland.rast == 110] <- 1
bhb.grassland.raster <- raster(bhb.grassland.rast)
bhb.grassland.raster[is.na(bhb.grassland.raster[])] <- 0 

names(bhb.grassland.raster)[names(bhb.grassland.raster) == "LC_class"] <- "grassland"

# Make conifer a continuous raster:
bhb.conifer.rast[bhb.conifer.rast == 210] <- 1
bhb.conifer.raster <- raster(bhb.conifer.rast)
bhb.conifer.raster[is.na(bhb.conifer.raster[])] <- 0 

names(bhb.conifer.raster)[names(bhb.conifer.raster) == "LC_class"] <- "coniferous_forest"

# Make broadleaf a continuous raster:
bhb.broadleaf.rast[bhb.broadleaf.rast == 220] <- 1
bhb.broadleaf.raster <- raster(bhb.broadleaf.rast)
bhb.broadleaf.raster[is.na(bhb.broadleaf.raster[])] <- 0 

names(bhb.broadleaf.raster)[names(bhb.broadleaf.raster) == "LC_class"] <- "broadleaf_forest"

# Make alpine mix a continuous raster:
bhb.alpinemix.rast[bhb.alpinemix.rast == 230] <- 1
bhb.alpinemix.raster <- raster(bhb.alpinemix.rast)
bhb.alpinemix.raster[is.na(bhb.alpinemix.raster[])] <- 0 

names(bhb.alpinemix.raster)[names(bhb.alpinemix.raster) == "LC_class"] <- "alpine_mixed_forest"


# Make a water / riparian raster:
bhb.water.rast[bhb.water.rast == 20] <- 1
bhb.water.raster <- raster(bhb.water.rast)
bhb.water.raster[is.na(bhb.water.raster[])] <- 0 

bhb.water <- wb + bhb.water.raster
bhb.water.r <- rast(bhb.water)
bhb.water.r[bhb.water.r == 2] <- 1
names(bhb.water.r)[names(bhb.water.r) == "layer"] <- "water"
bhb.water.raster <- raster(bhb.water.r)

wb.p <- wb.bhb %>%
  st_transform(., crs=crs(bhb.buf))
wb.v <- vect(wb.p)
bhb.wb.crop <- crop(wb.v, template.rast)

# Make dist to drainage raster:
dist2drainage <- terra::distance(template.rast, bhb.water.crop) 
dist2drainage.km <- measurements::conv_unit(dist2drainage, "m", "km")

# Make dist to waterbodies raster:
dist2waterbodies <- terra::distance(template.rast, bhb.wb.crop) 
dist2waterbodies.km <- measurements::conv_unit(dist2waterbodies, "m", "km")

# Make a agriculture raster:
bhb.ag.rast[bhb.ag.rast == 120] <- 1
bhb.ag.raster <- raster(bhb.ag.rast)
bhb.ag.raster[is.na(bhb.ag.raster[])] <- 0 

# Make generalist a continuous raster:
bhb.generalist.rast[bhb.generalist.rast >= 50] <- 1
bhb.generalist.raster <- raster(bhb.generalist.rast)
bhb.generalist.raster[is.na(bhb.generalist.raster[])] <- 0 

# Make exposed land a continuous raster:
bhb.exposed.rast[bhb.exposed.rast == 33] <- 1
bhb.exposed.raster <- raster(bhb.exposed.rast)
bhb.exposed.raster[is.na(bhb.exposed.raster[])] <- 0 
names(bhb.exposed.raster)[names(bhb.exposed.raster) == "LC_class"] <- "exposed"

# Make glacial land a continuous raster:
bhb.glacial.rast[bhb.glacial.rast == 31] <- 1
bhb.glacial.raster <- raster(bhb.glacial.rast)
bhb.glacial.raster[is.na(bhb.glacial.raster[])] <- 0 
names(bhb.glacial.raster)[names(bhb.glacial.raster) == "LC_class"] <- "glacial"

# Make rocky land a continuous raster:
bhb.rocky.rast[bhb.rocky.rast == 32] <- 1
bhb.rocky.raster <- raster(bhb.rocky.rast)
bhb.rocky.raster[is.na(bhb.rocky.raster[])] <- 0 
names(bhb.rocky.raster)[names(bhb.rocky.raster) == "LC_class"] <- "rocky"

# Make developed land a continuous raster:
bhb.dev.rast[bhb.dev.rast == 34] <- 1
bhb.dev.raster <- raster(bhb.dev.rast)
bhb.dev.raster[is.na(bhb.dev.raster[])] <- 0 
names(bhb.dev.raster)[names(bhb.dev.raster) == "LC_class"] <- "developed"

# CHECK FOR NA'S:
table(is.na(bhb.generalist.raster[])) # FALSE

# Save rasters
terra::writeRaster(ab.landcover.rast, "data/processed/ab_landcover.tif", overwrite=TRUE)
terra::writeRaster(bhb.landcover.type.rast, "data/processed/bhb_landcover.tif", overwrite=TRUE)
terra::writeRaster(bhb.shrubland.raster, "data/processed/bhb_shrubland.tif", overwrite=TRUE)
terra::writeRaster(bhb.grassland.raster, "data/processed/bhb_grassland.tif", overwrite=TRUE)
terra::writeRaster(bhb.conifer.raster, "data/processed/bhb_conifer_mix.tif", overwrite=TRUE)
terra::writeRaster(bhb.broadleaf.raster, "data/processed/bhb_broadleaf_mix.tif", overwrite=TRUE)
terra::writeRaster(bhb.alpinemix.raster, "data/processed/bhb_alpine_mix.tif", overwrite=TRUE)
writeRaster(bhb.water.r, "data/processed/bhb_water_areas.tif", overwrite=TRUE)
writeRaster(bhb.ag.raster, "data/processed/bhb_agriculture.tif", overwrite=TRUE)
writeRaster(bhb.generalist.raster, "data/processed/bhb_generalist_lc.tif", overwrite = TRUE)
terra::writeRaster(bhb.forest.raster, "data/processed/bhb_forest_land.tif", overwrite=TRUE)
terra::writeRaster(bhb.exposed.raster, "data/processed/bhb_exposed_land.tif", overwrite=TRUE)
terra::writeRaster(bhb.glacial.raster, "data/processed/bhb_glacial_land.tif", overwrite=TRUE)
terra::writeRaster(bhb.rocky.raster, "data/processed/bhb_rocky_land.tif", overwrite=TRUE)
terra::writeRaster(bhb.dev.raster, "data/processed/bhb_developed_land.tif", overwrite=TRUE)

writeRaster(dist2drainage.km, "data/processed/dist2drainage_km_bhb.tif", overwrite=TRUE)
writeRaster(dist2waterbodies.km, "data/processed/dist2waterbodies_km_bhb.tif", overwrite=TRUE)
