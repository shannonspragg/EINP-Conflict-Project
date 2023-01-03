
# Crop & Save Individual Habitat Variables --------------------------------
  # Here we crop our variable rasters to the Beaver Hills Watershed boundary and 


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.watershed <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
roads <- rast("data/processed/bhb_roads_adjusted.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
coniferous.forest <- rast("data/processed/bhb_conifer_mix.tif")
broadleaf.forest <- rast("data/processed/bhb_broadleaf_mix.tif")
alpine.mixed.forest <- rast("data/processed/bhb_alpine_mix.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
dist2wb <- rast("data/processed/dist2waterbodies_km_bhb.tif")
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
protected.areas <- rast("data/processed/bhb_protected_areas.tif")
crownlands <- rast("data/processed/bhb_crownlands.tif")
fire_history <-rast("data/processed/bhb_fire_history.tif")

bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Mask layers to the BHW buffer and boundary line -------------------------

# Crop our rasters to the BH watershed 50km buffer shape:

private.land.bhb <- terra::mask(private.land.rast, bhb.50km.v)
elevation.bhb <- terra::mask(elevation, bhb.50km.v)
slope.bhb <- terra::mask(slope, bhb.50km.v)
roads.bhb <- terra::mask(roads, bhb.50km.v)
dist2roads.bhb <- terra::mask(dist2roads, bhb.50km.v)
pop.dens.bhb <- terra::mask(pop.dens, bhb.50km.v)
shrubland.bhb <- terra::mask(shrubland, bhb.50km.v)
grassland.bhb <- terra::mask(grassland, bhb.50km.v)
conifer.bhb <- terra::mask(coniferous.forest, bhb.50km.v)
broadleaf.bhb <- terra::mask(broadleaf.forest, bhb.50km.v)
alpinemix.bhb <- terra::mask(alpine.mixed.forest, bhb.50km.v)
water.bhb <- terra::mask(waterways, bhb.50km.v)
dist2wb.bhb <- terra::mask(dist2wb, bhb.50km.v)
dist2water.bhb <- terra::mask(dist2water, bhb.50km.v)
ghm.bhb <- terra::mask(human.development, bhb.50km.v)
ag.land.bhb <- terra::mask(ag.land, bhb.50km.v)
pas.bhb <- terra::mask(protected.areas, bhb.50km.v)
crown.bhb <- terra::mask(crownlands, bhb.50km.v)
fire.bhb <- terra::mask(fire_history, bhb.50km.v)

# Crop our rasters to the BH watershed BOUNDARY:

private.land.bhw <- terra::mask(private.land.rast, bhw.v)
elevation.bhw <- terra::mask(elevation, bhw.v)
slope.bhw <- terra::mask(slope, bhw.v)
roads.bhw <- terra::mask(roads, bhw.v)
dist2roads.bhw <- terra::mask(dist2roads, bhw.v)
pop.dens.bhw <- terra::mask(pop.dens, bhw.v)
shrubland.bhw <- terra::mask(shrubland, bhw.v)
grassland.bhw <- terra::mask(grassland, bhw.v)
conifer.bhw <- terra::mask(coniferous.forest, bhw.v)
broadleaf.bhw <- terra::mask(broadleaf.forest, bhw.v)
alpinemix.bhw <- terra::mask(alpine.mixed.forest, bhw.v)
water.bhw <- terra::mask(waterways, bhw.v)
dist2wb.bhw <- terra::mask(dist2wb, bhw.v)
dist2water.bhw <- terra::mask(dist2water, bhw.v)
ghm.bhw <- terra::mask(human.development, bhw.v)
ag.land.bhw <- terra::mask(ag.land, bhw.v)
pas.bhw <- terra::mask(protected.areas, bhw.v)
crown.bhw <- terra::mask(crownlands, bhw.v)
fire.bhw <- terra::mask(fire_history, bhw.v)

# Check layer names: ------------------------------------------------------

private.land.bhw
elevation.bhw # adjust name
slope.bhw
roads.bhw # adjust name
dist2roads.bhw # adjust name
pop.dens.bhw
shrubland.bhw
grassland.bhw
conifer.bhw
broadleaf.bhw
alpinemix.bhw
water.bhw
dist2water.bhw # adjust name
ghm.bhw # adjust name
ag.land.bhw
pas.bhw
crown.bhw
fire.bhw

names(elevation.bhw)[names(elevation.bhw) == "CAN_msk_alt"] <- "elevation_km"
names(roads.bhw)[names(roads.bhw) == "category"] <- "roads"
names(dist2roads.bhw)[names(dist2roads.bhw) == "lyr.1"] <- "dist_to_roads_km"
names(dist2water.bhw)[names(dist2water.bhw) == "lyr.1"] <- "dist_to_water_km"
names(dist2wb.bhw)[names(dist2wb.bhw) == "lyr.1"] <- "dist_to_waterbodies_km"
names(ghm.bhw)[names(ghm.bhw) == "constant"] <- "human_modification"
names(pas.bhw)[names(pas.bhw) == "NAME_E"] <- "protected_areas"

names(elevation.bhb)[names(elevation.bhb) == "CAN_msk_alt"] <- "elevation_km"
names(roads.bhb)[names(roads.bhb) == "category"] <- "roads"
names(dist2roads.bhb)[names(dist2roads.bhb) == "lyr.1"] <- "dist_to_roads_km"
names(dist2wb.bhb)[names(dist2wb.bhb) == "lyr.1"] <- "dist_to_waterbodies_km"
names(dist2water.bhb)[names(dist2water.bhb) == "lyr.1"] <- "dist_to_water_km"
names(ghm.bhb)[names(ghm.bhb) == "constant"] <- "human_modification"
names(pas.bhb)[names(pas.bhb) == "NAME_E"] <- "protected_areas"


# Stack & plot together: --------------------------------------------------

hab.variables <- c(private.land.bhw, elevation.bhw, slope.bhw, roads.bhw, dist2roads.bhw, pop.dens.bhw, shrubland.bhw, grassland.bhw,
                   conifer.bhw, broadleaf.bhw, alpinemix.bhw, water.bhw, dist2water.bhw, ghm.bhw, ag.land.bhw)

hab.variables
plot(hab.variables)

# Save these layers: ------------------------------------------------------

  # Variables with 50km buffer of BHW:
writeRaster(private.land.bhb, "data/processed/bhw_privateland_50km.tif", overwrite=TRUE)
writeRaster(elevation.bhb, "data/processed/bhw_elevation_50km.tif", overwrite=TRUE)
writeRaster(slope.bhb, "data/processed/bhw_slope_50km.tif", overwrite=TRUE)
writeRaster(roads.bhb, "data/processed/bhw_roads_50km.tif", overwrite=TRUE)
writeRaster(dist2roads.bhb, "data/processed/bhw_dist2roads_50km.tif", overwrite=TRUE)
writeRaster(pop.dens.bhb, "data/processed/bhw_popdens_50km.tif", overwrite=TRUE)
writeRaster(shrubland.bhb, "data/processed/bhw_shrubland_50km.tif", overwrite=TRUE)
writeRaster(grassland.bhb, "data/processed/bhw_grassland_50km.tif", overwrite=TRUE)
writeRaster(conifer.bhb, "data/processed/bhw_conifer_50km.tif", overwrite=TRUE)
writeRaster(broadleaf.bhb, "data/processed/bhw_broadleaf_50km.tif", overwrite=TRUE)
writeRaster(alpinemix.bhb, "data/processed/bhw_alpinemix_50km.tif", overwrite=TRUE)
writeRaster(water.bhb, "data/processed/bhw_waterways_50km.tif", overwrite=TRUE)
writeRaster(dist2water.bhb, "data/processed/bhw_dist2water_50km.tif", overwrite=TRUE)
writeRaster(dist2wb.bhb, "data/processed/bhw_dist2waterbodies_50km.tif", overwrite=TRUE)
writeRaster(ghm.bhb, "data/processed/bhw_ghm_50km.tif", overwrite=TRUE)
writeRaster(ag.land.bhb, "data/processed/bhw_agriculture_50km.tif", overwrite=TRUE)
writeRaster(pas.bhb, "data/processed/bhw_protected_areas_50km.tif", overwrite=TRUE)
writeRaster(crown.bhb, "data/processed/bhw_crownlands_50km.tif", overwrite=TRUE)
writeRaster(fire.bhb, "data/processed/bhw_fire_history_50km.tif", overwrite=TRUE)


  # Variables with BH watershed boundary:
writeRaster(private.land.bhw, "data/processed/bhw_privateland.tif", overwrite=TRUE)
writeRaster(elevation.bhw, "data/processed/bhw_elevation.tif", overwrite=TRUE)
writeRaster(slope.bhw, "data/processed/bhw_slope.tif", overwrite=TRUE)
writeRaster(roads.bhw, "data/processed/bhw_roads.tif", overwrite=TRUE)
writeRaster(dist2roads.bhw, "data/processed/bhw_dist2roads.tif", overwrite=TRUE)
writeRaster(pop.dens.bhw, "data/processed/bhw_popdens.tif", overwrite=TRUE)
writeRaster(shrubland.bhw, "data/processed/bhw_shrubland.tif", overwrite=TRUE)
writeRaster(grassland.bhw, "data/processed/bhw_grassland.tif", overwrite=TRUE)
writeRaster(conifer.bhw, "data/processed/bhw_conifer.tif", overwrite=TRUE)
writeRaster(broadleaf.bhw, "data/processed/bhw_broadleaf.tif", overwrite=TRUE)
writeRaster(alpinemix.bhw, "data/processed/bhw_alpinemix.tif", overwrite=TRUE)
writeRaster(water.bhw, "data/processed/bhw_waterways.tif", overwrite=TRUE)
writeRaster(dist2water.bhw, "data/processed/bhw_dist2water.tif", overwrite=TRUE)
writeRaster(dist2wb.bhw, "data/processed/bhw_dist2waterbodies.tif", overwrite=TRUE)
writeRaster(ghm.bhw, "data/processed/bhw_human_mod.tif", overwrite=TRUE)
writeRaster(ag.land.bhw, "data/processed/bhw_agriculture.tif", overwrite=TRUE)
writeRaster(pas.bhw, "data/processed/bhw_protected_areas.tif", overwrite=TRUE)
writeRaster(crown.bhw, "data/processed/bhw_crownlands.tif", overwrite=TRUE)
writeRaster(fire.bhw, "data/processed/bhw_fire_history.tif", overwrite=TRUE)
