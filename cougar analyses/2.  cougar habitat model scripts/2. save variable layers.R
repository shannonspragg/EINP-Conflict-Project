
# Crop & Save Individual Habitat Variables --------------------------------
  # Here we crop our variable rasters to the Beaver Hills Watershed boundary and save them for individual mapping purposes


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
forests <- rast("data/processed/bhb_forest_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
glacial <- rast("data/processed/bhb_glacial_land.tif")
rocky <- rast("data/processed/bhb_rocky_land.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
protected.areas <- rast("data/processed/bhb_protected_areas.tif")
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
fire_history <-rast("data/processed/bhb_fire_history.tif")
livestock.density <- rast("data/processed/animal_production_density_raster.tif")
ungulate.density <- rast("data/processed/total_ungulate_density.tif")
road.density <- rast("data/processed/bhb_road_density_250m.tif")

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
forests.bhb <- terra::mask(forests, bhb.50km.v)
exposed.bhb <- terra::mask(exposed, bhb.50km.v)
glacial.bhb <- terra::mask(glacial, bhb.50km.v)
rocky.bhb <- terra::mask(rocky, bhb.50km.v)
water.bhb <- terra::mask(waterways, bhb.50km.v)
dist2water.bhb <- terra::mask(dist2water, bhb.50km.v)
ghm.bhb <- terra::mask(human.development, bhb.50km.v)
ag.land.bhb <- terra::mask(ag.land, bhb.50km.v)
pas.bhb <- terra::mask(protected.areas, bhb.50km.v)
dist2pas.bhb <- terra::mask(dist2pa.rast, bhb.50km.v)
livestock.dens.bhb <- terra::mask(livestock.density, bhb.50km.v)
fire.bhb <- terra::mask(fire_history, bhb.50km.v)
ungulate.dens.bhb <- terra::mask(ungulate.density, bhb.50km.v)
road.dens.bhb <- terra::mask(road.density, bhb.50km.v)

# Crop our rasters to the BH watershed BOUNDARY:

private.land.bhw <- terra::mask(private.land.rast, bhw.v)
elevation.bhw <- terra::mask(elevation, bhw.v)
slope.bhw <- terra::mask(slope, bhw.v)
roads.bhw <- terra::mask(roads, bhw.v)
dist2roads.bhw <- terra::mask(dist2roads, bhw.v)
pop.dens.bhw <- terra::mask(pop.dens, bhw.v)
shrubland.bhw <- terra::mask(shrubland, bhw.v)
grassland.bhw <- terra::mask(grassland, bhw.v)
forests.bhw <- terra::mask(forests, bhw.v)
exposed.bhw <- terra::mask(exposed, bhw.v)
glacial.bhw <- terra::mask(glacial, bhw.v)
rocky.bhw <- terra::mask(rocky, bhw.v)
water.bhw <- terra::mask(waterways, bhw.v)
dist2water.bhw <- terra::mask(dist2water, bhw.v)
ghm.bhw <- terra::mask(human.development, bhw.v)
ag.land.bhw <- terra::mask(ag.land, bhw.v)
livestock.dens.bhw <- terra::mask(livestock.density, bhw.v)
pas.bhw <- terra::mask(protected.areas, bhw.v)
dist2pas.bhw <- terra::mask(dist2pa.rast, bhw.v)
fire.bhw <- terra::mask(fire_history, bhw.v)
ungulate.dens.bhw <- terra::mask(ungulate.density, bhw.v)
road.dens.bhw <- terra::mask(road.density, bhw.v)

# Check layer names: ------------------------------------------------------

private.land.bhw
elevation.bhw # adjust name
slope.bhw
roads.bhw # adjust name
dist2roads.bhw # adjust name
pop.dens.bhw
shrubland.bhw
grassland.bhw
forests.bhw
water.bhw
dist2water.bhw # adjust name
ghm.bhw # adjust name
ag.land.bhw
pas.bhw
dist2pas.bhw
fire.bhw
ungulate.dens.bhw
road.dens.bhw

names(elevation.bhw)[names(elevation.bhw) == "CAN_msk_alt"] <- "elevation_km"
names(roads.bhw)[names(roads.bhw) == "category"] <- "roads"
names(dist2roads.bhw)[names(dist2roads.bhw) == "lyr.1"] <- "dist_to_roads_km"
names(dist2water.bhw)[names(dist2water.bhw) == "lyr.1"] <- "dist_to_water_km"
names(ghm.bhw)[names(ghm.bhw) == "constant"] <- "human_modification"
names(dist2pas.bhw)[names(dist2pas.bhw) == "lyr.1"] <- "dist_to_protected_areas_km"
names(pas.bhw)[names(pas.bhw) == "NAME_E"] <- "protected_areas"
names(road.dens.bhw)[names(road.dens.bhw) == "layer"] <- "road density"

names(elevation.bhb)[names(elevation.bhb) == "CAN_msk_alt"] <- "elevation_km"
names(roads.bhb)[names(roads.bhb) == "category"] <- "roads"
names(dist2roads.bhb)[names(dist2roads.bhb) == "lyr.1"] <- "dist_to_roads_km"
names(dist2water.bhb)[names(dist2water.bhb) == "lyr.1"] <- "dist_to_water_km"
names(ghm.bhb)[names(ghm.bhb) == "constant"] <- "human_modification"
names(dist2pas.bhb)[names(dist2pas.bhb) == "lyr.1"] <- "dist_to_protected_areas_km"
names(pas.bhb)[names(pas.bhb) == "NAME_E"] <- "protected_areas"
names(road.dens.bbw)[names(road.dens.bhb) == "layer"] <- "road density"


# Stack & plot together: --------------------------------------------------

hab.variables <- c(private.land.bhw, elevation.bhw, slope.bhw, road.dens.bhw, ungulate.dens.bhw, dist2roads.bhw, pop.dens.bhw, shrubland.bhw, grassland.bhw,
                   forests.bhw, exposed.bhw, glacial.bhw, rocky.bhw, water.bhw, dist2water.bhw, ghm.bhw, ag.land.bhw, livestock.dens.bhw, fire.bhw, pas.bhw, dist2pas.bhw)

hab.variables
plot(hab.variables)

# Save these layers: ------------------------------------------------------

  # Variables with 50km buffer of BHW:
writeRaster(private.land.bhb, "data/processed/bhw_privateland_50km.tif", overwrite=TRUE)
writeRaster(elevation.bhb, "data/processed/bhw_elevation_50km.tif", overwrite=TRUE)
writeRaster(slope.bhb, "data/processed/bhw_slope_50km.tif", overwrite=TRUE)
writeRaster(road.dens.bhb, "data/processed/bhw_road_density_50km.tif", overwrite=TRUE)
writeRaster(dist2roads.bhb, "data/processed/bhw_dist2roads_50km.tif", overwrite=TRUE)
writeRaster(pop.dens.bhb, "data/processed/bhw_popdens_50km.tif", overwrite=TRUE)
writeRaster(shrubland.bhb, "data/processed/bhw_shrubland_50km.tif", overwrite=TRUE)
writeRaster(grassland.bhb, "data/processed/bhw_grassland_50km.tif", overwrite=TRUE)
writeRaster(forests.bhb, "data/processed/bhw_forests_50km.tif", overwrite=TRUE)
writeRaster(exposed.bhb, "data/processed/bhw_exposedland_50km.tif", overwrite=TRUE)
writeRaster(glacial.bhb, "data/processed/bhw_glacial_50km.tif", overwrite=TRUE)
writeRaster(rocky.bhb, "data/processed/bhw_rocky_50km.tif", overwrite=TRUE)
writeRaster(water.bhb, "data/processed/bhw_waterways_50km.tif", overwrite=TRUE)
writeRaster(dist2water.bhb, "data/processed/bhw_dist2water_50km.tif", overwrite=TRUE)
writeRaster(ghm.bhb, "data/processed/bhw_ghm_50km.tif", overwrite=TRUE)
writeRaster(ag.land.bhb, "data/processed/bhw_agriculture_50km.tif", overwrite=TRUE)
writeRaster(livestock.dens.bhb, "data/processed/bhw_livestock_density_50km.tif", overwrite=TRUE)
writeRaster(pas.bhb, "data/processed/bhw_protected_areas_50km.tif", overwrite=TRUE)
writeRaster(dist2pas.bhb, "data/processed/bhw_dist2_protected_areas_50km.tif", overwrite=TRUE)
writeRaster(ungulate.dens.bhb, "data/processed/bhw_ungulate_density_50km.tif", overwrite=TRUE)
writeRaster(fire.bhb, "data/processed/bhw_fire_history_50km.tif", overwrite=TRUE)


  # Variables with BH watershed boundary:
writeRaster(private.land.bhw, "data/processed/bhw_privateland.tif", overwrite=TRUE)
writeRaster(elevation.bhw, "data/processed/bhw_elevation.tif", overwrite=TRUE)
writeRaster(slope.bhw, "data/processed/bhw_slope.tif", overwrite=TRUE)
writeRaster(road.dens.bhw, "data/processed/bhw_road_density.tif", overwrite=TRUE)
writeRaster(dist2roads.bhw, "data/processed/bhw_dist2roads.tif", overwrite=TRUE)
writeRaster(pop.dens.bhw, "data/processed/bhw_popdens.tif", overwrite=TRUE)
writeRaster(shrubland.bhw, "data/processed/bhw_shrubland.tif", overwrite=TRUE)
writeRaster(grassland.bhw, "data/processed/bhw_grassland.tif", overwrite=TRUE)
writeRaster(forests.bhw, "data/processed/bhw_forests.tif", overwrite=TRUE)
writeRaster(exposed.bhw, "data/processed/bhw_exposedland.tif", overwrite=TRUE)
writeRaster(glacial.bhw, "data/processed/bhw_glacial.tif", overwrite=TRUE)
writeRaster(rocky.bhw, "data/processed/bhw_rocky.tif", overwrite=TRUE)
writeRaster(water.bhw, "data/processed/bhw_waterways.tif", overwrite=TRUE)
writeRaster(dist2water.bhw, "data/processed/bhw_dist2water.tif", overwrite=TRUE)
writeRaster(ghm.bhw, "data/processed/bhw_human_mod.tif", overwrite=TRUE)
writeRaster(ag.land.bhw, "data/processed/bhw_agriculture.tif", overwrite=TRUE)
writeRaster(pas.bhw, "data/processed/bhw_protected_areas.tif", overwrite=TRUE)
writeRaster(livestock.dens.bhw, "data/processed/bhw_livestock_density.tif", overwrite=TRUE)
writeRaster(dist2pas.bhw, "data/processed/bhw_dist2_protected_areas.tif", overwrite=TRUE)
writeRaster(ungulate.dens.bhw, "data/processed/bhw_ungulate_density.tif", overwrite=TRUE)
writeRaster(fire.bhw, "data/processed/bhw_fire_history.tif", overwrite=TRUE)
