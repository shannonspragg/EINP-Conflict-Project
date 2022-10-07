
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

# model 1 Beckman et al., 2015:
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
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")


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
dist2water.bhb <- terra::mask(dist2water, bhb.50km.v)
ghm.bhb <- terra::mask(human.development, bhb.50km.v)
ag.land.bhb <- terra::mask(ag.land, bhb.50km.v)


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
dist2water.bhw <- terra::mask(dist2water, bhw.v)
ghm.bhw <- terra::mask(human.development, bhw.v)
ag.land.bhw <- terra::mask(ag.land, bhw.v)


# Save these layers: ------------------------------------------------------

  # Variables with 50km buffer of BHW:
writeRaster(private.land.bhb, "data/processed/bhw_privateland_50km.tif")
writeRaster(elevation.bhb, "data/processed/bhw_elevation_50km.tif")
writeRaster(slope.bhb, "data/processed/bhw_slope_50km.tif")
writeRaster(roads.bhb, "data/processed/bhw_roads_50km.tif")
writeRaster(dist2roads.bhb, "data/processed/bhw_dist2roads_50km.tif")
writeRaster(pop.dens.bhb, "data/processed/bhw_popdens_50km.tif")
writeRaster(shrubland.bhb, "data/processed/bhw_shrubland_50km.tif")
writeRaster(grassland.bhb, "data/processed/bhw_grassland_50km.tif")
writeRaster(conifer.bhb, "data/processed/bhw_conifer_50km.tif")
writeRaster(broadleaf.bhb, "data/processed/bhw_broadleaf_50km.tif")
writeRaster(alpinemix.bhb, "data/processed/bhw_alpinemix_50km.tif")
writeRaster(water.bhb, "data/processed/bhw_waterways_50km.tif")
writeRaster(dist2water.bhb, "data/processed/bhw_dist2water_50km.tif")
writeRaster(ghm.bhb, "data/processed/bhw_ghm_50km.tif")
writeRaster(ag.land.bhb, "data/processed/bhw_agriculture_50km.tif")



  # Variables with BH watershed boundary:
writeRaster(private.land.bhw, "data/processed/bhw_privateland.tif")
writeRaster(elevation.bhw, "data/processed/bhw_elevation.tif")
writeRaster(slope.bhw, "data/processed/bhw_slope.tif")
writeRaster(roads.bhw, "data/processed/bhw_roads.tif")
writeRaster(dist2roads.bhw, "data/processed/bhw_dist2roads.tif")
writeRaster(pop.dens.bhw, "data/processed/bhw_popdens.tif")
writeRaster(shrubland.bhw, "data/processed/bhw_shrubland.tif")
writeRaster(grassland.bhw, "data/processed/bhw_grassland.tif")
writeRaster(conifer.bhw, "data/processed/bhw_conifer.tif")
writeRaster(broadleaf.bhw, "data/processed/bhw_broadleaf.tif")
writeRaster(alpinemix.bhw, "data/processed/bhw_alpinemix.tif")
writeRaster(water.bhw, "data/processed/bhw_waterways.tif")
writeRaster(dist2water.bhw, "data/processed/bhw_dist2water.tif")
writeRaster(ghm.bhw, "data/processed/bhw_human_mod.tif")
writeRaster(ag.land.bhw, "data/processed/bhw_agriculture.tif")
