
# Crop & Save Individual Habitat Variables --------------------------------
  # Here we crop our variable rasters to the Beaver Hills wetlandshed boundary and save them for individual mapping purposes


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.wetlandshed <- st_read("data/original/BHB_Subwetlandshed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

forest.edge.rast <- rast("data/processed/forest_edge_habitats.tif")
ruggedness <- rast("data/processed/terrain_ruggedness_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
forests <- rast("data/processed/bhb_forest_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
developed <- rast("data/processed/bhb_developed_land.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
pipe.dens <- rast("data/processed/bhb_pipeline_density_250m.tif")
wetland <- rast("data/processed/bhb_wetland_areas.tif")
dist2wetlands <- rast("data/processed/dist2wetlandbodies_km_bhb.tif")
human.mod <- rast("data/processed/bhw_ghm.tif")
ungulate.density <- rast("data/processed/total_ungulate_density.tif")

bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.wetlandshed)

# Mask layers to the BHW buffer and boundary line -------------------------

# Crop our rasters to the BH wetlandshed 50km buffer shape:

forest.edge.bhb <- terra::mask(forest.edge.rast, bhb.50km.v)
ruggedness.bhb <- terra::mask(ruggedness, bhb.50km.v)
slope.bhb <- terra::mask(slope, bhb.50km.v)
roads.bhb <- terra::mask(roads, bhb.50km.v)
dist2roads.bhb <- terra::mask(dist2roads, bhb.50km.v)
pop.dens.bhb <- terra::mask(pop.dens, bhb.50km.v)
shrubland.bhb <- terra::mask(shrubland, bhb.50km.v)
grassland.bhb <- terra::mask(grassland, bhb.50km.v)
forests.bhb <- terra::mask(forests, bhb.50km.v)
exposed.bhb <- terra::mask(exposed, bhb.50km.v)
pipe.dens.bhb <- terra::mask(pipe.dens, bhb.50km.v)
developed.bhb <- terra::mask(developed, bhb.50km.v)
wetland.bhb <- terra::mask(wetlandways, bhb.50km.v)
dist2wetland.bhb <- terra::mask(dist2wetland, bhb.50km.v)
ghm.bhb <- terra::mask(human.development, bhb.50km.v)
ag.land.bhb <- terra::mask(ag.land, bhb.50km.v)
ungulate.dens.bhb <- terra::mask(ungulate.density, bhb.50km.v)
road.dens.bhb <- terra::mask(road.density, bhb.50km.v)

# Crop our rasters to the BH wetlandshed BOUNDARY:

forest.edge.bhw <- terra::mask(forest.edge.rast, bhw.v)
ruggedness.bhw <- terra::mask(ruggedness, bhw.v)
slope.bhw <- terra::mask(slope, bhw.v)
roads.bhw <- terra::mask(roads, bhw.v)
dist2roads.bhw <- terra::mask(dist2roads, bhw.v)
pop.dens.bhw <- terra::mask(pop.dens, bhw.v)
shrubland.bhw <- terra::mask(shrubland, bhw.v)
grassland.bhw <- terra::mask(grassland, bhw.v)
forests.bhw <- terra::mask(forests, bhw.v)
exposed.bhw <- terra::mask(exposed, bhw.v)
pipe.dens.bhw <- terra::mask(pipe.dens, bhw.v)
developed.bhw <- terra::mask(developed, bhw.v)
wetland.bhw <- terra::mask(wetlandways, bhw.v)
dist2wetland.bhw <- terra::mask(dist2wetland, bhw.v)
ghm.bhw <- terra::mask(human.development, bhw.v)
ag.land.bhw <- terra::mask(ag.land, bhw.v)
ungulate.dens.bhw <- terra::mask(ungulate.density, bhw.v)
road.dens.bhw <- terra::mask(road.density, bhw.v)

# Check layer names: ------------------------------------------------------

forest.edge.bhw
ruggedness.bhw # adjust name
slope.bhw
roads.bhw # adjust name
dist2roads.bhw # adjust name
pop.dens.bhw
shrubland.bhw
grassland.bhw
forests.bhw
wetland.bhw
dist2wetland.bhw # adjust name
ghm.bhw # adjust name
ag.land.bhw
ungulate.dens.bhw
road.dens.bhw

names(ruggedness.bhw)[names(ruggedness.bhw) == "CAN_msk_alt"] <- "ruggedness_km"
names(roads.bhw)[names(roads.bhw) == "category"] <- "roads"
names(dist2roads.bhw)[names(dist2roads.bhw) == "lyr.1"] <- "dist_to_roads_km"
names(dist2wetland.bhw)[names(dist2wetland.bhw) == "lyr.1"] <- "dist_to_wetland_km"
names(ghm.bhw)[names(ghm.bhw) == "constant"] <- "human_modification"
names(road.dens.bhw)[names(road.dens.bhw) == "layer"] <- "road density"

names(ruggedness.bhb)[names(ruggedness.bhb) == "CAN_msk_alt"] <- "ruggedness_km"
names(roads.bhb)[names(roads.bhb) == "category"] <- "roads"
names(dist2roads.bhb)[names(dist2roads.bhb) == "lyr.1"] <- "dist_to_roads_km"
names(dist2wetland.bhb)[names(dist2wetland.bhb) == "lyr.1"] <- "dist_to_wetland_km"
names(ghm.bhb)[names(ghm.bhb) == "constant"] <- "human_modification"
names(road.dens.bbw)[names(road.dens.bhb) == "layer"] <- "road density"


# Stack & plot together: --------------------------------------------------

hab.variables <- c(forest.edge.bhw, ruggedness.bhw, slope.bhw, road.dens.bhw, ungulate.dens.bhw, dist2roads.bhw, pop.dens.bhw, shrubland.bhw, grassland.bhw,
                   forests.bhw, exposed.bhw, pipe.dens.bhw, developed.bhw, wetland.bhw, dist2wetland.bhw, ghm.bhw, ag.land.bhw)

hab.variables
plot(hab.variables)

# Save these layers: ------------------------------------------------------

  # Variables with 50km buffer of BHW:
writeRaster(forest.edge.bhb, "data/processed/bhw_privateland_50km.tif", overwrite=TRUE)
writeRaster(ruggedness.bhb, "data/processed/bhw_ruggedness_50km.tif", overwrite=TRUE)
writeRaster(slope.bhb, "data/processed/bhw_slope_50km.tif", overwrite=TRUE)
writeRaster(road.dens.bhb, "data/processed/bhw_road_density_50km.tif", overwrite=TRUE)
writeRaster(dist2roads.bhb, "data/processed/bhw_dist2roads_50km.tif", overwrite=TRUE)
writeRaster(pop.dens.bhb, "data/processed/bhw_popdens_50km.tif", overwrite=TRUE)
writeRaster(shrubland.bhb, "data/processed/bhw_shrubland_50km.tif", overwrite=TRUE)
writeRaster(grassland.bhb, "data/processed/bhw_grassland_50km.tif", overwrite=TRUE)
writeRaster(forests.bhb, "data/processed/bhw_forests_50km.tif", overwrite=TRUE)
writeRaster(exposed.bhb, "data/processed/bhw_exposedland_50km.tif", overwrite=TRUE)
writeRaster(pipe.dens.bhb, "data/processed/bhw_pipe.dens_50km.tif", overwrite=TRUE)
writeRaster(developed.bhb, "data/processed/bhw_developed_50km.tif", overwrite=TRUE)
writeRaster(wetland.bhb, "data/processed/bhw_wetlandways_50km.tif", overwrite=TRUE)
writeRaster(dist2wetland.bhb, "data/processed/bhw_dist2wetland_50km.tif", overwrite=TRUE)
writeRaster(ghm.bhb, "data/processed/bhw_ghm_50km.tif", overwrite=TRUE)
writeRaster(ag.land.bhb, "data/processed/bhw_agriculture_50km.tif", overwrite=TRUE)
writeRaster(ungulate.dens.bhb, "data/processed/bhw_ungulate_density_50km.tif", overwrite=TRUE)


  # Variables with BH wetlandshed boundary:
writeRaster(forest.edge.bhw, "data/processed/bhw_privateland.tif", overwrite=TRUE)
writeRaster(ruggedness.bhw, "data/processed/bhw_ruggedness.tif", overwrite=TRUE)
writeRaster(slope.bhw, "data/processed/bhw_slope.tif", overwrite=TRUE)
writeRaster(road.dens.bhw, "data/processed/bhw_road_density.tif", overwrite=TRUE)
writeRaster(dist2roads.bhw, "data/processed/bhw_dist2roads.tif", overwrite=TRUE)
writeRaster(pop.dens.bhw, "data/processed/bhw_popdens.tif", overwrite=TRUE)
writeRaster(shrubland.bhw, "data/processed/bhw_shrubland.tif", overwrite=TRUE)
writeRaster(grassland.bhw, "data/processed/bhw_grassland.tif", overwrite=TRUE)
writeRaster(forests.bhw, "data/processed/bhw_forests.tif", overwrite=TRUE)
writeRaster(exposed.bhw, "data/processed/bhw_exposedland.tif", overwrite=TRUE)
writeRaster(pipe.dens.bhw, "data/processed/bhw_pipe.dens.tif", overwrite=TRUE)
writeRaster(developed.bhw, "data/processed/bhw_developed_land.tif", overwrite=TRUE)
writeRaster(wetland.bhw, "data/processed/bhw_wetlands.tif", overwrite=TRUE)
writeRaster(dist2wetland.bhw, "data/processed/bhw_dist2wetland.tif", overwrite=TRUE)
writeRaster(ghm.bhw, "data/processed/bhw_human_mod.tif", overwrite=TRUE)
writeRaster(ag.land.bhw, "data/processed/bhw_agriculture.tif", overwrite=TRUE)
writeRaster(ungulate.dens.bhw, "data/processed/bhw_ungulate_density.tif", overwrite=TRUE)
