# Build "Validated" Habitat Model for Wolves -----------------------------------------

## NOTE: the camera trap data is relatively limited for wolf sightings, and only covers an area surrounding EINP. Therefore, it may not be fully
## representative of habitat selection beyond this area

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
wolf.rsf <- readRDS("data/processed/wolf_ct_RSF.rds")

# bring in predictor rasters:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
roads <- rast("data/processed/bhb_roads_adjusted.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
coniferous.forest <- rast("data/processed/bhb_conifer_mix.tif")
broadleaf.forest <- rast("data/processed/bhb_broadleaf_mix.tif")
alpine.mixed.forest <- rast("data/processed/bhb_alpine_mix.tif")
forests <- rast("data/processed/bhb_forest_land.tif")
rocky <- rast("data/processed/bhb_rocky_land.tif")
snow.ice <- rast("data/processed/bhb_glacial_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
developed <- rast("data/processed/bhb_developed_land.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
dist2wb <- rast("data/processed/dist2waterbodies_km_bhb.tif")
human.mod <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
livestock.density <- rast("data/processed/animal_production_density_raster.tif")
ungulate.density <- rast("data/processed/total_ungulate_density.tif")
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
recent.wildfires <- rast("data/processed/bhb_fire_history.tif")

# Adjust some of these:
pop.dens.adj <- pop.dens / 10000 #making this meters
dist2pa.adj <- dist2pa.rast / 100
dist2waterways.adj <- dist2water / 100
dist2roads.adj <- dist2roads / 100
slope.adj <- slope / 10
road.dens.adj <- road.dens / 1000 #making this meters
ungulate.dens.adj <- ungulate.density / 100 


# Multiply Rasters by Coefficients: ----------------------------------------------------------
# Multiplying these variables by coefficients determined from our literature review of bear habitat predictors

road.dens.pred <- 0.0048607237 * road.dens.adj
prey.dens.pred <-  0.0502115311 * ungulate.dens.adj
forest.pred <- 1.2817276551 * forests
human.dens.pred <-  .0002044319 * pop.dens.adj
livestock.pred <- -0.8694228472 * livestock.density
shrubland.pred <- 0.00192448589 * shrubland
grassland.pred <- 0.05669781578 * grassland
coniferous.forest.pred <- -0.01151213113 * coniferous.forest
broadleaf.forest.pred <- -0.02454393855 * broadleaf.forest
alpine.mixed.forest.pred <- 0.05325289175 * alpine.mixed.forest
ag.land.pred <- -0.0095 * ag.land
waterbodies.pred <- 0.02053413636 * waterways
dist2water.pred <- 0.00717034487 * dist2waterways.adj
major.lake.pred <- -2.5 * bh.lake
recent.burn.pred <- 1.2211873138 * recent.wildfires
glacial.pred <- 0 * snow.ice
rocky.pred <- -0.00906307408 * rocky
developed.pred <- -0.01016350622 * developed
exposed.pred <- 1.8595579594  * exposed
private.land.pred <-  -0.07516588149 * private.land.rast
dist2pa.pred <- -0.00771797482 * dist2pa.adj
dist2roads.pred <- 0.00121410821 * dist2roads.adj
elevation.pred <- 2.2870644338 * elevation
slope.pred <- 0.00364028078 * slope.adj
human.mod.pred <- -0.05436663873 * human.mod

# Stack Precictor Rasters -------------------------------------------------

# Model 1:
wolf.hab.val <- c(road.dens.pred, prey.dens.pred, forest.pred, human.dens.pred, livestock.pred, shrubland.pred, grassland.pred, ag.land.pred, 
                    waterbodies.pred, dist2water.pred , major.lake.pred, recent.burn.pred, glacial.pred, rocky.pred, exposed.pred,developed.pred,
                    private.land.pred, dist2pa.pred, elevation.pred, slope.pred, human.mod.pred)

# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
wolf.hab.val.rast <- sum(wolf.hab.val, na.rm=TRUE)
wolf.habitat.prob.val <- (exp(wolf.hab.val.rast))/(1 + exp(wolf.hab.val.rast))
plot(wolf.habitat.prob.val)

# NOTE: I don't think we can use this... model coef suck

# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)

plot(wolf.habitat.prob.val)
plot(bhb.50km.v, add=TRUE)


# Mask Habitat Model to BHB Watershed -------------------------------------
wolf.habitat.val.bhw <- terra::mask(wolf.habitat.prob.val, bhw.v)
wolf.habitat.val.bhw.50km <- terra::mask(wolf.habitat.prob.val, bhb.50km.v)

plot(wolf.habitat.val.bhw)

# Save habitat model(s): -----------------------------------------------------
writeRaster(wolf.hab.val.rast, "data/processed/wolf_raw_ct_val_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(wolf.habitat.prob.valt, "data/processed/wolf_ct_validated_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(wolf.habitat.val.bhw.50km, "data/processed/wolf_ct_validated_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(wolf.habitat.val.bhw, "data/processed/wolf_ct_validated_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed

