
# Prep Covariate Rasters for HSI: -----------------------------------------
    # Here we bring in the covariates for our wolf HSI based on literature review:
    # land tenure (protected areas,  private lands), waterbodies, land cover (forest, shrub, grassland, riperian, glacial, rocky, open),
    # elevation, slope. human modification, road density, human density, ag density, ungulate density, and recently burned areas

    # We need to scale all of the rasters, make them continuous surfaces (0 or 1), and ensure they are all at the desired resolution

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

# Variables:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
forests <- rast("data/processed/bhb_forest_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
glacial <- rast("data/processed/bhb_glacial_land.tif")
rocky <- rast("data/processed/bhb_rocky_land.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
#dist2drainage <- rast("data/processed/dist2drainage_km_bhb.tif")
dist2waterways <- rast("data/processed/dist2waterbodies_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
recent.wildfires <- rast("data/processed/bhb_fire_history.tif")
livestock.density <- rast("data/processed/animal_production_density_raster.tif")
ungulate.density <- rast("data/processed/total_ungulate_density.tif")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bhb.buf.vect <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 250x250m 
private.land.rast
elevation
slope
road.dens 
pop.dens # might leave this out if using ghm
shrubland
grassland
forests
exposed
glacial
rocky
waterways
dist2waterways
dist2wb
human.development
ag.land
livestock.density
bh.lake
recent.wildfires
ungulate.density
dist2pa.rast 

# Adjust scale of some of these so they're on 0-1:
road.dens.adj <- road.dens / 1000 #making this meters
ungulate.dens.adj <- ungulate.density / 10000 # adjusting scale on here
pop.dens.adj <- pop.dens / 10000 #making this meters
dist2pa.adj <- dist2pa.rast / 100
dist2waterways.adj <- dist2waterways / 100
slope.adj <- slope / 10

# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predictors

road.dens.pred <- -1.25 * road.dens.adj
prey.dens.pred <- 1.45 * ungulate.dens.adj
forest.pred <- 1.35 * forests
human.dens.pred <- -1.30 * pop.dens.adj
livestock.pred <- -0.25 * livestock.density
shrubland.pred <- 0.95 * shrubland
grassland.pred <- 0.75 * grassland
ag.land.pred <- -0.75 * ag.land
waterbodies.pred <- 0.45 * waterways
dist2water.pred <- -1.2 * dist2waterways.adj
major.lake.pred <- -2.5 * bh.lake
recent.burn.pred <- -0.35 * recent.wildfires
glacial.pred <- 0.20 * glacial
rocky.pred <- 0.25 * rocky
exposed.pred <- 0.35 * exposed
private.land.pred <- -0.35 * private.land.rast
dist2pa.pred <- -1.05 * dist2pa.adj
elevation.pred <- 1.15 * elevation
slope.pred <- 0.95 * slope.adj
human.dev.pred <- -1.25 * human.development


# Stack Precictor Rasters -------------------------------------------------

# Model 1:
wolf.hab.stack <- c(road.dens.pred, prey.dens.pred, forest.pred, human.dens.pred, livestock.pred, shrubland.pred, grassland.pred, ag.land.pred, 
                    waterbodies.pred, dist2water.pred , major.lake.pred, recent.burn.pred, glacial.pred, rocky.pred, exposed.pred, private.land.pred, dist2pa.pred,
                    elevation.pred, slope.pred, human.dev.pred)

# Model 2:
wolf.hab.mod.simple <- c(road.dens.pred, forest.pred, prey.dens.pred, human.dens.pred, shrubland.pred, grassland.pred, dist2water.pred, elevation.pred,
                         dist2pa.pred, ag.land.pred)


# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
wolf.hab.rast <- sum(wolf.hab.stack, na.rm=TRUE)
wolf.habitat.prob.rast <- (exp(wolf.hab.rast))/(1 + exp(wolf.hab.rast))
plot(wolf.habitat.prob.rast)

# Model 2:
wolf.hab.simple <- sum(wolf.hab.mod.simple, na.rm=TRUE)
wolf.hab.prob.rast.2 <- (exp(wolf.hab.simple))/(1 + exp(wolf.hab.simple))
plot(wolf.hab.prob.rast.2)


# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)

plot(wolf.habitat.prob.rast)
plot(bhb.50km.v, add=TRUE)


# Mask Habitat Model to BHB Watershed -------------------------------------
wolf.habitat.bhw <- terra::mask(wolf.habitat.prob.rast, bhw.v)
wolf.habitat.bhw.50km <- terra::mask(wolf.habitat.prob.rast, bhb.50km.v)

# Save habitat model(s): -----------------------------------------------------
writeRaster(wolf.hab.rast, "data/processed/wolf_raw_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(wolf.habitat.prob.rast, "data/processed/wolf_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(wolf.habitat.bhw.50km, "data/processed/wolf_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(wolf.habitat.bhw, "data/processed/wolf_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed




