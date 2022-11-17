
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
dist2water
dist2wb
human.development
ag.land
livestock.density
bh.lake
recent.wildfires
ungulate.density
dist2pa.rast 

# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predictors

road.dens.pred <- -0.95 * road.dens
prey.dens.pred <- 1.35 * ungulate.density
forest.pred <- 1.45 * forests
human.dens.pred <- -0.90 * pop.dens
livestock.pred <- -0.25 * livestock.density
shrubland.pred <- 0.95 * shrubland
grassland.pred <- 0.75 * grassland
ag.land.pred <- -0.75 * ag.land
waterbodies.pred <- 1.20 * waterways
major.lake.pred <- -2.5 * bh.lake
recent.burn.pred <- -0.35 * recent.wildfires
glacial.pred <- 0.20 * glacial
rocky.pred <- 0.25 * rocky
exposed.pred <- 0.35 * exposed
private.land.pred <- -0.1 * private.land.rast
dist2pa.pred <- -1.25 * dist2pa.rast
elevation.pred <- 0.95 * elevation
slope.pred <- 0.85 * slope
human.dev.pred <- -0.75 * human.development


# Stack Precictor Rasters -------------------------------------------------

# Model 1:
wolf.hab.stack <- c(road.dens.pred, prey.dens.pred, forest.pred, human.dens.pred, livestock.pred, shrubland.pred, grassland.pred, ag.land.pred, 
                    waterbodies.pred, major.lake.pred, recent.burn.pred, glacial.pred, rocky.pred, exposed.pred, private.land.pred, dist2pa.pred,
                    elevation.pred, slope.pred, human.dev.pred)

# Model 2:
bear.hab.mod.no.dist <- c(private.land.pred, elevation.pred, slope.pred, shrubland.pred, roads.pred, waterways.pred,
                          grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred,
                         human.development.pred, ag.land.pred)


# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
bear.hab.rast <- sum(bear.hab.stack, na.rm=TRUE)
habitat.prob.rast <- (exp(bear.hab.rast))/(1 + exp(bear.hab.rast))
plot(habitat.prob.rast)

# Model 2:
bh.rast.2 <- sum(bear.hab.mod.no.dist, na.rm=TRUE)
habitat.prob.rast.2 <- (exp(bh.rast.2))/(1 + exp(bh.rast.2))
plot(habitat.prob.rast.2)


# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)

plot(habitat.prob.rast)
plot(bhb.50km.v, add=TRUE)


# Mask Habitat Model to BHB Watershed -------------------------------------
bear.habitat.bhw <- terra::mask(habitat.prob.rast, bhw.v)
bear.habitat.bhw.50km <- terra::mask(habitat.prob.rast, bhb.50km.v)

# Save habitat model(s): -----------------------------------------------------
writeRaster(bear.hab.rast, "data/processed/bbear_raw_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(habitat.prob.rast, "data/processed/bbear_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(bear.habitat.bhw.50km, "data/processed/bbear_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(bear.habitat.bhw, "data/processed/bbear_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed




