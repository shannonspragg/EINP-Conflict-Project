
# Prep Covariate Rasters for HSI: -----------------------------------------
    # Here we bring in the covariates for our black bear HSI (following Loosen et al., 2018):
    # land tenure (protected areas, crown lands, private lands), NDVI, shrubland, recently burned areas
    # (not grizzly bear use (GBU) -- there aren't GB pops in our study area)

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

# model 1 Beckman et al., 2015:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
roads <- rast("data/processed/bhb_roads.tif")
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
bh.lake <- rast("data/processed/beaverhills_lake.tif")

bhb.buf.vect <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 240x240m 
private.land.rast
elevation
slope
roads # need to adjust this
dist2roads
pop.dens # might leave this out if using ghm
shrubland
grassland
coniferous.forest
broadleaf.forest
alpine.mixed.forest
waterways
dist2water
human.development
ag.land
bh.lake

roads.adjust <- roads / 1
writeRaster(roads.adjust, "data/processed/bhb_roads_adjusted.tif")

# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predictors

private.land.pred <- -1.8454 * private.land.rast
elevation.pred <- -0.5012 * elevation 
slope.pred <- -0.2058 * slope
roads.pred <- -0.75 * roads.adjust
dist2roads.pred <- 1.5425 * dist2roads
pop.dens.pred <- -1 * pop.dens
shrubland.pred <- -0.35 * shrubland
grassland.pred <- -1.81 * grassland
coniferous.forest.pred <- 1.389 * coniferous.forest
broadleaf.forest.pred <- 2.101 * broadleaf.forest
alpine.mixed.forest.pred <- 2.323 * alpine.mixed.forest
waterways.pred <- -0.5489 * waterways
dist2water.pred <- -0.0995 * dist2water
human.development.pred <- -3.898 * human.development
ag.land.pred <- -2.303 * ag.land
bh.lake.pred <- -6.0 * bh.lake

# Stack Precictor Rasters -------------------------------------------------

# Model 1:
bear.hab.stack <- c(private.land.pred, elevation.pred, slope.pred, dist2roads.pred, shrubland.pred, roads.pred, waterways.pred,
                    grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred,
                    dist2water.pred, human.development.pred, ag.land.pred, bh.lake.pred)

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

writeRaster(habitat.prob.rast, "data/processed/bbear_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(bear.habitat.bhw.50km, "data/processed/bbear_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(bear.habitat.bhw, "data/processed/bbear_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed




