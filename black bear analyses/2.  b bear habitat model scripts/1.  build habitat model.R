
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

# model 1 Beckman et al., 2015:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_bhb.tif")
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


bhb.buf.vect <- vect(bhb.50km.boundary)
# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 240x240m 
private.land.rast
elevation
slope
roads
dist2roads
pop.dens
shrubland
grassland
coniferous.forest
broadleaf.forest
alpine.mixed.forest
waterways
dist2water
human.development
ag.land

roads[roads == 10003] <- 1
roads[roads == 10002] <- 0


# Crop our rasters to the BHB buffer shape:
#bhb.50km.v <- vect(bhb.50km.boundary)

# private.land.bhb <- terra::mask(private.land.rast, bhb.50km.v)
# elevation.bhb <- terra::mask(elevation, bhb.50km.v)
# dist2roads.bhb <- terra::mask(dist2roads, bhb.50km.v)
# pop.dens.bhb <- terra::mask(pop.dens, bhb.50km.v)
# road.dens.4km.bhb <- terra::mask(road.dens.4km, bhb.50km.v)
# road.dens.500m.bhb <- terra::mask(road.dens.500m, bhb.50km.v)
# evergreen.bhb <- terra::mask(evergreen.forest, bhb.50km.v)
# 
# pop.road.dens <- pop.dens.bhb * road.dens.500m.bhb
# pop.road.dens


# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predoctors

private.land.pred <- -1.8454 * private.land.rast
elevation.pred <- -0.5012 * elevation 
slope.pred <- -0.2058 * slope
roads.pred <- -0.75 * roads
dist2roads.pred <- 1.5425 * dist2roads
pop.dens.pred <- -1 * pop.dens
shrubland.pred <- -0.35 * shrubland
grassland.pred <- -1.81 * grassland
coniferous.forest.pred <- 1.389 * coniferous.forest
broadleaf.forest.pred <- 2.101 * broadleaf.forest
alpine.mixed.forest.pred <- 2.323 * alpine.mixed.forest
waterways.pred <- -0.5489 * waterways
dist2water.pred <- -0.0995 * dist2water
human.development.pred <- -5.898 * human.development
ag.land.pred <- -3.303 * ag.land

# NOTE: need to try scaling these.. the high numbers are throwing things off

# Stack Precictor Rasters -------------------------------------------------

# model 1:
bear.hab.stack <- c(private.land.pred, elevation.pred, slope.pred, roads.pred, dist2roads.pred, shrubland.pred, 
                    grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred, waterways.pred,
                    dist2water.pred, human.development.pred, ag.land.pred)

# testing with lower values:
bh.1 <- c(shrubland.pred, grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred, waterways.pred)
bh.r <- sum(bh.1)
bh.prob.rast <- (exp(bh.r))/(1 + exp(bh.r))
plot(bh.prob.rast) # want something like this as the result!

# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
bear.hab.rast <- sum(bear.hab.stack)
linpred.rast <- sum(bear.hab.stack, na.rm=TRUE)
habitat.prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))
plot(habitat.prob.rast)

# Model 2:
# linpred.rast.m <- sum(male.habitat.stack, na.rm=TRUE)
# habitat.prob.rast.m <- (exp(linpred.rast.m))/(1 + exp(linpred.rast.m))
# 
# linpred.rast.f <- sum(female.habitat.stack, na.rm=TRUE)
# habitat.prob.rast.f <- (exp(linpred.rast.f))/(1 + exp(linpred.rast.f))
# 
# plot(habitat.prob.rast.m)
# plot(habitat.prob.rast.f)

writeRaster(habitat.prob.rast.m, "data/processed/male_bbear_habitat_suitability.tif") # for 50km buf of beaver hills watershed
writeRaster(habitat.prob.rast.f, "data/processed/female_bbear_habitat_suitability.tif") # (crop to watershed after Omniscape)
