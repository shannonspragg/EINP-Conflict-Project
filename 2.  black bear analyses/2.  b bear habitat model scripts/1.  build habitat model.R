
# Prep Covariate Rasters for HSI: -----------------------------------------
    # Here we bring in the covariates for our black bear HSI :
    # land tenure (protected areas, crown lands, private lands), NDVI, shrubland, recently burned areas
    # (not grizzly bear use (GBU) -- there aren't GB pops in our study area)

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
dist2wb <- rast("data/processed/dist2waterbodies_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
recent.wildfires <- rast("data/processed/bhb_fire_history.tif")
rocky <- rast("data/processed/bhb_rocky_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
snow.ice <- rast("data/processed/bhb_glacial_land.tif")

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
dist2wb
human.development
ag.land
bh.lake
recent.wildfires

roads.adjust <- roads / 1
writeRaster(roads.adjust, "data/processed/bhb_roads_adjusted.tif")

# Adjust some of these:
pop.dens.a <- pop.dens / 10000 #making this meters
dist2water.a <- dist2water / 100
dist2wb.a <- dist2wb / 100
dist2roads.a <- dist2roads / 100
slope.a <- slope / 10


# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predictors

private.land.pred <- -1.7 * private.land.rast
elevation.pred <- -0.5012 * elevation 
slope.pred <- -0.2058 * slope.a
#roads.pred <- -0.75 * roads.adjust # don't use this AND dist to roads
dist2roads.pred <- 0.9 * dist2roads.a
pop.dens.pred <- -1 * pop.dens.a
shrubland.pred <- -0.35 * shrubland
grassland.pred <- -1.5 * grassland
coniferous.forest.pred <- 1.389 * coniferous.forest
broadleaf.forest.pred <- 2.101 * broadleaf.forest
alpine.mixed.forest.pred <- 2.323 * alpine.mixed.forest
waterways.pred <- -0.5489 * waterways
dist2water.pred <- -0.0995 * dist2water.a
dist2wb.pred <- -0.0995 * dist2wb.a
human.development.pred <- -3.898 * human.development
ag.land.pred <- -2.303 * ag.land
bh.lake.pred <- -6.0 * bh.lake
recent.wildfires.pred <- -1.1 * recent.wildfires

# Trying a slightly less varied model / integrating validated model:
private.land.pred2 <- -0.35 * private.land.rast
elevation.pred2 <- 0.90 * elevation 
slope.pred2 <- 0.15 * slope.a
#roads.pred2 <- -0.75 * roads.adjust # don't use this AND dist to roads
dist2roads.pred2 <- 0.10 * dist2roads.a
pop.dens.pred2 <- -0.95 * pop.dens.a
shrubland.pred2 <- -0.35 * shrubland
grassland.pred2 <- -0.25 * grassland
rocky.pred2 <- 0.10 * rocky
snow.ice.pred2 <- 1.25 * snow.ice
exposed.pred2 <- -0.65 * exposed
coniferous.forest.pred2 <- 1.20 * coniferous.forest
broadleaf.forest.pred2 <- 0.65 * broadleaf.forest
alpine.mixed.forest.pred2 <- 1.55 * alpine.mixed.forest
waterways.pred2 <- -1.55 * waterways
dist2water.pred2 <- -0.10 * dist2water.a
dist2wb.pred2 <- -0.10 * dist2wb.a
human.development.pred2 <- -1.55 * human.development
ag.land.pred2 <- -1.45 * ag.land
bh.lake.pred2 <- -1.5 * bh.lake
recent.wildfires.pred2 <- -0.30 * recent.wildfires

# Stack Precictor Rasters -------------------------------------------------

# Model 1:
bear.hab.stack <- c(private.land.pred, elevation.pred, slope.pred, dist2roads.pred, shrubland.pred, waterways.pred,
                    grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred,
                    dist2water.pred, dist2wb.pred, human.development.pred, ag.land.pred, bh.lake.pred, recent.wildfires.pred)

bear.hab.stack2 <- c(private.land.pred2, elevation.pred2, slope.pred2, dist2roads.pred2, shrubland.pred2, rocky.pred2, snow.ice.pred2, exposed.pred2, waterways.pred2,
                    grassland.pred2, coniferous.forest.pred2, broadleaf.forest.pred2, alpine.mixed.forest.pred2,
                    dist2water.pred2, dist2wb.pred2, human.development.pred2, ag.land.pred2, bh.lake.pred2, recent.wildfires.pred2)

# Model 2:
bear.hab.mod.no.dist <- c(private.land.pred, elevation.pred, slope.pred, dist2roads.pred, shrubland.pred, waterways.pred,
                          grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred,
                          dist2water.pred, dist2wb.pred, human.development.pred, ag.land.pred, bh.lake.pred, recent.wildfires.pred)


# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
bear.hab.rast <- sum(bear.hab.stack, na.rm=TRUE)
habitat.prob.rast <- (exp(bear.hab.rast))/(1 + exp(bear.hab.rast))
plot(habitat.prob.rast)

# Model 2:
bh.rast.2 <- sum(bear.hab.stack2, na.rm=TRUE)
habitat.prob.rast.2 <- (exp(bh.rast.2))/(1 + exp(bh.rast.2))
plot(habitat.prob.rast.2)


# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)

plot(habitat.prob.rast)
plot(bhb.50km.v, add=TRUE)


# Mask Habitat Model to BHB Watershed -------------------------------------
bear.habitat.bhw <- terra::mask(habitat.prob.rast, bhw.v)
bear.habitat.bhw.50km <- terra::mask(habitat.prob.rast, bhb.50km.v)

bear.habitat2.bhw <- terra::mask(habitat.prob.rast.2, bhw.v)
plot(bear.habitat2.bhw)

# Save habitat model(s): -----------------------------------------------------
writeRaster(bear.hab.rast, "data/processed/bbear_raw_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(habitat.prob.rast, "data/processed/bbear_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(bear.habitat.bhw.50km, "data/processed/bbear_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(bear.habitat.bhw, "data/processed/bbear_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed


writeRaster(habitat.prob.rast.2, "data/processed/bbear_integrated_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(bear.habitat2.bhw, "data/processed/bbear_integrated_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed


