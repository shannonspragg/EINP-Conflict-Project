
# Bear HSI Integrated to Validated Sweeping Graph ----------------------------------
# NOTE: may not be necessary if we use the validated model

  # Here we will produce multiple models in between the integrated and validated bbear HSI. We will hold most variables constant,
  # while adjusting only two of the major variables (human modification & pop density), to show how habitat use varies with these 
  # adjustments. We will use these sub-models to produce a "sweeping" range of habitat probability from the integrated / expert 
  # opinion model to the collar validated model.

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

private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_km_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
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

# Adjust some of these:
pop.dens.a <- pop.dens / 10000 #making this meters
dist2water.a <- dist2water / 100
dist2wb.a <- dist2wb / 100
dist2roads.a <- dist2roads / 100
slope.a <- slope / 10


# Bring in integrated model (representing baseline) and validated model (representing 100% change)  --------
hab.integrated.bhw <- rast("data/processed/bbear_integrated_habitat_bhw.tif")
hab.validated.bhw <- rast("data/processed/bbear_val_habitat_bhw.tif")

# Multiply Rasters by Coefficients: ----------------------------------------------------------

# Adjust integrated model by moving only key predictors by 25%:
  # Here we will move pop density DOWN and human mod UP, slightly and elevation up
private.land.pred25 <- -0.35 * private.land.rast
elevation.pred25 <- 1.55 * elevation 
slope.pred25 <- 0.50 * slope.a
dist2roads.pred25 <- 0.30 * dist2roads.a
pop.dens.pred25 <- -0.70 * pop.dens.a
shrubland.pred25 <- 0.15 * shrubland
grassland.pred25 <- -0.50 * grassland
rocky.pred25 <- 0.10 * rocky
snow.ice.pred25 <- 1.0 * snow.ice
exposed.pred25 <- -0.65 * exposed
coniferous.forest.pred25 <- 1.0 * coniferous.forest
broadleaf.forest.pred25 <- 0.85 * broadleaf.forest
alpine.mixed.forest.pred25 <- 1.0 * alpine.mixed.forest
waterways.pred25 <- 1.55 * waterways
dist2water.pred25 <- -0.10 * dist2water.a
dist2wb.pred25 <- -0.10 * dist2wb.a
human.development.pred25 <- -2.75 * human.development
ag.land.pred25 <- -0.95 * ag.land
bh.lake.pred25 <- -1.5 * bh.lake
recent.wildfires.pred25 <- -0.20 * recent.wildfires

# Here we will move things to ~50% change
private.land.pred50 <- -0.35 * private.land.rast
elevation.pred50 <- 1.90 * elevation 
slope.pred50 <- 0.50 * slope.a
dist2roads.pred50 <- 0.30 * dist2roads.a
pop.dens.pred50 <- -0.45 * pop.dens.a
shrubland.pred50 <- 0.15 * shrubland
grassland.pred50 <- -0.50 * grassland
rocky.pred50 <- 0.10 * rocky
snow.ice.pred50 <- 1.0 * snow.ice
exposed.pred50 <- -0.65 * exposed
coniferous.forest.pred50 <- 1.0 * coniferous.forest
broadleaf.forest.pred50 <- 0.85 * broadleaf.forest
alpine.mixed.forest.pred50 <- 1.0 * alpine.mixed.forest
waterways.pred50 <- 1.55 * waterways
dist2water.pred50 <- -0.10 * dist2water.a
dist2wb.pred50 <- -0.10 * dist2wb.a
human.development.pred50 <- -2.95 * human.development
ag.land.pred50 <- -0.95 * ag.land
bh.lake.pred50 <- -1.5 * bh.lake
recent.wildfires.pred50 <- -0.20 * recent.wildfires

# Here we will move pop dens and human mod to ~75% change
private.land.pred75 <- -0.35 * private.land.rast
elevation.pred75 <- 2.25 * elevation 
slope.pred75 <- 0.50 * slope.a
dist2roads.pred75 <- 0.30 * dist2roads.a
pop.dens.pred75 <- -0.20 * pop.dens.a
shrubland.pred75 <- 0.15 * shrubland
grassland.pred75 <- -0.50 * grassland
rocky.pred75 <- 0.10 * rocky
snow.ice.pred75 <- 1.0 * snow.ice
exposed.pred75 <- -0.65 * exposed
coniferous.forest.pred75 <- 1.0 * coniferous.forest
broadleaf.forest.pred75 <- 0.85 * broadleaf.forest
alpine.mixed.forest.pred75 <- 1.0 * alpine.mixed.forest
waterways.pred75 <- 1.55 * waterways
dist2water.pred75 <- -0.10 * dist2water.a
dist2wb.pred75 <- -0.10 * dist2wb.a
human.development.pred75 <- -3.20 * human.development
ag.land.pred75 <- -0.95 * ag.land
bh.lake.pred75 <- -1.5 * bh.lake
recent.wildfires.pred75 <- -0.20 * recent.wildfires

# Stack Precictor Rasters -------------------------------------------------

# Integrated model with 25% change in pop dens (-) and human mod (+):
bear.int.25 <- c(private.land.pred25, elevation.pred25, slope.pred25, dist2roads.pred25, shrubland.pred25, rocky.pred25, snow.ice.pred25, exposed.pred25, waterways.pred25,
                     grassland.pred25, coniferous.forest.pred25, broadleaf.forest.pred25, alpine.mixed.forest.pred25,
                     dist2water.pred25, dist2wb.pred25, human.development.pred25, ag.land.pred25, bh.lake.pred25, recent.wildfires.pred25)

# Integrated model with 50% change in pop dens (-) and human mod (+):
bear.int.50 <- c(private.land.pred50, elevation.pred50, slope.pred50, dist2roads.pred50, shrubland.pred50, rocky.pred50, snow.ice.pred50, exposed.pred50, waterways.pred50,
                 grassland.pred50, coniferous.forest.pred50, broadleaf.forest.pred50, alpine.mixed.forest.pred50,
                 dist2water.pred50, dist2wb.pred50, human.development.pred50, ag.land.pred50, bh.lake.pred50, recent.wildfires.pred50)

# Integrated model with 75% change in pop dens (-) and human mod (+):
bear.int.75 <- c(private.land.pred75, elevation.pred75, slope.pred75, dist2roads.pred75, shrubland.pred75, rocky.pred75, snow.ice.pred75, exposed.pred75, waterways.pred75,
                 grassland.pred75, coniferous.forest.pred75, broadleaf.forest.pred75, alpine.mixed.forest.pred75,
                 dist2water.pred75, dist2wb.pred75, human.development.pred75, ag.land.pred75, bh.lake.pred75, recent.wildfires.pred75)

# Convert to Probability Scale (IF NEEDED): -------------------------------

bh.rast.25 <- sum(bear.hab.stack2, na.rm=TRUE)
habitat.prob.rast.25 <- (exp(bh.rast.25))/(1 + exp(bh.rast.25))
plot(habitat.prob.rast.25)

bh.rast.50 <- sum(bear.hab.stack2, na.rm=TRUE)
habitat.prob.rast.50 <- (exp(bh.rast.50))/(1 + exp(bh.rast.50))
plot(habitat.prob.rast.50)

bh.rast.75 <- sum(bear.hab.stack2, na.rm=TRUE)
habitat.prob.rast.75 <- (exp(bh.rast.75))/(1 + exp(bh.rast.75))
plot(habitat.prob.rast.75)

# Mask Habitat Model to BHB Watershed -------------------------------------
bear.int.25.bhw <- terra::mask(habitat.prob.rast.25, bhw.v)
plot(bear.int.25.bhw)

bear.int.50.bhw <- terra::mask(habitat.prob.rast.50, bhw.v)
plot(bear.int.50.bhw)

bear.int.75.bhw <- terra::mask(habitat.prob.rast.75, bhw.v)
plot(bear.int.75.bhw)


# Compare all of our models in progression: -------------------------------
bear.hab.change <- c(hab.integrated.bhw, bear.int.25.bhw, bear.int.50.bhw, bear.int.75.bhw, hab.validated.bhw)

plot(bear.hab.change)
