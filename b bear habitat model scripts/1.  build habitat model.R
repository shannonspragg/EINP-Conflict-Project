
# Prep Covariate Rasters for HSI: -----------------------------------------
    # Here we bring in the covariates for our black bear HSI (following Loosen et al., 2018):
    # land tenure (protected areas, crown lands, private lands), NDVI, shrubland, recently burned areas,
    # and grizzly bear use (GBU) -- only if applicable

    # We need to scale all of the rasters, make them continuous surfaces (0 or 1), and ensure they are all at the desired resolution

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in covariate data: -------------------------------------------------------------

crownland.rast <- rast("data/processed/bhw_crownlands.tif")
private.land.rast <- rast("data/processed/bhw_privatelands.tif")
wildfires.rast <- rast("data/processed/bhw_fire_history.tif")
ndvi.rast <- rast("data/processed/bhw_ndvi.tif")
shrubland.rast <- rast("data/processed/bhw_shrubland.tif")


# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 500m
crownland.rast
private.land.rast
wildfires.rast
ndvi.rast
shrubland.rast



# Scale Rasters: ----------------------------------------------------------

crownland.sc <- terra::scale(crownland.rast)
privland.sc <- terra::scale(private.land.rast)
wildfires.sc <- terra::scale(wildfires.rast)
ndvi.sc <- terra::scale(ndvi.rast)
shrubland.sc <- terra::scale(shrubland.rast)

# ## the equivalent, computed in steps
# m <- global(r, "mean")
# rr <- r - m[,1]
# rms <- global(rr, "rms")
# ss <- rr / rms[,1]

# Combine & Multiply by Coefficients: -------------------------------------

  # Male Black Bears:
crownland.pred.m <- crownland.sc * -0.75
privland.pred.m <- privland.sc * -0.25
wildfires.pred.m <- wildfires.sc * -0.70
ndvi.pred.m <- ndvi.sc * 0.20
shrubland.pred.m <- shrubland.sc * 0.0

# Female Black Bears:
crownland.pred.f <- crownland.sc * -1.20
privland.pred.f <- privland.sc * -0.25
wildfires.pred.f <- wildfires.sc * -0.10
ndvi.pred.f <- ndvi.sc * 0.10
shrubland.pred.f <- shrubland.sc * 0.05


# Stack Precictor Rasters -------------------------------------------------

male.habitat.stack <- c(crownland.pred.m, privland.pred.m, wildfires.pred.m, ndvi.pred.m, shrubland.pred.m)

female.habitat.stack <- c(crownland.pred.f, privland.pred.f, wildfires.pred.f, ndvi.pred.f, shrubland.pred.f)


# Convert to Probability Scale (IF NEEDED): -------------------------------

linpred.rast.m <- sum(male.habitat.stack, na.rm=TRUE)
habitat.prob.rast.m <- (exp(linpred.rast.m))/(1 + exp(linpred.rast.m))

linpred.rast.f <- sum(female.habitat.stack, na.rm=TRUE)
habitat.prob.rast.f <- (exp(linpred.rast.f))/(1 + exp(linpred.rast.f))

plot(habitat.prob.rast.m)
plot(habitat.prob.rast.f)

writeRaster(habitat.prob.rast.m, "data/processed/male_bbear_habitat_suitability.tif") # for 50km buf of beaver hills watershed
writeRaster(habitat.prob.rast.f, "data/processed/female_bbear_habitat_suitability.tif") # (crop to watershed after Omniscape)
