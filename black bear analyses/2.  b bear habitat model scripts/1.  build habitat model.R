
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
bhb.50km.boundary <- st_read("data/processed/bhb_10km.shp")

# model 1 Beckman et al., 2015:
private.land.rast <- rast("data/processed/bhb_privatelands.tif")
elevation <- rast("data/processed/elevation_bhb.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
pop.dens <- rast("data/processed/human_dens_crop.tif")
road.dens <- rast("data/processed/bhb_road_density.tif")
evergreen.forest <- rast("data/processed/evergreen_sum_500m.tif")

# model 2, Loosen et al., 2018
# crownland.rast <- rast("data/processed/bhb_crownlands.tif")
# private.land.rast <- rast("data/processed/bhb_privatelands.tif")
# wildfires.rast <- rast("data/processed/bhb_fire_history.tif")
# ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
# shrubland.rast <- rast("data/processed/bhb_shrubland.tif")

bhb.buf.vect <- vect(bhb.10km.boundary)
# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 250m
crownland.rast
private.land.rast
wildfires.rast
ndvi.rast
shrubland.rast

plot(crownland.rast)
plot(private.land.rast)
plot(wildfires.rast)
plot(ndvi.rast)
plot(shrubland.rast)

  # Disaggregate rasters:
crownland <- disagg(crownland.rast, fact= 4)
private.land <- disagg(private.land.rast, fact=4)
wildfires <- disagg(wildfires.rast, fact=4)
ndvi <- disagg(ndvi.rast, fact= 4)
shrubland <- disagg(shrubland.rast, fact=4)

# Scale Rasters: ----------------------------------------------------------
  # This function scales by subtracting mean and dividing by 1 sd of the original data
  # We need to know the mean and sd of each variable from the top RSF models for male & female black bears
crownland.sc <- terra::scale(crownland.rast)
privland.sc <- terra::scale(private.land)
wildfires.sc <- terra::scale(wildfires.rast)
ndvi.sc <- terra::scale(ndvi.rast)
shrubland.sc <- terra::scale(shrubland)

# ## the equivalent, computed in steps
# m <- global(r, "mean")
# rr <- r - m[,1]
# rms <- global(rr, "rms")
# ss <- rr / rms[,1]

# Combine & Multiply by Coefficients: -------------------------------------
#  (x - mean) / sd = scaled coef. ; need to find x (actual coef) to multiply
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
