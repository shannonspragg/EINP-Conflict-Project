
# Build "Validated" Habitat Model -----------------------------------------

## NOTE: the bear collar data is relatively limited (3 bears), and only covers an area surrounding EINP. Therefore, it may not be fully
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
einp <- st_read("data/processed/einp.shp")
einp.reproj <- st_transform(einp, crs=st_crs(bhb.watershed))
st_write(einp.reproj, "data/processed/einp_reproj.shp")

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
rocky <- rast("data/processed/bhb_rocky_land.tif")
snow.ice <- rast("data/processed/bhb_glacial_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
waterways <- rast("data/processed/bhb_water_areas.tif")
dist2water <- rast("data/processed/dist2drainage_km_bhb.tif")
dist2wb <- rast("data/processed/dist2waterbodies_km_bhb.tif")
human.development <- rast("data/processed/bhw_ghm.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
recent.wildfires <- rast("data/processed/bhb_fire_history.tif")

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
snow.ice
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
# Multiplying these variables by coefficients determined from our rsf using bear collar data

private.land.pred <-  -0.228986729 * private.land.rast
elevation.pred <- 2.636984904 * elevation 
slope.pred <- 0.672187111 * slope.a
#roads.pred <- -0.116349970 * roads.adjust # don't use this AND dist to roads
dist2roads.pred <- -0.045513248 * dist2roads.a
pop.dens.pred <- -0.040123557 * pop.dens.a
shrubland.pred <- 0.203343818 * shrubland
grassland.pred <- -0.776574365 * grassland
coniferous.forest.pred <- -0.004110265 * coniferous.forest
broadleaf.forest.pred <- 0.828511177 * broadleaf.forest
alpine.mixed.forest.pred <- 0.155851896 * alpine.mixed.forest
rocky.pred <- 0 * rocky
snow.ice.pred <- 0 * snow.ice
exposed.pred <- 0.773917828 * exposed
waterways.pred <- 1.828283959 * waterways
dist2water.pred <- -0.023117239 * dist2water.a
dist2wb.pred <- -0.046315852 * dist2wb.a
human.development.pred <- -3.364712453 * human.development
ag.land.pred <- -0.503 * ag.land
bh.lake.pred <- -2.0 * bh.lake
recent.wildfires.pred <- 0.417258638 * recent.wildfires

# Stack Precictor Rasters -------------------------------------------------

# Model 1:
bear.hab.val <- c(private.land.pred, elevation.pred, slope.pred, dist2roads.pred, shrubland.pred, waterways.pred,
                  grassland.pred, coniferous.forest.pred, broadleaf.forest.pred, alpine.mixed.forest.pred, rocky.pred,
                  snow.ice.pred, exposed.pred, dist2water.pred, dist2wb.pred, human.development.pred, ag.land.pred, 
                  bh.lake.pred, recent.wildfires.pred)
# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
bear.hab.val.rast <- sum(bear.hab.val, na.rm=TRUE)
habitat.val.rast <- (exp(bear.hab.val.rast))/(1 + exp(bear.hab.val.rast))
plot(habitat.val.rast)


# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)
einp.10km.v <- vect(st_buffer(einp.reproj, 10000) )
plot(habitat.val.rast)
plot(bhw.v, add=TRUE)
plot(einp.10km.v, add=TRUE)

# Mask Habitat Model to BHB Watershed -------------------------------------
bear.habitat.val.bhw <- terra::mask(habitat.val.rast, bhw.v)
bear.habitat.val.bhw.50km <- terra::mask(habitat.val.rast, bhb.50km.v)
bear.hab.val.einp10km <- terra::mask(habitat.val.rast, einp.10km.v)
plot(bear.habitat.val.bhw)
plot(bear.hab.val.einp10km)

# Save habitat model(s): -----------------------------------------------------
writeRaster(bear.hab.val.rast, "data/processed/bbear_raw_validated_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(habitat.val.rast, "data/processed/bbear_validated_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(bear.habitat.val.bhw.50km, "data/processed/bbear_val_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(bear.habitat.val.bhw, "data/processed/bbear_val_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed
writeRaster(bear.hab.val.einp10km, "data/processed/bbear_val_habitat_einp_10km.tif", overwrite=TRUE)
