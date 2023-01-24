
# Prep Covariate Rasters for HSI: -----------------------------------------
    # Here we bring in the covariates for our cougar HSI based on literature review:
    #  waterbodies, land cover (forest, shrub, grassland, wetland, developed, edge, open),
    # terrain ruggedness, slope. human modification, road density, pipeline density, human density, and ungulate density

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Should have:
# edge habitat
# forest cover
# shrubland
# ag land
# wetland
# dist 2 water (opposite of proximity/closeness to water)
# slope 
# road density
# dist 2 roads
# pipeline / mining density
# developed land
# ungulate density
# exposed land
# grassland
# terrain ruggedness
# human density
# human modification

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.watershed <- st_read("data/original/BHB_Subwatershed_Boundary.shp")

# Variables:
forest.edge.rast <- rast("data/processed/forest_edge_habitats.tif")
ruggedness <- rast("data/processed/terrain_ruggedness_bhb.tif")
slope <- rast("data/processed/slope_bhb.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
dist2roads <- rast("data/processed/dist2roads_km_bhb.tif")
pop.dens <- rast("data/processed/human_dens_bhb.tif")
shrubland <- rast("data/processed/bhb_shrubland.tif")
grassland <- rast("data/processed/bhb_grassland.tif")
forests <- rast("data/processed/bhb_forest_land.tif")
exposed <- rast("data/processed/bhb_exposed_land.tif")
developed <- rast("data/processed/bhb_developed_land.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")
pipe.dens <- rast("data/processed/bhb_pipeline_density_250m.tif")
wetland <- rast("data/processed/bhb_water_areas.tif")
dist2waterways <- rast("data/processed/dist2waterbodies_km_bhb.tif")
human.mod <- rast("data/processed/bhw_ghm.tif")
ungulate.density <- rast("data/processed/total_ungulate_density.tif")
bh.lake <- rast("data/processed/beaverhills_lake.tif")

bhb.buf.vect <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Check Rasters: ----------------------------------------------------------
    # Desired resolution: 250x250m 
forest.edge.rast
ruggedness
slope
road.dens 
pop.dens # might leave this out if using ghm
shrubland
grassland
forests
exposed
developed
pipe.dens
wetland
dist2waterways
human.mod
ag.land
ungulate.density
 

# Adjust scale of some of these so they're on 0-1:
road.dens.adj <- road.dens / 1000 #making this meters
pipe.dens.adj <- pipe.dens / 1000 #making this meters
ungulate.dens.adj <- ungulate.density / 100 
pop.dens.adj <- pop.dens / 10000 #making this meters
dist2waterways.adj <- dist2waterways / 100
dist2roads.adj <- dist2roads / 100
slope.adj <- slope / 10

# Multiply Rasters by Coefficients: ----------------------------------------------------------
  # Multiplying these variables by coefficients determined from our literature review of bear habitat predictors
edge.hab.pred <- 0.70 * forest.edge.rast
road.dens.pred <- -0.75 * road.dens.adj
prey.dens.pred <- 0.60 * ungulate.dens.adj
forest.pred <- 0.80 * forests
human.dens.pred <- -1.55 * pop.dens.adj
shrubland.pred <- 0.75 * shrubland
grassland.pred <- 0.60 * grassland
ag.land.pred <- -0.95 * ag.land
wetland.pred <- 0.95 * wetland
dist2water.pred <- -0.95 * dist2waterways.adj
major.lake.pred <- -2.0 * bh.lake
pipeline.dens.pred <- -0.80 * pipe.dens.adj
developed.pred <- -1.85 * developed
exposed.pred <- -0.35 * exposed
dist2roads.pred <- 1.35 * dist2roads.adj
ruggedness.pred <- 0.85 * ruggedness
slope.pred <- 1.50 * slope.adj
human.mod.pred <- -1.95 * human.mod


# Stack Precictor Rasters -------------------------------------------------

# Model 1:
cougar.hab.stack <- c(edge.hab.pred, road.dens.pred, prey.dens.pred, forest.pred, human.dens.pred, shrubland.pred, grassland.pred, ag.land.pred, 
                    wetland.pred, dist2water.pred , major.lake.pred, pipeline.dens.pred, developed.pred, exposed.pred, ruggedness.pred, slope.pred, human.mod.pred)

# Model 2:
cougar.hab.mod.simple <- c(road.dens.pred, forest.pred, prey.dens.pred, human.dens.pred, shrubland.pred, grassland.pred, dist2water.pred, elevation.pred,
                         dist2pa.pred, ag.land.pred)


# Convert to Probability Scale (IF NEEDED): -------------------------------

# Model 1:
cougar.hab.rast <- sum(cougar.hab.stack, na.rm=TRUE)
cougar.habitat.prob.rast <- (exp(cougar.hab.rast))/(1 + exp(cougar.hab.rast))
plot(cougar.habitat.prob.rast)

# Model 2:
cougar.hab.simple <- sum(cougar.hab.mod.simple, na.rm=TRUE)
cougar.hab.prob.rast.2 <- (exp(cougar.hab.simple))/(1 + exp(cougar.hab.simple))
plot(cougar.hab.prob.rast.2)


# Overlay our boundary line: ----------------------------------------------
bhb.50km.v <- vect(bhb.50km.boundary)

plot(cougar.habitat.prob.rast)
plot(bhb.50km.v, add=TRUE)


# Mask Habitat Model to BHB Watershed -------------------------------------
cougar.habitat.bhw <- terra::mask(cougar.habitat.prob.rast, bhw.v)
cougar.habitat.bhw.50km <- terra::mask(cougar.habitat.prob.rast, bhb.50km.v)

plot(cougar.habitat.bhw)

# Save habitat model(s): -----------------------------------------------------
writeRaster(cougar.hab.rast, "data/processed/cougar_raw_habitat_suitability.tif", overwrite=TRUE) # use THIS ONE for conflict analysis
writeRaster(cougar.habitat.prob.rast, "data/processed/cougar_habitat_suitability.tif", overwrite=TRUE) # for region beaver hills watershed
writeRaster(cougar.habitat.bhw.50km, "data/processed/cougar_habitat_bhw_50km.tif", overwrite=TRUE) # for 50km buf of beaver hills watershed
writeRaster(cougar.habitat.bhw, "data/processed/cougar_habitat_bhw.tif", overwrite=TRUE) # for boundary of beaver hills watershed




