
# Prep Resistance Surfaces: -----------------------------------------------
    # Here we construct a general/species agnostic, generic focal species, and 
    # species-specific biophysical resistance surface

# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Load Data: --------------------------------------------------------------
bhs <- rast("data/processed/bbear_habitat_suitability.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ghm1 <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif") 
bhb.buf <- st_read("data/processed/bhb_50km.shp")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
gen.focal.hab <- rast("data/processed/general_focal_species_habitat.tif")
forest.specialist <- rast("data/processed/forest_specialist_species_habitat.tif")

# Prep gHM and elevation data:
ghm1.crp <- terra::project(ghm1, temp.rast) # crop to bhw buffer
# ghm2.crp <- project(ghm2, bhs)
# ghm.mos <- mosaic(ghm1.crp, ghm2.crp, fun="max")
ghm.conv <- ghm1.crp/65536

bhw.bound <- st_read("data/original/BHB_Subwatershed_Boundary.shp") %>% # MAKE SURE FILE IS UPDATED
  st_buffer(., 500000) %>% st_transform(., crs=crs(temp.rast)) %>% 
  as(., "SpatVector")
bhb.buf.v <- vect(bhb.buf)
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.can.crop <- crop(elev.can, terra::project(bhw.bound, elev.can)) #crop to bhw

rough <- terrain(elev.can.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- terra::project(rough.rescale, temp.rast)

# Save elev and ruggedness:
writeRaster(elev.can.crop, "data/processed/elevation_bhw.tif")
writeRaster(rough.proj, "data/processed/topo_roughness_bhw.tif")

# Prep Species agnostic resistance: ---------------------------------------
fuzzysum3 <- function(r1, r2, r3) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
}
# Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum3(ghm.conv, rough.proj, bh.lake)
writeRaster(biophys_fuzsum,"Data/processed/agnostic_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# species agnostic resistance:
agno_biophys_resistance <- (1+biophys_fuzsum)^10

#agno_bio_resist_crop <- mask(agno_biophys_resistance, bhb.buf.v)
writeRaster(agno_biophys_resistance, "Data/processed/agnostic_biophys_resist.tif", overwrite=TRUE)
#writeRaster(agno_bio_resist_crop, "Data/processed/agnostic_biophys_resist_bhb.tif", overwrite=TRUE) #trip tp bhb outline


# Generalist focal species biophysical resistance -------------------------
    # Prep model for "generalist" biophysical resistance. Combine land cover of alpine, forest, grassland, shrubland
    # as ideal habitat and then apply a resistance penalty to all other cover types (x conductance values by .5 for all pixels outside ideal veg type)

fuzzysum4 <- function(r1, r2, r3, r4) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  rc4.1m <- (1-r4)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
}
# First inverse this so 1=resistant habitat and 0=ideal habitat:
gen.focal.hab[gen.focal.hab == 1] <- 900 # increase this for resistance
gen.focal.hab[gen.focal.hab == 0] <- 0.5 # increase this for resistance
gen.focal.hab[gen.focal.hab == 900] <- 0 # increase this for resistance

# Need to lessen gHM resistance:
general_adjusted_ghm <- ghm.conv / 1.2

# Add together our general focal biophys attributes: generalist habitat, gHM, and roughness:
gen_focal_fuzsum <- fuzzysum4(general_adjusted_ghm, rough.proj, gen.focal.hab, bh.lake)
writeRaster(gen_focal_fuzsum,"Data/processed/general_focal_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# generalist focal species resistance:
gfocal_biophys_resistance <- (1+gen_focal_fuzsum)^10

writeRaster(gfocal_biophys_resistance, "Data/processed/general_focal_biophys_resist.tif", overwrite=TRUE)

# Specialist species biophysical resistance -------------------------
# Prep model for "generalist" biophysical resistance. Combine land cover of alpine, forest, grassland, shrubland
# as ideal habitat and then apply a resistance penalty to all other cover types (x conductance values by .5 for all pixels outside ideal veg type)

fuzzysum4 <- function(r1, r2, r3, r4) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  rc4.1m <- (1-r4)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
}
# Add together our general focal biophys attributes: generalist habitat, gHM, and roughness:
forest_specialist_fuzsum <- fuzzysum4(ghm.conv, rough.proj, forest.specialist, bh.lake)
writeRaster(gen_focal_fuzsum,"Data/processed/general_focal_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# generalist focal species resistance:
forest_specialist_biophys_resistance <- (1+forest_specialist_fuzsum)^10

writeRaster(forest_specialist_biophys_resistance, "Data/processed/forest_specialist_biophys_resist.tif", overwrite=TRUE)



# Add prob conflict for biophys + social surface (after running conflict models)--------------------------

prob.bear.conf <- rast("Data/processed/prob_conflict_bear.tif")

# fuzzysum3 <- function(r1, r2, r3) {
#   rc1.1m <- (1-r1)
#   rc2.1m <- (1-r2)
#   rc3.1m <- (1-r3)
#   fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
# }
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum4(ghm.conv, rough.proj, prob.bear.conf, bh.lake)
biosocial_resistance <- (1+bio_social_fuzzysum)^10

writeRaster(bio_social_fuzzysum, "data/processed/biosocial_fuzsum.tif",overwrite=TRUE)
writeRaster(biosocial_resistance, "data/processed/biosocial_resist.tif", overwrite=TRUE)
