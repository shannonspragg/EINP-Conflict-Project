
# Prep Resistance Surfaces: -----------------------------------------------
    # Here we construct a general/species agnostic, generic focal species, and 
    # species-specific biophysical resistance surface


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Load Data: --------------------------------------------------------------
bhs <- rast("data/processed/bear_habitat_suitability.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ghm1 <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif") 
# ghm2 <- rast("/Users/mattwilliamson/Google Drive/My Drive/SpaSES Lab/Shared Data Sets/Wildlife Survey/data/original/rasters/gHMv1_300m_2017_static-0000046592-0000000000.tif")


ghm1.crp <- project(ghm1, temp.rast) # crop to bhw buffer
# ghm2.crp <- project(ghm2, bhs)
# ghm.mos <- mosaic(ghm1.crp, ghm2.crp, fun="max")
ghm.conv <- ghm1.crp/65536

bhb.bound <- st_read("Data/original/bhb_boundary.shp") %>% # MAKE SURE FILE IS UPDATED
  st_buffer(., 500000) %>% st_transform(., crs=crs(bhs)) %>% 
  as(., "SpatVector")
elev.can <- rast(raster::getData('alt', country = 'CAN'))
# elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
elev.can.crop <- crop(elev.can, project(bhw.bound, elev.can)) #crop to bhw
# elev.us.crop <- crop(elev.us, project(ona.bound, elev.us))
# elev.mos <- mosaic(elev.can.crop, elev.us.crop, fun="mean")

rough <- terrain(elev.can.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- project(rough.rescale, bhs)

fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
# Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum2(ghm.conv, rough.proj)
writeRaster(biophys_fuzsum,"Data/processed/biophys_fuzsum.tif" )
biophys_resistance <- (1+biophys_fuzsum)^10
writeRaster(biophys_resistance, "Data/processed/biophys_resist.tif")


# Generalist focal species biophysical resistance -------------------------
    # Prep model for "generalist" biophysical resistance. Combine land cover of alpine, forest, grassland, shrubland
    # as ideal habitat and then apply a resistance penalty to all other cover types (x conductance values by .5 for all pixels outside ideal veg type)



# Add prob conflict for biophys + social surface --------------------------

prob.bear.conf <- rast("Data/processed/prob_conflict_bears_bhw.tif")

fuzzysum3 <- function(r1, r2, r3) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m)
}
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum3(ghm.conv, rough.proj, prob.bear.conf)
writeRaster(bio_social_fuzzysum, "Data/processed/biosocial_fuzsum.tif",overwrite=TRUE)
biosocial_resistance <- (1+bio_social_fuzzysum)^10
writeRaster(biosocial_resistance, "Data/processed/biosocial_resist.tif", overwrite=TRUE)
