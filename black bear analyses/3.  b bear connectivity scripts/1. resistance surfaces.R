
# Prep Resistance Surfaces: -----------------------------------------------
    # Here we construct a general/species agnostic, generic focal species, and 
    # species-specific biophysical resistance surface


# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)


# Load Data: --------------------------------------------------------------
bhs <- rast("data/processed/bbear_habitat_suitability.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ghm1 <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif") 
bhb.buf <- st_read("data/processed/bhb_50km.shp")
bh.lake <- rast("data/processed/beaverhills_lake.tif")

ghm1.crp <- project(ghm1, temp.rast) # crop to bhw buffer
# ghm2.crp <- project(ghm2, bhs)
# ghm.mos <- mosaic(ghm1.crp, ghm2.crp, fun="max")
ghm.conv <- ghm1.crp/65536

bhb.bound <- st_read("Data/original/BHB_BOUNDARY.shp") %>% # MAKE SURE FILE IS UPDATED
  st_buffer(., 500000) %>% st_transform(., crs=crs(temp.rast)) %>% 
  as(., "SpatVector")
bhb.buf.v <- vect(bhb.buf)
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.can.crop <- crop(elev.can, project(bhb.bound, elev.can)) #crop to bhw

rough <- terrain(elev.can.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.proj <- project(rough.rescale, temp.rast)

bh.lake[bh.lake == 1] <- 900 # increase this for resistance

fuzzysum2 <- function(r1, r2) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  fuz.sum <- 1-(rc1.1m*rc2.1m)
}
# Add together our biophys attributes: gHM and roughness
biophys_fuzsum <- fuzzysum2(ghm.conv, rough.proj)
writeRaster(biophys_fuzsum,"Data/processed/agnostic_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# species agnostic resistance:
agno_biophys_resistance <- (1+biophys_fuzsum)^10
agno_bio_resist <- agno_biophys_resistance + bh.lake # add in the lake

#agno_bio_resist_crop <- mask(agno_biophys_resistance, bhb.buf.v)
writeRaster(agno_biophys_resistance, "Data/processed/agnostic_biophys_resist.tif", overwrite=TRUE)
#writeRaster(agno_bio_resist_crop, "Data/processed/agnostic_biophys_resist_bhb.tif", overwrite=TRUE) #trip tp bhb outline


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
