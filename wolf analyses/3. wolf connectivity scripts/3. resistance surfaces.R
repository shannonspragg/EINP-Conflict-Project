
# Prep Resistance Surfaces: -----------------------------------------------
    # Here we construct a wolf biophysical resistance surface, including wolf specialized habitat (forest/shrubland), 
# preference for higher elevations and slope, and resistance of agriculture land (due to depredation) / large lakes

## NOTE FOR OMNISCAPE: Wolf home range is variable, with recovering populations ranging from 259 to 1,676 km^2. Because of this, I am selecting
#  a home range size of 1,000km^2 for our models, which will require a radius input of ~18 and a block size of 1.7 (~1/10 of 18, odd)

# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Load Data: --------------------------------------------------------------
whs <- rast("data/processed/wolf_habitat_suitability.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ghm1 <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif") 
bhb.buf <- st_read("data/processed/bhb_50km.shp")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
wolf.specialist <- rast("data/processed/wolf_specialist_habitat.tif")
ag.land <- rast("data/processed/bhb_agriculture.tif")

# Prep gHM and elevation data:
ghm1.crp <- terra::project(ghm1, temp.rast) # crop to bhw buffer
ghm.conv <- ghm1.crp/65536

# Prep gHM and elevation data ---------------------------------------------
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


# Adjust rasters for resistance input: ------------------------------------
elev.can.crop
rough.proj

# Need to invert roughness:
install.packages("spatialEco")
library(spatialEco)
#elev.inv <- raster.invert(elev.can.crop)
rough.inv <- raster.invert(rough.proj)

# Prep wolf biophys resistance: ---------------------------------------
fuzzysum5 <- function(r1, r2, r3, r4, r5) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  rc4.1m <- (1-r4)
  rc5.1m <- (1-r5)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m)
}
# Add together our wolf biophys attributes: wolf habitat, agriculture, gHM, and roughness:
wolf_biophys_fuzsum <- fuzzysum5(ghm.conv, rough.inv, wolf.specialist, ag.land, bh.lake)
writeRaster(wolf_biophys_fuzsum,"data/processed/general_focal_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# generalist focal species resistance:
wolf_biophys_resistance <- (1+wolf_biophys_fuzsum)^10

writeRaster(wolf_biophys_resistance, "data/processed/wolf_biophys_resist.tif", overwrite=TRUE)

# Add prob conflict for biophys + social surface (after running conflict models)--------------------------

prob.wolf.conf <- rast("Data/processed/prob_conflict_wolf.tif")

fuzzysum6 <- function(r1, r2, r3, r4, r5, r6) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  rc4.1m <- (1-r4)
  rc5.1m <- (1-r5)
  rc6.1m <- (1-r6)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m*rc6.1m)
}
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum5(ghm.conv, rough.inv, ag.land, bh.lake, prob.wolf.conf)
biosocial_resistance <- (1+bio_social_fuzzysum)^10

writeRaster(bio_social_fuzzysum, "data/processed/biosocial_fuzsum.tif",overwrite=TRUE)
writeRaster(biosocial_resistance, "data/processed/biosocial_resist.tif", overwrite=TRUE)
