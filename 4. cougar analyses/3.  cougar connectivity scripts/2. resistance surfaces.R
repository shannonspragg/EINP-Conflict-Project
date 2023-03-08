
# Prep Resistance Surfaces: -----------------------------------------------
    # Here we construct a cougar biophysical resistance surface, including cougar specialized habitat (forest/shrubland), 
# preference for higher elevations and slope, and resistance of agriculture land (due to depredation) / large lakes

## NOTE FOR OMNISCAPE: cougar home range is variable, with recovering populations ranging from 50 to 350 km^2. Because of this, I am selecting
#  a home range size of 140km^2 for our models, which will require a radius input of 8 and a block size of 0.7 (~1/10 of 18, odd)

# Load Packages: ----------------------------------------------------------
library(terra)
library(sf)
library(tidyverse)
library(raster)

# Load Data: --------------------------------------------------------------
chs <- rast("data/processed/cougar_habitat_suitability.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ghm1 <- rast("data/original/gHMv1_300m_2017_static-0000000000-0000000000.tif") 
bhb.buf <- st_read("data/processed/bhb_50km.shp")
bh.lake <- rast("data/processed/beaverhills_lake.tif")
cougar.specialist <- rast("data/processed/cougar_specialist_habitat_resist.tif") # this is already in "resistance" form (low values = more ideal habitat)
slope <- rast("data/processed/slope_bhb.tif")
elev.can.crop <- rast("data/processed/elevation_bhw.tif")
rough.proj <- rast("data/processed/topo_roughness_bhw.tif")

# Prep gHM and elevation data:
ghm1.crp <- terra::project(ghm1, temp.rast) # crop to bhw buffer
ghm.conv <- ghm1.crp/65536


# Adjust rasters for resistance input: ------------------------------------
slope
rough.proj

# Need to invert roughness:
#install.packages("spatialEco")
library(spatialEco)
#slope.inv <- raster.invert(slope)
rough.inv <- raster.invert(rough.proj) # let's use just one since this is so similar to slope

# Prep cougar biophys resistance: ---------------------------------------
# fuzzysum5 <- function(r1, r2, r3, r4, r5) {
#   rc1.1m <- (1-r1)
#   rc2.1m <- (1-r2)
#   rc3.1m <- (1-r3)
#   rc4.1m <- (1-r4)
#   rc5.1m <- (1-r5)
#   fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m)
# }
fuzzysum4 <- function(r1, r2, r3, r4) {
  rc1.1m <- (1-r1)
  rc2.1m <- (1-r2)
  rc3.1m <- (1-r3)
  rc4.1m <- (1-r4)
  fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m)
}
# Add together our cougar biophys attributes: cougar habitat, gHM, and roughness:
cougar_biophys_fuzsum <- fuzzysum4(ghm.conv, rough.inv, cougar.specialist, bh.lake)
writeRaster(cougar_biophys_fuzsum,"data/processed/cougar_biophys_fuzsum_bhb.tif", overwrite=TRUE)

# cougar resistance:
cougar_biophys_resistance <- (1+cougar_biophys_fuzsum)^10

writeRaster(cougar_biophys_resistance, "data/processed/cougar_biophys_resist.tif", overwrite=TRUE)

# Add prob conflict for biophys + social surface (after running conflict models)--------------------------

prob.cougar.conf <- rast("Data/processed/prob_conflict_cougar.tif") # try this with smoothed one

# fuzzysum6 <- function(r1, r2, r3, r4, r5, r6) {
#   rc1.1m <- (1-r1)
#   rc2.1m <- (1-r2)
#   rc3.1m <- (1-r3)
#   rc4.1m <- (1-r4)
#   rc5.1m <- (1-r5)
#   rc6.1m <- (1-r6)
#   fuz.sum <- 1-(rc1.1m*rc2.1m*rc3.1m*rc4.1m*rc5.1m*rc6.1m)
# }
# # Add together our biophys attributes + grizz inc resist: gHM, and roughness + grizz resist
bio_social_fuzzysum <- fuzzysum4(ghm.conv, rough.inv, bh.lake, prob.cougar.conf)
biosocial_resistance <- (1+bio_social_fuzzysum)^10

plot(biosocial_resistance)

writeRaster(bio_social_fuzzysum, "data/processed/cougar_biosocial_fuzsum.tif",overwrite=TRUE)
writeRaster(biosocial_resistance, "data/processed/cougar_biosocial_resist.tif", overwrite=TRUE)

writeRaster(bio_social_fuzzysum, "data/processed/cougar_biosocial_fuzsum_smoothed.tif",overwrite=TRUE)
writeRaster(biosocial_resistance, "data/processed/cougar_biosocial_resist_smoothed.tif", overwrite=TRUE)
