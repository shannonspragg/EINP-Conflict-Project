# Buildling synthesized carnivore connectivity model ----------------------
  ## Here we seek to merge the three carnivore species-specific connectivity models into one overall model. This can be done 
  # a few different ways, either through weighted sums (fuzzy sum approach) or by multiplying each surface by it's weighted value and 
  # then stacking these together.


# Load packages -----------------------------------------------------------
library(raster)
library(terra)
library(sf)
library(tidyverse)


# Bring in data -----------------------------------------------------------
bbear.conf.cumcurr <- rast("data/processed/bbear_conflict_cum_currmap.tif")
bbear.conf.norm <- rast("data/processed/bbear_conflict_normalized_cum_currmap.tif")

wolf.conf.cumcurr <- rast("data/processed/wolf_conf_cum_currmap.tif")
wolf.conf.norm <- rast("data/processed/wolf_conf_normalized_cum_currmap.tif")

cougar.conf.cumcurr <- rast("data/processed/cougar_conflict_cum_currmap.tif")
cougar.conf.norm <- rast("data/processed/cougar_conflict_normalized_cum_currmap.tif")

all.conflict.df <- st_read("data/processed/conflict_conf_comp_dataframe.shp")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")

# Method 1: Multiple across using conflict report proportions ------------------------
    # Here we will multiply each surface by the proportion of species reports to total reports. Then we will add these weighted surfaces
    # together into one 

  # Look at counts of each species - this will determine weight:
sum(all.conflict.df$bears) # 588
sum(all.conflict.df$wolves) # 60
sum(all.conflict.df$cougars) # 203

  # Total wildlife reports is 4827, so proportions are as follows:
  #     black bear - 0.1218
  #     wolf - 0.0124
  #     cougar - 0.0421

## Multiply weights to each species connectivity model:
bbear.cum.adj <- 0.1218 * bbear.conf.cumcurr
bbear.norm.adj <- 0.1218 * bbear.conf.norm

wolf.cum.adj <- 0.0124 * wolf.conf.cumcurr
wolf.norm.adj <- 0.0124 * wolf.conf.norm

cougar.cum.adj <- 0.0421 * cougar.conf.cumcurr
cougar.norm.adj <- 0.0421 * cougar.conf.norm

## Stack to make our two carnivore connectivity models:
carnivore.cumcurr <- c(bbear.cum.adj, wolf.cum.adj, cougar.cum.adj)
carnivore.norm <- c(bbear.norm.adj, wolf.norm.adj, cougar.norm.adj)

carnivore.cumcurr.sum <- sum(carnivore.cumcurr)
carnivore.norm.sum <- sum(carnivore.norm)



# Crop these to the BHW for mapping: --------------------------------------
bhw.v <- vect(bhw)

carnivore.cumcurr.bhw <- terra::mask(carnivore.cumcurr.sum, bhw.v)
carnivore.norm.bhw <- terra::mask(carnivore.norm.sum, bhw.v)

plot(carnivore.cumcurr.bhw)
plot(carnivore.norm.bhw)



# Save files: -------------------------------------------------------------
writeRaster(carnivore.cumcurr.sum, "data/processed/carnivore_summarized_cumcurr.tif", overwrite = TRUE)
writeRaster(carnivore.norm.sum, "data/processed/carnivore_summarized_normalized.tif", overwrite = TRUE)

writeRaster(carnivore.cumcurr.bhw, "data/processed/bhw_carnivore_summarized_cumcurr.tif", overwrite = TRUE)
writeRaster(carnivore.norm.bhw, "data/processed/bhw_carnivore_summarized_normalized.tif", overwrite = TRUE)

