# Building synthesized carnivore connectivity model ----------------------
  ## Here we seek to merge the three carnivore species-specific connectivity models into one overall model. This can be done 
  # a few different ways, either through weighted sums (fuzzy sum approach) or by multiplying each surface by it's weighted value and 
  # then stacking these together.

# Load packages -----------------------------------------------------------
library(raster)
library(terra)
library(sf)
library(tidyverse)

# Bring in data -----------------------------------------------------------
bbear.conf.cumcurr <- rast("data/processed/bbear_conf_collar_validated_cum_currmap.tif") # trying this with smoothed models
bbear.conf.norm <- rast("data/processed/bbear_conf_collar_validated_normalized_cum_currmap.tif")

wolf.conf.cumcurr <- rast("data/processed/wolf_conf_cum_currmap.tif")
wolf.conf.norm <- rast("data/processed/wolf_conf_normalized_cum_currmap.tif")

# cougar.conf.cumcurr <- rast("data/processed/smoothed_cougar_conflict_cum_currmap.tif")
# cougar.conf.norm <- rast("data/processed/smoothed_cougar_conflict_normalized_cum_currmap.tif")

all.conflict.df <- st_read("data/processed/conflict_conf_comp_dataframe.shp")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")

# Method 1: Multiple across using conflict report proportions ------------------------
    # Here we will multiply each surface by the proportion of species reports to total reports. Then we will add these weighted surfaces
    # together into one 

  # Look at counts of each species - this will determine weight:
sum(all.conflict.df$bears) # 588
sum(all.conflict.df$wolves) # 60
sum(all.conflict.df$cougars) # 203

  # Total carnivore reports is 851, so proportions are as follows:
  #     black bear - 0.691 (588/851) OR 0.907 (588/648 WITHOUT COUGAR)
  #     wolf - 0.071 (60/851) OR 0.0926 (60/648 WITHOUT COUGAR)
  #     cougar - 0.2385 (203/851)

## Multiply weights to each species connectivity model:
  ## IMPORTANT NOTE: multiplying weights will mess up the "normalized" value range (indicating diffuse = 0, restricted <0, etc.)
  ## We can't weight and stack the normalized outputs because they're already "weighted", so it would mess these up

bbear.cum.adj <- 0.907 * bbear.conf.cumcurr

wolf.cum.adj <- 0.0926 * wolf.conf.cumcurr

# cougar.cum.adj <- 0.0421 * cougar.conf.cumcurr

## Stack to make our two carnivore connectivity models:
# NOTE: we can't weight and stack the normalized outputs because they're already "weighted", so it would mess these up
carnivore.cumcurr <- c(bbear.cum.adj, wolf.cum.adj) #, cougar.cum.adj)
carnivore.cumcurr.sum <- sum(carnivore.cumcurr)

plot(carnivore.cumcurr.sum)

# Try to calculate % correlation for norm outputs -------------------------
carnivore.normalized.corr <- rasterCorrelation(bbear.conf.norm, wolf.conf.norm, s = 3, type = "spearman")
plot(carnivore.normalized.corr)
title("Estimated Bear and Wolf Connectivity Correlation for BH Watershed") 

# Crop these to the BHW for mapping: --------------------------------------
bhw.v <- vect(bhw)

carnivore.cumcurr.bhw <- terra::mask(carnivore.cumcurr.sum, bhw.v)
carnivore.norm.corr.bhw <- terra::mask(carnivore.normalized.corr, bhw.v)
plot(carnivore.cumcurr.bhw)
plot(carnivore.norm.corr.bhw)

# Save files: -------------------------------------------------------------
writeRaster(carnivore.cumcurr.sum, "data/processed/carnivore_summarized_cumcurr.tif", overwrite = TRUE)

writeRaster(carnivore.cumcurr.bhw, "data/processed/bhw_carnivore_summarized_cumcurr.tif", overwrite = TRUE)
writeRaster(carnivore.norm.corr.bhw, "data/processed/bhw_carnivore_normalized_correlation.tif", overwrite = TRUE)

