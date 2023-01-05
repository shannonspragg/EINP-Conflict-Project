# Compare cumulative current flow for connectivity models based on the integrated and collar "validated" HSI for bears --------------------------

## Here we look at the percent overlap of the two raster predictions.

## NOTE: Because the bear collar data is limited, this "validated" HSI extrapolates out to a much larger area than we have data for.
# So it may not be as accurate for the broader study region as it would be for the EINP area

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)
#install.packages("spatialEco")
library(spatialEco)


# Bring in data: ----------------------------------------------------------
# HSI layers:
biophys_integrated_cumcurr <- rast("data/processed/bbear_val_int_biophys_cum_currmap.tif")
biophys_validated_cumcurr <- rast("data/processed/collar_validated_cum_currmap.tif")

biophys_integrated_norm <- rast("data/processed/bbear_val_int_biophys_normalized_cum_currmap.tif")
biophys_validated_norm <- rast("data/processed/collar_validated_normalized_cum_currmap.tif")

plot(biophys_integrated_norm, col=plasma(256), axes = TRUE, main = "Biophysical Integrated HSI Connectivity")
plot(biophys_validated_norm, col=plasma(256), axes = TRUE, main = "Biophysical Validated HSI Connectivity")

# Compare % overlap of the two models: ------------------------------------

#For Subwatershed boundary:
bbear.hsi.current.corr <- rasterCorrelation(biophys_integrated_norm, biophys_validated_norm, type = "pearson")
plot(bbear.hsi.current.corr)

# Filter these for values less than 0.8 (looking for > 80% overlap:
bbear.hsi.current.corr[bbear.hsi.current.corr > 0.8] <- 1
plot(bbear.hsi.current.corr)
title("% Correlation of Integrated and Collar Validated Habitat Connectivity") 

writeRaster(bbear.hsi.current.corr, "data/processed/bbear_hsi_connectivity_correlation.tif")

