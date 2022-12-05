
# Compare original bear HSI with "validated" HSI --------------------------

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
original_bbear_habsuit <- rast("data/processed/bbear_habitat_bhw.tif")
validated_bbear_habsui <- rast("data/processed/bbear_val_habitat_bhw.tif")

original_bbear_50km <- rast("data/processed/bbear_habitat_bhw_50km.tif")
validated_bbear_50km <- rast("data/processed/bbear_val_habitat_bhw_50km.tif")

  # Bear collar data:
bear.collars <- st_read("data/processed/bear_collar_data.shp")
projection(bear.collars)

einp <- st_read("data/processed/einp_reproj.shp") 
einp.10km <- st_buffer(einp, 10000)
einp10km.v <- vect(einp.10km)
st_write(einp.10km, "data/processed/einp_10km.shp")
# Look at collar points over our study area -------------------------------
bear.collars.sv <- vect(bear.collars)

  # With original HSI model:
plot(original_bbear_habsuit)
plot(bear.collars.sv, add=T)

  # With "validated" HSI model:
plot(validated_bbear_habsui)
plot(bear.collars.sv, add=T)

  # Plot bears in 10km EINP buffer:
plot(einp10km.v)
plot(bear.collars.sv, add=TRUE, col="red")

# Compare % overlap of the two models: ------------------------------------

  #For Subwatershed boundary:
bbear.habitat.corr <- rasterCorrelation(original_bbear_habsuit, validated_bbear_habsui, type = "pearson")
plot(bbear.habitat.corr)
#contour(bbear.habitat.corr, add=TRUE)
title("Estimated and Validated Habitat Correlation for BH Subwatershed") 

writeRaster(bbear.habitat.corr, "data/processed/bbear_habitat_val_correlation.tif")

  # For 50km buffer:
bbear.habitat.50km.corr <- rasterCorrelation(original_bbear_50km, validated_bbear_50km, type = "pearson")
plot(bbear.habitat.50km.corr)
title("Estimated and Validated Habitat Correlation for 50km buffer of BH Subwatershed") 

# Filter these for values less than 0.5 (looking for > 50% overlap:
bbear.habitat.corr[bbear.habitat.corr > 0.6] <- 1
bbear.habitat.corr[bbear.habitat.corr < 0.59] <- 0

plot(bbear.habitat.corr)
#contour(bbear.habitat.corr, add=TRUE)
title("Estimated and Validated Habitat Correlation for BH Subwatershed") 
writeRaster(bbear.habitat.corr, "data/processed/bbear_habitat_corr_60%.tif")

# Plotting the Correlation: -----------------------------------------------

# Plot all together:
plot(bbear.habitat.corr)
plot(bear.collars.sv, pch=2, col = "red", add=TRUE) #this works...
# plot(g.bears.vect, pch=19, col = "black",add=TRUE)
title("% Correlation between Estimated and Validated Bbear Habitat for Subwatershed") 
legend("topright",   # set position
       inset = 0.05, # Distance from the margin as a fraction of the plot region
       legend = c("Black Bear Collar Points"),
       pch = c(2),
       col = c("red"))


