# Prep Private Land Data ----------------------------------------------------
    # We will create a private lands raster by taking the inverse of public (crown), protected, and 
    # first nation lands in AB

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Load First Nation data -------------------------------------------------------------

ab_firstnations <- st_read("data/original/AL_TA_AB_2_146_CONFIRMED_eng.shp")


# Crop to our Region --------------------------------------------------------
bhb.buf <- st_read("data/processed/bhb_50km.shp") # change to bhw

firstnation.reproj<- st_transform(ab_firstnations, st_crs(bhb.buf))

st_crs(firstnation.reproj) == st_crs(bhb.buf)

# st_is_valid(ab_firstnations)

# Try this in terra:
template.rast <- rast("data/processed/dist2pa_km_bhb.tif") # should be bhw

bhb.v <- vect(bhb.buf)

firstnat.proj <- firstnation.reproj %>% 
  st_transform(., crs=crs(template.rast)) %>%
  as(., "SpatVector")

# Plot to see if there are any within our boundary:
plot(firstnat.proj)
plot(bhb.v, add=TRUE) # No First Nation reserves in the BHW

# bhb.firstnation.rast <- terra::rasterize(firstnat.proj, template.rast, field = "NAME1")
# bhb.firstnation.rast <- terra::mask(bhb.firstnation.rast, bhb.v)

# Stack our public,  protected & native lands: ----------------------------

  # Load PA and crown rasters:
bhb.pas <- rast("data/processed/bhb_protected_areas.tif") #update to bhw

bhb.crowns <- rast("data/processed/bhb_crownlands.tif")

# Stack these into one raster:
land.tenure.stack <- terra::merge(bhb.pas, bhb.crowns) #Need these to be in categories somehow

bhb.rast <- terra::rasterize(bhb.v, template.rast, field = "OBJECTID")
land.tenure.rsmpl <- terra::resample(land.tenure.stack, bhb.rast)
# land.tenure.stack[land.tenure.stack > -1] <- 1 # make our public, native, and protected lands 1




# Make private lands from the inverse of our other land tenures
private.lands <- terra::mask(land.tenure.rsmpl, bhb.rast, updatevalue=150)
private.lands[private.lands == 1] <- 0
private.lands[private.lands == 100] <- 1


private.lands.rast <- terra::mask(private.lands, bhb.v)
names(private.lands.rast)[names(private.lands.rast) == "OBJECTID"] <- "private_lands"

terra::writeRaster(private.lands.rast, "data/processed/bhb_privatelands.tif", overwrite=TRUE)
