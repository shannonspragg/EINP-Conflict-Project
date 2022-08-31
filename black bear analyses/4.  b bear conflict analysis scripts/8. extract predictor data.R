# Add predictors to the Conflict & PresAbs Data ---------------------------
    # Here we will extract our predictors to the conflict dataframe and pres-abs dataframe using our
    # produced predictor rasters


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)

# Bring in Data: ----------------------------------------------------------

# Conflict Data:
conflict.dataframe <- st_read("data/processed/conflict_reports_bhb.shp")
pres.abs.dataframe <- st_read("data/processed/conflict_pres_abs_dataframe.shp")

# Predictor Rasters:
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_crop.tif")
animal.prod.rast <- rast("data/processed/animal_production_density_raster.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
ghm.rast <- rast("data/processed/bhb_ghm.tif")

# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the WARP and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

pres.abs.buf <- pres.abs.df %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

# Make the buffered points spat vectors:
conflict.sv.buf <- vect(conflict.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

crs(conflict.sv.buf) == crs(animal.prod.rast) #TRUE



