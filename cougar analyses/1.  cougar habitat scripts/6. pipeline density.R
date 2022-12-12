
# Calculate pipeline density ----------------------------------------------
    ## Here we bring in the pipeline data and crop it to the BHW area, then calculate the density of pipelines per km


# Load Packages -----------------------------------------------------------
library(sf)
library(terra)
library(raster)
library(dplyr)
library(tidyverse)


# Bring in data: ----------------------------------------------------------
pipelines <- st_read("data/original/Pipelines_GCS_NAD83.shp")
bhb.bound <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
temp.raster <- raster("data/processed/dist2pa_km_bhb.tif")

ab.pipes.reproj <- st_transform(pipelines, st_crs(bhb.bound))


# Make Pipelines Raster -------------------------------------------------------
bhb.buf.v <- vect(bhb.bound)
ab.pipes.v <- vect(ab.pipes.reproj)

bhb.pipes.crop <- terra::crop(ab.pipes.v, temp.rast)

# Pipelines raster:
bhb.pipelines <- terra::rasterize(bhb.pipes.crop, temp.rast, field = "OBJECTID")
bhb.pipelines[bhb.pipelines <= 61903044] <- 1

bhb.pipelines.raster <- raster(bhb.pipelines)
bhb.pipelines.raster[is.na(bhb.pipelines.raster[])] <- 0 

# Calculate pipeline density:

template.rast.1km <- rast(res=c(1000,1000), ext=ext(temp.raster)) # Let's do a 30x30 res to match land cover
crs(template.rast.1km) <- "epsg:32612" # UTM zone 12N for AB
values(template.rast.1km) <- rep(1, ncell(template.rast.1km))
temp.raster.1km <- raster(template.rast.1km)

pipes.crop <- st_crop(ab.pipes.reproj, c(xmin=257856.3, xmax=488106.3, ymin=5837774, ymax=6087024))
pipe.density.1km <- rasterize(pipes.crop, temp.raster.1km, fun='count', background=0)

pd.disagg <- raster::disaggregate(pipe.density.1km, 4) # change to 250m resolution
pd.rast <- rast(pd.disagg)
pd.crop <- terra::project(pd.rast, temp.rast)

# Save
writeRaster(pipe.density.1km, "data/processed/bhb_pipeline_density_1km.tif", overwrite=TRUE)
writeRaster(pd.crop, "data/processed/bhb_pipeline_density_250m.tif", overwrite=TRUE)
st_write(pipes.crop, "data/processed/bhb_pipelines.shp")
writeRaster(bhb.pipelines.raster, "data/processed/bhb_pipelines.tif", overwrite=TRUE)

