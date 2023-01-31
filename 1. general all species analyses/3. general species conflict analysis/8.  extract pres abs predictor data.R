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
library(raster)

# Bring in Data: ----------------------------------------------------------

# Conflict Data:
pres.abs.df <- st_read("data/processed/conflict_pres_abs_df.shp")

# Predictor Rasters:
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
animal.prod.rast <- rast("data/processed/animal_production_density_raster.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
agnostic_bio_cumcurrmap <- rast("data/processed/agnostic_cum_currmap.tif")
#gen_focal_bio_cumcurrmap <- rast("data/processed/general_focal_cum_currmap.tif") # I don't think we need this since we have species agnostic biophys 


# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons

pres.abs.buf <- pres.abs.df %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

pres.abs.reproj <- st_make_valid(pres.abs.buf) %>% 
  st_transform(crs=crs(dist2pa.rast))


# Make the buffered points spat vectors:
pres.abs.sv.buf <- vect(pres.abs.reproj)

crs(pres.abs.sv.buf) == crs(ndvi.rast) #TRUE

# Overlay conflict points with predictor rasters  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
pa.d2pa.ext <- terra::extract(dist2pa.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.humdens.ext <- terra::extract(hum.dens.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.animal.prod.ext <- terra::extract(animal.prod.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ground.crop.ext <- terra::extract(ground.crop.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ndvi.ext <- terra::extract(ndvi.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ghm.ext <- terra::extract(ghm.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.agno.ext <- terra::extract(agnostic_bio_cumcurrmap, pres.abs.sv.buf, mean, na.rm = TRUE)
#pa.gfocal.ext <- terra::extract(gen_focal_bio_cumcurrmap, pres.abs.sv.buf, mean, na.rm = TRUE)

# Create New Column(s) for Extracted Values:
pres.abs.reproj$dist2pa_km <- pa.d2pa.ext[,2]
pres.abs.reproj$hum_dens <- pa.humdens.ext[,2]
pres.abs.reproj$animal_farms <- pa.animal.prod.ext[,2]
pres.abs.reproj$ground_crop <- pa.ground.crop.ext[,2]
pres.abs.reproj$ndvi <- pa.ndvi.ext[,2]
pres.abs.reproj$gHM <- pa.ghm.ext[,2]
pres.abs.reproj$agno_biophys <- pa.agno.ext[,2]
#pres.abs.reproj$gfocal_biophys <- pa.gfocal.ext[,2]


# Check for NA's:
which(is.na(pres.abs.reproj$dist2pa_km)) #none
which(is.na(pres.abs.reproj$hum_dens)) #none
which(is.na(pres.abs.reproj$animal_farms)) #none
which(is.na(pres.abs.reproj$ground_crops)) #none
which(is.na(pres.abs.reproj$ndvi)) #none
which(is.na(pres.abs.reproj$gHM)) #none
which(is.na(pres.abs.reproj$agno_biophys)) #none
#which(is.na(pres.abs.reproj$gfocal_biophys)) #none

# remove the NA quick:
#pres.abs.reproj <- pres.abs.reproj[-c(1130), ]  

# Save this as new file ---------------------------------------------------
st_write(pres.abs.reproj, "Data/processed/pres_abs_full_df.shp", append=FALSE)


