# Add bear predictors to the Conflict & PresAbs Data ---------------------------
    # Here we will extract our predictors to the conflict dataframe using our
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
conflict.conf.df <- st_read("data/processed/conflict_conf_comp_dataframe.shp")

# Predictor Rasters:
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
animal.prod.rast <- rast("data/processed/animal_production_density_raster.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
bhs <- rast("data/processed/bbear_validated_habitat_suitability.tif")
bbear_bio_cumcurrmap <- rast("data/processed/bbear_collar_validated_cum_currmap.tif")

# Pull out just bear reports (for mapping purposes):
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bear.reports <- conflict.conf.df %>% filter(conflict.reproj$bears == "1")
br.reproj <- st_transform(bear.reports, st_crs(bhw))
bear.reports.bhw <- st_intersection(br.reproj, bhw) # This gives 2057 total reports
bear.reports.bhw <- bear.reports.bhw %>% distinct(id, .keep_all = TRUE) #rid of duplicates
st_write(bear.reports.bhw, "Data/processed/confirmed_bear_reports.shp", append = FALSE)

# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.conf.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

conflict.reproj <- st_make_valid(conflict.buf) %>% 
    st_transform(crs=crs(dist2pa.rast))

# Make the buffered points spat vectors:
conflict.sv.buf <- vect(conflict.reproj)

crs(conflict.sv.buf) == crs(ndvi.rast) #TRUE

# Overlay conflict points with predictor rasters  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
conf.d2pa.ext <- terra::extract(dist2pa.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.humdens.ext <- terra::extract(hum.dens.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.animal.prod.ext <- terra::extract(animal.prod.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.ground.crop.ext <- terra::extract(ground.crop.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.ndvi.ext <- terra::extract(ndvi.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.ghm.ext <- terra::extract(ghm.rast, conflict.sv.buf, mean, na.rm = TRUE)
conf.bhs.ext <- terra::extract(bhs, conflict.sv.buf, mean, na.rm = TRUE)
conf.biophys.ext <- terra::extract(bbear_bio_cumcurrmap, conflict.sv.buf, mean, na.rm = TRUE)


# Create New Column(s) for Extracted Values:
conflict.reproj$dist2pa_km <- conf.d2pa.ext[,2]
conflict.reproj$hum_dens <- conf.humdens.ext[,2]
conflict.reproj$animal_farms <- conf.animal.prod.ext[,2]
conflict.reproj$ground_crops <- conf.ground.crop.ext[,2]
conflict.reproj$ndvi <- conf.ndvi.ext[,2]
conflict.reproj$gHM <- conf.ghm.ext[,2]
conflict.reproj$bhs <- conf.bhs.ext[,2]
conflict.reproj$biophys <- conf.biophys.ext[,2]

# Check for NA's:
which(is.na(conflict.reproj$dist2pa_km)) #none
which(is.na(conflict.reproj$hum_dens)) #none
which(is.na(conflict.reproj$animal_farms)) #none
which(is.na(conflict.reproj$ground_crops)) #none
which(is.na(conflict.reproj$ndvi)) #none
which(is.na(conflict.reproj$gHM)) #none
which(is.na(conflict.reproj$bhs)) #none
which(is.na(conflict.reproj$biophys)) #none

# remove the NA quick:
#conflict.reproj <- conflict.reproj[-c(1130), ]  

# Save this as new file ---------------------------------------------------
conflict.reproj
st_write(conflict.reproj, "Data/processed/bbear_confirmed_reports_full_df.shp", append = FALSE)


