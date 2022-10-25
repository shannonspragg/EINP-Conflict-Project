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
conflict.conf.df <- st_read("data/processed/conflict_conf_iem_dataframe.shp")
pres.abs.df <- st_read("data/processed/conflict_pres_abs_df.shp")

# Predictor Rasters:
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
animal.prod.rast <- rast("data/processed/animal_production_density_raster.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
bhs <- rast("data/processed/bbear_habitat_suitability.tif")
agnostic_bio_cumcurrmap <- rast("data/processed/agnostic_cum_currmap.tif")
forest_specialist_bio_cumcurrmap <- rast("data/processed/forest_specialist_cum_currmap.tif")
gen_focal_bio_cumcurrmap <- rast("data/processed/general_focal_cum_currmap.tif")


# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.conf.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

pres.abs.buf <- pres.abs.df %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

conflict.reproj <- st_make_valid(conflict.buf) %>% 
    st_transform(crs=crs(dist2pa.rast))
pres.abs.reproj <- st_make_valid(pres.abs.buf) %>% 
  st_transform(crs=crs(dist2pa.rast))


# Make the buffered points spat vectors:
conflict.sv.buf <- vect(conflict.reproj)
pres.abs.sv.buf <- vect(pres.abs.reproj)

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
conf.agno.ext <- terra::extract(agnostic_bio_cumcurrmap, conflict.sv.buf, mean, na.rm = TRUE)
conf.gfocal.ext <- terra::extract(gen_focal_bio_cumcurrmap, conflict.sv.buf, mean, na.rm = TRUE)
conf.forest.sp.ext <- terra::extract(forest_specialist_bio_cumcurrmap, conflict.sv.buf, mean, na.rm = TRUE)

pa.d2pa.ext <- terra::extract(dist2pa.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.humdens.ext <- terra::extract(hum.dens.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.animal.prod.ext <- terra::extract(animal.prod.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ground.crop.ext <- terra::extract(ground.crop.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ndvi.ext <- terra::extract(ndvi.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.ghm.ext <- terra::extract(ghm.rast, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.bhs.ext <- terra::extract(bhs, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.agno.ext <- terra::extract(agnostic_bio_cumcurrmap, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.gfocal.ext <- terra::extract(gen_focal_bio_cumcurrmap, pres.abs.sv.buf, mean, na.rm = TRUE)
pa.forest.sp.ext <- terra::extract(forest_specialist_bio_cumcurrmap, pres.abs.sv.buf, mean, na.rm = TRUE)

# Create New Column(s) for Extracted Values:
conflict.reproj$dist2pa_km <- conf.d2pa.ext[,2]
conflict.reproj$hum_dens <- conf.humdens.ext[,2]
conflict.reproj$animal_farms <- conf.animal.prod.ext[,2]
conflict.reproj$ground_crops <- conf.ground.crop.ext[,2]
conflict.reproj$ndvi <- conf.ndvi.ext[,2]
conflict.reproj$gHM <- conf.ghm.ext[,2]
conflict.reproj$bhs <- conf.bhs.ext[,2]
conflict.reproj$agno_biophys <- conf.agno.ext[,2]
conflict.reproj$gfocal_biophys <- conf.gfocal.ext[,2]
conflict.reproj$forest_sp_biophys <- conf.forest.sp.ext[,2]

pres.abs.reproj$dist2pa_km <- pa.d2pa.ext[,2]
pres.abs.reproj$hum_dens <- pa.humdens.ext[,2]
pres.abs.reproj$animal_farms <- pa.animal.prod.ext[,2]
pres.abs.reproj$ground_crop <- pa.ground.crop.ext[,2]
pres.abs.reproj$ndvi <- pa.ndvi.ext[,2]
pres.abs.reproj$gHM <- pa.ghm.ext[,2]
pres.abs.reproj$bhs <- pa.bhs.ext[,2]
pres.abs.reproj$agno_biophys <- pa.agno.ext[,2]
pres.abs.reproj$gfocal_biophys <- pa.gfocal.ext[,2]
pres.abs.reproj$forest_sp_biophys <- pa.forest.sp.ext[,2]


# Check for NA's:
which(is.na(conflict.reproj$dist2pa_km)) #none
which(is.na(conflict.reproj$hum_dens)) #none
which(is.na(conflict.reproj$animal_farms)) #none
which(is.na(conflict.reproj$ground_crops)) #none
which(is.na(conflict.reproj$ndvi)) #none
which(is.na(conflict.reproj$gHM)) #none
which(is.na(conflict.reproj$bhs)) #none
which(is.na(conflict.reproj$agno_biophys)) #none
which(is.na(conflict.reproj$gfocal_biophys)) #none
which(is.na(conflict.reproj$forest_sp_biophys)) #none

which(is.na(pres.abs.reproj$dist2pa_km)) #none
which(is.na(pres.abs.reproj$hum_dens)) #none
which(is.na(pres.abs.reproj$animal_farms)) #none
which(is.na(pres.abs.reproj$ground_crops)) #none
which(is.na(pres.abs.reproj$ndvi)) #none
which(is.na(pres.abs.reproj$gHM)) #none
which(is.na(pres.abs.reproj$bhs)) #none
which(is.na(pres.abs.reproj$agno_biophys)) #none
which(is.na(pres.abs.reproj$gfocal_biophys)) #none
which(is.na(pres.abs.reproj$forest_sp_biophys)) #none


# Save this as new file ---------------------------------------------------

st_write(conflict.reproj, "Data/processed/confirmed_reports_full_df.shp", append = FALSE)
st_write(pres.abs.reproj, "Data/processed/pres_abs_full_df.shp", append=FALSE)

# Pull out just bear reports (for mapping purposes):
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bear.reports <- conflict.reproj %>% filter(conflict.reproj$bears == "1")
br.reproj <- st_transform(bear.reports, st_crs(bhw))
bear.reports.bhw <- st_intersection(br.reproj, bhw) # This gives 2057 total reports
bear.reports.bhw <- bear.reports.bhw[!duplicated(bear.reports.bhw), ]
st_write(bear.reports.bhw, "Data/processed/confirmed_bear_reports.shp", append = FALSE)

