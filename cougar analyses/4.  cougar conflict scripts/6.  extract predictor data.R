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
conflict.wolf.df <- st_read("data/processed/conflict_conf_iem_dataframe.shp")
w.pres.abs.df <- st_read("data/processed/wolf_conflict_pres_abs_df.shp")

# Predictor Rasters:
dist2pa.rast <- rast("data/processed/dist2pa_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
animal.prod.rast <- rast("data/processed/animal_production_density_raster.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ungulate.dens.rast <- rast("data/processed/total_ungulate_density.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
whs <- rast("data/processed/wolf_habitat_suitability.tif")
wolf.inc <- rast("data/processed/bhw_wolf_increase.tif") # STILL NEED
wolf_bio_cumcurrmap <- rast("data/processed/wolf_biophys_cum_currmap.tif")

# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.wolf.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

pres.abs.buf <- w.pres.abs.df %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

w.conflict.reproj <- st_make_valid(conflict.buf) %>% 
    st_transform(crs=crs(dist2pa.rast))
w.pres.abs.reproj <- st_make_valid(pres.abs.buf) %>% 
  st_transform(crs=crs(dist2pa.rast))


# Make the buffered points spat vectors:
wolf.con.buf.v <- vect(w.w.conflict.reproj)
pres.abs.buf.v <- vect(w.w.pres.abs.reproj)

crs(wolf.con.buf.v) == crs(ungulate.dens.rast) #TRUE


# Overlay conflict points with predictor rasters  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
conf.d2pa.ext <- terra::extract(dist2pa.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.humdens.ext <- terra::extract(hum.dens.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.animal.prod.ext <- terra::extract(animal.prod.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.ground.crop.ext <- terra::extract(ground.crop.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.ungulate.dens.ext <- terra::extract(ungulate.dens.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.road.dens.ext <- terra::extract(road.dens, wolf.con.buf.v, mean, na.rm = TRUE)
conf.ghm.ext <- terra::extract(ghm.rast, wolf.con.buf.v, mean, na.rm = TRUE)
conf.whs.ext <- terra::extract(whs, wolf.con.buf.v, mean, na.rm = TRUE)
conf.bio.ext <- terra::extract(wolf_bio_cumcurrmap, wolf.con.buf.v, mean, na.rm = TRUE)
conf.wolf.inc.ext <- terra::extract(wolf.inc, wolf.con.buf.v, mean, na.rm = TRUE)

pa.d2pa.ext <- terra::extract(dist2pa.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.humdens.ext <- terra::extract(hum.dens.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.animal.prod.ext <- terra::extract(animal.prod.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.ground.crop.ext <- terra::extract(ground.crop.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.ungulate.dens.ext <- terra::extract(ungulate.dens.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.road.dens.ext <- terra::extract(road.dens, pres.abs.buf.v, mean, na.rm = TRUE)
pa.ghm.ext <- terra::extract(ghm.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.whs.ext <- terra::extract(whs, pres.abs.buf.v, mean, na.rm = TRUE)
pa.bio.ext <- terra::extract(wolf_bio_cumcurrmap, pres.abs.buf.v, mean, na.rm = TRUE)
pa.wolf.inc.ext <- terra::extract(wolf.inc, pres.abs.buf.v, mean, na.rm = TRUE)

# Create New Column(s) for Extracted Values:
w.conflict.reproj$dist2pa_km <- conf.d2pa.ext[,2]
w.conflict.reproj$hum_dens <- conf.humdens.ext[,2]
w.conflict.reproj$animal_farms <- conf.animal.prod.ext[,2]
w.conflict.reproj$ground_crops <- conf.ground.crop.ext[,2]
w.conflict.reproj$ungulate_dens <- conf.ungulate.dens.ext[,2]
w.conflict.reproj$road_dens <- conf.road.dens.ext[,2]
w.conflict.reproj$gHM <- conf.ghm.ext[,2]
w.conflict.reproj$whs <- conf.whs.ext[,2]
w.conflict.reproj$wolf_biophys <- conf.bio.ext[,2]
w.conflict.reproj$wolf_inc <- conf.wolf.inc.ext[,2]

w.pres.abs.reproj$dist2pa_km <- pa.d2pa.ext[,2]
w.pres.abs.reproj$hum_dens <- pa.humdens.ext[,2]
w.pres.abs.reproj$animal_farms <- pa.animal.prod.ext[,2]
w.pres.abs.reproj$ground_crop <- pa.ground.crop.ext[,2]
w.pres.abs.reproj$ungulate_dens <- pa.ungulate.dens.ext[,2]
w.pres.abs.reproj$road_dens <- pa.road.dens.ext[,2]
w.pres.abs.reproj$gHM <- pa.ghm.ext[,2]
w.pres.abs.reproj$whs <- pa.whs.ext[,2]
w.pres.abs.reproj$wolf_biophys <- pa.bio.ext[,2]
w.pres.abs.reproj$wolf_inc <- pa.wolf.inc.ext[,2]

# Check for NA's:
which(is.na(w.conflict.reproj$dist2pa_km)) #none
which(is.na(w.conflict.reproj$hum_dens)) #none
which(is.na(w.conflict.reproj$animal_farms)) #none
which(is.na(w.conflict.reproj$ground_crops)) #none
which(is.na(w.conflict.reproj$ungulate_dens)) #none
which(is.na(w.conflict.reproj$road_dens)) #none
which(is.na(w.conflict.reproj$gHM)) #none
which(is.na(w.conflict.reproj$whs)) #none
which(is.na(w.conflict.reproj$wolf_biophys)) #none
which(is.na(w.conflict.reproj$wolf_inc)) #none

which(is.na(w.pres.abs.reproj$dist2pa_km)) #none
which(is.na(w.pres.abs.reproj$hum_dens)) #none
which(is.na(w.pres.abs.reproj$animal_farms)) #none
which(is.na(w.pres.abs.reproj$ground_crops)) #none
which(is.na(w.pres.abs.reproj$ungulate_dens)) #none
which(is.na(w.pres.abs.reproj$road_dens)) #none
which(is.na(w.pres.abs.reproj$gHM)) #none
which(is.na(w.pres.abs.reproj$whs)) #none
which(is.na(w.pres.abs.reproj$wolf_biophys)) #none
which(is.na(w.pres.abs.reproj$wolf_inc)) #none

# Save this as new file ---------------------------------------------------

st_write(w.conflict.reproj, "Data/processed/wolf_confirmed_reports_full_df.shp", append = FALSE)
st_write(w.pres.abs.reproj, "Data/processed/wolf_pres_abs_full_df.shp", append=FALSE)

# Pull out just wolf reports (for mapping purposes):
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
wolf.reports <- w.conflict.reproj %>% filter(w.conflict.reproj$wolves == "1")
wr.reproj <- st_transform(wolf.reports, st_crs(bhw))
wolf.reports.bhw <- st_intersection(br.reproj, bhw) # This gives 2057 total reports
wolf.reports.bhw <- wolf.reports.bhw %>% distinct(id, .keep_all = TRUE) #rid of duplicates
st_write(wolf.reports.bhw, "Data/processed/confirmed_wolf_reports.shp", append = FALSE)

