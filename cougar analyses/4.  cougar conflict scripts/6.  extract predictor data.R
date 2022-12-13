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
conflict.cougar.df <- st_read("data/processed/conflict_conf_iem_dataframe.shp")
c.pres.abs.df <- st_read("data/processed/cougar_conflict_pres_abs_df.shp")

# Predictor Rasters:
dist2water.rast <- rast("data/processed/dist2drainage_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
ungulate.dens.rast <- rast("data/processed/total_ungulate_density.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
pipeline.dens.rast <- rast("data/processed/bhb_pipeline_density_250m.tif")
edge.hab.rast <- rast("data/processed/forest_edge_habitats.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
chs <- rast("data/processed/cougar_habitat_suitability.tif")
cougar.inc <- rast("data/processed/bhw_cougar_increase.tif") # STILL NEED
cougar_bio_cumcurrmap <- rast("data/processed/cougar_biophys_cum_currmap.tif")

# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.cougar.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

pres.abs.buf <- c.pres.abs.df %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

c.conflict.reproj <- st_make_valid(conflict.buf) %>% 
    st_transform(crs=crs(dist2water.rast))
c.pres.abs.reproj <- st_make_valid(pres.abs.buf) %>% 
  st_transform(crs=crs(dist2water.rast))


# Make the buffered points spat vectors:
cougar.con.buf.v <- vect(c.c.conflict.reproj)
pres.abs.buf.v <- vect(c.c.pres.abs.reproj)

crs(cougar.con.buf.v) == crs(ungulate.dens.rast) #TRUE


# Overlay conflict points with predictor rasters  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
conf.d2water.ext <- terra::extract(dist2water.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.humdens.ext <- terra::extract(hum.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.edge.hab.ext <- terra::extract(edge.hab.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.pipeline.dens.ext <- terra::extract(pipeline.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ungulate.dens.ext <- terra::extract(ungulate.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.road.dens.ext <- terra::extract(road.dens, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ghm.ext <- terra::extract(ghm.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.chs.ext <- terra::extract(chs, cougar.con.buf.v, mean, na.rm = TRUE)
conf.bio.ext <- terra::extract(cougar_bio_cumcurrmap, cougar.con.buf.v, mean, na.rm = TRUE)
conf.cougar.inc.ext <- terra::extract(cougar.inc, cougar.con.buf.v, mean, na.rm = TRUE)

pa.d2water.ext <- terra::extract(dist2water.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.humdens.ext <- terra::extract(hum.dens.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.edge.hab.ext <- terra::extract(edge.hab.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.pipeline.dens.ext <- terra::extract(pipeline.dens.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.ungulate.dens.ext <- terra::extract(ungulate.dens.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.road.dens.ext <- terra::extract(road.dens, pres.abs.buf.v, mean, na.rm = TRUE)
pa.ghm.ext <- terra::extract(ghm.rast, pres.abs.buf.v, mean, na.rm = TRUE)
pa.chs.ext <- terra::extract(chs, pres.abs.buf.v, mean, na.rm = TRUE)
pa.bio.ext <- terra::extract(cougar_bio_cumcurrmap, pres.abs.buf.v, mean, na.rm = TRUE)
pa.cougar.inc.ext <- terra::extract(cougar.inc, pres.abs.buf.v, mean, na.rm = TRUE)

# Create New Column(s) for Extracted Values:
c.conflict.reproj$dist2water_km <- conf.d2water.ext[,2]
c.conflict.reproj$hum_dens <- conf.humdens.ext[,2]
c.conflict.reproj$edge_habitats <- conf.edge.hab.ext[,2]
c.conflict.reproj$pipeline_dens <- conf.pipeline.dens.ext[,2]
c.conflict.reproj$ungulate_dens <- conf.ungulate.dens.ext[,2]
c.conflict.reproj$road_dens <- conf.road.dens.ext[,2]
c.conflict.reproj$gHM <- conf.ghm.ext[,2]
c.conflict.reproj$chs <- conf.chs.ext[,2]
c.conflict.reproj$cougar_biophys <- conf.bio.ext[,2]
c.conflict.reproj$cougar_inc <- conf.cougar.inc.ext[,2]

c.pres.abs.reproj$dist2water_km <- pa.d2water.ext[,2]
c.pres.abs.reproj$hum_dens <- pa.humdens.ext[,2]
c.pres.abs.reproj$edge_habitats <- pa.edge.hab.ext[,2]
c.pres.abs.reproj$ground_crop <- pa.pipeline.dens.ext[,2]
c.pres.abs.reproj$ungulate_dens <- pa.ungulate.dens.ext[,2]
c.pres.abs.reproj$road_dens <- pa.road.dens.ext[,2]
c.pres.abs.reproj$gHM <- pa.ghm.ext[,2]
c.pres.abs.reproj$chs <- pa.chs.ext[,2]
c.pres.abs.reproj$cougar_biophys <- pa.bio.ext[,2]
c.pres.abs.reproj$cougar_inc <- pa.cougar.inc.ext[,2]

# Check for NA's:
which(is.na(c.conflict.reproj$dist2water_km)) #none
which(is.na(c.conflict.reproj$hum_dens)) #none
which(is.na(c.conflict.reproj$edge_habitats)) #none
which(is.na(c.conflict.reproj$pipeline_dens)) #none
which(is.na(c.conflict.reproj$ungulate_dens)) #none
which(is.na(c.conflict.reproj$road_dens)) #none
which(is.na(c.conflict.reproj$gHM)) #none
which(is.na(c.conflict.reproj$chs)) #none
which(is.na(c.conflict.reproj$cougar_biophys)) #none
which(is.na(c.conflict.reproj$cougar_inc)) #none

which(is.na(c.pres.abs.reproj$dist2water_km)) #none
which(is.na(c.pres.abs.reproj$hum_dens)) #none
which(is.na(c.pres.abs.reproj$edge_habitats)) #none
which(is.na(c.pres.abs.reproj$pipeline_dens)) #none
which(is.na(c.pres.abs.reproj$ungulate_dens)) #none
which(is.na(c.pres.abs.reproj$road_dens)) #none
which(is.na(c.pres.abs.reproj$gHM)) #none
which(is.na(c.pres.abs.reproj$chs)) #none
which(is.na(c.pres.abs.reproj$cougar_biophys)) #none
which(is.na(c.pres.abs.reproj$cougar_inc)) #none

# Save this as new file ---------------------------------------------------

st_write(c.conflict.reproj, "Data/processed/cougar_confirmed_reports_full_df.shp", append = FALSE)
st_write(c.pres.abs.reproj, "Data/processed/cougar_pres_abs_full_df.shp", append=FALSE)

# Pull out just cougar reports (for mapping purposes):
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
cougar.reports <- c.conflict.reproj %>% filter(c.conflict.reproj$wolves == "1")
wr.reproj <- st_transform(cougar.reports, st_crs(bhw))
cougar.reports.bhw <- st_intersection(br.reproj, bhw) # This gives 2057 total reports
cougar.reports.bhw <- cougar.reports.bhw %>% distinct(id, .keep_all = TRUE) #rid of duplicates
st_write(cougar.reports.bhw, "Data/processed/confirmed_cougar_reports.shp", append = FALSE)

