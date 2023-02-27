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
conflict.cougar.df <- st_read("data/processed/conflict_conf_comp_dataframe.shp")

# Predictor Rasters:
dist2water.rast <- rast("data/processed/dist2drainage_km_bhb.tif")
hum.dens.rast <- rast("data/processed/human_dens_bhb.tif")
ungulate.dens.rast <- rast("data/processed/total_ungulate_density.tif")
road.dens <- rast("data/processed/bhb_road_density_250m.tif")
pipeline.dens.rast <- rast("data/processed/bhb_pipeline_density_250m.tif")
ground.crop.rast <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.rast <- rast("data/processed/bhb_ndvi.tif")
edge.hab.rast <- rast("data/processed/forest_edge_habitats.tif")
ghm.rast <- rast("data/processed/bhw_ghm.tif")
chs <- rast("data/processed/cougar_habitat_suitability.tif")
#cougar.inc <- rast("data/processed/bhw_cougar_increase.tif") # STILL NEED
cougar_bio_cumcurrmap <- rast("data/processed/cougar_biophys_cum_currmap.tif")

# Pull out just cougar reports (for mapping purposes):
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
cougar.reports <- conflict.cougar.df %>% filter(conflict.cougar.df$cougars == "1")
cr.reproj <- st_transform(cougar.reports, st_crs(bhw))
cougar.reports.bhw <- st_intersection(cr.reproj, bhw) # This gives 2057 total reports
cougar.reports.bhw <- cougar.reports.bhw %>% distinct(id, .keep_all = TRUE) #rid of duplicates
st_write(cougar.reports.bhw, "Data/processed/confirmed_cougar_reports.shp", append = FALSE)

# Buffer Conflict Points Before Attributing Predictor Values -----------------------
# Here we buffer the conflict and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
conflict.buf <- conflict.cougar.df %>% 
  st_buffer(., 5000)
plot(st_geometry(conflict.buf)) # Check the buffers

c.conflict.reproj <- st_make_valid(conflict.buf) %>% 
    st_transform(crs=crs(dist2water.rast))

# Make the buffered points spat vectors:
cougar.con.buf.v <- vect(c.conflict.reproj)

crs(cougar.con.buf.v) == crs(ungulate.dens.rast) #TRUE

# Overlay conflict points with predictor rasters  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
conf.d2water.ext <- terra::extract(dist2water.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.humdens.ext <- terra::extract(hum.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.edge.hab.ext <- terra::extract(edge.hab.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.pipeline.dens.ext <- terra::extract(pipeline.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ground.dens.ext <- terra::extract(ground.crop.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ndvi.ext <- terra::extract(ndvi.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ungulate.dens.ext <- terra::extract(ungulate.dens.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.road.dens.ext <- terra::extract(road.dens, cougar.con.buf.v, mean, na.rm = TRUE)
conf.ghm.ext <- terra::extract(ghm.rast, cougar.con.buf.v, mean, na.rm = TRUE)
conf.chs.ext <- terra::extract(chs, cougar.con.buf.v, mean, na.rm = TRUE)
conf.bio.ext <- terra::extract(cougar_bio_cumcurrmap, cougar.con.buf.v, mean, na.rm = TRUE)
#conf.cougar.inc.ext <- terra::extract(cougar.inc, cougar.con.buf.v, mean, na.rm = TRUE)

# Create New Column(s) for Extracted Values:
c.conflict.reproj$dist2water_km <- conf.d2water.ext[,2]
c.conflict.reproj$hum_dens <- conf.humdens.ext[,2]
c.conflict.reproj$edge_habitats <- conf.edge.hab.ext[,2]
c.conflict.reproj$pipeline_dens <- conf.pipeline.dens.ext[,2]
c.conflict.reproj$groundcrop_dens <- conf.ground.dens.ext[,2]
c.conflict.reproj$ndvi <- conf.ndvi.ext[,2]
c.conflict.reproj$ungulate_dens <- conf.ungulate.dens.ext[,2]
c.conflict.reproj$road_dens <- conf.road.dens.ext[,2]
c.conflict.reproj$gHM <- conf.ghm.ext[,2]
c.conflict.reproj$chs <- conf.chs.ext[,2]
c.conflict.reproj$cougar_biophys <- conf.bio.ext[,2]
#c.conflict.reproj$cougar_inc <- conf.cougar.inc.ext[,2]


# Check for NA's:
which(is.na(c.conflict.reproj$dist2water_km)) #one
which(is.na(c.conflict.reproj$hum_dens)) #one
which(is.na(c.conflict.reproj$edge_habitats)) #one
which(is.na(c.conflict.reproj$pipeline_dens)) #one
which(is.na(c.conflict.reproj$groundcrop_dens)) #one
which(is.na(c.conflict.reproj$ndvi)) #one
which(is.na(c.conflict.reproj$ungulate_dens)) #one
which(is.na(c.conflict.reproj$road_dens)) #one
which(is.na(c.conflict.reproj$gHM)) #one
which(is.na(c.conflict.reproj$chs)) #one
which(is.na(c.conflict.reproj$cougar_biophys)) #one
#which(is.na(c.conflict.reproj$cougar_inc)) #one

# Remove 1130th row
#c.conflict.reproj <- c.conflict.reproj[-c(1130), ]

# Save this as new file ---------------------------------------------------

st_write(c.conflict.reproj, "Data/processed/cougar_confirmed_reports_full_df.shp", append = FALSE)


