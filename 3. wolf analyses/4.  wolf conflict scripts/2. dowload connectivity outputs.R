
# Dowload Biophysical Connectivity Models: --------------------------------

### Here we download our biophysical connectivity outputs for the conflict analysis

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Biophysical Connectivity Models:
folder_url <- "https://drive.google.com/drive/u/0/folders/1nA45dcS9BW6JvW6fQaHgJqGSEd4AkJ-j" # biophys outputs wolves 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/processed/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Conflict Connectivity Models: (completed after conflict analysis)
folder_url <- "https://drive.google.com/drive/u/0/folders/1ZYs53kD_VvilaS42DGwkHwu3tpwu_5SS" # biophys outputs wolf
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/processed/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Let's crop these to the BHW boundary and 50km buffer --------------------
library(terra)

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.watershed <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

wolf_biophys_cumcurr <- rast("data/processed/wolf_biophys_cum_currmap.tif")
wolf_biophys_norm <- rast("data/processed/wolf_biophys_normalized_cum_currmap.tif")

# Conflict connectivity outputs: (you won't have these until after conflict analysis)
wolf_conflict_cumcurr <- rast("data/processed/wolf_conf_cum_currmap.tif")
wolf_conflict_norm <- rast("data/processed/wolf_conf_normalized_cum_currmap.tif")

# wolf_conf_smooth_cumcurr <- rast("data/processed/smoothed_wolf_conf_cum_currmap.tif")
# wolf_conf_smooth_norm <- rast("data/processed/smoothed_wolf_conf_normalized_cum_currmap.tif")

# Make these vectors:
bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Mask layers to the BHW buffer and boundary line -------------------------

# Crop to BHW boundary:
wolf_bio_cumcurr.bhw <- terra::mask(wolf_biophys_cumcurr, bhw.v)
wolf_bio_norm.bhw <- terra::mask(wolf_biophys_norm, bhw.v)

wolf_conflict_cumcurr.bhw <- terra::mask(wolf_conflict_cumcurr, bhw.v)
wolf_conflict_norm.bhw <- terra::mask(wolf_conflict_norm, bhw.v)

# wolf_conf_smooth_cumcurr.bhw <- terra::mask(wolf_conf_smooth_cumcurr, bhw.v)
# wolf_conf_smooth_norm.bhw <- terra::mask(wolf_conf_smooth_norm, bhw.v)

plot(wolf_conflict_cumcurr.bhw)
plot(wolf_conflict_norm.bhw)

# Variables with boundary of BHW:
writeRaster(wolf_bio_cumcurr.bhw, "data/processed/bhw_wolf_biophys_cumcurr.tif", overwrite=TRUE)
writeRaster(wolf_bio_norm.bhw, "data/processed/bhw_wolf_biophys_norm.tif", overwrite=TRUE)

writeRaster(wolf_conflict_cumcurr.bhw, "data/processed/bhw_wolf_conflict_cumcurr.tif", overwrite=TRUE)
writeRaster(wolf_conflict_norm.bhw, "data/processed/bhw_wolf_conflict_norm.tif", overwrite=TRUE)

# writeRaster(wolf_conf_smooth_cumcurr.bhw, "data/processed/bhw_smoothed_wolf_conflict_cumcurr.tif", overwrite=TRUE)
# writeRaster(wolf_conf_smooth_norm.bhw, "data/processed/bhw_smoothed_wolf_conflict_norm.tif", overwrite=TRUE)
