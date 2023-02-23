
# Download Biophysical Connectivity Models: --------------------------------

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
folder_url <- "https://drive.google.com/drive/u/0/folders/1Duzc_0Ed-YsdQgpNq0SW7flPpPXewPx9" # biophys outputs male b bears 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/processed/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Conflict Connectivity Models: (completed after conflict analysis)
folder_url <- "https://drive.google.com/drive/u/0/folders/1VQwuyleCv9idrK2nZKiyPiUrwWtE2gqV" # conflict outputs b bears 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/processed/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Let's crop these to the BHW boundary and 50km buffer --------------------
library(terra)
library(sf)

# Bring in covariate data: -------------------------------------------------------------
bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
bhb.watershed <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

bbear_collar_biophys_cumcurr <- rast("data/processed/bbear_collar_validated_cum_currmap.tif")
bbear_collar_biophys_norm <- rast("data/processed/bbear_collar_validated_normalized_cum_currmap.tif")

# Conflict connectivity outputs: (you won't have these until after conflict analysis)
bbear_conflict_cumcurr <- rast("data/processed/smoothed_bbear_conf_collar_validated_cum_currmap.tif") # trying with smoothed ones
bbear_conflict_norm <- rast("data/processed/smoothed_bbear_conf_collar_validated_normalized_cum_currmap.tif")

# Make these vectors:
bhw.v <- vect(bhb.watershed)

# Mask layers to the BHW  boundary line -------------------------

# Crop to BHW boundary:
bbear_biophys_cumcurr.bhw <- terra::mask(bbear_collar_biophys_cumcurr, bhw.v)
bbear_biophys_norm.bhw <- terra::mask(bbear_collar_biophys_norm, bhw.v)

bbear_conflict_cumcurr.bhw <- terra::mask(bbear_conflict_cumcurr, bhw.v)
bbear_conflict_norm.bhw <- terra::mask(bbear_conflict_norm, bhw.v)

bbear_conflict_cumcurr.smooth.bhw <- terra::mask(bbear_conflict_cumcurr, bhw.v)
bbear_conflict_norm.smooth.bhw <- terra::mask(bbear_conflict_norm, bhw.v)

# Plot
plot(bbear_conflict_cumcurr.smooth.bhw)
plot(bbear_conflict_norm.smooth.bhw)

# Variables with boundary of BHW:
writeRaster(bbear_biophys_cumcurr.bhw, "data/processed/bhw_bbear_collar_biophys_cumcurr.tif", overwrite=TRUE)
writeRaster(bbear_biophys_norm.bhw, "data/processed/bhw_bbear_collar_biophys_norm.tif", overwrite=TRUE)

writeRaster(bbear_conflict_cumcurr.bhw, "data/processed/bhw_bbear_conflict_cumcurr.tif", overwrite=TRUE)
writeRaster(bbear_conflict_norm.bhw, "data/processed/bhw_bbear_conflict_norm.tif", overwrite=TRUE)

writeRaster(bbear_conflict_cumcurr.smooth.bhw, "data/processed/bhw_smooth_bbear_conflict_cumcurr.tif", overwrite=TRUE)
writeRaster(bbear_conflict_norm.smooth.bhw, "data/processed/bhw_smooth_bbear_conflict_norm.tif", overwrite=TRUE)
