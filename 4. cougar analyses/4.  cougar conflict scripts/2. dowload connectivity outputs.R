
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
folder_url <- "https://drive.google.com/drive/u/0/folders/1QWytFrA7to6sdhhFjrQz5ex1SXm2bqVf" # biophys outputs cougars 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/processed/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Conflict Connectivity Models: (completed after conflict analysis)
folder_url <- "https://drive.google.com/drive/u/0/folders/1uTsMaX2p__Z0FrkKu9JemxlbpVREVm37" # conflict outputs cougars
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

cougar_biophys_cumcurr <- rast("data/processed/cougar_biophys_cum_currmap.tif")
cougar_biophys_norm <- rast("data/processed/cougar_biophys_normalized_cum_currmap.tif")

# Conflict connectivity outputs: (you won't have these until after conflict analysis)
cougar_conflict_cumcurr <- rast("data/processed/bbear_conflict_cum_currmap.tif")
cougar_conflict_norm <- rast("data/processed/bbear_conflict_normalized_cum_currmap.tif")

# Make these vectors:
bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Mask layers to the BHW buffer and boundary line -------------------------

# Crop our rasters to the BH watershed 50km buffer shape:

cougar_bio_cumcurr.bhb <- terra::mask(cougar_biophys_cumcurr, bhb.50km.v)
cougar_bio_norm.bhb <- terra::mask(cougar_biophys_norm, bhb.50km.v)

cougar_conflict_cumcurr.bhb <- terra::mask(cougar_conflict_cumcurr, bhb.50km.v)
cougar_conflict_norm.bhb <- terra::mask(cougar_conflict_norm, bhb.50km.v)

# Crop to BHW boundary:
cougar_bio_cumcurr.bhw <- terra::mask(cougar_biophys_cumcurr, bhw.v)
cougar_bio_norm.bhw <- terra::mask(cougar_biophys_norm, bhw.v)

cougar_conflict_cumcurr.bhw <- terra::mask(cougar_conflict_cumcurr, bhw.v)
cougar_conflict_norm.bhw <- terra::mask(cougar_conflict_norm, bhw.v)

# Variables with 50km buffer of BHW:
writeRaster(cougar_bio_cumcurr.bhb, "data/processed/bhw_cougar_biophys_cumcurr_50km.tif", overwrite=TRUE)
writeRaster(cougar_bio_norm.bhb, "data/processed/bhw_cougar_biophys_norm_50km.tif", overwrite=TRUE)

writeRaster(bbear_conflict_cumcurr.bhb, "data/processed/bhw_cougar_conflict_cumcurr_50km.tif", overwrite=TRUE)
writeRaster(bbear_conflict_norm.bhb, "data/processed/bhw_cougar_conflict_norm_50km.tif", overwrite=TRUE)

# Variables with boundary of BHW:
writeRaster(cougar_bio_cumcurr.bhw, "data/processed/bhw_cougar_biophys_cumcurr.tif", overwrite=TRUE)
writeRaster(cougar_bio_norm.bhw, "data/processed/bhw_cougar_biophys_norm.tif", overwrite=TRUE)

writeRaster(cougar_conflict_cumcurr.bhw, "data/processed/bhw_cougar_conflict_cumcurr.tif", overwrite=TRUE)
writeRaster(cougar_conflict_norm.bhw, "data/processed/bhw_cougar_conflict_norm.tif", overwrite=TRUE)


