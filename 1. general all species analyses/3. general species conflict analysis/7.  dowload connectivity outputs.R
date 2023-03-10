
# Download Biophysical Connectivity Models: --------------------------------

### Here we download our biophysical connectivity outputs (that resulted from our Omniscape models) 
## for the general conflict analysis

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Biophysical Connectivity Models:
folder_url <- "https://drive.google.com/drive/u/0/folders/1Duzc_0Ed-YsdQgpNq0SW7flPpPXewPx9" # biophys outputs 
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

sp_agnostic_cumcurr <- rast("data/processed/agnostic_cum_currmap.tif")
sp_agnostic_norm <- rast("data/processed/agnostic_normalized_cum_currmap.tif")

gen_focal_cumcurr <- rast("data/processed/general_focal_cum_currmap.tif")
gen_focal_norm <- rast("data/processed/general_focal_normalized_cum_currmap.tif")


# Make these vectors:
bhb.50km.v <- vect(bhb.50km.boundary)
bhw.v <- vect(bhb.watershed)

# Mask layers to the BHW buffer and boundary line -------------------------

# Crop our rasters to the BH watershed 50km buffer shape:

agno_cumcurr.bhb <- terra::mask(sp_agnostic_cumcurr, bhb.50km.v)
agno_norm.bhb <- terra::mask(sp_agnostic_norm, bhb.50km.v)

gfocal_cumcurr.bhb <- terra::mask(gen_focal_cumcurr, bhb.50km.v)
gfocal_norm.bhb <- terra::mask(gen_focal_norm, bhb.50km.v)

# Crop to BHW boundary:
agno_cumcurr.bhw <- terra::mask(sp_agnostic_cumcurr, bhw.v)
agno_norm.bhw <- terra::mask(sp_agnostic_norm, bhw.v)

gfocal_cumcurr.bhw <- terra::mask(gen_focal_cumcurr, bhw.v)
gfocal_norm.bhw <- terra::mask(gen_focal_norm, bhw.v)

# Variables with 50km buffer of BHW:
writeRaster(agno_cumcurr.bhb, "data/processed/bhw_agno_cumcurr_50km.tif", overwrite=TRUE)
writeRaster(agno_norm.bhb, "data/processed/bhw_agno_norm_50km.tif", overwrite=TRUE)

writeRaster(gfocal_cumcurr.bhb, "data/processed/bhw_gfocal_cumcurr_50km.tif", overwrite=TRUE)
writeRaster(gfocal_norm.bhb, "data/processed/bhw_gfocal_norm_50km.tif", overwrite=TRUE)


# Variables with boundary of BHW:
writeRaster(agno_cumcurr.bhw, "data/processed/bhw_agno_cumcurr.tif", overwrite=TRUE)
writeRaster(agno_norm.bhw, "data/processed/bhw_agno_norm.tif", overwrite=TRUE)

writeRaster(gfocal_cumcurr.bhw, "data/processed/bhw_gfocal_cumcurr.tif", overwrite=TRUE)
writeRaster(gfocal_norm.bhw, "data/processed/bhw_gfocal_norm.tif", overwrite=TRUE)

