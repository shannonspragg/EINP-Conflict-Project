
# Download Data -----------------------------------------------------------
### Here we download all of our "original" data

# Load Packages -------------------------------------------------------
library(googledrive)


# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Landcover:
folder_url <- "https://drive.google.com/drive/u/0/folders/17dnk1EGflfjieLTm5FqlRpg0mTS7cGIN" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/alberta_landcover/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Waterbodies:
folder_url <- "https://drive.google.com/drive/u/0/folders/13ZYA7j24didb0y3YAB4SBgp5vEK9aKzt" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/canada_waterbodies/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Crown lands:
folder_url <- "https://drive.google.com/drive/u/0/folders/1RUkqeZwTx-6YUgvhw8bi0GviRCk1cahY" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/crown_lands/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Forest inventory:
folder_url <- "https://drive.google.com/drive/u/0/folders/1HGyXYmvq2H8P-f47hC3wqlRtTd_Zj1Dz" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/forest_inventory.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# gHM:
folder_url <- "https://drive.google.com/drive/u/0/folders/1KJ-szySEHTQiYPw7NqGfxHTvuxgTxCy7" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/gHMv1_300m_2017_static/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# protected areas:
folder_url <- "https://drive.google.com/drive/u/0/folders/1Y_1qqMKRPM_v_sReqCHD_p7aaZeVQ-Zp" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/protected_areas/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# wildfire perimiters:
folder_url <- "https://drive.google.com/drive/u/0/folders/1uSWFXLtxlAw_LpU06rJAp7skUq74zHK_" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/wildfire_perimiters/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

