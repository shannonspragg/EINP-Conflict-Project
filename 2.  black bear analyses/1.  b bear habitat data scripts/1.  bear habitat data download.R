
# Download Data: Habitat Suitability -----------------------------------------------------------
### Here we download all of our "original" data

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)


# Crown lands:
folder_url <- "https://drive.google.com/drive/u/0/folders/1RUkqeZwTx-6YUgvhw8bi0GviRCk1cahY" # crown reservations data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# First Nation lands:
folder_url <- "https://drive.google.com/drive/u/0/folders/1zuzl3x5uD5-CRsXQ7w5GOkeQsHhoQofJ" # crown reservations data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# wildfire perimiters:
folder_url <- "https://drive.google.com/drive/u/0/folders/1uSWFXLtxlAw_LpU06rJAp7skUq74zHK_" # fire data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Road Network:
folder_url <- "https://drive.google.com/drive/u/0/folders/17HuuAcAdNvgmTwE-feWKhvQI2Yg97Ouh" # road data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))
