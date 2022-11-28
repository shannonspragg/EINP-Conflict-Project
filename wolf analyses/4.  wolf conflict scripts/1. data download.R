
# Download Data: Conflict Analysis  -----------------------------------------------------------
### Here we download any additional data needed for the conflict analysis

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Conflict reports:
folder_url <- "https://drive.google.com/drive/u/0/folders/1PtzYIXgkjPRiMGVMnHMwbzp2-8oYuhpc" # conflict data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Road Collision reports:
folder_url <- "https://drive.google.com/drive/folders/1aaITDOTD42WmuKx9Xuhr7Yp3-EA-I-6O" # collision data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# IEM reports:
folder_url <- "https://drive.google.com/drive/folders/1oUDvk1cdn00Xj9Letg6AyTzfpvhuqd4g" # IEM data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# BHW Boundary:
folder_url <- "https://drive.google.com/drive/u/0/folders/1Tf0IsrzM-F9mMHC9DOqCpNt8NfY9sUMn" # beaver hills watershed data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Human pop density:
folder_url <- "https://drive.google.com/drive/u/0/folders/1iu0qivB35FBkdGd-bV4yqkV5UTXvMEt_" # beaver hills watershed data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Agriculture Census:
folder_url <- "https://drive.google.com/drive/u/0/folders/1Ld354pG-79SNjh4In7SPBEqQLpRsDUJ1" # beaver hills watershed data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Consolidated Census Subdivision Regions:
folder_url <- "https://drive.google.com/drive/u/0/folders/1daTiZfnGEY8ylY7BQMA8lI4fk-msiutH" # canada CCS regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Road Network:
folder_url <- "https://drive.google.com/drive/u/0/folders/17HuuAcAdNvgmTwE-feWKhvQI2Yg97Ouh" # beaver hills watershed data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

