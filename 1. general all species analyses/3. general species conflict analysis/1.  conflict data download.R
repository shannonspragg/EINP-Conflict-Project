
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

# Conflict reports (+ wolf depredations):
folder_url <- "https://drive.google.com/drive/u/0/folders/1PtzYIXgkjPRiMGVMnHMwbzp2-8oYuhpc" # conflict data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Predator Compensations claims:
folder_url <- "https://drive.google.com/drive/u/0/folders/18GaXCxqceCKoAsB9wWdPuj-OJKDIX2Gc" # compensation data
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

# Large Carnivore Sightings:
folder_url <- "https://drive.google.com/drive/u/0/folders/1fiHabmnFNJ_FTJAI8Cx0P-IQbxoeytT0" # compensation data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))
