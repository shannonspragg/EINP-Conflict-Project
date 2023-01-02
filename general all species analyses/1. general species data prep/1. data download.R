
# Download Data: Variables for General Wildlife Conflict -----------------------------------------------------------
### Here we download all of our "original" data

# Load Packages -------------------------------------------------------
library(googledrive)
library(tidyverse)

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# Landcover:
folder_url <- "https://drive.google.com/drive/u/0/folders/17dnk1EGflfjieLTm5FqlRpg0mTS7cGIN" # NDVI data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# CANADA NDVI:
folder_url <- "https://drive.google.com/drive/u/0/folders/1fp8q0uPjcw9X1yR9J2QQynZebj8v7O7l" # lc data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Waterbodies:
folder_url <- "https://drive.google.com/drive/u/0/folders/13ZYA7j24didb0y3YAB4SBgp5vEK9aKzt" # water data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# gHM:
folder_url <- "https://drive.google.com/drive/u/0/folders/1KJ-szySEHTQiYPw7NqGfxHTvuxgTxCy7" # human mod data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# protected areas: 
folder_url <- "https://drive.google.com/drive/u/0/folders/1KGY3mL4bpzJLHaB7HPtP9wiRF3LTUIy9" 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/CPCAD-BDCAPC_Dec2020.gdb/ "), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# BHB watershed boundary:
folder_url <- "https://drive.google.com/drive/u/0/folders/1KPP40pBp5vFG0NOyR-VA5nRWXX4F3k0m" # bbh watershed boundary data
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
