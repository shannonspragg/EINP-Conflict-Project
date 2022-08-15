
# Download Data -----------------------------------------------------------
### Here we download all of our "original" data

# Load Packages -------------------------------------------------------
library(googledrive)


# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# AG Census
folder_url <- "https://drive.google.com/drive/u/0/folders/1BIlP5M-I3gJbGgMt8XbJQDr5myt7lRut" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))
