
# Prep conflict data for analysis: ----------------------------------------

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in our Original Data --------------------------------------------
 
conflicts <- read.csv("data/original/Beaver Hills Biosphere - all species.csv")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
can.ccs.shp<- st_make_valid(st_read("Data/original/lccs000b21a_e.shp"))

  # Add columns: Convert selected species to 1's and all others to 0's:
conflict.data <- conflicts %>% 
  mutate(conflicts, bears = if_else(OCC_SPECIES == "BLACK BEAR", 1, 0)) %>%
           mutate(conflicts, cougar = if_else(OCC_SPECIES == "WOLF", 1, 0)) %>%
           mutate(conflicts, cougars = if_else(OCC_SPECIES == "COUGAR", 1, 0))

head(conflict.data) # Check this to make sure it looks good

  # Total NA locations:
sum(is.na(conflict.data$OCC_LATITIUDE))
sum(is.na(conflict.data$OCC_LONGITUDE))

  # Drop reports with NA locations:
conflict.no.na <- conflict.data[complete.cases(conflict.data[,c("OCC_LATITIUDE")]),] # This leaves us 9759 total obs

  # Check # of target species:
sum(conflict.no.na$OCC_SPECIES == "BLACK BEAR") # still 928 b bear
sum(conflict.no.na$OCC_SPECIES == "WOLF") # still 116 wolf
sum(conflict.no.na$OCC_SPECIES == "COUGAR") # still 523 cougar

  # Making Conflict Data a Spatial Dataframe 
xy<-conflict.no.na[,c(6,5)]
conflict.spdf<-SpatialPointsDataFrame(coords = xy,data = conflict.no.na,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
# conflict.data.sf <- as(conflict.spdf, "sf")
conflict.data.sf <- st_as_sf(conflict.spdf)

head(conflict.data.sf)
str(conflict.data.sf)

# Set projection:
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

conflict.data.reproj <- conflict.data.sf %>% st_transform(., crs(temp.rast))
str(conflict.data.reproj)

# Filter to only columns we need:

conflict.data.filt <- conflict.data.reproj %>% 
  dplyr::select(., c('OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'cougar', 'cougars', 'geometry', 'OCC_OCCURRENCE_TMST', 'SITE_NAME'))

conflict.data.filt <- mutate(conflict.data.filt, id = row_number())
conflict.data.filt <- conflict.data.filt %>%           # Reorder data frame
  dplyr::select("id", everything())
conflict.data.filt$id <- as.numeric(conflict.data.filt$id)

# Save for next script:
st_write(conflict.data.filt, "data/processed/conflict_no_filt.shp", append = FALSE)
