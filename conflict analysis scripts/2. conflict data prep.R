
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
bhb.10k.buf <- st_read("data/processed/bhb_10km.shp")

  # Add columns: Convert selected species to 1's and all others to 0's:
conflict.data <- conflicts %>% 
  mutate(conflicts, bears = if_else(OCC_SPECIES == "BLACK BEAR", 1, 0)) %>%
           mutate(conflicts, wolves = if_else(OCC_SPECIES == "WOLF", 1, 0)) %>%
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
conflict.data.reproj <- mutate(conflict.data.reproj, id = row_number())
conflict.data.reproj <- conflict.data.reproj %>%           # Reorder data frame
  dplyr::select("id", everything())
conflict.data.reproj$id <- as.numeric(conflict.data.reproj$id)
# Filter to only columns we need:

conflict.dataset <- conflict.data.reproj %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                                        'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'geometry'))

# Crop reports down to BHB watershed:
st_crs(conflict.dataset) == st_crs(bhb.10k.buf) #FALSE
conflict.reproj <- st_transform(conflict.dataset, st_crs(bhb.10k.buf))
st_crs(conflict.reproj) == st_crs(bhb.10k.buf) #TRUE

conflict.bhb.10k.buf <- st_intersection(conflict.reproj, bhb.10k.buf) # This gives 2,876 total reports
sum(conflict.bhb.10k.buf$OCC_SPECIES == "BLACK BEAR") # still 142 b bear
sum(conflict.bhb.10k.buf$OCC_SPECIES == "WOLF") # 14 wolf
sum(conflict.bhb.10k.buf$OCC_SPECIES == "COUGAR") # still 212 cougar

head(conflict.bhb.10k.buf)
conflict.bhb <- conflict.bhb.10k.buf %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'AREA', 'Area_sqkm', 'geometry'))


st_write(conflict.dataset, "data/processed/conflict_dataframe.shp", append = FALSE)
st_write(conflict.bhb, "data/processed/conflict_reports_bhb.shp", append = FALSE)

