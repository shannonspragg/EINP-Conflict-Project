
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

# Filter data to confirmed and probable occurances (let's see if this is enough):
  # Just confirmed and probable:
conflict.data.conf <- dplyr::filter(conflict.data.reproj, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE")

sum(conflict.data.conf$OCC_SPECIES == "BLACK BEAR") #  185 b bear
sum(conflict.data.conf$OCC_SPECIES == "WOLF") # 21 wolf
sum(conflict.data.conf$OCC_SPECIES == "COUGAR") # 39 cougar

# Include confirmed, probable, and unknown (all but disproved):
conflict.data.filt <- dplyr::filter(conflict.data.reproj, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE"
                                    | OCC_VALIDITY_INFORMATION == "CANNOT BE JUDGED" | OCC_VALIDITY_INFORMATION == "")

sum(conflict.data.filt$OCC_SPECIES == "BLACK BEAR") #  911 b bear
sum(conflict.data.filt$OCC_SPECIES == "WOLF") # 108 wolf
sum(conflict.data.filt$OCC_SPECIES == "COUGAR") # 484 cougar


# Filter to only columns we need:

conflict.dataset.conf <- conflict.data.conf %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                                        'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'geometry'))

# Crop reports down to BHB watershed:
st_crs(conflict.dataset.conf) == st_crs(bhb.50k.buf) #FALSE
conflict.reproj <- st_transform(conflict.dataset.conf, st_crs(bhb.50k.buf))
st_crs(conflict.reproj) == st_crs(bhb.50k.buf) #TRUE

conflict.bhb.50k.buf <- st_intersection(conflict.reproj, bhb.50k.buf) # This gives 2057 total reports
sum(conflict.bhb.50k.buf$OCC_SPECIES == "BLACK BEAR") # 298 b bear
sum(conflict.bhb.50k.buf$OCC_SPECIES == "WOLF") # 28 wolf
sum(conflict.bhb.50k.buf$OCC_SPECIES == "COUGAR") # 70 cougar

conflict.conf.bhb <- conflict.bhb.50k.buf %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'AREA_HA', 'geometry'))

  # Let's try cropping the full dataset down to 50km buffer:
conflict.dataset.filt <- conflict.data.filt %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'geometry'))

conflict.filt.reproj <- st_transform(conflict.dataset.filt, st_crs(bhb.50k.buf))
st_crs(conflict.filt.reproj) == st_crs(bhb.50k.buf) #TRUE

conflict.f.bhb.50k.buf <- st_intersection(conflict.filt.reproj, bhb.50k.buf) # This gives 787 total reports
sum(conflict.f.bhb.50k.buf$OCC_SPECIES == "BLACK BEAR") # 441 b bear
sum(conflict.f.bhb.50k.buf$OCC_SPECIES == "WOLF") # 45 wolf
sum(conflict.f.bhb.50k.buf$OCC_SPECIES == "COUGAR") # 406 cougar

  # Note: I think we should use the dataset with just the "doubtful" and "confirmed not true" excluded, since there are many fields where
  # validity was not filled in at all. This at least reduces some of the misreporting, but still keeps our sample size large

head(conflict.f.bhb.50k.buf)
  # Trim this down:
conflict.f.bhb <- conflict.f.bhb.50k.buf %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'AREA', 'Area_sqkm', 'geometry'))
head(conflict.bhb) #7,877 observations
head(conflict.conf.bhb) #2057 observations

st_write(conflict.conf.bhb, "data/processed/conflict_confirmed_dataframe.shp", append = FALSE)
st_write(conflict.bhb, "data/processed/conflict_reports_bhb.shp", append = FALSE)

