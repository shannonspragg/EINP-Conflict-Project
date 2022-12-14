
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

# Filter data to confirmed and probable occurances (let's see if this is enough):
  # Just confirmed and probable:
conflict.data.conf <- dplyr::filter(conflict.data.reproj, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE")

sum(conflict.data.conf$OCC_SPECIES == "BLACK BEAR") #  185 b bear
sum(conflict.data.conf$OCC_SPECIES == "WOLF") # 21 wolf
sum(conflict.data.conf$OCC_SPECIES == "COUGAR") # 39 cougar

  # See if we can further expand this: NOTE: may use this later on if we bigger samples for running models
conflict.data.expanded <- dplyr::filter(conflict.data.reproj, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE" | OCC_VALIDITY_INFORMATION == "CANNOT BE JUDGED")

sum(conflict.data.expanded$OCC_SPECIES == "BLACK BEAR") #  210 b bear
sum(conflict.data.expanded$OCC_SPECIES == "WOLF") # 30 wolf
sum(conflict.data.expanded$OCC_SPECIES == "COUGAR") # 86 cougar


# Filter to only columns we need:

conflict.dataset.conf <- conflict.data.conf %>% 
  dplyr::select(., c('OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                                        'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'geometry', 'OCC_OCCURRENCE_TMST', 'SITE_NAME'))

conflict.dataset.conf <- mutate(conflict.dataset.conf, id = row_number())
conflict.dataset.conf <- conflict.dataset.conf %>%           # Reorder data frame
  dplyr::select("id", everything())
conflict.dataset.conf$id <- as.numeric(conflict.dataset.conf$id)

# Crop reports down to BHB watershed:
st_crs(conflict.dataset.conf) == st_crs(bhb.50k.buf) #FALSE
conflict.reproj <- st_transform(conflict.dataset.conf, st_crs(bhb.50k.buf))
st_crs(conflict.reproj) == st_crs(bhb.50k.buf) #TRUE

conflict.bhb.50k.buf <- st_intersection(conflict.reproj, bhb.50k.buf) # This gives 2057 total reports
conflict.bhb.50k.buf <- conflict.bhb.50k.buf %>% distinct(OCC_FILE_NUMBER, .keep_all = TRUE) #rid of duplicates

sum(conflict.bhb.50k.buf$OCC_SPECIES == "BLACK BEAR") # 175 b bear
sum(conflict.bhb.50k.buf$OCC_SPECIES == "WOLF") # 21 wolf
sum(conflict.bhb.50k.buf$OCC_SPECIES == "COUGAR") # 38 cougar

conflict.conf.bhb <- conflict.bhb.50k.buf %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION',  'OCC_OCCURRENCE_TMST', 'SITE_NAME', 'bears', 'wolves', 'cougars', 'AREA_HA', 'geometry'))

head(conflict.conf.bhb) #1086 observations


# Add in the 2 Collision Reports ------------------------------------------
road.kills <- read_csv("data/original/AWW-exp-2022-10-04 black bear.csv")

# Making Conflict Data a Spatial Dataframe 
xy2<-road.kills[,c(14,13)]
roadkill.spdf<-SpatialPointsDataFrame(coords = xy2,data = road.kills,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
# conflict.data.sf <- as(conflict.spdf, "sf")
roadkill.data.sf <- st_as_sf(roadkill.spdf)

roadkill.data.reproj <- roadkill.data.sf %>% st_transform(., crs(temp.rast))
head(roadkill.data.reproj)
roadkill.data.reproj <- mutate(roadkill.data.reproj, id = row_number())
roadkill.data.reproj <- roadkill.data.reproj %>%           # Reorder data frame
  dplyr::select("id", everything())
roadkill.data.reproj$id <- as.numeric(roadkill.data.reproj$id)

# Drop to columns we need:
roadkill.filt <- roadkill.data.reproj %>% 
  dplyr::select(., c('id','Record ID', 'Species', 'geometry'))
names(roadkill.filt)[names(roadkill.filt) == 'Species'] <- 'OCC_SPECIES'
names(roadkill.filt)[names(roadkill.filt) == 'Record ID'] <- 'OCC_FILE_NUMBER'

# Add the missing columns:

roadkill.filt['OCCURRENCE_TYPE_DESC'] <- NA
roadkill.filt['ACTION_TYPE_DESCRIPTION'] <- NA
roadkill.filt['OCC_CITY'] <- NA
roadkill.filt['OCC_POSTAL_CODE'] <- NA
roadkill.filt['OCC_WMU_CODE'] <- NA
roadkill.filt['OCC_NUMBER_ANIMALS'] <- 1
roadkill.filt['OCC_PRIMARY_ATTRACTANT'] <- "highway"
roadkill.filt['OCC_VALIDITY_INFORMATION'] <- NA
roadkill.filt['OCC_OCCURRENCE_TMST'] <- NA
roadkill.filt['SITE_NAME'] <- "HIGHWAY 16"
roadkill.filt['bears'] <- 1
roadkill.filt['wolves'] <- 0
roadkill.filt['cougars'] <- 0  
roadkill.filt['AREA_HA'] <- NA

# Reorder the columns to match:
roadkills.sf <- roadkill.filt[ , c(1,2,5,6,7,8,9,3,10,11,12,13,14,15,16,17,18,4)]

# Join our reports together:
roadkill.reproj <- st_transform(roadkills.sf, st_crs(conflict.conf.bhb))
conf.conflict.all <- rbind(conflict.conf.bhb, roadkill.reproj)


# Add in the census regions for varying intercept: ------------------

# Filter CCS Files to AB Only ---------------------------------------------------
# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRUID) # Shows that the name for BC is "British Columbia / Colombie-Britannique"

# Filter down to just BC:
ab.ccs<-can.ccs.sf %>%
  filter(., PRUID == "48") 

# Save this for later:
st_write(ab.ccs, "data/processed/AB_CCS.shp", append = FALSE)

############################ Next, Add in the CCS Region Names to the Data:
# Project the CCS Regions to match our data: ------------------------------
ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50k.buf))

# Check to see if the projections match:
st_crs(ab.ccs.reproj) == st_crs(bhb.50k.buf) # [TRUE] 

st_crs(conf.conflict.all) == st_crs(ab.ccs.reproj) # [TRUE] 

# Plot these together to make sure:
plot(st_geometry(ab.ccs.reproj))
plot(st_geometry(bhb.50k.buf), add=TRUE)

# Crop CCS Down to SOI 10km Extent: --------------------------------------------
bhw.ccs.crop <- st_intersection(ab.ccs.reproj, bhb.50k.buf)

plot(st_geometry(bhw.ccs.crop))
plot(st_geometry(conf.conflict.all), add=TRUE)

# Write this as a .shp for later:
st_write(bhw.ccs.crop, "data/processed/bhw_CCS_50km.shp", append=FALSE)

# Assign the WARP Points to a CCS Region: ---------------------------------
## Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
#  make sure this is a factor, to fit this as a varying intercept

# Assign our points to a CCS category:
conflict.ccs.join <- st_join(conf.conflict.all, left = TRUE, ab.ccs.reproj) # join points

head(conflict.ccs.join) # Assigned points to a CCS category

conflict.ccs.join <- conflict.ccs.join %>% 
  dplyr::select(., -c(22,21,19))

head(conflict.ccs.join)

# Save conflict df: -------------------------------------------------------

st_write(conflict.ccs.join, "data/processed/conflict_confirmed_dataframe.shp", append = FALSE)

