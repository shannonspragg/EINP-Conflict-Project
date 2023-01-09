
# Add in Sightings Data ---------------------------------------------------

## Here we bring in the large carnivore sightings data and add it to our df.


# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)
library(raster)

# Bring in our  Data --------------------------------------------

sightings <- read_csv("data/original/large_carnivore_sightings.csv")
upd.conflict <- st_read("data/processed/conflict_conf_full_dataframe.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
head(sightings)
head(upd.conflict)

# Drop reports with NA locations:
sight.no.na <- sightings[complete.cases(sightings[,c("Lat")]),] # This leaves us 9759 total obs

# Making Sightings Data a Spatial Data frame 
xy3<-sight.no.na[,c(8,7)]
sight.spdf<-SpatialPointsDataFrame(coords = xy3,data = sight.no.na,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
sight.data.sf <- st_as_sf(sight.spdf)

sight.reproj <- sight.data.sf %>% st_transform(., crs(temp.rast))
head(sight.reproj)
sight.reproj <- mutate(sight.reproj, id = row_number())
sight.reproj <- sight.reproj %>%           # Reorder data frame
  dplyr::select("id", everything())
sight.reproj$id <- as.numeric(sight.reproj$id)

# Add and rename cols to match:
sight.reproj <- sight.reproj %>% 
  mutate(sight.reproj, bears = if_else(Species == "black bear", 1, 0)) %>%
  mutate(sight.reproj, wolves = if_else(Species == "wolf", 1, 0)) %>%
  mutate(sight.reproj, cougars = if_else(Species == "cougar", 1, 0))

sight.reproj['OCC_FIL'] <- NA
names(sight.reproj)[names(sight.reproj) == 'Sighting Description'] <- 'OCCURRE'
names(sight.reproj)[names(sight.reproj) == 'Notes'] <- 'ACTION_'
sight.reproj['OCC_CIT'] <- NA
sight.reproj['OCC_POS'] <- NA
sight.reproj['OCC_WMU'] <- NA
sight.reproj['OCC_POS'] <- NA
names(sight.reproj)[names(sight.reproj) == 'Species'] <- 'OCC_SPE'
names(sight.reproj)[names(sight.reproj) == 'Number of Animals'] <- 'OCC_NUM'
sight.reproj['OCC_PRI'] <- NA
sight.reproj['OCC_VAL'] <- 'PROBABLE'
names(sight.reproj)[names(sight.reproj) == 'Date'] <- 'OCC_OCC'
names(sight.reproj)[names(sight.reproj) == 'Location'] <- 'SITE_NA'

sight.reproj <- sight.reproj %>% 
  dplyr::select(., -c(6, 8, 9, 10, 11, 12, 13))

# Reorder the columns to match:
sight.sf <- sight.reproj[ , c(1,12,4,7,13,14,15,3,5,16,17,2,6,9,10,11,8)]

# Assign the sight points to a CCS Region: ---------------------------------
## Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
#  make sure this is a factor, to fit this as a varying intercept
ab.ccs <- st_read("data/processed/AB_CCS.shp")
ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50k.buf))

# Assign our points to a CCS category:
sight.rep <- st_transform(sight.sf, st_crs(ab.ccs.reproj))
sight.ccs.join <- st_join(sight.rep, left = TRUE, ab.ccs.reproj) # join points

head(sight.ccs.join) # Assigned points to a CCS category

sight.ccs.join <- sight.ccs.join %>% 
  dplyr::select(., -c(22,21,19))

# Add a column for conflict presence:
sight.ccs.join$cnflct_ <- 1

sight.ccs.join <- sight.ccs.join[ , c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,17)]

head(upd.conflict)

# Join our reports together:
conf.conflict.comp <- rbind(upd.conflict, sight.ccs.join) # now 1136 reports

# Update our file
st_write(conf.conflict.comp, "data/processed/conflict_conf_comp_dataframe.shp", append = FALSE)
# NOTE: this df now has provincial conflict reports, IEM bear reports, roadkill bears, predator compensations, wolf depred claims, and sightings 



