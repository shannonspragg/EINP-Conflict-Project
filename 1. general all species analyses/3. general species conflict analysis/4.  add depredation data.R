
# Review IEM Data: --------------------------------------------------------
    ## Here we bring in the depredation claims to our conflict df.


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

depred <- read_csv("data/original/wolf depredation reports.csv")
updated.conflict <- st_read("data/processed/conflict_conf_iem_dataframe.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
head(depred)
head(updated.conflict)

# Trim down conflict df:
conflict.filt <- updated.conflict %>% 
  dplyr::select(., c('id', 'OCC_FIL', 'OCCURRE', 'ACTION_', 'OCC_CIT', 'OCC_POS', 'OCC_WMU', 'OCC_SPE',
                     'OCC_NUM', 'OCC_PRI', 'OCC_VAL',  'OCC_OCC', 'SITE_NA','bears', 'wolves', 'cougars', 'CCSUID', 'CCSNAME', 'cnflct_', 'geometry'))



# Making Depred Data a Spatial Data frame 
xy2<-depred[,c(6,5)]
depred.spdf<-SpatialPointsDataFrame(coords = xy2,data = depred,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
depred.data.sf <- st_as_sf(depred.spdf)

depred.reproj <- depred.data.sf %>% st_transform(., crs(temp.rast))
head(depred.reproj)
depred.reproj <- mutate(depred.reproj, id = row_number())
depred.reproj <- depred.reproj %>%           # Reorder data frame
  dplyr::select("id", everything())
depred.reproj$id <- as.numeric(depred.reproj$id)

# Drop to columns we need:
depred.filt <- depred.reproj %>% 
  dplyr::select(., c('id', 'OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION',  'OCC_OCCURRENCE_TMST', 'SITE_NAME', 'geometry'))
#Add the missing columns:
depred.filt['bears'] <- 0
depred.filt['wolves'] <- 1
depred.filt['cougars'] <- 0  
depred.filt['AREA_HA'] <- NA

### Now adjust comp data cols:
names(depred.filt)[names(depred.filt) == 'OCC_FILE_NUMBER'] <- 'OCC_FIL'
names(depred.filt)[names(depred.filt) == 'OCCURRENCE_TYPE_DESC'] <- 'OCCURRE'
names(depred.filt)[names(depred.filt) == 'ACTION_TYPE_DESCRIPTION'] <- 'ACTION_'
names(depred.filt)[names(depred.filt) == 'OCC_CITY'] <- 'OCC_CIT'
names(depred.filt)[names(depred.filt) == 'OCC_POSTAL_CODE'] <- 'OCC_POS'
names(depred.filt)[names(depred.filt) == 'OCC_WMU_CODE'] <- 'OCC_WMU'
names(depred.filt)[names(depred.filt) == 'OCC_SPECIES'] <- 'OCC_SPE'
names(depred.filt)[names(depred.filt) == 'OCC_NUMBER_ANIMALS'] <- 'OCC_NUM'
names(depred.filt)[names(depred.filt) == 'OCC_PRIMARY_ATTRACTANT'] <- 'OCC_PRI'
names(depred.filt)[names(depred.filt) == 'OCC_VALIDITY_INFORMATION'] <- 'OCC_VAL'
names(depred.filt)[names(depred.filt) == 'OCC_OCCURRENCE_TMST'] <- 'OCC_OCC'
names(depred.filt)[names(depred.filt) == 'SITE_NAME'] <- 'SITE_NA'


# Reorder the columns to match:
depred.sf <- depred.filt[ , c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,14)]

# Assign the depred points to a CCS Region: ---------------------------------
## Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
#  make sure this is a factor, to fit this as a varying intercept
ab.ccs <- st_read("data/processed/AB_CCS.shp")
ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50k.buf))

# Assign our points to a CCS category:
depred.rep <- st_transform(depred.sf, st_crs(ab.ccs.reproj))
depred.ccs.join <- st_join(depred.rep, left = TRUE, ab.ccs.reproj) # join points

head(depred.ccs.join) # Assigned points to a CCS category

depred.ccs.join <- depred.ccs.join %>% 
  dplyr::select(., -c(23,22,20,17))

# Add a column for conflict presence:
depred.ccs.join$cnflct_ <- 1

depred.ccs.join <- depred.ccs.join[ , c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,17)]

head(conflict.filt)

# Join our reports together:
depred.reproj <- st_transform(depred.ccs.join, st_crs(conflict.conf.bhb))
conf.conflict.full <- rbind(conflict.filt, depred.ccs.join) # now 1136 reports

# Update our file
st_write(conf.conflict.full, "data/processed/conflict_conf_full_dataframe.shp", append = FALSE)
# NOTE: this df now has provincial conflict reports, IEM bear reports, roadkill bears, predator compensations, and wolf depred claims 


