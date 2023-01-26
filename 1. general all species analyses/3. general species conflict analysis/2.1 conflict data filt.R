# Crop conflict data for analysis: ----------------------------------------

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in our previous Data --------------------------------------------
conflict.data <- st_read("data/processed/conflict_no_filt.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")

# Crop reports down to BHB watershed:
st_crs(conflict.data) == st_crs(bhb.50k.buf) #FALSE
conflict.rep <- st_transform(conflict.data, st_crs(bhb.50k.buf))
st_crs(conflict.rep) == st_crs(bhb.50k.buf) #TRUE

conflict.bhb.50k.buf <- st_intersection(conflict.rep, bhb.50k.buf) # This gives 18k total reports
conflict.bhb.50k.buf <- conflict.bhb.50k.buf %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 9k

conflict.bhb <- conflict.bhb.50k.buf %>% 
  dplyr::select(., c('id', 'OCC_FIL', 'OCCURRE', 'ACTION_', 'OCC_CIT', 'OCC_POS', 'OCC_WMU', 'OCC_SPE',
                     'OCC_NUM', 'OCC_PRI', 'OCC_VAL',  'OCC_OCC', 'SITE_NA', 'bears', 'wolves', 'cougars', 'AREA_HA', 'geometry'))
head(conflict.bhb) 
sum(conflict.bhb$OCC_SPE == "BLACK BEAR") # 894 b bear
sum(conflict.bhb$OCC_SPE == "WOLF") # 115 wolf
sum(conflict.bhbf$OCC_SPE == "COUGAR") # 496 cougar

st_write(conflict.bhb, "data/processed/conflict_bhb_no_filt.shp", append = FALSE)
