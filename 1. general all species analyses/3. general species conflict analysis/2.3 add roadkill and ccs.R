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
 all.conf <- st_read("data/processed/full_confirmed_conflict_df.shp")
 
 
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
 
 roadkill.filt['OCCURRE'] <- NA
 roadkill.filt['ACTION_'] <- NA
 roadkill.filt['OCC_CIT'] <- NA
 roadkill.filt['OCC_POS'] <- NA
 roadkill.filt['OCC_WMU'] <- NA
 roadkill.filt['OCC_NUM'] <- 1
 roadkill.filt['OCC_PRI'] <- "highway"
 roadkill.filt['OCC_VAL'] <- "CONFIRMED"
 roadkill.filt['OCC_OCC'] <- NA
 roadkill.filt['SITE_NA'] <- "HIGHWAY 16"
 roadkill.filt['bears'] <- 1
 roadkill.filt['wolves'] <- 0
 roadkill.filt['cougars'] <- 0  
 roadkill.filt['AREA_HA'] <- NA
 
 # Reorder the columns to match:
 roadkills.sf <- roadkill.filt[ , c(1,2,5,6,7,8,9,3,10,11,12,13,14,15,16,17,18,4)]
 
 # Join our reports together:
 roadkill.reproj <- st_transform(roadkills.sf, st_crs(all.conf))
 conf.conflict.all <- rbind(all.conf, roadkill.reproj)
 
 
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
 