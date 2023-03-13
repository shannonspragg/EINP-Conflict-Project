
# Review IEM Data: --------------------------------------------------------
    ## Here we bring in the IEM conflict data and review it for usability. We need to cross reference it with the
    # province conflict records in order to rule out and possible duplicate reports.


# Load Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)
library(raster)

# Bring in our  Data --------------------------------------------

iem.reports <- read.csv("data/original/IEM Download_2022-10-01 .csv") # these are all black bear reports
comp.reports <- read.csv("data/original/willdife_predator_compensations.csv")
prov.conflict <- st_read("data/processed/conflict_confirmed_dataframe.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
head(iem.reports)
head(comp.reports)

# Make IEM spatial --------------------------------------------------------

# Making Conflict Data a Spatial Dataframe 
iem.df <- iem.reports %>%  filter(!row_number() %in% c(1))

# NOTE: X = Incedent Type , X.1 = Area , X.2 = Incident Date , X.3 = Incident Description , X.4 = Incident # , X.5 = latitude
# X.6 = longitude , X.7 = Risk , X.8 = Species , X.9 = Total Time

xy<-iem.df[,c(8,7)]
xy$X.6 <- as.numeric(xy$X.6)
xy$X.5 <- as.numeric(xy$X.5)

iem.spdf<-SpatialPointsDataFrame(coords = xy,data = iem.df,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# Now compensation reports:
comp.reports <- comp.reports %>% 
  mutate(comp.reports, bears = if_else(OCC_SPECIES == "BLACK BEAR", 1, 0)) %>%
  mutate(comp.reports, wolves = if_else(OCC_SPECIES == "WOLF", 1, 0)) %>%
  mutate(comp.reports, cougars = if_else(OCC_SPECIES == "COUGAR", 1, 0))

xy2<-comp.reports[,c(6,5)]
comp.spdf<-SpatialPointsDataFrame(coords = xy2,data = comp.reports,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


# Ensure this is a sf data frame:
# conflict.data.sf <- as(conflict.spdf, "sf")
iem.data.sf <- st_as_sf(iem.spdf)

comp.sf <- st_as_sf(comp.spdf)

comp.sf <- comp.sf %>% 
  dplyr::select(., c('OCC_FILE_NUMBER', 'OCCURRENCE_TYPE_DESC', 'ACTION_TYPE_DESCRIPTION', 'OCC_CITY', 'OCC_POSTAL_CODE', 'OCC_WMU_CODE', 'OCC_SPECIES',
                     'OCC_NUMBER_ANIMALS', 'OCC_PRIMARY_ATTRACTANT', 'OCC_VALIDITY_INFORMATION', 'bears', 'wolves', 'cougars', 'geometry', 'OCC_OCCURRENCE_TMST', 'SITE_NAME'))

comp.sf <- mutate(comp.sf, id = row_number())
comp.sf <- comp.sf %>%           # Reorder data frame
  dplyr::select("id", everything())
comp.sf$id <- as.numeric(comp.sf$id)

# Need to compare time stamps for duplicates:
prov.bears <- prov.conflict %>% filter(prov.conflict$OCC_SPE == "BLACK BEAR") # FILTER to just our bear reports

unique(prov.bears$OCC_OCC)
unique(iem.data.sf$X.2)
# Upon manual search and inspection, there are no duplicates between the 2 report sets. 
# I can merge these into the other report df


# Join IEM to Province conflict data: -------------------------------------

iem.data.reproj <- iem.data.sf %>% st_transform(., crs = st_crs(prov.conflict))
head(iem.data.reproj)
iem.data.reproj <- mutate(iem.data.reproj, id = row_number())
iem.data.reproj <- iem.data.reproj %>%           # Reorder data frame
  dplyr::select("id", everything())
iem.data.reproj$id <- as.numeric(iem.data.reproj$id)

# Drop to columns we need:
names(iem.data.reproj)[names(iem.data.reproj) == 'X.8'] <- 'OCC_SPE'
names(iem.data.reproj)[names(iem.data.reproj) == 'X.1'] <- 'SITE_NA'
names(iem.data.reproj)[names(iem.data.reproj) == 'X.2'] <- 'OCC_OCC'
names(iem.data.reproj)[names(iem.data.reproj) == 'X.3'] <- 'OCCURRE'

IEM.filt <- iem.data.reproj %>% 
  dplyr::select(., c('id','SITE_NA', 'OCC_OCC', 'OCCURRE', 'OCC_SPE', 'geometry'))

# Add the missing columns:
IEM.filt['OCC_FIL'] <- NA
IEM.filt['ACTION_'] <- NA
IEM.filt['OCC_CIT'] <- NA
IEM.filt['OCC_POS'] <- NA
IEM.filt['OCC_WMU'] <- NA
IEM.filt['OCC_NUM'] <- 1
IEM.filt['OCC_PRI'] <- NA
IEM.filt['OCC_VAL'] <- "CONFIRMED"
IEM.filt['bears'] <- 1
IEM.filt['wolves'] <- 0
IEM.filt['cougars'] <- 0  
IEM.filt['AREA_HA'] <- NA

# Reorder the columns to match:
IEM.sf <- IEM.filt[ , c(1,7,4,8,9,10,11,5,12,13,14,3,2,15,16,17,18,6)]

### Now adjust comp data cols:
comp.sf <- rename(comp.sf, OCC_FIL = OCC_FILE_NUMBER)
comp.sf <- rename(comp.sf, OCCURRE = OCCURRENCE_TYPE_DESC)
comp.sf <- rename(comp.sf, ACTION_ = ACTION_TYPE_DESCRIPTION)
comp.sf <- rename(comp.sf, OCC_CIT = OCC_CITY)
comp.sf <- rename(comp.sf, OCC_POS = OCC_POSTAL_CODE)
comp.sf <- rename(comp.sf, OCC_WMU = OCC_WMU_CODE)
comp.sf <- rename(comp.sf, OCC_SPE = OCC_SPECIES)
comp.sf <- rename(comp.sf, OCC_NUM = OCC_NUMBER_ANIMALS)
comp.sf <- rename(comp.sf, OCC_PRI = OCC_PRIMARY_ATTRACTANT)
comp.sf <- rename(comp.sf, OCC_VAL = OCC_VALIDITY_INFORMATION)
comp.sf <- rename(comp.sf, OCC_OCC = OCC_OCCURRENCE_TMST)
comp.sf <- rename(comp.sf, SITE_NA = SITE_NAME)
comp.sf['AREA_HA'] <- NA

comp.sf <- comp.sf[, c(1,2,3,4,5,6,7,8,9,10,11,15,16,12,13,14,18,17)]

# Assign CCS regions: -----------------------------------------------------

# Write this as a .shp for later:
ab.ccs <- st_read("data/processed/AB_CCS.shp")
ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50k.buf))
iem.reproj <- st_transform(IEM.sf, st_crs(bhb.50k.buf))
comp.reproj <- st_transform(comp.sf, st_crs(bhb.50k.buf))

# Assign our points to a CCS category:
iem.ccs.join <- st_join(iem.reproj, left = TRUE, ab.ccs.reproj) # join points
comp.ccs.join <- st_join(comp.reproj, left = TRUE, ab.ccs.reproj) # join points

head(iem.ccs.join) # Assigned points to a CCS category

iem.ccs.join <- iem.ccs.join %>% 
  dplyr::select(., -c(22,21,19))

comp.ccs.join <- comp.ccs.join %>% 
  dplyr::select(., -c(22,21,19))

# Join our reports together:
conf.conflict.all <- rbind(prov.conflict, iem.ccs.join, comp.ccs.join) # now we have 4861 obs!
head(conf.conflict.all) # now 4861 total reports

# Add a column for conflict presence:
conf.conflict.all$conflict_pres <- 1

# Plot
plot(st_geometry(bhw))
plot(st_geometry(conf.conflict.all), add=TRUE)

# Update our file
st_write(conf.conflict.all, "data/processed/conflict_conf_iem_dataframe.shp", append = FALSE)


