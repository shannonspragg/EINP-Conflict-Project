
# Testing Cat's Data: -----------------------------------------------------
 # NOTE: this is just a test script to determine usability of the data. There is no need to re-run this, and the input file 
 # for Compiled Bear Reports is not in the downloaded files for this project.

# Load packages:
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in Data:
ct.reports <- read.csv("/Users/shannonspragg/EINP Data/Compiled Bear Reports.csv")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
ab.pas <- st_read("data/processed/alberta_protected_areas.shp")

head(ct.reports)
unique(ct.reports$Dataset)

# Look at only CT reports
ct_only <- ct.reports %>% filter(ct.reports$Dataset == "camera_trap_predators")
ct_filtered <- ct_only %>% 
  dplyr::select(., c('FID', 'Dataset', 'Species', 'Latitude', 'Longitude', 'Year'))
head(ct_filtered)


# Make spatial:

xy.ct<-ct_filtered[,c(5,4)]
ct.spdf<-SpatialPointsDataFrame(coords = xy.ct,data = ct_filtered,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Drop reports with NA locations:
ct.no.na <- ct.reports[complete.cases(ct.reports[,c("Latitude", "Longitude")]),] # This leaves us 9759 total obs

xy.cd<-ct.no.na[,c(5,4)]
cd.spdf<-SpatialPointsDataFrame(coords = xy.cd,data = ct.no.na,
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Ensure this is a sf data frame:
# conflict.data.sf <- as(conflict.spdf, "sf")
ct.data.sf <- st_as_sf(ct.spdf)
ct.all.sf <- st_as_sf(cd.spdf) # this includes all misc reports in the file

head(ct.data.sf)
ct.reproj <- st_transform(ct.data.sf, st_crs(bhb.50k.buf))
cd.reproj <- st_transform(ct.all.sf, st_crs(bhb.50k.buf))

ab.pa.reproj <- st_transform(ab.pas, st_crs(bhb.50k.buf))

# Plot just the CT locations:
plot(st_geometry(ab.pa.reproj))
plot(st_geometry(ct.reproj), add=TRUE, col="red") 
# These locations appear to be all over Alberta - which is not what we want.
# Either these CT reports are sourced from elsewhere or the coordinates are wrong.

# Plot all report locations:
plot(st_geometry(ab.pa.reproj))
plot(st_geometry(cd.reproj), add=TRUE, col="red") 

plot(st_geometry(bhb.50k.buf)) # zoom in
plot(st_geometry(cd.reproj), add=TRUE, col="red") # decent amount in / around the park - from conflict reports (I think)
