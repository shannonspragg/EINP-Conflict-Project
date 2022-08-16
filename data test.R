
# This is just a test script to see what data looks like ------------------

library(tidyverse)
library(sf)
library(sp)
library(rgeos)
#library(raster)
library(rgdal)
#library(fasterize)
library(terra)

landcover <- st_read("/Users/shannonspragg/EINP Data/2010LanCoverShapeFiles/Lancover_Polygons_2010.shp")
plot(landcover['LC_Class']) # these are the different land cover categories -- see guide for which is which

water_bodies <- st_read("/Users/shannonspragg/EINP Data/canvec_1M_CA_Hydro/waterbody_2.shp")

crown_lands <- st_read("/Users/shannonspragg/EINP Data/crown-shape/Crown_Reservations_2018Dec.shp")
plot(crown_lands['SUBTYPE'])

landuse <- st_read("/Users/shannonspragg/EINP Data/AB LUF Regional Boundaries Shapefiles 2011-04/LUF Integrated Regional Plan Boundaries.shp")

historic_wildfires <- st_read("/Users/shannonspragg/EINP Data/HistoricalWildfirePerimeters/WildfirePerimeters1931to2021v2.shp")
recent_wildfires <- historic_wildfires %>% filter(historic_wildfires$YEAR >= 2003) # Filter to last 20 years
plot(recent_wildfires['FIRE_NUMBE'])

prov_rec_area <- st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='PRA')
eco_reserve <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='ER')
natural_areas <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='NA')
nat_park <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='NP')
heritage_rangeland <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='HR')
prov_park <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='PP')
wilderness_park <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='WP')
wilderness_area <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='WA')
wildland_park <-st_read("/Users/shannonspragg/EINP Data/protected-area-kmz-outline/Protected Area KMZ Outline 2022 July.kml", layer='WPP')

protected_areas <- rbind(prov_rec_area, eco_reserve, natural_areas, nat_park, heritage_rangeland, prov_park, wilderness_park, wilderness_area, wildland_park)



