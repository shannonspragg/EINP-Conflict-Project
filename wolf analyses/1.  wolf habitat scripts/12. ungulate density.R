
# Prepping Ungulate density data ------------------------------------------


# Load packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(raster)


# Bring in data: ----------------------------------------------------------
wmu <- st_read("data/original/bf_wmu_polygons-polygon.shp")
bhw <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ungulate.harvest <- read_csv("data/original/ungulate_harvest_counts.csv") # remove notes col

wmu.reproj <- st_transform(wmu, st_crs(crs(temp.rast)))

# Crop down to BHW buffer: ------------------------------------------------
bhb.buf.v <- vect(bhw)
wmu.v <- vect(wmu.reproj)
crs(wmu.v) == crs(temp.rast)

wmu.crop <- terra::crop(wmu.v, temp.rast)

# Make sf again:
wmu.bhw <- as(wmu.crop, "Spatial")
wmu.bhw.sf <- st_as_sf(wmu.bhw)

str(ungulate.harvest)
ungulate.harvest <- ungulate.harvest[ -c(7) ]
ungulate.harvest$WMU <- as.character(ungulate.harvest$WMU)

# Join WMU with harvest data ----------------------------------------------
wmu.bhw.sf$WMU <- str_sub(wmu.bhw.sf$WMU, -3, -1)

wmu.harvest.join <- wmu.bhw.sf %>% 
  left_join(., ungulate.harvest, by = c("WMU" = "WMU"))


# Calculate densities: ----------------------------------------------------
# Calculate our areas for the two objects: 
# Make our area units kilometers:
wmu.harvest.join$AREA_SQ_KM <- units::set_units(st_area(wmu.harvest.join), km^2)

# Now we make a new col with our ungulates per sq km:
wmu.harvest.join$Total_ungulates_per_sq_km <- wmu.harvest.join$`total_ungulate_harvest` / wmu.harvest.join$AREA_SQ_KM
head(wmu.harvest.join)

  # Just white tailed deer
wmu.harvest.join$Total_wt_deer_per_sq_km <- wmu.harvest.join$`wt_deer_harvest_count` / wmu.harvest.join$AREA_SQ_KM
head(wmu.harvest.join)
  # Mule deer
wmu.harvest.join$Total_mule_deer_per_sq_km <- wmu.harvest.join$`mule_deer_harvest_count` / wmu.harvest.join$AREA_SQ_KM
head(wmu.harvest.join)
  # Elk
wmu.harvest.join$Total_elk_per_sq_km <- wmu.harvest.join$`elk_harvest_count` / wmu.harvest.join$AREA_SQ_KM
head(wmu.harvest.join)

# Make this col numeric:
wmu.harvest.join$Total_ungulates_per_sq_km <- as.numeric(wmu.harvest.join$Total_ungulates_per_sq_km)
wmu.harvest.join$Total_wt_deer_per_sq_km <- as.numeric(wmu.harvest.join$Total_wt_deer_per_sq_km)
wmu.harvest.join$Total_mule_deer_per_sq_km <- as.numeric(wmu.harvest.join$Total_mule_deer_per_sq_km)
wmu.harvest.join$Total_elk_per_sq_km <- as.numeric(wmu.harvest.join$Total_elk_per_sq_km)

# Make rasters for ungulate count ------------------------------------------
harvest.v <- vect(wmu.harvest.join)

total.ungulate.rast <- terra::rasterize(harvest.v, temp.rast, field = "Total_ungulates_per_sq_km")
total.elk.rast <- terra::rasterize(harvest.v, temp.rast, field = "Total_elk_per_sq_km")
total.muledeer.rast <- terra::rasterize(harvest.v, temp.rast, field = "Total_mule_deer_per_sq_km")
total.whitetailed.deer.rast <- terra::rasterize(harvest.v, temp.rast, field = "Total_wt_deer_per_sq_km")


# Save these: -------------------------------------------------------------
writeRaster(total.ungulate.rast, "data/processed/total_ungulate_density.tif", overwrite=TRUE)
writeRaster(total.elk.rast, "data/processed/total_elk_density.tif", overwrite=TRUE)
writeRaster(total.muledeer.rast, "data/processed/total_muledeer_density.tif", overwrite=TRUE)
writeRaster(total.whitetailed.deer.rast, "data/processed/total_white_tailed_deer_density.tif", overwrite=TRUE)
st_write(wmu.harvest.join, "data/processed/ungulate_harvest_per_wmu.shp", append = FALSE)

