
# Prep PA Data ----------------------------------------------------
# Filter to AB and IUCN status


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)

# Load BC PAs -------------------------------------------------------------
fgdb <- "data/original/CPCAD-BDCAPC_Dec2020.gdb"
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf")
ab.PAs <- fc.sf %>% 
  filter(., LOC_E == "Alberta") 

# Filter by IUCN status (Muise et al., 2022 https://esajournals.onlinelibrary.wiley.com/doi/10.1002/eap.2603)
ab.PAs.iucn.filtered <- ab.PAs %>% 
  filter(., IUCN_CAT == "Ia" | IUCN_CAT == "Ib" | IUCN_CAT == "II" | IUCN_CAT == "IV") %>% 
  st_make_valid()
ab.PAs.iucn.filtered$areaha <- st_area(ab.PAs.iucn.filtered) 
units(ab.PAs.iucn.filtered$areaha) <- units::make_units(ha)
ab.PAs.iucn.filtered$areaha <- as.numeric(ab.PAs.iucn.filtered$areaha) 

# Filter by PA's larger than 100 ha:
ab.PAs.fin <- filter(ab.PAs.iucn.filtered, areaha > 100) 

st_write(ab.PAs.fin, "data/processed/alberta_protected_areas.shp")
#ab.PAs.fin <- st_read("data/processed/alberta_protected_areas.shp")
# create template raster --------------------------------------------------

# Bring in county boundary:
bhb <- st_read("data/original/BHB_Subwatershed_Boundary.shp") # This will prob change to watershed

bhb.buffer <- bhb %>%  # buffer by 50km
  st_buffer(., 50000) 

bhb.buf.v <- bhb.buffer %>% as(., "SpatVector")
bhb.buf.v <- vect(bhb.buffer)

st_write(bhb.buffer, "data/processed/bhb_50km.shp", append=FALSE)

temp.rast <- rast(res=c(250,250), ext=ext(bhb.buf.v)) # Let's do a 250x250 res for computational purposes
crs(temp.rast) <- "epsg:32612" # UTM zone 12N for AB
values(temp.rast) <- rep(1, ncell(temp.rast))

ab.pa.proj <- ab.PAs.fin %>% 
  st_transform(., crs=crs(temp.rast)) %>%
  as(., "SpatVector")

bhb.pa.rast <- terra::rasterize(ab.pa.proj, temp.rast, field = "NAME_E") 
#bhb.pa.crop <- terra::mask(bhb.pa.rast, bhb.buf.v) # we want the uncropped one for our model

# Pull out EINP for later:
einp <- ab.PAs.fin %>% dplyr::filter(ab.PAs.fin$NAME_E == "Elk Island National Park Of Canada")
st_write(einp, "data/processed/einp.shp")

# Dist to PA raster:
dist2pa <- terra::distance(temp.rast, ab.pa.proj)
dist2pa.km <- measurements::conv_unit(dist2pa, "m", "km")
writeRaster(dist2pa.km, "data/processed/dist2pa_km_bhb.tif", overwrite=TRUE)
writeRaster(bhb.pa.rast, "data/processed/bhb_protected_areas.tif", overwrite=TRUE)
writeRaster(temp.rast, "data/processed/bhb_50km_template_rast.tif", overwrite=TRUE)