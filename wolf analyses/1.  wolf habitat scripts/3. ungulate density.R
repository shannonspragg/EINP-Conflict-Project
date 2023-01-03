
# Prepping Ungulate density data ------------------------------------------


# Load packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(raster)


# Bring in data: ----------------------------------------------------------
wmu <- st_read("data/original/WMU_Biologist_Contact_32611.shp")
bhw <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")
ungulate.densities <- read_csv("data/original/BH ungulate densities.csv") # remove notes col
einp <- st_read("data/processed/einp.shp")
wmu.reproj <- st_transform(wmu, st_crs(crs(temp.rast)))
einp.reproj <- st_transform(einp, st_crs(crs(temp.rast)))

# Merge EINP and WMU shapefile: -------------------------------------------
wmu.reproj.filt <- wmu.reproj %>% dplyr::select(., c(1,2,3,4,5,14)) # filt to needed cols

head(einp.reproj)
einp.reproj$WMUNIT_COD <- "00000"
einp.reproj$WMUNIT_NAM <- "Elk Island National Park"
einp.reproj$WMUNIT_N_1 <- "Elk Island National Park"
einp.reproj$WMUNIT_C_1 <- "000"
einp.reproj$Region <- "South"

einp.reproj.filt <- einp.reproj %>% dplyr::select(., c(40,39,38,37,36,35)) # filt to needed cols
einp.reproj.filt <- einp.reproj.filt[ , c(4,5,3,2,1,6)]

wmu.einp.join <- rbind(wmu.reproj.filt, einp.reproj.filt) # join layers

# Crop down to BHW buffer: ------------------------------------------------
bhb.buf.v <- vect(bhw)
wmu.v <- vect(wmu.einp.join)
crs(wmu.v) == crs(temp.rast)

wmu.crop <- terra::crop(wmu.v, temp.rast)
wmu.rast <- terra::rasterize(wmu.crop, temp.rast, field = "WMUNIT_N_1" )
wmu.NUM.rast <- terra::rasterize(wmu.crop, temp.rast, field = "WMUNIT_COD" )

writeRaster(wmu.rast, "data/processed/wmu_einp_rast_bhw.tif")

# Make sf again:
wmu.bhw <- as(wmu.crop, "Spatial")
wmu.bhw.sf <- st_as_sf(wmu.bhw)

str(ungulate.densities)
ungulate.densities <- ungulate.densities[ -c(9) ]
ungulate.densities$WMU <- as.character(ungulate.densities$WMU)
# ungulate.densities[ungulate.densities$WMU=="0"]<- "000"
ungulate.densities <- ungulate.densities %>% 
  dplyr::mutate(WMU = replace(WMU, WMU == '0', '000'))

# Join WMU with harvest data ----------------------------------------------
wmu.bhw.sf$WMU <- str_sub(wmu.bhw.sf$WMUNIT_COD, -3, -1)

wmu.density.join <- wmu.bhw.sf %>% 
  left_join(., ungulate.densities, by = c("WMU" = "WMU"))

# Calculate densities: ----------------------------------------------------
# Calculate our areas for the two objects: 
# Make our area units kilometers:
wmu.density.join$AREA_SQ_KM <- units::set_units(st_area(wmu.density.join), km^2)

# Subset to species:
wtd.density <- wmu.density.join %>% dplyr::filter(wmu.density.join$Species == "WHITE-TAILED DEER")
muledeer.density <- wmu.density.join %>% dplyr::filter(wmu.density.join$Species == "MULE DEER")
elk.density <- wmu.density.join %>% dplyr::filter(wmu.density.join$Species == "ELK")
moose.density <- wmu.density.join %>% dplyr::filter(wmu.density.join$Species == "MOOSE")

# Now we make a new col with our ungulates per sq km:
wmu.density.join$`Total_ungulate_density` <- wmu.density.join$`Estimated Population` / wmu.density.join$AREA_SQ_KM
head(wmu.density.join)

  # Just white tailed deer
wtd.density$Total_wt_deer_per_sq_km <- wtd.density$`Estimated Population` / wtd.density$AREA_SQ_KM
head(wtd.density)
  # Mule deer
muledeer.density$Total_mule_deer_per_sq_km <- muledeer.density$`Estimated Population` / muledeer.density$AREA_SQ_KM
head(muledeer.density)
  # Elk
elk.density$Total_elk_per_sq_km <- elk.density$`Estimated Population` / elk.density$AREA_SQ_KM
head(elk.density)
# Moose
moose.density$Total_moose_per_sq_km <- moose.density$`Estimated Population` / moose.density$AREA_SQ_KM
head(moose.density)

# Make this col numeric:
wmu.density.join$Total_ungulates_per_sq_km <- as.numeric(wmu.density.join$Total_ungulate_density)
wtd.density$Total_wt_deer_per_sq_km <- as.numeric(wtd.density$Total_wt_deer_per_sq_km)
muledeer.density$Total_mule_deer_per_sq_km <- as.numeric(muledeer.density$Total_mule_deer_per_sq_km)
elk.density$Total_elk_per_sq_km <- as.numeric(elk.density$Total_elk_per_sq_km)
moose.density$Total_moose_per_sq_km <- as.numeric(moose.density$Total_moose_per_sq_km)

# Make rasters for ungulate count ------------------------------------------
total.dens.v <- vect(wmu.density.join)
wtd.v <- vect(wtd.density)
mule.v <- vect(muledeer.density)
elk.v <- vect(elk.density)
moose.v <- vect(moose.density)

total.ungulate.rast <- terra::rasterize(total.dens.v, temp.rast, field = "Total_ungulates_per_sq_km")
total.elk.rast <- terra::rasterize(elk.v, temp.rast, field = "Total_elk_per_sq_km")
total.muledeer.rast <- terra::rasterize(mule.v, temp.rast, field = "Total_mule_deer_per_sq_km")
total.whitetailed.deer.rast <- terra::rasterize(wtd.v, temp.rast, field = "Total_wt_deer_per_sq_km")
total.moose.rast <- terra::rasterize(moose.v, temp.rast, field = "Total_moose_per_sq_km")

total.ung.raster <- raster(total.ungulate.rast)
total.ung.raster[is.na(total.ung.raster[])] <- 0 

total.elk.raster <- raster(total.elk.rast)
total.elk.raster[is.na(total.elk.raster[])] <- 0 

total.muledeer.raster <- raster(total.muledeer.rast)
total.muledeer.raster[is.na(total.muledeer.raster[])] <- 0 

total.wtd.raster <- raster(total.whitetailed.deer.rast)
total.wtd.raster[is.na(total.wtd.raster[])] <- 0 

total.moose.raster <- raster(total.moose.rast)
total.moose.raster[is.na(total.moose.raster[])] <- 0 

# Save these: -------------------------------------------------------------
writeRaster(total.ung.raster, "data/processed/total_ungulate_density.tif", overwrite=TRUE)
writeRaster(total.elk.raster, "data/processed/total_elk_density.tif", overwrite=TRUE)
writeRaster(total.muledeer.raster, "data/processed/total_muledeer_density.tif", overwrite=TRUE)
writeRaster(total.wtd.raster, "data/processed/total_white_tailed_deer_density.tif", overwrite=TRUE)
writeRaster(total.moose.raster, "data/processed/total_moose_density.tif", overwrite=TRUE)
st_write(wmu.density.join, "data/processed/ungulate_density_per_wmu.shp", append = FALSE)

