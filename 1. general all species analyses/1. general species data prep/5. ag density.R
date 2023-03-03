# Prep Agriculture Density --------------------------------------
  # Here we prep the density of row crop and livestock farms predictor

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)

# Load Data ---------------------------------------------------------------
farm.type <- read.csv("data/original/farm_type_2021_32100231.csv")

can.ccs.shp<- st_make_valid(st_read("Data/original/lccs000b16a_e.shp"))

bhb.50km.boundary <- st_read("data/processed/bhb_50km.shp")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

# Filter CCS and Ag Files to AB Only ---------------------------------------------------
# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"

# Filter down to just BC:
ab.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "Alberta") %>%
  st_make_valid()

ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50km.boundary))

# Save this for later:
st_write(ab.ccs, "data/processed/AB_CCS.shp", append = FALSE)

# Filter the Ag Files down to just AB districts: --------------------------
farm.type.ab <- farm.type %>% filter(grepl("Alberta", farm.type$GEO)) 

# Filtering to just the AB regions with a CCS number (so we can join to the CCS spatial data):
ab.farm.filter.ccs<-farm.type.ab %>%
  filter(., grepl("*CCS48*", farm.type.ab$GEO))

# Check to see what specific farm types exist in BC:
unique(farm.type.ab$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (the data had 2011 and 2016):
ab.farm.2021.ccs<-ab.farm.filter.ccs %>%
  filter(., REF_DATE == "2021") 

# Join Farm and CCS data:
ab.farm.2021.ccs$geoid <- str_sub(ab.farm.2021.ccs$DGUID, -7, -1)

farm.ccs.join <- ab.ccs.reproj %>% 
  left_join(., ab.farm.2021.ccs, by = c("CCSUID" = "geoid"))

# Calculate densities for ag categories: ----------------------------------
# Start by cropping the data down to BHB buffer:
#farm.ccs.sf <- st_transform(farm.ccs.join, st_crs(bhb.50km.boundary))

#farm.ccs.bhb <- st_intersection(farm.ccs.join, bhb.50km.boundary) # what if we skipped this step..


# Subset the data - separate total farms out of NAIC:
farm.bhb.subset <- subset(farm.ccs.join, North.American.Industry.Classification.System..NAICS. != "Total number of farms")
names(farm.bhb.subset)[names(farm.bhb.subset) == "North.American.Industry.Classification.System..NAICS."] <- "N_A_I_C"

# Condense Farm Types to Animal & Ground Crop Production:
animal.product.farming <- dplyr::filter(farm.bhb.subset,  N_A_I_C == "Cattle ranching and farming [1121]" | N_A_I_C == "Hog and pig farming [1122]" | N_A_I_C == "Poultry and egg production [1123]"| N_A_I_C == "Sheep and goat farming [1124]" | N_A_I_C =="Other animal production [1129]") 


ground.crop.production <- dplyr::filter(farm.bhb.subset, N_A_I_C == "Fruit and tree nut farming [1113]" | N_A_I_C == "Greenhouse, nursery and floriculture production [1114]" | N_A_I_C == "Vegetable and melon farming [1112]"
                                        | N_A_I_C == "Oilseed and grain farming [1111]" | N_A_I_C == "Other crop farming [1119]")
# Summarize these:
animal.prod.sf <- animal.product.farming %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))
ground.crop.sf <- ground.crop.production %>% 
  group_by(CCSUID) %>% 
  summarise(., "Total Farms in CCS" = sum(VALUE))

# Calculate our areas for the two objects: 
# Make our area units kilometers:
animal.prod.sf$AREA_SQ_KM <- units::set_units(st_area(animal.prod.sf), km^2)
ground.crop.sf$AREA_SQ_KM <- units::set_units(st_area(ground.crop.sf), km^2)

# Now we make a new col with our farms per sq km:
animal.prod.sf$Farms_per_sq_km <- animal.prod.sf$`Total Farms in CCS` / animal.prod.sf$AREA_SQ_KM
head(animal.prod.sf)

ground.crop.sf$Farms_per_sq_km <- ground.crop.sf$`Total Farms in CCS` / ground.crop.sf$AREA_SQ_KM
head(ground.crop.sf)

# Make this col numeric:
animal.prod.sf$Farms_per_sq_km <- as.numeric(animal.prod.sf$Farms_per_sq_km)
ground.crop.sf$Farms_per_sq_km <- as.numeric(ground.crop.sf$Farms_per_sq_km)


# Save these as .shp's for later:
st_write(animal.prod.sf,"data/processed/animal_product_farming.shp", append = FALSE)

st_write(ground.crop.sf, "data/processed/ground_crop_production.shp", append = FALSE) 

# Rasterize Farm Data  ---------------------------------------
## Here we make rasters for the farm type categories within our BHB watershed:

# Make these spat vectors:
animal.prod.sv <- vect(animal.prod.sf)
ground.crop.sv <- vect(ground.crop.sf)

# animal.prod.crop <- crop(animal.prod.sv, temp.rast)
# ground.crop.crop <- crop(ground.crop.sv, temp.rast)

# Rasterize our subset rasters:
animal.prod.rast <- terra::rasterize(animal.prod.sv, temp.rast, field = "Farms_per_sq_km")
ground.crop.rast <- terra::rasterize(ground.crop.sv, temp.rast, field = "Farms_per_sq_km")

# Make the NA's here 0's
animal.prod.raster <- raster(animal.prod.rast)
ground.crop.raster <- raster(ground.crop.rast)

animal.prod.raster[is.na(animal.prod.raster[])] <- 0 
ground.crop.raster[is.na(ground.crop.raster[])] <- 0

farm.density.combined <- animal.prod.raster + ground.crop.raster

# Save these Farm Rasters:
terra::writeRaster(animal.prod.raster, "data/processed/animal_production_density_raster.tif", overwrite=TRUE)
terra::writeRaster(ground.crop.raster, "data/processed/ground_crop_density_raster.tif" , overwrite=TRUE)
terra::writeRaster(farm.density.combined, "data/processed/combined_farm_density.tif" , overwrite=TRUE)

