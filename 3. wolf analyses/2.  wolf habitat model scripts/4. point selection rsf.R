# Prepping CT Data Validation for wolf HSI ----------------------------


# Load packages -----------------------------------------------------------
library(raster)
library(rgdal)
library(sp)
library(sf)
library(terra)

# Bring in data: ----------------------------------------------------------
# Landcover:
land <- raster("data/processed/bhb_landcover.tif") 
landcover <- rast("data/processed/bhb_landcover.tif")
private.land.rast <- raster("data/processed/bhb_privatelands.tif")
elevation <- raster("data/processed/elevation_km_bhb.tif")
slope <- raster("data/processed/slope_bhb.tif")
road.dens <- raster("data/processed/bhb_road_density_250m.tif")
dist2roads <- raster("data/processed/dist2roads_km_bhb.tif")
pop.dens <- raster("data/processed/human_dens_bhb.tif")
dist2waterways <- raster("data/processed/dist2waterbodies_km_bhb.tif")
human.development <- raster("data/processed/bhw_ghm.tif")
recent.wildfires <- raster("data/processed/bhb_fire_history.tif")
livestock.density <- raster("data/processed/animal_production_density_raster.tif")
ungulate.density <- raster("data/processed/total_ungulate_density.tif")
dist2pa.rast <- raster("data/processed/dist2pa_km_bhb.tif")
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

projection(land)

# Bear collar data:
wolves <- readOGR("data/processed/wolf_ct_data.shp")
wolves.sf <- st_read("data/processed/wolf_ct_data.shp")
projection(wolves)

land <- projectRaster(land, crs = "EPSG:26912") # UTM zone 12N for AB
private <- projectRaster(private.land.rast, crs = "EPSG:26912")
elevation <- projectRaster(elevation, crs = "EPSG:26912")
slope <- projectRaster(slope, crs = "EPSG:26912")
road.dens <- projectRaster(road.dens, crs = "EPSG:26912")
dist2roads <- projectRaster(dist2roads, crs = "EPSG:26912")
hum.dens <- projectRaster(pop.dens, crs = "EPSG:26912")
recent.burn <- projectRaster(recent.wildfires, crs = "EPSG:26912")
dist2drainage <- projectRaster(dist2waterways, crs = "EPSG:26912")
livestock.dens <- projectRaster(livestock.density, crs = "EPSG:26912")
hum.dev <- projectRaster(human.development, crs = "EPSG:26912")
ungulate.dens <- projectRaster(ungulate.density, crs = "EPSG:26912")
dist2pa <- projectRaster(dist2pa.rast, crs = "EPSG:26912")
temp.rast <- rast(dist2pa)

crs(land) 
crs(private)
crs(road.dens)
crs(wolves)

crs.land <- CRS("EPSG:26912")

## Look at wolf data:
summary(wolves)
unique(wolves$Anml_ID) # The unique bear ID's 1, 2 and 3
wolves$Anml_ID <- as.factor(wolves$Anml_ID)

plot(land) 
points(wolves, col= wolves$Anml_ID)

plot(wolves, add=T)

# Reclassify landcover data for analysis: ---------------------------------
# create layers that represent continuous key land cover types w/ moving window
forests<-land
values(forests) <- 0
forests[land == 1 | land == 2 | land == 6] <- 1

#shrub and grasslands
shrubgrass <- land
values(shrubgrass) <- 0 
shrubgrass[land == 5 | land == 8] <- 1

# agriculture land
agriculture <- land
values(agriculture) <- 0 
agriculture[land == 0] <- 1

# developed land
developed <- land
values(developed) <- 0 
developed[land == 3] <- 1

# exposed land
exposed <- land
values(exposed) <- 0 
exposed[land == 4] <- 1

# rocky
rock <- land
values(rock) <- 0 
rock[land == 7] <- 1

# snow/ice
snow.ice <- land
values(snow.ice) <- 0 
snow.ice[land == 9] <- 1

# water
water <- land
values(water) <- 0 
water[land == 10] <- 1

#/moving window t o get neighborhood proportion
fw <- focalWeight(land, 5000, 'circle')
forest.focal <-focal(forests,w=fw, fun="sum",na.rm=T) 
shrubgrass.focal <- focal(shrubgrass, w = fw, fun= "sum", na.rm= T)
ag.focal <- focal(agriculture, w = fw, fun= "sum", na.rm= T)
developed.focal <- focal(developed, w = fw, fun= "sum", na.rm= T)
exposed.focal <- focal(exposed, w = fw, fun= "sum", na.rm= T)
rock.focal <- focal(rock, w = fw, fun= "sum", na.rm= T)
snow.focal <- focal(snow.ice, w = fw, fun= "sum", na.rm= T)
water.focal <- focal(water, w = fw, fun= "sum", na.rm= T)

#match landcover to other layers
# land.p <- terra::project(landcover, temp.rast)
# landcover <- rasterize(land.p)

layers <- stack(land, forest.focal, shrubgrass.focal, ag.focal, developed.focal, exposed.focal, rock.focal, snow.focal, 
                water.focal, private, elevation, slope, road.dens, dist2roads, hum.dens, recent.burn, livestock.dens, 
                dist2drainage, ungulate.dens, dist2pa, hum.dev)

names(layers) <- c("landcover", "forested","shrub/grass", "agriculture", "developed", "exposed", "rock", "snow/ice", "water", 
                   "privateland", "elevation", "slope", "road dens", "dist2roads", "human.dens", "recent.burns",
                   "livestock.dens", "dist2drainage", "ungulate.dens", "dist2pa", "human.mod") 
plot(layers)

# Point Selection: --------------------------------------------------------
library(reshape2)

use <- raster::extract(layers, wolves, method = 'bilinear',  na.rm = TRUE)
use <- data.frame(use)
use$WolfID <- as.factor(wolves$Anml_ID)

# use reshape2. dcast function: let's try to skip this...
useWolfID <- dcast(use, WolfID ~ landcover, length, value.var = "WolfID")
#newclass.names <- unique(classification[,3:4])

# use sample random from raster to create availability
set.seed(8)
rand.II <- sampleRandom(layers, size= 1000)
rand.II.land <- data.frame(rand.II)

# sum up counts of each land cover type
# table(rand.II.land)
# avail.II <- tapply(rand.II.land, rand.II.land, length)

# Presence only distribution data:
use.cov <- data.frame(use[,1:21], use= 1)
back.cov <- data.frame(rand.II, use = 0)
all.cov <- data.frame(rbind(use.cov, back.cov))

# Run two models: NEED TO GET LANDCOVER TO BE CATEGORICAL / HAVE LAYERS
rsf.all <- glm(use ~ forested + shrub.grass + agriculture + developed + exposed + rock + snow.ice + water + 
                 privateland + elevation + slope + road.dens + dist2roads + human.dens + recent.burns + livestock.dens
               + dist2drainage + ungulate.dens + dist2pa + human.mod, family = binomial(link = logit), data = all.cov)

rsf.simple <- glm(use ~ forested + shrub.grass + elevation + slope + road.dens  + human.dens +
                   dist2drainage + ungulate.dens + human.mod, family = binomial(link = logit), data = all.cov)

rsf.null <- glm(use ~ 1, family = binomial(link = logit), data = all.cov)

# AIC
anova(rsf.all, rsf.simple, rsf.null, test = "LRT")

## NOTE: models are VERY poor due to the very small number of sightings for wolf so far

saveRDS(rsf.all, "data/processed/wolf_ct_RSF.rds")

