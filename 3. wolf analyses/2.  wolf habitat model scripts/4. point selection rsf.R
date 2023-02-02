# Prepping CT Data Validation for wolf HSI ----------------------------


# Load packages -----------------------------------------------------------
library(raster)
library(rgdal)
library(sp)
library(sf)
library(terra)
library(dplyr)

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
#temp.rast <- rast(dist2pa)

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

#/moving window t o get neighborhood proportion
fw <- focalWeight(land, 5000, 'circle')
forest.focal <-focal(forests,w=fw, fun="sum",na.rm=T) 
shrubgrass.focal <- focal(shrubgrass, w = fw, fun= "sum", na.rm= T)

layers <- stack(forest.focal, shrubgrass.focal, private, elevation, slope, road.dens, dist2roads, hum.dens, recent.burn, livestock.dens, 
                dist2drainage, ungulate.dens, dist2pa, hum.dev) #ag.focal, developed.focal, exposed.focal, rock.focal, snow.focal, water.focal,

names(layers) <- c( "forested","shrub/grass", "privateland", "elevation", "slope", "road dens", "dist2roads", "human.dens", "recent.burns",
                   "livestock.dens", "dist2drainage", "ungulate.dens", "dist2pa", "human.mod") 
plot(layers)

# Point Selection: --------------------------------------------------------
library(reshape2)

use <- raster::extract(layers, wolves, method = 'bilinear',  na.rm = TRUE)
use <- data.frame(use)
use$landcover <- raster::extract(land, wolves,  na.rm = TRUE)
use$landcover <- as.factor(use$landcover)

# make landcover categories: 
levels(use$landcover) # these are the LC values present at each point

use <- use %>%
  dplyr::mutate(land.desc = 
                  case_when(use$landcover == 1.00000002736691 ~ "Broadleaf Forest",
                            use$landcover == 2 ~ "Coniferous Forest",
                            use$landcover == 2.00000000594696 ~ "Coniferous Forest",
                            use$landcover == 2.00000000606547 ~ "Coniferous Forest",
                            use$landcover == 3.00000000029057 ~ "Developed",
                            use$landcover == 3.00000001062592 ~ "Developed",
                            use$landcover == 3.00000002745353 ~ "Developed",
                            use$landcover == 4.99999999955297 ~ "Grassland", # 5
                            use$landcover == 7.9999999730892 ~ "Shrubland", # 8
                            use$landcover == 8.00000001115724 ~ "Shrubland"))
                            
use$WolfID <- as.factor(wolves$Anml_ID)

# use reshape2. dcast function: let's try to skip this...
useWolfID <- dcast(use, WolfID ~ landcover, length, value.var = "WolfID")
#newclass.names <- unique(classification[,3:4])

# use sample random from raster to create availability
set.seed(8)
rand.II <- sampleRandom(layers, size= 1000)
rand.II.land <- data.frame(rand.II)
rand.II.land$landcover <- sampleRandom(land, size= 1000)

# Try replacing values to match whole numbers:
rand.II.land$landcover[rand.II.land$landcover >= 9.1 & rand.II.land$landcover <= 10.1] <- 10
rand.II.land$landcover[rand.II.land$landcover >= 8.1 & rand.II.land$landcover <= 9.1] <- 9
rand.II.land$landcover[rand.II.land$landcover >= 7.1 & rand.II.land$landcover <= 8.1] <- 8
rand.II.land$landcover[rand.II.land$landcover >= 6.1 & rand.II.land$landcover <= 7.1] <- 7
rand.II.land$landcover[rand.II.land$landcover >= 5.1 & rand.II.land$landcover <= 6.1] <- 6
rand.II.land$landcover[rand.II.land$landcover >= 4.1 & rand.II.land$landcover <= 5.1] <- 5
rand.II.land$landcover[rand.II.land$landcover >= 3.1 & rand.II.land$landcover <= 4.1] <- 4
rand.II.land$landcover[rand.II.land$landcover >= 2.1 & rand.II.land$landcover <= 3.1] <- 3
rand.II.land$landcover[rand.II.land$landcover >= 1.1 & rand.II.land$landcover <= 2.1] <- 2
rand.II.land$landcover[rand.II.land$landcover >= 0.1 & rand.II.land$landcover <= 1.1] <- 1
rand.II.land$landcover[rand.II.land$landcover >= -1 & rand.II.land$landcover <= 0.1] <- 0
unique(rand.II.land$landcover)

rand.II.land <- rand.II.land %>% # STILL GETTING NA'S... NEED TO FIX THIS with a range assignment
  dplyr::mutate(land.desc = 
                  case_when(rand.II.land$landcover == 10 ~ "Water",
                            rand.II.land$landcover == 9 ~ "Snow/Ice",
                            rand.II.land$landcover == 7 ~ "Rock/Rubble",
                            rand.II.land$landcover == 4 ~ "Exposed Land",
                            rand.II.land$landcover == 3 ~ "Developed",
                            rand.II.land$landcover == 8 ~ "Shrubland",
                            rand.II.land$landcover == 5 ~ "Grassland", # meadow / grassland
                            rand.II.land$landcover == 0 ~ "Agriculture",
                            rand.II.land$landcover == 2 ~ "Coniferous Forest", # conifer mixed forest
                            rand.II.land$landcover == 1 ~ "Broadleaf Forest", # birch, oak and aspen
                            rand.II.land$landcover == 6 ~ "Mixed Forest", # alpine mixed forest
                  ))

# sum up counts of each land cover type
# table(rand.II.land)
# avail.II <- tapply(rand.II.land, rand.II.land, length)

# Presence only distribution data:
use.cov <- data.frame(use[,1:16], use= 1)
back.cov <- data.frame(rand.II.land[,1:16], use = 0)
all.cov <- data.frame(rbind(use.cov, back.cov))

# Run two models: NEED TO GET LANDCOVER TO BE CATEGORICAL / HAVE LAYERS
rsf.all <- glm(use ~ land.desc + forested + shrub.grass + privateland + elevation + slope + road.dens + dist2roads + human.dens + recent.burns + livestock.dens
               + dist2drainage + ungulate.dens + dist2pa + human.mod, family = binomial(link = logit), data = all.cov)

rsf.simple <- glm(use ~ forested + shrub.grass + elevation + slope + road.dens  + human.dens +
                   dist2drainage + ungulate.dens + human.mod, family = binomial(link = logit), data = all.cov)

rsf.null <- glm(use ~ 1, family = binomial(link = logit), data = all.cov)

# AIC
anova(rsf.all, rsf.simple, rsf.null, test = "LRT")

## NOTE: models are VERY poor due to the very small number of sightings for wolf so far
# NEED TO: run a k-fold cross validation on these to see how they cover the area..

saveRDS(rsf.all, "data/processed/wolf_ct_RSF.rds")
rsf.all <- readRDS("data/processed/wolf_ct_RSF.rds")
