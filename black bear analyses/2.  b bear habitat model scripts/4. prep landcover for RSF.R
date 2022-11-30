
# Prepping Collar Data Validation for bear HSI ----------------------------


# Load packages -----------------------------------------------------------
library(raster)
library(rgdal)
library(sp)
library(sf)
library(terra)

# Bring in data: ----------------------------------------------------------
  # Landcover:
land <- raster("data/processed/bhb_landcover.tif") # TRY WITH AB raster (not cropped)
projection(land)

  # Bear collar data:
bears <- readOGR("data/processed/bear_collar_data.shp")
projection(bears)

crs(land) 
crs(bears)

land <- projectRaster(land, crs = "EPSG:26912") # UTM zone 12N for AB
#land <- raster(land)

## Look at bear data:
summary(bears)
unique(bears$animlID) # The unique bear ID's 1, 2 and 3
bears$animlID <- as.factor(bears$animlID)

plot(land) 
points(bears, col= bears$animlID)


# Reclassify landcover data for analysis: ---------------------------------
classification <- read.table("landcover reclass.txt", header=T)
head(classification)

levels(classification$Description2) # look at new classification levels

# convert to matrix to reclassify
class <- as.matrix(classification[,c(1,3)])
land_sub <- reclassify(land, rcl = class)

# create layers that represent continuous key land cover types w/ moving window
forests<-land
values(forests) <- 0
forests[land == 2 | land == 3 | land == 7] <- 1

#forested uplands
shrubgrass <- land
values(shrubgrass) <- 0 
shrubgrass[land == 6 | land == 9] <- 1

#/moving window t o get neighborhood proportion
fw <- focalWeight(land, 5000, 'circle')
forest.focal <-focal(forests,w=fw, fun="sum",na.rm=T) 
shrubgrass.focal <- focal(shrubgrass, w = fw, fun= "sum", na.rm= T)

#merge raster data
layers <- stack(land, forest.focal, shrubgrass.focal)
names(layers) <- c( "landcover", "forested", "shrub/grass") 
plot (layers)


# Point Selection Function ------------------------------------------------
install.packages("reshape2")
library(reshape2)

use <- extract(layers, bears)
use$bearID <- as.factor(bears$animlID)

#use reshape2, dcast function:
useCatID <- dcast (use, CatID - landcover, lengch, value.var = "Cat:ID") 
newclass.names <- unique(classification[,3:4])
names(useCatID) <- c("CatID", as.character(newclass.names[l:13,2]))

# Generate random points and extract landcover categories:

#use sampleRandomfunction from raster to create availability 
set.seed (8 )
rand.II <- sampleRandom(landcover, size=lOOO) 
rand.II.land <- data.frame(rand.II)
#s u m u μ c o u n c s o f e a c h l a n d c o v e r t y p e 
table(rand.II.land)




