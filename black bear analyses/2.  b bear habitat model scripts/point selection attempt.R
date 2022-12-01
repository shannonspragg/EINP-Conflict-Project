
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

land <- projectRaster(land, crs = "EPSG:26912") # UTM zone 12N for AB
#land <- raster(land)
crs(land) 
crs(bears)

crs.land <- CRS("EPSG:26912")

## Look at bear data:
summary(bears)
unique(bears$animlID) # The unique bear ID's 1, 2 and 3
bears$animlID <- as.factor(bears$animlID)

plot(land) 
points(bears, col= bears$animlID)


# Reclassify landcover data for analysis: ---------------------------------
classification <- read.table("data/original/landcover reclass.txt", header=T)
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

use <- raster::extract(layers, bears)
use$BearID <- as.factor(bears$animlID)

#use reshape2, dcast function:
useBearID <- dcast(use, BearID ~ land, length, value.var = "BearID")

newclass.names <- unique(classification[,3:4])
names(useCatID) <- c("CatID", as.character(newclass.names[l:13,2]))

# Generate random points and extract landcover categories:

#use sampleRandomfunction from raster to create availability 
set.seed (8)
rand.II <- sampleRandom(land, size = 1000) 
rand.II.land <- data.frame(rand.II)
#s u m u μ c o u n c s o f e a c h l a n d c o v e r t y p e 
table(rand.II.land)

#sum up counts of each landcover type
avail.II <- tapply(rand.II.land, rand.II.land, length) 
names(avail.II) <- as.characcer(newclass.names[1:14, 2] )
avail.II
#remove exotics, which was not observed in sample of use
avai1.II <-avai1.II[c(-14)]


# Running an MCP: ---------------------------------------------------------
library(sp)
install.packages("adehabitatHS")
library(adehabitatHS)

bear.unique <- unique(bears$animlID)
samples <- 200
rand.III <- matrix(nrow=0, ncol = 2)

# loop for all individuals
for(i in 1:length(bear.unique)) {
  id.i <- bear.unique[i]
  bear.i <- bears[bears$animlID == id.i,]
  mcp.i <- mcp(SpatialPoints(coordinates(bear.i)), percent = 99)
  rand.i <- spsample(mcp.i, type = "random", n= samples)
  rand.i.sample <- raster::extract(land, rand.i)
  
  # make matrix of id and rand samples
  bear.i <- rep(bear.unique[i], length(rand.i))
  rand.bear.i <- cbind(bear.i, rand.i.sample)
  rand.III <- rbind(rand.III, rand.bear.i)
}

# reshape data with dcast function:
rand.III <- data.frame(rand.III)
rand.III$bear.i <- as.factor(rand.III$bear.i)
colnames(rand.III) = c("bear.i", "land") 
avail.III <- dcast(rand.III,bear.i ~ land, length, value.var = "bear.i")
#names(avail.III) <- c( "BearID", as.character(newclass.names[1:13,2])) 
avail.III

# Calculate selection ratios
library(adehabitatHS)

sel.ratioII <- widesII(u = useBearID[,c(2:ncol(useBearID))],
                       a = as.vector(avail.II),
                       avknown = F, alpha = 0.05)
summary(sel.ratioII)
sel.ratioII
sel.ratioII$wi # selection ratios
sel.ratioII$se.wi # selection ratio SEs
plot(sel.ratioII)


