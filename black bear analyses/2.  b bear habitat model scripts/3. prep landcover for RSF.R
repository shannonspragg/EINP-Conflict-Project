
# Prepping Collar Data Validation for bear HSI ----------------------------


# Load packages -----------------------------------------------------------
library(raster)
library(rgdal)



# Bring in data: ----------------------------------------------------------
  # Landcover:
land <- raster("")
projection(land)

  # Bear collar data:
bears <- readOGR("")
projection(bears)

## Look at bear data:
summary(bears)
unique(bears$ID) # The unique bear ID's
bears$ID <- as.factor(bears$ID)

plot(land)
points(bears, col= bears$ID)

# Reclassify landcover data for analysis: ---------------------------------
classification <- read.table("landcover reclass.txt", header=T)
head(classification)

levels(classification$Description2) # look at new classification levels

# convert to matrix to reclassify
class <- as.matrix(classification[,c(1,3)])
land_sub <- reclassify(land, rcl = class)

# create layers that represent continuous key land cover types w/ moving window
wetforest<-land_sub
values(wetforest) <- 0
wetforest[land_sub==9 | land_sub==11]<-l

#forested uplands
dryforest <- land_sub
values(dryforest) <- 0 
dryforest[land_sub==10 | land_sub== 12] <- 1

#/moving window t o get neighborhood proportion
fw <- focalWeight(land_sub, 5000, 'circle')
dry.focal <-focal(dryforest,w=fw, fun="sum",na.rm=T) 
wet.focal <- focal(wetforest, w = fw, fun= "sum", na.rm= T)

#merge raster data
layers <- stack(land_sub, wet.focal, dry.focal)
names(layers) <- c( "landcover", "wetforest", "dryforest") 
plot (layers)


# Save landcover raster ---------------------------------------------------
writeRaster()

