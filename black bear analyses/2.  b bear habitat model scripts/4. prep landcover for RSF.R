
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



# Step Selection Functions ------------------------------------------------
# Split bear datetime col into two:
library(dplyr)
library(tidyr)
library(tidyverse)

bears.sf <- st_as_sf(bears)
bears.sf$Date <- str_sub(bears.sf$DateTim, 1, 10)
bears.sf$Time <- str_sub(bears.sf$DateTim, 12, 20)

#bears.sf <- bears.sf %>% tidyr::separate(DateTim, c('Date', 'Time'), sep = " ")
bears <- as(bears.sf, "Spatial")

# Prep trajectory:
install.packages("adehabitatLT")
library(adehabitatLT)

#reformat date ro a POSIXct object for date:
# substrRight <- function(x, n) {
#   substr(x, nchar(x) - n+1, nchar(x))
# }

# bears$Date <- as.character(bears$Date)
# bear.date <- as.numeric(substrRight(bears$Date, 3))
# #bear.date <- as.numeric(bears$Date)
# bears$Date <- as.Date(bear.date, origin = as.Date("2021-08-11 1:00:01"))
# head(bears)
# # create POSIXct object:
# bears$Date <- as.POSIXct(bears$Date, "%Y-%m-%d %H:%M:%S")
# bear.ltrag <- as.ltraj(xy = coordinates(bears), date = bears$Date, id = bears$animlID, typeII = T)

bears.ltraj <- as.ltraj(xy = coordinates(bears), 
                          date =  as.POSIXct(paste(bears$Date, bears$Time, sep = " ")), 
                          id = bears$animlID)
plot(bears.ltraj, id = "U01")

# distance for first bear id:
bears.ltraj[[1]][,6]
hist(bears.ltraj[[1]][,6], main = "First BearID")

#plots of relative movement angles for second CatID 
#relativeangles:changeindireccionfromprevious timestep 
rose.diag(na.omit(bears.ltraj[[1]][,10]), bins=12, prop= 1.5) 
circ.plot(bears.ltraj[[2]][,10], pch = 1)

# Step selection generating 10 locations to sample habitat use
stepdata <- data.frame(coordinates(bears))
stepdata$BearID <- as.factor(bears$animlID)
names(stepdata) <- c("X", "Y", "BearID")
n.use <- dim(stepdata)[[1]]
n.avail <- n.use * 10

# generate random samples of step lengths and turning angles:
# convert trajectory back to df for manipulation
traj.df <- ld(bears.ltraj)

# sample steps with replacement
avail.dist <- matrix(sample(na.omit(traj.df$dist), 
                            size = n.avail, replace = T), ncol = 10)
avail.angle <- matrix(sample(na.omit(traj.df$rel.angle), 
                            size = n.avail, replace = T), ncol = 10)
#name cols:
colnames(avail.dist) <- c("a.dist1", "a.dist2", "a.dist3", "a.dist4","a.dist5","a.dist6","a.dist7","a.dist8","a.dist9","a.dist10")
colnames(avail.angle) <- c("a.angle1", "a.angle2", "a.angle3", "a.angle4", "a.angle5", "a.angle6", "a.angle7", "a.angle8", "a.angle9", "a.angle10")

# link availible distances/angles to observations:
traj.df <- cbind(traj.df, avail.dist, avail.angle)

#calculatecoordinates in t+l from t using absolute angle: 
traj.df[2,"x"] + traj.df[2,"dist"] * cos(traj.df[2,"abs.angle"])
traj.df[2, "y"] + traj.df[2, "dist"] * sin(traj.df[2,"abs.angle"])
# check:
traj.df[3, c("x", "y")]

# create new values for df where av ailible xy coords are created and linked to appropriate use coords
traj.df$abs.angle_t_1 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_1[i] <- ifelse(traj.df$id[i] ==
  traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
  }
traj.df$abs.angle_t_2 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_2[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_3 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_3[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}

# calc new coords using trig
  # use coords for t + 1
traj.df$x_t1 <- traj.df[, "x"] + traj.df[,"dist"] * cos(traj.df[, "abs.angle"]) 
traj.df$y_t1 <- traj.df[, "y"] + traj.df[, "dist"] * sin(traj.df[,"abs.angle"])

#calculate avail coords for t+l
traj.df$x_a1 <- traj.df[, "x"] + traj.df[, "a.dist1"] * cos(traj.df[, "abs.angle_t_1"] + traj.df[, "a.angle1"]) 
traj.df$y_a1 <- traj.df[, "y"] + traj.df[, "a.dist1"] * sin(traj.df[, "abs.angle_t_1"] + traj.df[, "a.angle1"])

traj.df$x_a2 <- traj.df[, "x"] + traj.df[, "a.dist2"] * cos(traj.df[, "abs.angle_t_2"] + traj.df[, "a.angle2"]) 
traj.df$y_a2 <- traj.df[, "y"] + traj.df[, "a.dist2"] * sin(traj.df[, "abs.angle_t_2"] + traj.df[, "a.angle2"])

traj.df$x_a3 <- traj.df[, "x"] + traj.df[, "a.dist3"] * cos(traj.df[, "abs.angle_t_3"] + traj.df[, "a.angle3"]) 
traj.df$y_a3 <- traj.df[, "y"] + traj.df[, "a.dist3"] * sin(traj.df[, "abs.angle_t_3"] + traj.df[, "a.angle3"])

# reformat data for step selection:
traj.df <- traj.df[complete.cases(traj.df),] #remove NAs
traj.use <- data.frame(use = rep(1, nrow(traj.df)),
                       traj.df[,c("id", "pkey", "date", "x_t1", "y_t1")])
traj.a1 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a1", "y_a1")])
traj.a2 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a2", "y_a2")])
traj.a3 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a3", "y_a3")])
names(traj.use) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a1) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a2) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a3) <- c("use", "id", "pair", "date", "x", "y")

# append use and availible data together: (traj.a4-10 should be created in same way as above)











