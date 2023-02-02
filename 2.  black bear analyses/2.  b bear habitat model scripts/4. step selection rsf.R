
# Prepping Collar Data Validation for bear HSI ----------------------------


# Load packages -----------------------------------------------------------
library(raster)
library(rgdal)
library(sp)
library(sf)
library(terra)
#install.packages("adehabitatLT")

# Bring in data: ----------------------------------------------------------
# Landcover:
land <- raster("data/processed/bhb_landcover.tif") 
private.land.rast <- raster("data/processed/bhb_privatelands.tif")
#elevation <- raster("data/processed/elevation_km_bhb.tif") # let's leave this out
slope <- raster("data/processed/slope_bhb.tif")
roads <- raster("data/processed/bhb_roads.tif")
dist2roads <- raster("data/processed/dist2roads_km_bhb.tif")
pop.dens <- raster("data/processed/human_dens_bhb.tif")
recent.wildfires <- raster("data/processed/bhb_fire_history.tif")
water <- raster("data/processed/bhb_water_areas.tif")
dist2drainage <- raster("data/processed/dist2drainage_km_bhb.tif")
dist2wb <- raster("data/processed/dist2waterbodies_km_bhb.tif")
human.dev <- raster("data/processed/bhw_ghm.tif")

projection(land)

# Bear collar data:
bears <- readOGR("data/processed/bear_collar_data.shp")
projection(bears)

land <- projectRaster(land, crs = "EPSG:26912") # UTM zone 12N for AB
private <- projectRaster(private.land.rast, crs = "EPSG:26912")
#elevation <- projectRaster(elevation, crs = "EPSG:26912")
slope <- projectRaster(slope, crs = "EPSG:26912")
roads <- projectRaster(roads, crs = "EPSG:26912")
dist2roads <- projectRaster(dist2roads, crs = "EPSG:26912")
hum.dens <- projectRaster(pop.dens, crs = "EPSG:26912")
recent.burn <- projectRaster(recent.wildfires, crs = "EPSG:26912")
water <- projectRaster(water, crs = "EPSG:26912")
dist2drainage <- projectRaster(dist2drainage, crs = "EPSG:26912")
dist2waterbodies <- projectRaster(dist2wb, crs = "EPSG:26912")
hum.dev <- projectRaster(human.dev, crs = "EPSG:26912")

crs(land) 
crs(private)
crs(roads)
crs(bears)

crs.land <- CRS("EPSG:26912")

## Look at bear data:
summary(bears)
unique(bears$animlID) # The unique bear ID's 1, 2 and 3
bears$animlID <- as.factor(bears$animlID)

plot(land) 
points(bears, col= bears$animlID)

# land categories using attributes(land) - it sorts them alphabetically
# 0. Agriculture
# 1. Broafleaf
# 2. Coniferous
# 3. Developed
# 4. Exposed
# 5. Grassland
# 6. Mixed forest
# 7. Rock
# 8. shrubland
# 9. Snow / Ice
# 10. water

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

#merge raster data
#layers <- stack(land, forest.focal, shrubgrass.focal, private)
layers <- stack(land, forest.focal, private, slope, roads, dist2roads, hum.dens, 
                recent.burn, water, dist2drainage, dist2waterbodies, hum.dev)

names(layers) <- c( "landcover", "forested", "privateland", "slope", "roads", "dist2roads", "human.dens",
                    "recent.burns", "water", "dist2drainage", "dist2waterbodies", "human.mod") 
plot(layers)

# Step Selection Functions ------------------------------------------------
# Split bear datetime col into two:
library(dplyr)
library(tidyr)
library(tidyverse)

bears.sf <- st_as_sf(bears)
bears.sf$Date <- str_sub(bears.sf$DateTim, 1, 10)
bears.sf$Time <- str_sub(bears.sf$DateTim, 12, 20)
bears <- as(bears.sf, "Spatial")

# Prep trajectory:
library(adehabitatLT)

bears.ltraj <- as.ltraj(xy = coordinates(bears), 
                        date =  as.POSIXct(paste(bears$Date, bears$Time, sep = " ")), 
                        id = bears$animlID)
plot(bears.ltraj, id = "U01")

# distance for first bear id:
bears.ltraj[[1]][,6]
hist(bears.ltraj[[1]][,6], main = "First BearID")

#plots of relative movement angles for second CatID 
#relative angles:change ind ireccion from previous time step 
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
traj.df$abs.angle_t_4 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_4[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_5 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_5[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_6 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_6[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_7 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_7[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_8 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_8[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_9 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_9[i] <- ifelse(traj.df$id[i] ==
                                       traj.df$id[i - 1], traj.df$abs.angle[i - 1] , NA) 
}
traj.df$abs.angle_t_10 <- NA 
for(i in 2:nrow(traj.df)) {
  traj.df$abs.angle_t_10[i] <- ifelse(traj.df$id[i] ==
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

traj.df$x_a4 <- traj.df[, "x"] + traj.df[, "a.dist4"] * cos(traj.df[, "abs.angle_t_4"] + traj.df[, "a.angle4"]) 
traj.df$y_a4 <- traj.df[, "y"] + traj.df[, "a.dist4"] * sin(traj.df[, "abs.angle_t_4"] + traj.df[, "a.angle4"])

traj.df$x_a5 <- traj.df[, "x"] + traj.df[, "a.dist5"] * cos(traj.df[, "abs.angle_t_5"] + traj.df[, "a.angle5"]) 
traj.df$y_a5 <- traj.df[, "y"] + traj.df[, "a.dist5"] * sin(traj.df[, "abs.angle_t_5"] + traj.df[, "a.angle5"])

traj.df$x_a6 <- traj.df[, "x"] + traj.df[, "a.dist6"] * cos(traj.df[, "abs.angle_t_6"] + traj.df[, "a.angle6"]) 
traj.df$y_a6 <- traj.df[, "y"] + traj.df[, "a.dist6"] * sin(traj.df[, "abs.angle_t_6"] + traj.df[, "a.angle6"])

traj.df$x_a7 <- traj.df[, "x"] + traj.df[, "a.dist7"] * cos(traj.df[, "abs.angle_t_7"] + traj.df[, "a.angle7"]) 
traj.df$y_a7 <- traj.df[, "y"] + traj.df[, "a.dist7"] * sin(traj.df[, "abs.angle_t_7"] + traj.df[, "a.angle7"])

traj.df$x_a8 <- traj.df[, "x"] + traj.df[, "a.dist8"] * cos(traj.df[, "abs.angle_t_8"] + traj.df[, "a.angle8"]) 
traj.df$y_a8 <- traj.df[, "y"] + traj.df[, "a.dist8"] * sin(traj.df[, "abs.angle_t_8"] + traj.df[, "a.angle8"])

traj.df$x_a9 <- traj.df[, "x"] + traj.df[, "a.dist9"] * cos(traj.df[, "abs.angle_t_9"] + traj.df[, "a.angle9"]) 
traj.df$y_a9 <- traj.df[, "y"] + traj.df[, "a.dist9"] * sin(traj.df[, "abs.angle_t_9"] + traj.df[, "a.angle9"])

traj.df$x_a10 <- traj.df[, "x"] + traj.df[, "a.dist10"] * cos(traj.df[, "abs.angle_t_10"] + traj.df[, "a.angle10"]) 
traj.df$y_a10 <- traj.df[, "y"] + traj.df[, "a.dist10"] * sin(traj.df[, "abs.angle_t_10"] + traj.df[, "a.angle10"])

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
traj.a4 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a4", "y_a4")])
traj.a5 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a5", "y_a5")])
traj.a6 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a6", "y_a6")])
traj.a7 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a7", "y_a7")])
traj.a8 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a8", "y_a8")])
traj.a9 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a9", "y_a9")])
traj.a10 <- data.frame(use = rep(0, nrow(traj.df)),
                      traj.df[,c("id", "pkey", "date", "x_a10", "y_a10")])

names(traj.use) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a1) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a2) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a3) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a4) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a5) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a6) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a7) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a8) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a9) <- c("use", "id", "pair", "date", "x", "y")
names(traj.a10) <- c("use", "id", "pair", "date", "x", "y")

# append use and availible data together: (traj.a4-10 should be created in same way as above)
stepdata.final <- rbind(traj.use, traj.a1, traj.a2, traj.a3, traj.a4, traj.a5, traj.a6, traj.a7, traj.a8, traj.a9, traj.a10)

# use extract function to get info on environmental covs betweewn paired use and availability locations
# create spdf
step.coords <- SpatialPoints(stepdata.final[,c("x", "y")], proj4string = CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"))
# extract covariates from layers
cov <- raster::extract(layers, step.coords)
# add covs to dataframe of use/availible:
stepdata.final <- data.frame(cbind(stepdata.final, cov))

# make landcover categories:
stepdata.final <- stepdata.final %>%
  dplyr::mutate(landcover.desc = 
                  case_when(stepdata.final$landcover == 10 ~ "Water",
                            stepdata.final$landcover == 9 ~ "Snow/Ice",
                            stepdata.final$landcover == 7 ~ "Rock/Rubble",
                            stepdata.final$landcover == 4 ~ "Exposed Land",
                            stepdata.final$landcover == 3 ~ "Developed",
                            stepdata.final$landcover == 8 ~ "Shrubland",
                            stepdata.final$landcover == 5 ~ "Grassland", # meadow / grassland
                            stepdata.final$landcover == 0 ~ "Agriculture",
                            stepdata.final$landcover == 2 ~ "Coniferous Forest", # conifer mixed forest
                            stepdata.final$landcover == 1 ~ "Broadleaf Forest", # birch, oak and aspen
                            stepdata.final$landcover == 6 ~ "Mixed Forest", # alpine mixed forest
                  ))

# fit two conditional logit models and contrast to conventional logistic regression RSF:
#install.packages("survival")
library(survival)

# NOTE: we are leaving out elevation because there is very minimal elevation change across the study area (<200m), and don't want it to
# skew the data. We also are leaving out recent burned areas as they are very minimal and also skewing the data.
# We may also remove the forested moving window bc it covers so much area

# conditional logistic
logit.ssf <- clogit(use ~ landcover.desc + privateland + slope + roads + dist2roads + human.dens + water + dist2drainage + dist2waterbodies + human.mod + strata(pair), data = stepdata.final)

# including bearID as cluster:
logit.bear.ssf <- clogit(use ~  landcover.desc + privateland + slope + roads + dist2roads + human.dens +  water + dist2drainage + dist2waterbodies + human.mod + strata(pair) + cluster(id), method = "approximate", data = stepdata.final)

# logistic ignoring local pairing structure of data
logit.rsf <- glm(use ~  landcover.desc + privateland + slope + roads + dist2roads + human.dens + water + dist2drainage + dist2waterbodies + human.mod, family = "binomial", data = stepdata.final)
logit.rsf2 <- glm(use ~  landcover.desc + privateland + slope + roads + dist2roads + human.dens , family = "binomial", data = stepdata.final)

# compare coefficients
logit.ssf
coef(logit.ssf)
logit.bear.ssf
coef(logit.bear.ssf) # Here are the coefficients we use to build our "validated" habitat model
logit.rsf
coef(logit.rsf) 
logit.rsf2

saveRDS(logit.bear.ssf, "data/processed/bbear_collar_ssf.rds")
saveRDS(logit.ssf, "data/processed/bbear_ssf.rds")
