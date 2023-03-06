
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
#bear.full.mod.quad <- readRDS("Data/processed/bear_quad_reg.rds")
# bear.int.only <- readRDS("Data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("Data/processed/bear_full_mod.rds")
bear.conflict.df.scl <- readRDS("Data/processed/bear_conf_df_scl.rds")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(bear.full.mod)
var.int <- ranef(bear.full.mod)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/AB_CCS.shp")
ccs.reproj <- st_transform(ccs.sf, st_crs(bhw))
ccs.sf.join <- ccs.reproj %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Lac la Biche County",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_km_bhb.tif") 
hum.dens.r <- rast("data/processed/human_dens_bhb.tif")
animal.dens <- rast("data/processed/animal_production_density_raster.tif")
ground.dens <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.r <- rast("data/processed/bhb_ndvi.tif")
ghm.r <- rast("data/processed/bhw_ghm.tif")
bhs <- rast("data/processed/bbear_habitat_suitability.tif")
biophys <- rast("data/processed/bbear_collar_validated_cum_currmap.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif") # Only need this if using model with conflict

bbear.conf.pred.stack <- c(dist.2.pa, hum.dens.r, animal.dens, ground.dens, ndvi.r, ghm.r, bhs, biophys, conflict)


#Create global intercept raster
global.int <- dist.2.pa
global.int[!is.na(global.int)] <- fixed.effects[[1]]

#create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.pa, field='(Intercept)')
ccs.int <- raster(ccs.int)
ccs.int[is.na(ccs.int[])] <- 0 
ccs.int <- rast(ccs.int)

#scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(bear.conflict.df.scl$dist2pa)[[2]])/attributes(bear.conflict.df.scl$dist2pa)[[3]]

pop.dens.scl <- (hum.dens.r - attributes(bear.conflict.df.scl$humandens)[[2]])/attributes(bear.conflict.df.scl$humandens)[[3]]

animal.dens.scl <- (animal.dens - attributes(bear.conflict.df.scl$livestockOps)[[2]])/attributes(bear.conflict.df.scl$livestockOps)[[3]]

row.crop.dens.scl <- (ground.dens - attributes(bear.conflict.df.scl$rowcropOps)[[2]])/attributes(bear.conflict.df.scl$rowcropOps)[[3]]

ndvi.scl <- (ndvi.r - attributes(bear.conflict.df.scl$ndvi)[[2]])/attributes(bear.conflict.df.scl$ndvi)[[3]]

bhs.scl <- (bhs - attributes(bear.conflict.df.scl$habsuit)[[2]])/attributes(bear.conflict.df.scl$habsuit)[[3]]

gHM.scl <- (ghm.r - attributes(bear.conflict.df.scl$gHM)[[2]])/attributes(bear.conflict.df.scl$gHM)[[3]]

biophys.scl <- (biophys - attributes(bear.conflict.df.scl$connectivity)[[2]])/attributes(bear.conflict.df.scl$connectivity)[[3]]

conflict.scl <- (conflict - attributes(bear.conflict.df.scl$conflictprob)[[2]])/attributes(bear.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2pa.pred <- dist.2.pa.scl * fixed.effects[['dist2pa']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['humandens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['livestockOps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['rowcropOps']]
ndvi.pred <- ndvi.scl * fixed.effects[['ndvi']]
bhs.pred <- bhs.scl * fixed.effects[['habsuit']]
ghm.pred <- gHM.scl * fixed.effects[['gHM']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
# conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

# Add our Rasters: NOTE: animal prod is making this a little wonky bc it is very large -
bear.pred.stack <- c(global.int, ccs.int, dist2pa.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred, ndvi.pred, bhs.pred, ghm.pred, biophys.pred) #, global.int, ccs.int, )

bear.linpred.rst <- sum(bear.pred.stack)
bear.prob.rast <- (exp(bear.linpred.rst))/(1 + exp(bear.linpred.rst))
plot(bear.prob.rast)

# Apply Moving Window to smooth harsh lines -------------------------------
fw <- focalWeight(bear.prob.rast, 1262, 'circle') # 1.3km radius, so an area of 5km^2
bear.prob.smooth <- focal(bear.prob.rast, w=fw, fun="sum",na.rm=T)
plot(bear.prob.smooth)

# Crop to BHW Boundary:
bear.prob.rast.bhw <- mask(bear.prob.rast, bhw.v)
bear.prob.smooth.bhw <- mask(bear.prob.smooth, bhw.v)
plot(bear.prob.rast.bhw)
plot(bear.prob.smooth.bhw)

# Save these:
writeRaster(bear.prob.smooth, "Data/processed/prob_conflict_bear_smoothed.tif", overwrite=TRUE) # We will try this as a resistance input to help with harsh connectivity slow lines
writeRaster(bear.prob.smooth.bhw, "Data/processed/prob_conflict_bear_smooth_bhw.tif", overwrite=TRUE)

writeRaster(bear.prob.rast, "Data/processed/prob_conflict_bear.tif", overwrite=TRUE)
writeRaster(bear.prob.rast.bhw, "Data/processed/prob_conflict_bear_bhw.tif", overwrite=TRUE)
