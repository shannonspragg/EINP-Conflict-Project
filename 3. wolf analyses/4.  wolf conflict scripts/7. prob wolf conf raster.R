
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
wolf.full.mod.quad <- readRDS("Data/processed/wolf_quad_reg.rds")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(wolf.full.mod.quad)
var.int <- ranef(wolf.no.conf)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/AB_CCS.shp")
ccs.reproj <- st_transform(ccs.sf, st_crs(bhw))
ccs.sf.join <- ccs.reproj %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Lac la Biche County",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_km_bhb.tif") 
hum.dens.r <- rast("data/processed/human_dens_bhb.tif")
animal.dens <- rast("data/processed/animal_production_density_raster.tif")
ground.dens <- rast("data/processed/ground_crop_density_raster.tif")
ungulate.r <- rast("data/processed/total_ungulate_density.tif")
ghm.r <- rast("data/processed/bhw_ghm.tif")
whs <- rast("data/processed/wolf_habitat_suitability.tif")
biophys <- rast("data/processed/wolf_biophys_cum_currmap.tif")
#wolf.inc.r <- rast("data/processed/wolf_increase_bhw.tif")
road.dens.r <- rast("data/processed/bhb_road_density_250m.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif") # Only need this if using model with conflict
ccs.int <- rast("data/processed/varying_intercept_ccs.tif")

wolf.conf.pred.stack <- c(dist.2.pa, hum.dens.r, animal.dens, ground.dens, ungulate.r, ghm.r, whs, road.dens.r ,biophys, conflict) #, wolf.inc.r 

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
dist.2.pa.scl <- (dist.2.pa - attributes(wolf.conflict.df.scl$dist2pa)[[2]])/attributes(wolf.conflict.df.scl$dist2pa)[[3]]

pop.dens.scl <- (hum.dens.r - attributes(wolf.conflict.df.scl$humandens)[[2]])/attributes(wolf.conflict.df.scl$humandens)[[3]]

animal.dens.scl <- (animal.dens - attributes(wolf.conflict.df.scl$livestockOps)[[2]])/attributes(wolf.conflict.df.scl$livestockOps)[[3]]

row.crop.dens.scl <- (ground.dens - attributes(wolf.conflict.df.scl$rowcropOps)[[2]])/attributes(wolf.conflict.df.scl$rowcropOps)[[3]]

ungulate.scl <- (ungulate.r - attributes(wolf.conflict.df.scl$ungulatedens)[[2]])/attributes(wolf.conflict.df.scl$ungulatedens)[[3]]

whs.scl <- (whs - attributes(wolf.conflict.df.scl$habsuit)[[2]])/attributes(wolf.conflict.df.scl$habsuit)[[3]]

gHM.scl <- (ghm.r - attributes(wolf.conflict.df.scl$gHM)[[2]])/attributes(wolf.conflict.df.scl$gHM)[[3]]

biophys.scl <- (biophys - attributes(wolf.conflict.df.scl$connectivity)[[2]])/attributes(wolf.conflict.df.scl$connectivity)[[3]]

road.dens.scl <- (ghm.r - attributes(wolf.conflict.df.scl$roaddens)[[2]])/attributes(wolf.conflict.df.scl$roaddens)[[3]]

#wolf.inc.scl <- (ghm.r - attributes(wolf.conflict.df.scl$wolfinc)[[2]])/attributes(wolf.conflict.df.scl$wolfinc)[[3]]

conflict.scl <- (conflict - attributes(wolf.conflict.df.scl$conflictprob)[[2]])/attributes(wolf.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2pa.pred <- dist.2.pa.scl * fixed.effects[['dist2pa']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['humandens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['livestockOps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['rowcropOps']]
ungulate.pred <- ungulate.scl * fixed.effects[['ungulatedens']]
whs.pred <- whs.scl * fixed.effects[['habsuit']]
ghm.pred <- gHM.scl * fixed.effects[['gHM']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
road.dens.pred <- road.dens.scl * fixed.effects[['roaddens']]
#wolfinc.pred <- wolf.inc.scl * fixed.effects[['wolfinc']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

# Add our Rasters: NOTE: including global and ccs intercept (which is very high), messes this up
wolf.pred.stack <- c(global.int, ccs.int, dist2pa.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred, ungulate.pred, whs.pred, ghm.pred, biophys.pred, road.dens.pred, conflict.pred) # wolfinc.pred,

wolf.linpred.rst <- sum(wolf.pred.stack)
wolf.prob.rast <- (exp(wolf.linpred.rst))/(1 + exp(wolf.linpred.rst))
plot(wolf.prob.rast)

# Crop to BHW Boundary:
wolf.prob.rast.bhw <- mask(wolf.prob.rast, bhw.v)
plot(wolf.prob.rast.bhw)

writeRaster(wolf.prob.rast, "Data/processed/prob_conflict_wolf.tif", overwrite=TRUE)
writeRaster(wolf.prob.rast.bhw, "Data/processed/prob_conflict_wolf_bhw.tif", overwrite=TRUE)
