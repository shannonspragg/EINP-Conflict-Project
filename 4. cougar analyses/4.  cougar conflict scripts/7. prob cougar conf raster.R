
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
#cougar.full.mod.quad <- readRDS("Data/processed/cougar_quad_reg.rds")
# cougar.int.only <- readRDS("Data/processed/cougar_int_only.rds")
 cougar.full.mod <- readRDS("Data/processed/cougar_full_mod.rds")
#cougar.no.conf <- readRDS("Data/processed/cougar_no_conf.rds")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(cougar.full.mod)
var.int <- ranef(cougar.no.conf)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/AB_CCS.shp")
ccs.reproj <- st_transform(ccs.sf, st_crs(bhw))
ccs.sf.join <- ccs.reproj %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Lesser Slave River No. 124",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_km_bhb.tif") 
hum.dens.r <- rast("data/processed/human_dens_bhb.tif")
animal.dens <- rast("data/processed/animal_production_density_raster.tif")
ground.dens <- rast("data/processed/ground_crop_density_raster.tif")
ungulate.r <- rast("data/processed/bhb_ungulate_density.tif")
ghm.r <- rast("data/processed/bhw_ghm.tif")
whs <- rast("data/processed/cougar_habitat_suitability.tif")
biophys <- rast("data/processed/cougar_biophys_cum_currmap.tif")
cougar.inc.r <- rast("data/processed/cougar_increase_bhw.tif")
road.dens.r <- rast("data/processed/bhb_road_density_250m.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif") # Only need this if using model with conflict

cougar.conf.pred.stack <- c(dist.2.pa, hum.dens.r, animal.dens, ground.dens, ungulate.r, ghm.r, whs, road.dens.r , cougar.inc.r ,biophys, conflict)


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
dist.2.pa.scl <- (dist.2.pa - attributes(cougar.conflict.df.scl$dist2pa)[[2]])/attributes(cougar.conflict.df.scl$dist2pa)[[3]]

pop.dens.scl <- (hum.dens.r - attributes(cougar.conflict.df.scl$humandens)[[2]])/attributes(cougar.conflict.df.scl$humandens)[[3]]

animal.dens.scl <- (animal.dens - attributes(cougar.conflict.df.scl$livestockOps)[[2]])/attributes(cougar.conflict.df.scl$livestockOps)[[3]]

row.crop.dens.scl <- (ground.dens - attributes(cougar.conflict.df.scl$rowcropOps)[[2]])/attributes(cougar.conflict.df.scl$rowcropOps)[[3]]

ungulate.scl <- (ndvi.r - attributes(cougar.conflict.df.scl$ungulate_dens)[[2]])/attributes(cougar.conflict.df.scl$ungulate_dens)[[3]]

bhs.scl <- (bhs - attributes(cougar.conflict.df.scl$habsuit)[[2]])/attributes(cougar.conflict.df.scl$habsuit)[[3]]

gHM.scl <- (ghm.r - attributes(cougar.conflict.df.scl$gHM)[[2]])/attributes(cougar.conflict.df.scl$gHM)[[3]]

biophys.scl <- (biophys - attributes(cougar.conflict.df.scl$connectivity)[[2]])/attributes(cougar.conflict.df.scl$connectivity)[[3]]

road.dens.scl <- (ghm.r - attributes(cougar.conflict.df.scl$roaddens)[[2]])/attributes(cougar.conflict.df.scl$roaddens)[[3]]

cougar.inc.scl <- (ghm.r - attributes(cougar.conflict.df.scl$cougarinc)[[2]])/attributes(cougar.conflict.df.scl$cougarinc)[[3]]

conflict.scl <- (conflict - attributes(cougar.conflict.df.scl$conflictprob)[[2]])/attributes(cougar.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2pa.pred <- dist.2.pa.scl * fixed.effects[['dist2pa']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['humandens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['livestockOps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['rowcropOps']]
ungulate.pred <- ungulate.scl * fixed.effects[['ungulate_dens']]
bhs.pred <- bhs.scl * fixed.effects[['habsuit']]
ghm.pred <- gHM.scl * fixed.effects[['gHM']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
road.dens.pred <- road.dens.scl * fixed.effects[['roaddens']]
cougarinc.pred <- cougar.inc.scl * fixed.effects[['cougarinc']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
# conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

# Add our Rasters:
cougar.pred.stack <- c(dist2pa.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred, ungulate.pred, bhs.pred, ghm.pred, biophys.pred, road.dens.pred, cougarinc.pred, conflict.pred) 

cougar.linpred.rst <- sum(cougar.pred.stack)
cougar.prob.rast <- (exp(cougar.linpred.rst))/(1 + exp(cougar.linpred.rst))

# Crop to BHW Boundary:
cougar.prob.rast.bhw <- mask(cougar.prob.rast, bhw.v)

writeRaster(cougar.prob.rast, "Data/processed/prob_conflict_cougar.tif", overwrite=TRUE)
writeRaster(cougar.prob.rast.bhw, "Data/processed/prob_conflict_cougar_bhw.tif", overwrite=TRUE)
