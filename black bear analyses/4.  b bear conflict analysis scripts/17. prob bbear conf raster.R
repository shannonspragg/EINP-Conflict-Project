
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
bear.full.mod.quad <- readRDS("Data/processed/bear_quad_reg.rds")
bear.int.only <- readRDS("Data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("Data/processed/bear_full.rds")
bear.no.conf <- readRDS("Data/processed/bear_no_conf.rds")

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(bear.full.mod.quad)
var.int <- ranef(bear.full.mod.quad)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")


ccs.sf <- st_read("Data/processed/SOI_CCS_10km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join$`(Intercept)`[is.na(ccs.sf.join$`(Intercept)`)] <- 0

#load predictor rasters
dist.2.pa <- rast("Data/processed/dist2pa_SOI_10km.tif") 
pop.dens <- rast("Data/processed/human_dens_SOI_10km.tif")
animal.dens <- rast("Data/processed/animal_production_density_cropped.tif")
rowcrop.dens <- rast("Data/processed/ground_crop_density_cropped.tif")
dist.2.grizz <- rast("Data/processed/dist2grizz_pop_raster.tif")
bhs <- rast("Data/processed/bhs_SOI_10km.tif")
grizinc <- rast("Data/processed/grizz_inc_SOI_10km.tif")
biophys <- rast("Data/processed/biophys_SOI_10km.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif")


pop.d.crop <- crop(pop.dens, animal.dens)
pop.dens <- mask(pop.d.crop, animal.dens)
bhs <- crop(bhs, animal.dens)
grizinc <- crop(grizinc, animal.dens)
writeRaster(bhs, "Data/processed/bhs_SOI_10km.tif", overwrite=TRUE)
writeRaster(grizinc, "Data/processed/grizinc_SOI_10km.tif")

#Create global intercept raster
global.int <- dist.2.pa
global.int[!is.na(global.int)] <- fixed.effects[[1]]

#create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.pa, field='(Intercept)')

#scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(bear.conflict.df.scl$dist2pa)[[2]])/attributes(bear.conflict.df.scl$dist2pa)[[3]]

pop.dens.scl <- (pop.dens - attributes(bear.conflict.df.scl$humandens)[[2]])/attributes(bear.conflict.df.scl$humandens)[[3]]

animal.dens.scl <- (animal.dens - attributes(bear.conflict.df.scl$livestockOps)[[2]])/attributes(bear.conflict.df.scl$livestockOps)[[3]]

row.crop.dens.scl <- (rowcrop.dens - attributes(bear.conflict.df.scl$rowcropOps)[[2]])/attributes(bear.conflict.df.scl$rowcropOps)[[3]]

grizz.dist.scl <- (dist.2.grizz - attributes(bear.conflict.df.scl$dist2grizz)[[2]])/attributes(bear.conflict.df.scl$dist2grizz)[[3]]

bhs.scl <- (bhs - attributes(bear.conflict.df.scl$habsuit)[[2]])/attributes(bear.conflict.df.scl$habsuit)[[3]]

grizzinc.scl <- (grizinc - attributes(bear.conflict.df.scl$grizzinc)[[2]])/attributes(bear.conflict.df.scl$grizzinc)[[3]]

biophys.scl <- (biophys - attributes(bear.conflict.df.scl$connectivity)[[2]])/attributes(bear.conflict.df.scl$connectivity)[[3]]

conflict.scl <- (conflict - attributes(bear.conflict.df.scl$conflictprob)[[2]])/attributes(bear.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2pa.pred <- dist.2.pa.scl * fixed.effects[['dist2pa']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['humandens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['livestockOps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['rowcropOps']]
grizz.dist.pred <- grizz.dist.scl * fixed.effects[['dist2grizz']]
bhs.pred <- bhs.scl * fixed.effects[['habsuit']]
grizzinc.pred <- grizzinc.scl * fixed.effects[['grizzinc']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

# Add our Rasters:
pred.stack <- c(dist2pa.pred, pop.dens.pred, animal.dens.pred,rowcrop.dens.pred, grizz.dist.pred, bhs.pred, grizzinc.pred, biophys.pred, conflict.pred, conflict.quad.prd)

linpred.rst <- sum(pred.stack)
prob.rast <- (exp(linpred.rst))/(1 + exp(linpred.rst))
writeRaster(prob.rast, "Data/processed/prob_conflict_bear.tif")