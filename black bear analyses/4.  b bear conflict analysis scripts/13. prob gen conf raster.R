
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
post.pa.full <- readRDS("Data/processed/post_pa_full.rds")
post.pa.full.quad <- readRDS("Data/processed/post_pa_full_quad.rds")
post.int.only <- readRDS("Data/processed/post_int_only.rds")

# Generating raster predictions -------------------------------------------
fixed.effects <- fixef(post.pa.full)
var.int <- ranef(post.pa.full)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/SOI_CCS_10km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Powell River A",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

# Load predictor rasters:
dist.2.pa <- rast("Data/processed/dist2pa_SOI_10km.tif") 
dist.2.met <- rast("Data/processed/dist2metro_SOI_10km.tif")
pop.dens <- rast("Data/processed/human_dens_SOI_10km.tif") # need to match extent
animal.dens <- rast("Data/processed/animal_production_density_cropped.tif")
rowcrop.dens <- rast("Data/processed/ground_crop_density_cropped.tif")

pop.d.crop <- crop(pop.dens, animal.dens)
pop.dens <- mask(pop.d.crop, animal.dens)
writeRaster(pop.dens, "Data/processed/pop_dens_SOI_10km.tif")

# Create global intercept raster
global.int <- dist.2.met
global.int[!is.na(global.int)] <- fixed.effects[[1]] 

# Create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.met, field='(Intercept)')

# Scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(pres.abs.scl$dist.2.pa.ps)[[2]])/attributes(pres.abs.scl$dist.2.pa.ps)[[3]]
dist.2.met.scl <- (dist.2.met - attributes(pres.abs.scl$dist.2.met.ps)[[2]])/attributes(pres.abs.scl$dist.2.met.ps)[[3]]
pop.dens.scl <- (pop.dens - attributes(pres.abs.scl$pop.dens)[[2]])/attributes(pres.abs.scl$pop.dens)[[3]]
animal.dens.scl <- (animal.dens - attributes(pres.abs.scl$animal.farm.dens.ps)[[2]])/attributes(pres.abs.scl$animal.farm.dens.ps)[[3]]
row.crop.dens.scl <- (rowcrop.dens - attributes(pres.abs.scl$ground.crop.dens.ps)[[2]])/attributes(pres.abs.scl$ground.crop.dens.ps)[[3]]

dist.2.pa.pred <- dist.2.pa.scl * fixed.effects[['dist.2.pa.ps']]
dist.2.met.pred <- dist.2.met.scl * fixed.effects[['dist.2.met.ps']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['pop.dens']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['animal.farm.dens.ps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['ground.crop.dens.ps']]

# Combine our Rasters:
pred.stack <- c(global.int, ccs.int, dist.2.pa.pred, dist.2.met.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred)
linpred.rast <- sum(pred.stack)
prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))

# Save these:
writeRaster(prob.rast, "Data/processed/prob_conflict_all.tif")
# saveRDS(post.int.only, "Data/processed/int_only_reg.rds")
# saveRDS(post.pa.full, "Data/processed/full_mod_reg.rds")
# saveRDS(post.pa.full.quad, "Data/processed/full_mod_quad.rds")
