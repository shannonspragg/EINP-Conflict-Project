
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
post.pa.full <- readRDS("Data/processed/post_pa_full.rds")
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)

# Generating raster predictions -------------------------------------------
fixed.effects <- fixef(post.pa.full)
var.int <- ranef(post.pa.full)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/bhw_CCS_50km.shp")
ccs.sf.join <- ccs.sf %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Lesser Slave River No. 124",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

# Load predictor rasters:
dist.2.pa <- rast("Data/processed/dist2pa_km_bhb.tif") 
hum.dens.r <- rast("data/processed/human_dens_bhb.tif")
animal.dens <- rast("data/processed/animal_production_density_raster.tif")
ground.dens <- rast("data/processed/ground_crop_density_raster.tif")
ndvi.r <- rast("data/processed/bhb_ndvi.tif")
ghm.r <- rast("data/processed/bhw_ghm.tif")
agnostic_bio_cumcurrmap <- rast("data/processed/agnostic_cum_currmap.tif")

# Mask to 50km buffer
# pop.d.crop <- crop(pop.dens, animal.dens)
# pop.dens <- mask(pop.d.crop, animal.dens)
# writeRaster(pop.dens, "Data/processed/pop_dens_SOI_10km.tif")

# Create global intercept raster
global.int <- dist.2.pa
global.int[!is.na(global.int)] <- fixed.effects[[1]] 

# Create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.pa, field='(Intercept)')

# Scale predictor values based on dataframe
dist.2.pa.scl <- (dist.2.pa - attributes(pres.abs.scl$dist.2.pa.ps)[[2]])/attributes(pres.abs.scl$dist.2.pa.ps)[[3]]
ndvi.scl <- (ndvi.r - attributes(pres.abs.scl$ndvi.ps)[[2]])/attributes(pres.abs.scl$ndvi.ps)[[3]]
pop.dens.scl <- (hum.dens.r - attributes(pres.abs.scl$human.dens.ps)[[2]])/attributes(pres.abs.scl$human.dens.ps)[[3]]
animal.dens.scl <- (animal.dens - attributes(pres.abs.scl$animal.farm.dens.ps)[[2]])/attributes(pres.abs.scl$animal.farm.dens.ps)[[3]]
row.crop.dens.scl <- (ground.dens - attributes(pres.abs.scl$ground.crop.dens.ps)[[2]])/attributes(pres.abs.scl$ground.crop.dens.ps)[[3]]
ghm.scl <- (ghm.r - attributes(pres.abs.scl$gHM.ps)[[2]])/attributes(pres.abs.scl$gHM.ps)[[3]]
agno.bio.scl <- (agnostic_bio_cumcurrmap - attributes(pres.abs.scl$agno.biophys.ps)[[2]])/attributes(pres.abs.scl$agno.biophys.ps)[[3]]

dist.2.pa.pred <- dist.2.pa.scl * fixed.effects[['dist.2.pa.ps']]
ndvi.pred <- ndvi.scl * fixed.effects[['ndvi.ps']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['human.dens.ps']]
animal.dens.pred <- animal.dens.scl * fixed.effects[['animal.farm.dens.ps']]
rowcrop.dens.pred <- row.crop.dens.scl * fixed.effects[['ground.crop.dens.ps']]
ghm.pred <- ghm.scl * fixed.effects[['gHM.ps']]
agno.bio.pred <- agno.bio.scl * fixed.effects[['agno.biophys.ps']]

# Combine our Rasters:
pred.stack <- c(global.int, ccs.int, dist.2.pa.pred, ndvi.pred, pop.dens.pred, animal.dens.pred, rowcrop.dens.pred, ghm.pred, agno.bio.pred)
linpred.rast <- sum(pred.stack)
prob.rast <- (exp(linpred.rast))/(1 + exp(linpred.rast))

# Crop to BHW Boundary:
prob.rast.bhw <- mask(prob.rast, bhw.v)

# Save these:
writeRaster(prob.rast, "Data/processed/prob_conflict_all.tif")
writeRaster(prob.rast.bhw, "Data/processed/prob_conflict_all_bhw.tif")
