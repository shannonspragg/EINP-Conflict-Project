
# Produce Probability of General Conflict raster --------------------------

# Load Packages: ----------------------------------------------------------
library(raster)
library(tidyverse)
library(sf)
library(rstanarm)
library(terra)


# Bring in Data: ----------------------------------------------------------
cougar.full.mod.quad <- readRDS("Data/processed/cougar_quad_reg.rds")
# cougar.int.only <- readRDS("Data/processed/cougar_int_only.rds")
cougar.full.mod <- readRDS("Data/processed/cougar_full_mod.rds")
#cougar.no.conf <- readRDS("Data/processed/cougar_no_conf.rds") # try this and see..
bhw <- st_read("data/original/BHB_Subwatershed_Boundary.shp")
bhw.v <- vect(bhw)

# generate spatial pred ---------------------------------------------------
fixed.effects <- fixef(cougar.full.mod)
var.int <- ranef(cougar.full.mod)$CCSNAME.ps %>% tibble::rownames_to_column(., "CCSNAME")

ccs.sf <- st_read("Data/processed/AB_CCS.shp")
ccs.reproj <- st_transform(ccs.sf, st_crs(bhw))
ccs.sf.join <- ccs.reproj %>% left_join(., var.int)
ccs.sf.join[ccs.sf$CCSNAME == "Lac la Biche County",]$`(Intercept)` <- 0 #no points from this CCS; setting to 0 results in use of global intercept

#load predictor rasters
dist.2.wetland <- rast("Data/processed/dist2drainage_km_bhb.tif") 
hum.dens.r <- rast("data/processed/human_dens_bhb.tif")
edge.hab <- rast("data/processed/forest_edge_habitats.tif")
pipeline.dens <- rast("data/processed/bhb_pipeline_density_250m.tif")
ungulate.r <- rast("data/processed/total_ungulate_density.tif")
ghm.r <- rast("data/processed/bhw_ghm.tif")
chs <- rast("data/processed/cougar_habitat_suitability.tif")
biophys <- rast("data/processed/cougar_biophys_cum_currmap.tif")
#cougar.inc.r <- rast("data/processed/cougar_increase_bhw.tif")
road.dens.r <- rast("data/processed/bhb_road_density_250m.tif")
conflict <- rast("Data/processed/prob_conflict_all.tif") # Only need this if using model with conflict

cougar.conf.pred.stack <- c(dist.2.wetland, hum.dens.r, edge.hab, pipeline.dens, ungulate.r, ghm.r, chs, road.dens.r  ,biophys, conflict) # , cougar.inc.r


#Create global intercept raster
global.int <- dist.2.wetland
global.int[!is.na(global.int)] <- fixed.effects[[1]]

#create var int raster
ccs.vect <- vect(ccs.sf.join)
ccs.int <- rasterize(ccs.vect, dist.2.wetland, field='(Intercept)')
ccs.int <- raster(ccs.int)
ccs.int[is.na(ccs.int[])] <- 0 
ccs.int <- rast(ccs.int)

#scale predictor values based on dataframe
dist.2.wetland.scl <- (dist.2.wetland - attributes(cougar.conflict.df.scl$dist2wetland)[[2]])/attributes(cougar.conflict.df.scl$dist2wetland)[[3]]

pop.dens.scl <- (hum.dens.r - attributes(cougar.conflict.df.scl$humandens)[[2]])/attributes(cougar.conflict.df.scl$humandens)[[3]]

edge.hab.scl <- (edge.hab - attributes(cougar.conflict.df.scl$edge_habitat)[[2]])/attributes(cougar.conflict.df.scl$edge_habitat)[[3]]

pipeline.dens.scl <- (pipeline.dens - attributes(cougar.conflict.df.scl$pipeline_dens)[[2]])/attributes(cougar.conflict.df.scl$pipeline_dens)[[3]]

ungulate.scl <- (ungulate.r - attributes(cougar.conflict.df.scl$ungulatedens)[[2]])/attributes(cougar.conflict.df.scl$ungulatedens)[[3]]

chs.scl <- (chs - attributes(cougar.conflict.df.scl$habsuit)[[2]])/attributes(cougar.conflict.df.scl$habsuit)[[3]]

gHM.scl <- (ghm.r - attributes(cougar.conflict.df.scl$gHM)[[2]])/attributes(cougar.conflict.df.scl$gHM)[[3]]

biophys.scl <- (biophys - attributes(cougar.conflict.df.scl$connectivity)[[2]])/attributes(cougar.conflict.df.scl$connectivity)[[3]]

road.dens.scl <- (road.dens.r - attributes(cougar.conflict.df.scl$road_dens)[[2]])/attributes(cougar.conflict.df.scl$road_dens)[[3]]

#cougar.inc.scl <- (ghm.r - attributes(cougar.conflict.df.scl$cougarinc)[[2]])/attributes(cougar.conflict.df.scl$cougarinc)[[3]]

conflict.scl <- (conflict - attributes(cougar.conflict.df.scl$conflictprob)[[2]])/attributes(cougar.conflict.df.scl$conflictprob)[[3]]

# Generate lin pred
dist2wetland.pred <- dist.2.wetland.scl * fixed.effects[['dist2wetland']]
pop.dens.pred <- pop.dens.scl * fixed.effects[['humandens']]
edge.hab.pred <- edge.hab.scl * fixed.effects[['edge_habitat']]
pipeline.dens.pred <- pipeline.dens.scl * fixed.effects[['pipeline_dens']]
ungulate.pred <- ungulate.scl * fixed.effects[['ungulatedens']]
#chs.pred <- chs.scl * fixed.effects[['habsuit']]
chs.pred <- chs.scl * 4.0 # what if we reduce coefficients by half..
ghm.pred <- gHM.scl * fixed.effects[['gHM']]
biophys.pred <- biophys.scl * fixed.effects[['connectivity']]
road.dens.pred <- road.dens.scl * fixed.effects[['road_dens']]
#cougarinc.pred <- cougar.inc.scl * fixed.effects[['cougarinc']]
conflict.pred <- conflict.scl * fixed.effects[['conflictprob']]
# conflict.quad.prd <- (conflict.scl)^2 * fixed.effects[['I(conflictprob^2)']]

# Add our Rasters: #NOTE : also issues with including intercept -  CHS / connectivity are quite large
cougar.pred.stack <- c(global.int, ccs.int, dist2wetland.pred, pop.dens.pred, edge.hab.pred, pipeline.dens.pred, ungulate.pred, chs.pred, ghm.pred, biophys.pred, road.dens.pred, conflict.pred) # cougarinc.pred, 

# Try without global int: helps, but main issue is with how large CHS is
cougar.pred.stack <- c( ccs.int, dist2wetland.pred, pop.dens.pred, edge.hab.pred, pipeline.dens.pred, ungulate.pred, chs.pred, ghm.pred, biophys.pred, road.dens.pred, conflict.pred) # cougarinc.pred, 

cougar.linpred.rst <- sum(cougar.pred.stack)
cougar.prob.rast <- (exp(cougar.linpred.rst))/(1 + exp(cougar.linpred.rst))
plot(cougar.prob.rast)

# Apply Moving Window to smooth harsh lines -------------------------------
fw <- focalWeight(cougar.prob.rast, 1262, 'circle') # 1.3km radius, so an area of 5km^2
cougar.prob.smooth <- focal(cougar.prob.rast, w=fw, fun="sum",na.rm=T) 
plot(cougar.prob.smooth)

# Crop to BHW Boundary:
cougar.prob.rast.bhw <- mask(cougar.prob.rast, bhw.v)
cougar.prob.rast.smooth.bhw <- mask(cougar.prob.smooth, bhw.v)
plot(cougar.prob.rast.smooth.bhw)

writeRaster(cougar.prob.rast, "Data/processed/prob_conflict_cougar.tif", overwrite=TRUE)
writeRaster(cougar.prob.rast.bhw, "Data/processed/prob_conflict_cougar_bhw.tif", overwrite=TRUE)

writeRaster(cougar.prob.smooth, "Data/processed/prob_conflict_cougar_smoothed.tif", overwrite=TRUE)
writeRaster(cougar.prob.rast.smooth.bhw, "Data/processed/prob_conflict_cougar_smoothed_bhw.tif", overwrite=TRUE)
