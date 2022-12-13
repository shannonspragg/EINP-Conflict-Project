
# General Conflict Model --------------------------------------------------
## Here we run our general conflict full and null models. 


# Load Packages: ----------------------------------------------------------
library(rstanarm)
library(tidyverse)
library(sf)
options(mc.cores = parallel::detectCores())
library(loo)
library(bayesplot)
library(tidybayes)
library(ggeffects)
library(viridis)
library(patchwork)
library(modelr)
theme_set(bayesplot::theme_default(base_family = "sans"))


# Bring in Data: ----------------------------------------------------------
c.conf.pres.abs <- st_read(here::here("./Data/processed/cougar_pres_abs_df.shp")) %>% st_drop_geometry()


#filter some of the absences
c.pres.abs.filter <- conf.pres.abs %>% 
  dplyr::select(., c(cnflct_, dst2p_k, hum_dns, anml_fr, grnd_cr, ndvi, gHM, agn_bph, gfcl_bp, frst_s_, CCSNAME)) 

colnames(c.pres.abs.filter) <- c("conflict_presence_ps", "dist.2.pa.ps", "human.dens.ps", "edge.hab.ps", "ground.crop.dens.ps", "ndvi.ps", 
                               "gHM.ps", "agno.biophys.ps", "gen.focal.biophys.ps", "forest.sp.biophys.ps",  "CCSNAME.ps")


# Scale Data for Analysis: ------------------------------------------------
## Here we scale the predictors for analysis
c.pres.abs.scl <- c.pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "human.dens.ps", "edge.hab.ps", "ground.crop.dens.ps", "ndvi.ps", 
              "gHM.ps", "agno.biophys.ps", "gen.focal.biophys.ps", "forest.sp.biophys.ps"), scale)
saveRDS(c.pres.abs.scl, "data/processed/wolf_pres_abs_scl.rds")

# Run General Conflict Models: --------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
SEED<-14124869

# Full Model:
c.post.pa.full <- stan_glmer(conflict_presence_ps ~ dist.2.pa.ps + human.dens.ps + edge.hab.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducibility
# Partial Model:
c.post.pa.partial <- stan_glmer(conflict_presence_ps ~ edge.hab.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducibility

# Full Model + Quadratic for Pop Dens:
c.post.pa.full.quad <- update(post.pa.full, formula = conflict_presence_ps ~ I(human.dens.ps^2) + dist.2.pa.ps + edge.hab.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps + (1 | CCSNAME.ps), QR = TRUE)

# Intercept-only model:
c.post.int.only <-  update(post.pa.full, formula = conflict_presence_ps ~ 1+ (1 | CCSNAME.ps), QR = FALSE)

saveRDS(c.post.pa.full, "data/processed/wolf_post_pa_full.rds")
saveRDS(c.post.pa.partial, "data/processed/wolf_post_pa_partial.rds")
saveRDS(c.post.pa.full.quad, "data/processed/wolf_post_pa_full_quad.rds")
saveRDS(c.post.int.only, "data/processed/wolf_post_int_only.rds")
