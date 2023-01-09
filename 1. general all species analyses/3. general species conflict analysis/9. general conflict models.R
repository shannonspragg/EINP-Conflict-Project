
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
conf.pres.abs <- st_read(here::here("./Data/processed/pres_abs_full_df.shp")) %>% st_drop_geometry()


#filter some of the absences
pres.abs.filter <- conf.pres.abs %>% 
  dplyr::select(., c(cnflct_, dst2p_k, hum_dns, anml_fr, grnd_cr, ndvi, gHM, agn_bph, CCSNAME)) 

colnames(pres.abs.filter) <- c("conflict_presence_ps", "dist.2.pa.ps", "human.dens.ps", "animal.farm.dens.ps", "ground.crop.dens.ps", "ndvi.ps", 
                               "gHM.ps", "agno.biophys.ps",  "CCSNAME.ps")


# Scale Data for Analysis: ------------------------------------------------
## Here we scale the predictors for analysis
pres.abs.scl <- pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "human.dens.ps", "animal.farm.dens.ps", "ground.crop.dens.ps", "ndvi.ps", 
              "gHM.ps", "agno.biophys.ps"), scale)

saveRDS(pres.abs.scl, "data/processed/pres_abs_scl.rds")

# Run General Conflict Models: --------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
SEED <- 14124869

# Full Model:
post.pa.full <- stan_glmer(conflict_presence_ps ~ dist.2.pa.ps + human.dens.ps + animal.farm.dens.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps  + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducibility
# Partial Model:
post.pa.partial <- stan_glmer(conflict_presence_ps ~ animal.farm.dens.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps +  (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducibility

# Full Model + Quadratic for Pop Dens:
post.pa.full.quad <- update(post.pa.full, formula = conflict_presence_ps ~ I(human.dens.ps^2) + dist.2.pa.ps + animal.farm.dens.ps + ground.crop.dens.ps + ndvi.ps + gHM.ps + agno.biophys.ps  + (1 | CCSNAME.ps), QR = TRUE)

# Intercept-only model:
post.int.only <-  update(post.pa.full, formula = conflict_presence_ps ~ 1+ (1 | CCSNAME.ps), QR = FALSE)


saveRDS(post.pa.full, "data/processed/post_pa_full.rds")
saveRDS(post.pa.partial, "data/processed/post_pa_partial.rds")
saveRDS(post.pa.full.quad, "data/processed/post_pa_full_quad.rds")
saveRDS(post.int.only, "data/processed/post_int_only.rds")
