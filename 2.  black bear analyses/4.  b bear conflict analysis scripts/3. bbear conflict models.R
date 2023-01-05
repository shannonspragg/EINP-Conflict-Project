
# Bear Conflict Regression Models: ----------------------------------------
## Here we prep the bear conflict models


# Load packages: ----------------------------------------------------------
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
library(terra)
library(modelr)
theme_set(bayesplot::theme_default(base_family = "sans"))


# Bring in Data & Prep: ----------------------------------------------------------
bear.conflict <- st_read("data/processed/bbear_confirmed_reports_full_df.shp") %>% 
  st_buffer(., 5000)

# Add General Conflict into Bear Data:
prob.conflict <- rast("data/processed/prob_conflict_all.tif")
gen.prob.conflict <- terra::extract(prob.conflict, vect(bear.conflict), mean, na.rm=TRUE)
bear.conflict$genconflictprob <- gen.prob.conflict[,2]

bear.conflict.df <- bear.conflict %>% 
  st_drop_geometry() %>% 
  dplyr::select(., bears, CCSNAME, dst2p_k, hum_dns, anml_fr, grnd_cr, ndvi, gHM, bhs, frst_s_, genconflictprob)

colnames(bear.conflict.df) <- c("bear_conflict", "CCSNAME.ps", "dist2pa", "humandens", "livestockOps", "rowcropOps", "ndvi", "gHM", "habsuit", "connectivity", "conflictprob")

# Scale Data:
bear.conflict.df.scl <- bear.conflict.df %>% 
  mutate_at(c("dist2pa", "humandens", "livestockOps", "rowcropOps", "ndvi", "gHM", "habsuit", "connectivity", "conflictprob"), scale) 
saveRDS(bear.conflict.df.scl, "data/processed/bear_conf_df_scl.rds")

# Run Bear Conflict Models: -----------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

SEED<-14124869

# Full Model:
bear.full.mod <- stan_glmer(bear_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + (1 | CCSNAME.ps), 
                            data = bear.conflict.df.scl,
                            family = binomial(link = "logit"), # define our binomial glm
                            prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                            iter = 3000, chains=5,
                            seed = SEED)

# Full Model + Quadratic for GenConf:
bear.full.mod.quad <- update(bear.full.mod, formula = bear_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

# Full Model - GenConf:
bear.no.conf <- update(bear.full.mod, formula = bear_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + (1 | CCSNAME.ps), QR=TRUE)

# Intercept Only Model: 
bear.int.only <- update(bear.full.mod, formula = bear_conflict ~ 1 + (1 | CCSNAME.ps) , QR = FALSE)

saveRDS(bear.full.mod.quad, "data/processed/bear_quad_reg.rds")
saveRDS(bear.int.only, "data/processed/bear_int_only.rds")
saveRDS(bear.full.mod, "data/processed/bear_full_mod.rds")
saveRDS(bear.no.conf, "data/processed/bear_no_conf.rds")
