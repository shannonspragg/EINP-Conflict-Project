
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
wolf.conflict <- st_read("data/processed/wolf_confirmed_reports_full_df.shp") %>% 
  st_buffer(., 5000)

# Add General Conflict into Bear Data:
prob.conflict <- rast("data/processed/prob_conflict_all.tif")
gen.prob.conflict <- terra::extract(prob.conflict, vect(wolf.conflict), mean, na.rm=TRUE)
wolf.conflict$genconflictprob <- gen.prob.conflict[,2]

wolf.conflict.df <- wolf.conflict %>% 
  st_drop_geometry() %>% 
  dplyr::select(., bears, CCSNAME, dst2p_k, hum_dns, anml_fr, grnd_cr, ung_dens, gHM, whs, biophys, wolf_inc, road_dens, genconflictprob)

colnames(wolf.conflict.df) <- c("wolf_conflict", "CCSNAME.ps", "dist2pa", "humandens", "livestockOps", "rowcropOps", "ungulatedens", "gHM", "habsuit", "connectivity", "wolfincrease", "roaddens", "conflictprob")

# Scale Data:
wolf.conflict.df.scl <- wolf.conflict.df %>% 
  mutate_at(c("dist2pa", "humandens", "livestockOps", "rowcropOps", "ungulate_dens", "gHM", "habsuit", "connectivity", "wolfincrease", "roaddens", "conflictprob"), scale) 
saveRDS(wolf.conflict.df.scl, "data/processed/wolf_conf_df_scl.rds")

# Run Bear Conflict Models: -----------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

SEED<-14124869

# Full Model:
wolf.full.mod <- stan_glmer(wolf_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + (1 | CCSNAME.ps), 
                            data = wolf.conflict.df.scl,
                            family = binomial(link = "logit"), # define our binomial glm
                            prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                            iter = 3000, chains=5,
                            seed = SEED)

# Full Model + Quadratic for GenConf:
wolf.full.mod.quad <- update(wolf.full.mod, formula = wolf_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

# Full Model - GenConf:
wolf.no.conf <- update(wolf.full.mod, formula = wolf_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + (1 | CCSNAME.ps), QR=TRUE)

# Intercept Only Model: 
wolf.int.only <- update(wolf.full.mod, formula = wolf_conflict ~ 1 + (1 | CCSNAME.ps) , QR = FALSE)

saveRDS(wolf.full.mod.quad, "data/processed/wolf_quad_reg.rds")
saveRDS(wolf.int.only, "data/processed/wolf_int_only.rds")
saveRDS(wolf.full.mod, "data/processed/wolf_full_mod.rds")
saveRDS(wolf.no.conf, "data/processed/wolf_no_conf.rds")
