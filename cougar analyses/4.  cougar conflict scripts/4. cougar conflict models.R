
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
cougar.conflict <- st_read("data/processed/cougar_confirmed_reports_full_df.shp") %>% 
  st_buffer(., 5000)

# Add General Conflict into Bear Data:
prob.conflict <- rast("data/processed/prob_conflict_all.tif")
gen.prob.conflict <- terra::extract(prob.conflict, vect(cougar.conflict), mean, na.rm=TRUE)
cougar.conflict$genconflictprob <- gen.prob.conflict[,2]

cougar.conflict.df <- cougar.conflict %>% 
  st_drop_geometry() %>% 
  dplyr::select(., bears, CCSNAME, dst2p_k, hum_dns, anml_fr, grnd_cr, ung_dens, gHM, whs, biophys, cougar_inc, road_dens, genconflictprob)

colnames(cougar.conflict.df) <- c("cougar_conflict", "CCSNAME.ps", "dist2pa", "humandens", "livestockOps", "rowcropOps", "ungulatedens", "gHM", "habsuit", "connectivity", "cougarincrease", "roaddens", "conflictprob")

# Scale Data:
cougar.conflict.df.scl <- cougar.conflict.df %>% 
  mutate_at(c("dist2pa", "humandens", "livestockOps", "rowcropOps", "ungulate_dens", "gHM", "habsuit", "connectivity", "cougarincrease", "roaddens", "conflictprob"), scale) 
saveRDS(cougar.conflict.df.scl, "data/processed/cougar_conf_df_scl.rds")

# Run Bear Conflict Models: -----------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

SEED<-14124869

# Full Model:
cougar.full.mod <- stan_glmer(cougar_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + (1 | CCSNAME.ps), 
                            data = cougar.conflict.df.scl,
                            family = binomial(link = "logit"), # define our binomial glm
                            prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                            iter = 3000, chains=5,
                            seed = SEED)

# Full Model + Quadratic for GenConf:
cougar.full.mod.quad <- update(cougar.full.mod, formula = cougar_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

# Full Model - GenConf:
cougar.no.conf <- update(cougar.full.mod, formula = cougar_conflict ~ dist2pa + humandens + livestockOps + rowcropOps + ndvi + gHM + habsuit + connectivity + (1 | CCSNAME.ps), QR=TRUE)

# Intercept Only Model: 
cougar.int.only <- update(cougar.full.mod, formula = cougar_conflict ~ 1 + (1 | CCSNAME.ps) , QR = FALSE)

saveRDS(cougar.full.mod.quad, "data/processed/cougar_quad_reg.rds")
saveRDS(cougar.int.only, "data/processed/cougar_int_only.rds")
saveRDS(cougar.full.mod, "data/processed/cougar_full_mod.rds")
saveRDS(cougar.no.conf, "data/processed/cougar_no_conf.rds")
