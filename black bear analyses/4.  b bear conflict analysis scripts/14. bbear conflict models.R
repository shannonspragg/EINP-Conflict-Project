
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
bear.conflict <- st_read("Data/processed/warp_final.shp") %>% 
  st_buffer(., 5000)

# Add General Conflict into Bear Data:
prob.conflict <- rast("Data/processed/prob_conflict_all.tif")
bear.prob.conflict <- terra::extract(prob.conflict, vect(bear.conflict), mean, na.rm=TRUE)
bear.conflict$conflictprob <- bear.prob.conflict[,2]

bear.conflict.df <- bear.conflict %>% 
  st_drop_geometry() %>% 
  select(., bears, CCSNAME, dst__PA, dst__GP, Anml_Fr, Grnd_Cr, Biophys, GrizzInc, BHS, Human_Dens,conflictprob)

colnames(bear.conflict.df) <- c("conflict", "CCSNAME.ps", "dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "conflictprob")

# Scale Data:
bear.conflict.df.scl <- bear.conflict.df %>% 
  mutate_at(c("dist2pa", "dist2grizz", "livestockOps", "rowcropOps", "connectivity", "grizzinc", "habsuit", "humandens", "conflictprob"), scale) 

# Run Bear Conflict Models: -----------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

SEED<-14124869

# Full Model:
bear.full.mod <- stan_glmer(conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps + connectivity + grizzinc + habsuit + humandens + conflictprob + (1 | CCSNAME.ps), 
                            data = bear.conflict.df.scl,
                            family = binomial(link = "logit"), # define our binomial glm
                            prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                            iter = 3000, chains=5,
                            seed = SEED)

# Full Model + Quadratic for GenConf:
bear.full.mod.quad <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps  + connectivity + grizzinc + habsuit + humandens + conflictprob + I(conflictprob^2) + (1 | CCSNAME.ps), QR=TRUE)

# Full Model - GenConf:
bear.no.conf <- update(bear.full.mod, formula = conflict ~ dist2pa + dist2grizz + livestockOps + rowcropOps  + connectivity + grizzinc + habsuit + humandens + (1 | CCSNAME.ps), QR=TRUE)

# Intercept Only Model: 
bear.int.only <- update(bear.full.mod, formula = conflict ~ 1 + (1 | CCSNAME.ps) , QR = FALSE)

saveRDS(bear.full.mod.quad, "Data/processed/bear_quad_reg.rds")
saveRDS(bear.int.only, "Data/processed/bear_int_only.rds")
saveRDS(bear.full.mod, "Data/processed/bear_full.rds")
saveRDS(bear.no.conf, "Data/processed/bear_no_conf.rds")
