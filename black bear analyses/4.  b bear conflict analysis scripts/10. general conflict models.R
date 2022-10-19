
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
warp.pres.abs <- st_read(here::here("./Data/processed/pres_abs_final.shp")) %>% st_drop_geometry()

#filter some of the absences
pres.abs.filter <- warp.pres.abs %>%
  filter(anyCnfl == 0) %>%
  slice_sample(n = 9000) %>%
  bind_rows(warp.pres.abs %>% filter(anyCnfl != 0)) %>% 
  dplyr::select(., c(anyCnfl, dst__PA, dst_t_M, Anml_Fr, Grnd_Cr, CCSNAME, Human_Dens)) 

colnames(pres.abs.filter) <- c("conflict_presence_ps", "dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps", "CCSNAME.ps", "pop.dens")


# Scale Data for Analysis: ------------------------------------------------
## Here we scale the predictors for analysis
pres.abs.scl <- pres.abs.filter %>% 
  mutate_at(c("dist.2.pa.ps", "dist.2.met.ps", "animal.farm.dens.ps", "ground.crop.dens.ps",  "pop.dens"), scale)


# Run General Conflict Models: --------------------------------------------
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
SEED<-14124869
# Full Model:
post.pa.full <- stan_glmer(conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + pop.dens + (1 | CCSNAME.ps), 
                           data = pres.abs.scl,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           iter = 3000, chains=5,
                           seed = SEED) # we add seed for reproducibility

# Full Model + Quadratic for Pop Dens:
post.pa.full.quad <- update(post.pa.full, formula = conflict_presence_ps ~ dist.2.pa.ps + dist.2.met.ps + animal.farm.dens.ps + ground.crop.dens.ps + pop.dens + I(pop.dens^2) + (1 | CCSNAME.ps), QR = TRUE)

# Intercept-only model:
post.int.only <-  update(post.pa.full, formula = conflict_presence_ps ~ 1+ (1 | CCSNAME.ps), QR = FALSE)

saveRDS(post.pa.full, "Data/processed/post_pa_full.rds")
saveRDS(post.pa.full.quad, "Data/processed/post_pa_full_quad.rds")
saveRDS(post.int.only, "Data/processed/post_int_only.rds")
