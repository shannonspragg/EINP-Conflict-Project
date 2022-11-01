
# Run LOOIC tests and ROC curves ------------------------------------------
## We test the data simulations and plot results 

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
 
# Read data in:
bear.full.mod.quad <- readRDS("data/processed/bear_quad_reg.rds")
bear.int.only <- readRDS("data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("data/processed/bear_full_mod.rds")
bear.no.conf <- readRDS("data/processed/bear_no_conf.rds")
bear.conflict.df.scl <- readRDS("data/processed/bear_conf_df_scl.rds")

# Model Comparison: -------------------------------------------------------
loo1b <- loo(bear.full.mod, save_psis = TRUE, k_threshold = 0.7) # there is one point that has a poor loo value
loo2b <- loo(bear.full.mod.quad, save_psis = TRUE, k_threshold = 0.7)
loo3b <- loo(bear.no.conf, save_psis = TRUE, k_threshold = 0.7)
loo0b <- loo(bear.int.only, save_psis = TRUE, k_threshold = 0.7)

saveRDS(loo1b, "Data/processed/bear_full_loo.rds")
saveRDS(loo2b, "Data/processed/bear_full_quad_loo.rds")
saveRDS(loo3b, "Data/processed/bear_no_conf_loo.rds")
saveRDS(loo0b, "Data/processed/bear_int_only_loo.rds")

# Bring back in later:
# loo1b <- readRDS("Data/processed/bear_full_loo.rds")
# loo2b <- readRDS("Data/processed/bear_full_quad_loo.rds")
# loo3b <- readRDS("Data/processed/bear_no_conf_loo.rds")
# loo0b <- readRDS("Data/processed/bear_int_only_loo.rds")

bear.loo.comp <- loo_compare(loo1b, loo2b, loo3b, loo0b)
saveRDS(bear.loo.comp, "data/processed/bear_loo_comp.rds")

plot(
  loo2b,
  diagnostic = c("k", "n_eff"),
  label_points = TRUE,
  main = "PSIS diagnostic plot"
)

preds3 <- posterior_epred(bear.no.conf)
preds2 <- posterior_epred(bear.full.mod.quad)
preds1 <- posterior_epred(bear.full.mod)
preds0 <- posterior_epred(bear.int.only)
pred3 <- colMeans(preds3)
pred2 <- colMeans(preds2)
pred1 <- colMeans(preds1)
pred0 <- colMeans(preds0)
pr3 <- as.integer(pred3 >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr1 <- as.integer(pred1 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)

round(mean(xor(pr3,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.84
round(mean(xor(pr2,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.84
round(mean(xor(pr1,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #0.84
round(mean(xor(pr0,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #0.81

ploo1 <- E_loo(preds1, loo1b$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod))$value

ploo2<-E_loo(preds2, loo2b$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3b$psis_object, type="mean", log_ratios = -log_lik(bear.no.conf))$value

ploo0<-E_loo(preds0, loo0b$psis_object, type="mean", log_ratios = -log_lik(bear.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.84
round(mean(xor(ploo2>0.5,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.84
round(mean(xor(ploo3>0.5,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.84
round(mean(xor(ploo0>0.5,as.integer(bear.conflict.df.scl$bear_conflict==0))),2) #.81

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(bear.conflict.df.scl$bear_conflict, bear.full.mod.quad$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(bear.conflict.df.scl$bear_conflict, bear.full.mod$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)

pROC::plot.roc(bear.conflict.df.scl$bear_conflict, bear.no.conf$fitted.values, percent=TRUE, col='#B090D0', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
pROC::plot.roc(bear.conflict.df.scl$bear_conflict, bear.int.only$fitted.values, percent=TRUE, col='#FFAA00', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=35)

legend("bottomright", legend=c("Quad Model", "Full Model",  "No Conflict Model", "Varying Intercept-only Model"),
       col=c("#377eb8", "#4daf4a", "#B090D0", "#FFAA00"), lwd = 4)

# We will use full model with quadratic term as their predictive accuracy is similar and the predictor estimates seem more stable -- note that model without
# general conflict is not really any different in AUC
