
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
bear.full.mod.quad <- readRDS("Data/processed/bear_quad_reg.rds")
bear.int.only <- readRDS("Data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("Data/processed/bear_full.rds")
bear.no.conf <- readRDS("Data/processed/bear_no_conf.rds")

# Model Comparison: -------------------------------------------------------
loo1 <- loo(bear.full.mod, save_psis = TRUE)
loo2 <- loo(bear.full.mod.quad, save_psis = TRUE)
loo3 <- loo(bear.no.conf, save_psis = TRUE)
loo0 <- loo(bear.int.only, save_psis = TRUE)

saveRDS(loo1, "Data/processed/bear_full_loo.rds")
saveRDS(loo2, "Data/processed/bear_full_quad_loo.rds")
saveRDS(loo3, "Data/processed/bear_no_conf_loo.rds")
saveRDS(loo0, "Data/processed/bear_int_only_loo.rds")

# Bring back in later:
# loo1 <- readRDS("Data/processed/bear_full_loo.rds")
# loo2 <- readRDS("Data/processed/bear_full_quad_loo.rds")
# loo3 <- readRDS("Data/processed/bear_no_conf_loo.rds")
# loo0 <- readRDS("Data/processed/bear_int_only_loo.rds")

bear.loo.comp <- loo_compare(bloo1, bloo2, bloo3, bloo0)
saveRDS(bear.loo.comp, "Data/processed/bear_loo_comp.rds")

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

round(mean(xor(pr3,as.integer(bear.conflict.df.scl$conflict==0))),2) #.68 
round(mean(xor(pr2,as.integer(bear.conflict.df.scl$conflict==0))),2) #.68
round(mean(xor(pr1,as.integer(bear.conflict.df.scl$conflict==0))),2) #0.68
round(mean(xor(pr0,as.integer(bear.conflict.df.scl$conflict==0))),2) #0.68

ploo1 <- E_loo(preds1, loo1$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod))$value

ploo2<-E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(bear.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3$psis_object, type="mean", log_ratios = -log_lik(bear.no.conf))$value

ploo0<-E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(bear.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67
round(mean(xor(ploo2>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67
round(mean(xor(ploo3>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67
round(mean(xor(ploo0>0.5,as.integer(bear.conflict.df.scl$conflict==0))),2) #.67

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(bear.conflict.df.scl$conflict, bear.full.mod.quad$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(bear.conflict.df.scl$conflict, bear.full.mod.quad$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)

legend("bottomright", legend=c("Full Model", "Varying Intercept Model"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
par(opar)

# We will use full model without quadratic term as their predictive accuracy is similar and the predictor estimates seem more stable
