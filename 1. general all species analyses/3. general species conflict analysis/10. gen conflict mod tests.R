
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


# Bring in Data: ----------------------------------------------------------
post.pa.full <- readRDS("data/processed/post_pa_full.rds")
post.pa.partial <- readRDS("data/processed/post_pa_partial.rds")
post.pa.full.quad <- readRDS("data/processed/post_pa_full_quad.rds")
post.int.only <- readRDS("data/processed/post_int_only.rds")
pres.abs.scl <- readRDS("data/processed/pres_abs_scl.rds")

# Run LOOIC and Posterior Comparisons: ------------------------------------
loo1 <- loo(post.pa.full, save_psis = TRUE)
loo2 <- loo(post.pa.full.quad, save_psis = TRUE)
loo3 <- loo(post.pa.partial, save_psis = TRUE)
loo0 <- loo(post.int.only, save_psis = TRUE)

post_pa_loo_comp <- loo_compare(loo1, loo2, loo3, loo0)

preds <- posterior_epred(post.pa.full)
preds2 <- posterior_epred(post.pa.full.quad)
preds3 <- posterior_epred(post.pa.partial)
preds0 <- posterior_epred(post.int.only)
pred <- colMeans(preds)
pred2 <- colMeans(preds2)
pred3 <- colMeans(preds3)
pred0 <- colMeans(preds0)
pr <- as.integer(pred >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr3 <- as.integer(pred3 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)
round(mean(xor(pr,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.8
round(mean(xor(pr2,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.79
round(mean(xor(pr3,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.79
round(mean(xor(pr0,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.77

ploo <- E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post.pa.full))$value
ploo2 <- E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(post.pa.full.quad))$value
ploo3 <- E_loo(preds3, loo3$psis_object, type="mean", log_ratios = -log_lik(post.pa.partial))$value
ploo0 <- E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(post.int.only))$value

saveRDS(loo1, "data/processed/post_pa_full_loo.rds")
saveRDS(loo2, "data/processed/post_pa_full_quad_loo.rds")
saveRDS(loo3, "data/processed/post_pa_partial_loo.rds")
saveRDS(loo0, "data/processed/post_int_only_loo.rds")
saveRDS(post_pa_loo_comp, "data/processed/post_pa_loo_comp.rds")

# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.8
round(mean(xor(ploo2>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.79
round(mean(xor(ploo3>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.79
round(mean(xor(ploo0>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.77


# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(pres.abs.scl$conflict_presence_ps, post.pa.full$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.int.only$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=35)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.pa.full.quad$fitted.values, percent=TRUE, col='#B090D0', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.pa.partial$fitted.values, percent=TRUE, col='#FFAA00', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Full Model", "Full Model + Quadratic Pop Dens","Partial Model", "Varying Intercept-only Model"),
       col=c("#377eb8", "#B090D0", "#FFAA00", "#4daf4a"), lwd = 4)

# Full model has 89.4% AUC, Quad model has 88.9% AUC, partial has 88.6% AUC, and intercept only has 87.1%
# We will use full model without quadratic term as their predictive accuracy is similar and the predictor estimates seem more stable
