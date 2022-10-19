
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
post.pa.full <- readRDS("Data/processed/post_pa_full.rds")
post.pa.full.quad <- readRDS("Data/processed/post_pa_full_quad.rds")
post.int.only <- readRDS("Data/processed/post_int_only.rds")

# Run LOOIC and Posterior Comparisons: ------------------------------------
loo1 <- loo(post.pa.full, save_psis = TRUE)
loo2 <- loo(post.pa.full.quad, save_psis = TRUE)
loo0 <- loo(post.int.only, save_psis = TRUE)

post_pa_loo_comp <- loo_compare(loo1, loo2, loo0)

preds <- posterior_epred(post.pa.full)
preds2 <- posterior_epred(post.pa.full.quad)
preds0 <- posterior_epred(post.int.only)
pred <- colMeans(preds)
pred2 <- colMeans(preds2)
pred0 <- colMeans(preds0)
pr <- as.integer(pred >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)
round(mean(xor(pr,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89  # 0.89
round(mean(xor(pr2,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.88 # 0.89
round(mean(xor(pr0,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.82 # 0.82

ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post.pa.full))$value
ploo2 <- E_loo(preds2, loo2$psis_object, type="mean", log_ratios = -log_lik(post.pa.full.quad))$value

ploo0 <- E_loo(preds0, loo0$psis_object, type="mean", log_ratios = -log_lik(post.int.only))$value

saveRDS(loo1, "Data/processed/post_pa_full_loo.rds")
saveRDS(loo2, "Data/processed/post_pa_full_quad_loo.rds")
saveRDS(loo0, "Data/processed/post_int_only_loo.rds")
saveRDS(post_pa_loo_comp, "Data/processed/post_pa_loo_comp.rds")

# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89  # 0.89
round(mean(xor(ploo2>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.89 # 0.89
round(mean(xor(ploo0>0.5,as.integer(pres.abs.scl$conflict_presence_ps==0))),2) #0.82 # 0.82


# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(pres.abs.scl$conflict_presence_ps, post.pa.full$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(pres.abs.scl$conflict_presence_ps, post.int.only$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)

legend("bottomright", legend=c("Full Model", "Varying Intercept Model"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
par(opar)

# We will use full model without quadratic term as their predictive accuracy is similar and the predictor estimates seem more stable
