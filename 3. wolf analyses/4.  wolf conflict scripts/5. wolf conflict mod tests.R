
# Run LOOIC tests and ROC curves ------------------------------------------
## We test the data simulations and plot results 

## NOTE: use shinystan::launch_shinystan(model) to check posterior predictive details

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
install.packages("performance") 
library(performance)

# Read data in:
wolf.full.mod.quad <- readRDS("data/processed/wolf_quad_reg.rds")
wolf.int.only <- readRDS("data/processed/wolf_int_only.rds")
wolf.full.mod <- readRDS("data/processed/wolf_full_mod.rds")
wolf.no.conf <- readRDS("data/processed/wolf_no_conf.rds")
wolf.conflict.df.scl <- readRDS("data/processed/wolf_conf_df_scl.rds")

# Use shinystan::launch_shinystan(yourmod) to check posterior preformance

# Model Comparison: -------------------------------------------------------
loo1w <- loo(wolf.full.mod, save_psis = TRUE, k_threshold = 0.7) # there is one point that has a poor loo value
loo2w <- loo(wolf.full.mod.quad, save_psis = TRUE, k_threshold = 0.7)
loo3w <- loo(wolf.no.conf, save_psis = TRUE, k_threshold = 0.7)
loo0w <- loo(wolf.int.only, save_psis = TRUE, k_threshold = 0.7)

saveRDS(loo1w, "Data/processed/wolf_full_loo.rds")
saveRDS(loo2w, "Data/processed/wolf_full_quad_loo.rds")
saveRDS(loo3w, "Data/processed/wolf_no_conf_loo.rds")
saveRDS(loo0w, "Data/processed/wolf_int_only_loo.rds")

# Bring back in later:
# loo1w <- readRDS("Data/processed/wolf_full_loo.rds")
# loo2w <- readRDS("Data/processed/wolf_full_quad_loo.rds")
# loo3w <- readRDS("Data/processed/wolf_no_conf_loo.rds")
# loo0w <- readRDS("Data/processed/wolf_int_only_loo.rds")

wolf.loo.comp <- loo_compare(loo1w, loo2w, loo3w, loo0w)
saveRDS(wolf.loo.comp, "data/processed/wolf_loo_comp.rds")

plot(
  loo2b,
  diagnostic = c("k", "n_eff"),
  label_points = TRUE,
  main = "PSIS diagnostic plot"
)

preds3 <- posterior_epred(wolf.no.conf)
preds2 <- posterior_epred(wolf.full.mod.quad)
preds1 <- posterior_epred(wolf.full.mod)
preds0 <- posterior_epred(wolf.int.only)
pred3 <- colMeans(preds3)
pred2 <- colMeans(preds2)
pred1 <- colMeans(preds1)
pred0 <- colMeans(preds0)
pr3 <- as.integer(pred3 >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr1 <- as.integer(pred1 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)

round(mean(xor(pr3,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99
round(mean(xor(pr2,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99
round(mean(xor(pr1,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #0.99
round(mean(xor(pr0,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #0.99

ploo1 <- E_loo(preds1, loo1b$psis_object, type="mean", log_ratios = -log_lik(wolf.full.mod))$value

ploo2<-E_loo(preds2, loo2b$psis_object, type="mean", log_ratios = -log_lik(wolf.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3b$psis_object, type="mean", log_ratios = -log_lik(wolf.no.conf))$value

ploo0<-E_loo(preds0, loo0b$psis_object, type="mean", log_ratios = -log_lik(wolf.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99
round(mean(xor(ploo2>0.5,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99
round(mean(xor(ploo3>0.5,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99
round(mean(xor(ploo0>0.5,as.integer(wolf.conflict.df.scl$wolf_conflict==0))),2) #.99


# Try model averaging: ----------------------------------------------------
    ## It looks like there is uncertainty with model selection due to the similarity of the models. 
    # Here we try model averaging and stacking to differentiate the full model from the full model + quad (both with delta LOOIC < -2:
lpd_point <- cbind(
  loo1w$pointwise[,"elpd_loo"], 
  loo2w$pointwise[,"elpd_loo"]
)
pbma_wts <- pseudobma_weights(lpd_point, BB=FALSE)
pbma_BB_wts <- pseudobma_weights(lpd_point) # default is BB=TRUE
stacking_wts <- stacking_weights(lpd_point)
round(cbind(pbma_wts, pbma_BB_wts, stacking_wts), 2)
# This shows us that the first model (wolf.full.model) is holding the majority of the weight

# Apply these weights to the posterior predictions:
yrep1 <- posterior_predict(wolf.full.mod, draws=(0.91*7500)) # 6825 draws
yrep2 <- posterior_predict(wolf.full.mod.quad, draws=round(0.09*7500)) # only 675 draws

yrep <- c(yrep1, yrep2)

# Based on the above, it looks like the best model is #3: wolf.no.conflict , followed by the wolf.full.mod, wolf.full.mod.quad, and wolf.int.only (respectively)

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(wolf.conflict.df.scl$wolf_conflict, wolf.full.mod.quad$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(wolf.conflict.df.scl$wolf_conflict, wolf.full.mod$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=55)

pROC::plot.roc(wolf.conflict.df.scl$wolf_conflict, wolf.no.conf$fitted.values, percent=TRUE, col='#B090D0', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)
pROC::plot.roc(wolf.conflict.df.scl$wolf_conflict, wolf.int.only$fitted.values, percent=TRUE, col='#FFAA00', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Full Model", "Quad Model",  "No Conflict Model", "Varying Intercept-only Model"),
       col=c("#4daf4a","#377eb8", "#B090D0", "#FFAA00"), lwd = 4)

# We will use the full model, as it has the lowest LOOIC, the best AUC, and seems to be decent
