
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
install.packages("performance") 
library(performance)

# Read data in:
cougar.full.mod.quad <- readRDS("data/processed/cougar_quad_reg.rds")
cougar.int.only <- readRDS("data/processed/cougar_int_only.rds")
cougar.full.mod <- readRDS("data/processed/cougar_full_mod.rds")
cougar.no.conf <- readRDS("data/processed/cougar_no_conf.rds")
cougar.conflict.df.scl <- readRDS("data/processed/cougar_conf_df_scl.rds")

# Model Comparison: -------------------------------------------------------
loo1w <- loo(cougar.full.mod, save_psis = TRUE, k_threshold = 0.7) # there is one point that has a poor loo value
loo2w <- loo(cougar.full.mod.quad, save_psis = TRUE, k_threshold = 0.7)
loo3w <- loo(cougar.no.conf, save_psis = TRUE, k_threshold = 0.7)
loo0w <- loo(cougar.int.only, save_psis = TRUE, k_threshold = 0.7)

saveRDS(loo1b, "Data/processed/cougar_full_loo.rds")
saveRDS(loo2b, "Data/processed/cougar_full_quad_loo.rds")
saveRDS(loo3b, "Data/processed/cougar_no_conf_loo.rds")
saveRDS(loo0b, "Data/processed/cougar_int_only_loo.rds")

# Bring back in later:
# loo1w <- readRDS("Data/processed/cougar_full_loo.rds")
# loo2w <- readRDS("Data/processed/cougar_full_quad_loo.rds")
# loo3w <- readRDS("Data/processed/cougar_no_conf_loo.rds")
# loo0w <- readRDS("Data/processed/cougar_int_only_loo.rds")

cougar.loo.comp <- loo_compare(loo1w, loo2w, loo3w, loo0w)
saveRDS(cougar.loo.comp, "data/processed/cougar_loo_comp.rds")

plot(
  loo2b,
  diagnostic = c("k", "n_eff"),
  label_points = TRUE,
  main = "PSIS diagnostic plot"
)

preds3 <- posterior_epred(cougar.no.conf)
preds2 <- posterior_epred(cougar.full.mod.quad)
preds1 <- posterior_epred(cougar.full.mod)
preds0 <- posterior_epred(cougar.int.only)
pred3 <- colMeans(preds3)
pred2 <- colMeans(preds2)
pred1 <- colMeans(preds1)
pred0 <- colMeans(preds0)
pr3 <- as.integer(pred3 >= 0.5)
pr2 <- as.integer(pred2 >= 0.5)
pr1 <- as.integer(pred1 >= 0.5)
pr0 <- as.integer(pred0 >=0.5)

round(mean(xor(pr3,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.84
round(mean(xor(pr2,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.85
round(mean(xor(pr1,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #0.85
round(mean(xor(pr0,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #0.81

ploo1 <- E_loo(preds1, loo1b$psis_object, type="mean", log_ratios = -log_lik(cougar.full.mod))$value

ploo2<-E_loo(preds2, loo2b$psis_object, type="mean", log_ratios = -log_lik(cougar.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3b$psis_object, type="mean", log_ratios = -log_lik(cougar.no.conf))$value

ploo0<-E_loo(preds0, loo0b$psis_object, type="mean", log_ratios = -log_lik(cougar.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.84
round(mean(xor(ploo2>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.84
round(mean(xor(ploo3>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.84
round(mean(xor(ploo0>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.81


# Try model averaging: ----------------------------------------------------
    ## It looks like there is uncertainty with model selection due to the similarity of the models. 
    # Here we try model averaging and stacking to differentiate the full model from the full model + quad (both with delta LOOIC < -2:
lpd_point <- cbind(
  loo1b$pointwise[,"elpd_loo"], 
  loo2b$pointwise[,"elpd_loo"]
)
pbma_wts <- pseudobma_weights(lpd_point, BB=FALSE)
pbma_BB_wts <- pseudobma_weights(lpd_point) # default is BB=TRUE
stacking_wts <- stacking_weights(lpd_point)
round(cbind(pbma_wts, pbma_BB_wts, stacking_wts), 2)
# This shows us that the first model (cougar.full.model) is holding the majority of the weight

# Apply these weights to the posterior predictions:
yrep1 <- posterior_predict(cougar.full.mod, draws=(0.91*7500)) # 6825 draws
yrep2 <- posterior_predict(cougar.full.mod.quad, draws=round(0.09*7500)) # only 675 draws

yrep <- c(yrep1, yrep2)

# Based on the above, it looks like the best model is #3: cougar.no.conflict , followed by the cougar.full.mod, cougar.full.mod.quad, and cougar.int.only (respectively)

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(cougar.conflict.df.scl$cougar_conflict, cougar.full.mod.quad$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lwd=4, print.auc=TRUE)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.full.mod$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=55)

pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.no.conf$fitted.values, percent=TRUE, col='#B090D0', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.int.only$fitted.values, percent=TRUE, col='#FFAA00', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Full Model", "Quad Model",  "No Conflict Model", "Varying Intercept-only Model"),
       col=c("#4daf4a","#377eb8", "#B090D0", "#FFAA00"), lwd = 4)

# We will use the full model, as it has the lowest LOOIC, the best AUC, and seems to be stable.
