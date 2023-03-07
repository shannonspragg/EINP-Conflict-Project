
# Run LOOIC tests and ROC curves ------------------------------------------
## we test the data simulations and plot results 

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
library(patchcork)
library(modelr)
theme_set(bayesplot::theme_default(base_family = "sans"))
install.packages("performance") 
library(performance)

# Read data in:
cougar.full.mod.quad <- readRDS("data/processed/cougar_quad_reg.rds")
cougar.old.mod <- readRDS("data/processed/cougar_old_mod.rds")
cougar.int.only <- readRDS("data/processed/cougar_int_only.rds")
cougar.full.mod <- readRDS("data/processed/cougar_full_mod.rds")
cougar.no.conf <- readRDS("data/processed/cougar_no_conf.rds")
cougar.conflict.df.scl <- readRDS("data/processed/cougar_conf_df_scl.rds")

# Model Comparison: -------------------------------------------------------
loo1c <- loo(cougar.full.mod, save_psis = TRUE, k_threshold = 0.7) # there is one point that has a poor loo value
loo2c <- loo(cougar.full.mod.quad, save_psis = TRUE, k_threshold = 0.7)
loo3c <- loo(cougar.exp.mod, save_psis = TRUE, k_threshold = 0.7)
loo4c <- loo(cougar.no.conf, save_psis = TRUE, k_threshold = 0.7)
loo0c <- loo(cougar.int.only, save_psis = TRUE, k_threshold = 0.7)

saveRDS(loo1c, "Data/processed/cougar_full_loo.rds")
saveRDS(loo2c, "Data/processed/cougar_full_quad_loo.rds")
saveRDS(loo3c, "Data/processed/cougar_exp_loo.rds")
saveRDS(loo4c, "Data/processed/cougar_no_conf_loo.rds")
saveRDS(loo0c, "Data/processed/cougar_int_only_loo.rds")

# Bring back in later:
# loo1c <- readRDS("Data/processed/cougar_full_loo.rds")
# loo2c <- readRDS("Data/processed/cougar_full_quad_loo.rds")
# loo3c <- readRDS("Data/processed/cougar_no_conf_loo.rds")
# loo0c <- readRDS("Data/processed/cougar_int_only_loo.rds")

cougar.loo.comp <- loo_compare(loo1c, loo2c, loo3c,loo4c, loo0c)
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

round(mean(xor(pr3,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96
round(mean(xor(pr2,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96
round(mean(xor(pr1,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #0.96
round(mean(xor(pr0,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #0.96

ploo1 <- E_loo(preds1, loo1c$psis_object, type="mean", log_ratios = -log_lik(cougar.full.mod))$value

ploo2<-E_loo(preds2, loo2c$psis_object, type="mean", log_ratios = -log_lik(cougar.full.mod.quad))$value

ploo3<-E_loo(preds3, loo3c$psis_object, type="mean", log_ratios = -log_lik(cougar.no.conf))$value

ploo0<-E_loo(preds0, loo0c$psis_object, type="mean", log_ratios = -log_lik(cougar.int.only))$value

round(mean(xor(ploo1>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96
round(mean(xor(ploo2>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96
round(mean(xor(ploo3>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96
round(mean(xor(ploo0>0.5,as.integer(cougar.conflict.df.scl$cougar_conflict==0))),2) #.96


# Try model averaging: ----------------------------------------------------
    ## It looks like there is uncertainty with model selection due to the similarity of the models. 
    # Here we try model averaging and stacking to differentiate the full model from the full model + quad (both with delta LOOIC < -2:
lpd_point <- cbind(
  loo1c$pointwise[,"elpd_loo"], 
  loo2c$pointwise[,"elpd_loo"],
  loo3c$pointwise[,"elpd_loo"],
  loo0c$pointwise[,"elpd_loo"]
)
pbma_wts <- pseudobma_weights(lpd_point, BB=FALSE)
pbma_BB_wts <- pseudobma_weights(lpd_point) # default is BB=TRUE
stacking_wts <- stacking_weights(lpd_point)
round(cbind(pbma_wts, pbma_BB_wts, stacking_wts), 2)
# This shows us that the third model (cougar.exp.model) is holding the majority of the weight, aside from the intercept-only

# Apply these weights to the posterior predictions:
yrep1 <- posterior_predict(cougar.full.mod, dracs=(0.04*4827)) # 
yrep2 <- posterior_predict(cougar.full.mod.quad, dracs=round(0.02*4827)) # 
yrep3 <- posterior_predict(cougar.no.conf, dracs=round(0.40*4827)) # 
yrep0 <- posterior_predict(cougar.int.only, dracs=round(0.55*4827)) # 

yrep <- c(yrep1, yrep2, yrep3, yrep0)

# Based on the above, it looks like the best model is #0: cougar.int.only, followed by the cougar.no.conflict , cougar.full.mod, and cougar.full.mod.quad  (respectively)

# Building plots of results -----------------------------------------------

# Plotting AUC
opar <- par()
par(pty = "s")
pROC::roc(cougar.conflict.df.scl$cougar_conflict, cougar.exp.mod$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE , 
          xlab= "False Positive Percentage", ylab= "True Positive Percentage",
          col="#377eb8", lcd=4, print.auc=TRUE, print.auc.y=55)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.full.mod$fitted.values, percent=TRUE, col='#4daf4a', lcd=4, print.auc=TRUE, add=TRUE, print.auc.y=50)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.full.mod.quad$fitted.values, percent=TRUE, col='red', lcd=4, print.auc=TRUE, add=TRUE, print.auc.y=45)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.no.conf$fitted.values, percent=TRUE, col='#B090D0', lcd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
pROC::plot.roc(cougar.conflict.df.scl$cougar_conflict, cougar.int.only$fitted.values, percent=TRUE, col='#FFAA00', lcd=4, print.auc=TRUE, add=TRUE, print.auc.y=35)

legend("bottomright", legend=c("Expanded Model", "Full Model", "Quad Model",  "No Conflict Model", "Varying Intercept-only Model"),
       col=c("#377eb8", "#4daf4a","red", "#B090D0", "#FFAA00"), lwd = 4)

# We will use the expanded model, as it has the best AUC, even though LOOIC doesn't look great.
