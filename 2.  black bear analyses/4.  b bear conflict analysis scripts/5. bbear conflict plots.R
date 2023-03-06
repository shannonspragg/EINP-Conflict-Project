
# Mixed effects plots of bbear conflict results -----------------------------

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
library(bayestestR)
# install.packages("see")
#install.packages("insight")
library(see)
library(insight)
library(ggplot2)


# Bring in Data: ----------------------------------------------------------
bear.full.mod <- readRDS("data/processed/bear_full_mod.rds")
bear.conflict.df.scl <- readRDS("data/processed/bear_conf_df_scl.rds")

# Plot Effects of Posterior Coefficients:
bear.full.result <- p_direction(bear.full.mod)
bear.mod.preds.plot <- plot(bear.full.result, title = "Predictor Effects for Bear Conflict")
bear.mod.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

bear.coef.plot <- plot(bear.full.mod, pars = c("dist2pa","humandens",
                                              "livestockOps",
                                              "rowcropOps",
                                              "ndvi", "gHM", "habsuit", "connectivity", "conflictprob"), main = "Predictor Effects for Black Bear Conflict")

saveRDS(bear.mod.preds.plot, "data/processed/bear_noconf_predsplot.rds")
saveRDS(bear.coef.plot, "data/processed/bear_coef_plot.rds")

# Plot results ------------------------------------------------------------

posterior <- as.matrix(bear.full.mod)
parnames <- names(fixef(bear.full.mod))[2:9] # change range based on model variables
p <- mcmc_intervals(posterior,
                    pars = parnames,
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist2pa" = "Dist. to PA",
                              "humandens" = "Human Population Density",
                              "livestockOps" = "Dens. of livestock ops.",
                              "rowcropOps" = "Dens. of row-crop ops.",
                              "connectivity" = "Bear Biophysical Connectivity",
                              "ndvi" = "Normalized Differential Vegetation Index",
                              "habsuit" = "Black bear habitat suitability",
                               "gHM" = "Human modification" ,
                               "conflictprob" = "Prob of wildlife conflict"))
                              # "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2))) # only use these if using conflict model


# Prep Dist to PA Plot ----------------------------------------------------
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = seq_range(dist2pa, n=300),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, # changing to add_elpd_draws from add_fitted_draws
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2pa_un <- (postdraws$dist2pa * attributes(bear.conflict.df.scl$dist2pa)[[3]])+attributes(bear.conflict.df.scl$dist2pa)[[2]]

# Plot Dist to PA:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2pa_un, conflictprob) %>% # if using conflict model, this is conflictprob
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")

dist2pa.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2pa_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2pa_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Protected Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(dist2pa.plot.b, "data/processed/bear_dist2pa_mixe_plot.rds")
dist2pa.plot.b

# Prep Human Density Plot: ----------------------------------------------------
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = seq_range(humandens, n=300),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod,
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$humandens <- (postdraws$humandens * attributes(bear.conflict.df.scl$humandens)[[3]])+attributes(bear.conflict.df.scl$humandens)[[2]]

# Plot Human Dens:
plot.df <- postdraws %>%
  mutate_at(., vars(conflictprob), as.factor) %>%
  group_by(humandens, conflictprob) %>%
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
pop.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = humandens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=humandens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") +
  xlab("Human Population Density")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(pop.dens.plot, "data/processed/bear_popdens_mixe_plot.rds")
pop.dens.plot

# Prep Livestock plot: ----------------------------------------------------
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = seq_range(livestockOps, n=300),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$livestockOps_un <- (postdraws$livestockOps * attributes(bear.conflict.df.scl$livestockOps)[[3]])+attributes(bear.conflict.df.scl$livestockOps)[[2]]

# Plot Livestock Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(livestockOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
livestockOps.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = livestockOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=livestockOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(livestockOps.plot.b, "data/processed/bear_livestockOps_mixe_plot.rds")
livestockOps.plot.b

# Prep Row Crop Plot: -----------------------------------------------------

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = seq_range(rowcropOps, n=300),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$rowcropOps_un <- (postdraws$rowcropOps * attributes(bear.conflict.df.scl$rowcropOps)[[3]])+attributes(bear.conflict.df.scl$rowcropOps)[[2]]

# Plot Row Crop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(rowcropOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
rowcropOps.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = rowcropOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=rowcropOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Row-crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(rowcropOps.plot.b, "data/processed/bear_rowcrops_mixe_plot.rds")
rowcropOps.plot.b

# Prep Connectivity Plot: -------------------------------------------------
simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = seq_range(connectivity, n=300),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$connectivity_un <- (postdraws$connectivity * attributes(bear.conflict.df.scl$connectivity)[[3]])+attributes(bear.conflict.df.scl$connectivity)[[2]]

# Plot Biophys Current:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(connectivity_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
connectivity.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = connectivity_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=connectivity_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Cumulative Current Flow (Amperes)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(connectivity.plot, "data/processed/bear_connectivity_mixe_plot.rds")
connectivity.plot

# Prep NDVI Plot: ---------------------------------------------------------

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = seq_range(ndvi, n=300),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ndvi <- (postdraws$ndvi * attributes(bear.conflict.df.scl$ndvi)[[3]]) + attributes(bear.conflict.df.scl$ndvi)[[2]]

# Plot NDVI:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(ndvi, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
ndvi.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = ndvi, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ndvi, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Normalized Difference Vegetation Index (NDVI)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(ndvi.plot.b, "data/processed/bear_ndvi_mixe_plot.rds")
ndvi.plot.b

# Prep BHS Plot -----------------------------------------------------------

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = seq_range(habsuit, n=300),
                    gHM = mean(gHM),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$habsuit_un <- (postdraws$habsuit * attributes(bear.conflict.df.scl$habsuit)[[3]])+attributes(bear.conflict.df.scl$habsuit)[[2]]

#Plot BHS:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(habsuit_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
habsuit.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = habsuit_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=habsuit_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Predicted Black Bear Habitat Suitability")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(habsuit.plot, "data/processed/bear_bhs_mixe_plot.rds")
habsuit.plot

# Prep gHM Plot -----------------------------------------------------------

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ndvi = mean(ndvi),
                    habsuit = mean(habsuit),
                    gHM = seq_range(gHM, n=300),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ghm <- (postdraws$gHM * attributes(bear.conflict.df.scl$gHM)[[3]])+attributes(bear.conflict.df.scl$gHM)[[2]]

# Plot gHM:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(gHM, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
human.mod.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = gHM, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=gHM, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Degree of Human Modification (gHM)")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(human.mod.plot, "data/processed/bear_gHM_mixe_plot.rds")
human.mod.plot

# Add Plots together:
biophys.p <-  connectivity.plot + habsuit.plot + dist2pa.plot.b + ndvi.plot.b + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p <-  livestockOps.plot.b + rowcropOps.plot.b + human.mod.plot + pop.dens.plot +  plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

bear.plot.all <- connectivity.plot + habsuit.plot + dist2pa.plot.b + ndvi.plot.b + livestockOps.plot.b + rowcropOps.plot.b + human.mod.plot + pop.dens.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')
bear.plot.all

saveRDS(biophys.p, "data/processed/biophys_bear_conf_plots.rds")
saveRDS(social.p, "data/processed/social_bear_conf_plots.rds")
saveRDS(bear.plot.all, "data/processed/all_bear_conf_plots.rds")

