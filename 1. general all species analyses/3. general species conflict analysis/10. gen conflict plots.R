
# Mixed effects plots of gen conflict results -----------------------------

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
post.pa.full <- readRDS("data/processed/post_pa_full.rds")
pres.abs.scl <- readRDS("data/processed/pres_abs_scl.rds")

# Plot Effects of Posterior Coefficients:

post.pa.result <- p_direction(post.pa.full)
post.pa.full.preds.plot <- plot(post.pa.result, title = "Predictor Effects for General Wildlife Conflict")
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

gen_conf_coef_plot <- plot(post.pa.full, pars = c("dist.2.pa.ps","human.dens.ps",
                                                  "animal.farm.dens.ps",
                                                  "ground.crop.dens.ps",
                                                  "ndvi.ps", "gHM.ps", "agno.biophys.ps"), main = "Predictor Effects for General Wildlife Conflict")

saveRDS(post.pa.full.preds.plot, "data/processed/post_pa_full_predsplot.rds")
saveRDS(gen_conf_coef_plot, "data/processed/post_pa_coef_plot.rds")

# Simulate Data & Posterior Predictive Draws: -----------------------------

posterior <- as.matrix(post.pa.full)
p <- mcmc_intervals(posterior,
                    pars = c("dist.2.pa.ps",
                             "human.dens.ps",
                             "animal.farm.dens.ps",
                             "ground.crop.dens.ps",
                             "ndvi.ps", "gHM.ps", "agno.biophys.ps", "gen.focal.biophys.ps"),
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist.2.pa.ps" = "Dist. to PA",
                              "human.dens.ps" = "Population Density",
                              "animal.farm.dens.ps" = "Dens. of livestock ops.",
                              "ground.crop.dens.ps" = "Dens. of row-crop ops.",
                              "ndvi.ps" = "NDVI", "gHM.ps" = "Human Modification", "agno.biophys.ps" = "Agnostic Biophys. Connectivity",
                              "gen.focal.biophys.ps" = "General Focal Species Biophys. Connectivity")) 

# Prep plot for Dist 2 PAs:

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = seq_range(dist.2.pa.ps, n=300),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist.2.pa.ps <- (postdraws$dist.2.pa.ps * attributes(pres.abs.scl$dist.2.pa.ps)[[3]])+attributes(pres.abs.scl$dist.2.pa.ps)[[2]]

# Plotting Dist2PA's: # NOTE: changing .value to .epred to match update from add_epred_draws
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(dist.2.pa.ps, human.dens.ps) %>% 
  dplyr::summarise(., mean = mean(.epred),
                   lo = quantile(.epred, 0.2),
                   hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
dist2pa.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.pa.ps, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.pa.ps, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Distance to Protected Areas (km)"))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

saveRDS(dist2pa.plot, "data/processed/dist2pa_mixe_plot.rds")

# Prep plot for Livestock density:

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = seq_range(animal.farm.dens.ps, n=300),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$animal.farm.dens <- (postdraws$animal.farm.dens.ps * attributes(pres.abs.scl$animal.farm.dens.ps)[[3]])+attributes(pres.abs.scl$animal.farm.dens.ps)[[2]]

# Plotting Livestock Density: # NOTE: changing .value to .epred to match update from add_epred_draws
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(animal.farm.dens, human.dens.ps) %>% 
  dplyr::summarise(., mean = mean(.epred),
                   lo = quantile(.epred, 0.2),
                   hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
animal.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = animal.farm.dens, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

saveRDS(animal.dens.plot, "data/processed/animal_dens_mixe_plot.rds")

###### Testing other color palette:
# library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
# 
# animal.dens.plot <- ggplot(data=plot.df) +
#   geom_line(aes(x = animal.farm.dens, y = mean, colour =pop.dens), lwd=1.5) +
#   geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = pop.dens), alpha = 0.2) +
#   discrete_scale(scale_name, palette="Dark2", "Population Density")+
#   discrete_scale(scale_name, palette="Dark2", "Population Density") +
#   ylab("Probability of Conflict") + 
#   xlab(expression("Density of Livestock Operations per"~km^{2}))+
#   # guides(fill=guide_legend(title="Population Density"))+
#   theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

# Prep plot for Row crop density:

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = seq_range(ground.crop.dens.ps, n=300),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ground.crop.dens <- (postdraws$ground.crop.dens.ps * attributes(pres.abs.scl$ground.crop.dens.ps)[[3]])+attributes(pres.abs.scl$ground.crop.dens.ps)[[2]]

# Plotting Row Crop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(ground.crop.dens, human.dens.ps) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
ground.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = ground.crop.dens, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ground.crop.dens, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Row-Crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(ground.dens.plot, "data/processed/groundcrop_dens_mixe_plot.rds")

# Prep plot for NDVI:
simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = seq_range(ndvi.ps, n=300),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ndvi.ps <- (postdraws$ndvi.ps * attributes(pres.abs.scl$ndvi.ps)[[3]])+attributes(pres.abs.scl$ndvi.ps)[[2]]

# Plot Dist 2 Metro Areas:
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(ndvi.ps, human.dens.ps) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
ndvi.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = ndvi.ps, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ndvi.ps, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Normalized Difference Vegetation Index (NDVI)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(ndvi.plot, "data/processed/ndvi_mixe_plot.rds")

# Prep gHM plot:
simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = seq_range(gHM.ps, n=300),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$gHM.ps <- (postdraws$gHM.ps * attributes(pres.abs.scl$gHM.ps)[[3]])+attributes(pres.abs.scl$gHM.ps)[[2]]

# Plotting gHM's:
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(gHM.ps, human.dens.ps) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
gHM.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = gHM.ps, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=gHM.ps, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Global Human Modification (gHM)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(gHM.plot, "data/processed/gHM_mixe_plot.rds")

# Prep Species Agnostic Biophysical connectivity plot:
simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = seq_range(agno.biophys.ps, n=300),
                    gen.focal.biophys.ps = mean(gen.focal.biophys.ps))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$agno.biophys.ps <- (postdraws$agno.biophys.ps * attributes(pres.abs.scl$agno.biophys.ps)[[3]])+attributes(pres.abs.scl$agno.biophys.ps)[[2]]

# Plotting Species Agno's:
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(agno.biophys.ps, human.dens.ps) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
agno.biophys.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = agno.biophys.ps, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=agno.biophys.ps, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Species Agnostic Biophysical Connectivity")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(agno.biophys.plot, "data/processed/agno_bio_mixe_plot.rds")

# Prep Gen Focal Biophysical connectivity plot:
simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    human.dens.ps = quantile(pres.abs.scl$human.dens.ps, probs = c(0.1, 0.5, 0.9)),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    ndvi.ps = mean(ndvi.ps),
                    gHM.ps = mean(gHM.ps),
                    agno.biophys.ps = mean(agno.biophys.ps),
                    gen.focal.biophys.ps = seq_range(gen.focal.biophys.ps, n=300))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$gen.focal.biophys.ps <- (postdraws$gen.focal.biophys.ps * attributes(pres.abs.scl$gen.focal.biophys.ps)[[3]])+attributes(pres.abs.scl$gen.focal.biophys.ps)[[2]]

# Plotting Dist 2 PA's:
plot.df <- postdraws %>% 
  mutate_at(., vars(human.dens.ps), as.factor) %>% 
  group_by(gen_focal_bio_cumcurrmap, human.dens.ps) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$human.dens.ps) <-  c("Lower 10%", "Mean", "Upper 10%")
gfocal.biophys.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = gen.focal.biophys.ps, y = mean, colour =human.dens.ps), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=gen.focal.biophys.ps, fill = human.dens.ps), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="C", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("General Focal Species Biophysical Connectivity")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(gfocal.biophys.plot, "data/processed/gen_focal_bio_mixe_plot.rds")

p.all <- animal.dens.plot + ground.dens.plot + ndvi.plot + dist2pa.plot + gHM.plot + agno.biophys.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

saveRDS(p.all, "data/processed/general_conflict_all_pred_plot.rds")

