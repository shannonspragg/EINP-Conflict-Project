
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
post.pa.full <- readRDS("Data/processed/post_pa_full.rds")
post.pa.full.quad <- readRDS("Data/processed/post_pa_full_quad.rds")
post.int.only <- readRDS("Data/processed/post_int_only.rds")


# Plot Effects of Posterior Coefficients:

post.pa.result <- p_direction(post.pa.full)
post.pa.full.preds.plot <- plot(post.pa.result, title = "Predictor Effects for General Wildlife Conflict")
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

gen_conf_coef_plot <- plot(post.pa.full, pars = c("dist.2.pa.ps","dist.2.met.ps",
                                                  "animal.farm.dens.ps",
                                                  "ground.crop.dens.ps",
                                                  "pop.dens"), main = "Predictor Effects for General Wildlife Conflict")

saveRDS(post.pa.full.preds.plot, "Data/processed/post_pa_full_predsplot.rds")
saveRDS(gen_conf_coef_plot, "Data/processed/post_pa_coef_plot.rds")

# Simulate Data & Posterior Predictive Draws: -----------------------------

posterior <- as.matrix(post.pa.full)
p <- mcmc_intervals(posterior,
                    pars = c("dist.2.pa.ps",
                             "dist.2.met.ps",
                             "animal.farm.dens.ps",
                             "ground.crop.dens.ps",
                             "pop.dens"),
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist.2.pa.ps" = "Dist. to PA",
                              "dist.2.met.ps" = "Dist. to metro",
                              "animal.farm.dens.ps" = "Dens. of livestock ops.",
                              "ground.crop.dens.ps" = "Dens. of row-crop ops.",
                              "pop.dens" = "Population dens.")) 



simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = seq_range(animal.farm.dens.ps, n=300),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

# postdraws <- tidybayes::add_fitted_draws(post.pa.full, 
#                                  newdata=simdata,
#                                  ndraws=1000,
#                                  re_formula=NA)

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$animal.farm.dens <- (postdraws$animal.farm.dens.ps * attributes(pres.abs.scl$animal.farm.dens.ps)[[3]])+attributes(pres.abs.scl$animal.farm.dens.ps)[[2]]

# Plotting Livestock Density: # NOTE: changing .value to .epred to match update from add_epred_draws
plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(animal.farm.dens, pop.dens) %>% 
  dplyr::summarise(., mean = mean(.epred),
                   lo = quantile(.epred, 0.2),
                   hi = quantile(.epred, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
animal.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = animal.farm.dens, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=animal.farm.dens, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

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


simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = seq_range(ground.crop.dens.ps,n=300),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ground.crop.dens <- (postdraws$ground.crop.dens.ps * attributes(pres.abs.scl$ground.crop.dens.ps)[[3]])+attributes(pres.abs.scl$ground.crop.dens.ps)[[2]]

# Plotting Row Crop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(ground.crop.dens, pop.dens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
ground.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = ground.crop.dens, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ground.crop.dens, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab(expression("Density of Row-Crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))


simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = mean(dist.2.pa.ps),
                    dist.2.met.ps = seq_range(dist.2.met.ps, n=300),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist.2.met <- (postdraws$dist.2.met.ps * attributes(pres.abs.scl$dist.2.met.ps)[[3]])+attributes(pres.abs.scl$dist.2.met.ps)[[2]]

# Plot Dist 2 Metro Areas:
plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(dist.2.met, pop.dens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2met.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.met, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.met, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Metro Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- pres.abs.scl %>%
  modelr::data_grid(dist.2.pa.ps = seq_range(dist.2.pa.ps, n=300),
                    dist.2.met.ps = mean(dist.2.met.ps),
                    animal.farm.dens.ps = mean(animal.farm.dens.ps),
                    ground.crop.dens.ps = mean(ground.crop.dens.ps),
                    pop.dens = quantile(pres.abs.scl$pop.dens, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(post.pa.full, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist.2.pa <- (postdraws$dist.2.pa.ps * attributes(pres.abs.scl$dist.2.pa.ps)[[3]])+attributes(pres.abs.scl$dist.2.pa.ps)[[2]]

# Plotting Dist 2 PA's:
plot.df <- postdraws %>% 
  mutate_at(., vars(pop.dens), as.factor) %>% 
  group_by(dist.2.pa, pop.dens) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$pop.dens) <-  c("Lower 10%", "Mean", "Upper 10%")
dist.2pa.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist.2.pa, y = mean, colour =pop.dens), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist.2.pa, fill = pop.dens), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","Population Density")+
  scale_fill_viridis(discrete = "TRUE", option="D", "Population Density") +
  ylab("Probability of Conflict") + 
  xlab("Distance to Protected Area (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

p.all <- animal.dens.plot + ground.dens.plot + dist.2met.plot + dist.2pa.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

saveRDS(p.all, "Data/processed/general_conflict_all_pred_plot.rds")

