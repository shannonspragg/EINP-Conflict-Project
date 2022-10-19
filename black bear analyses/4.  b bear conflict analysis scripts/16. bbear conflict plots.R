
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
bear.full.mod.quad <- readRDS("Data/processed/bear_quad_reg.rds")
bear.int.only <- readRDS("Data/processed/bear_int_only.rds")
bear.full.mod <- readRDS("Data/processed/bear_full.rds")
bear.no.conf <- readRDS("Data/processed/bear_no_conf.rds")

# Plot Effects of Posterior Coefficients:
library(bayestestR)
# install.packages("see")
#install.packages("insight")
library(see)
library(insight)
library(ggplot2)

bear.quad.preds.plot <- plot(bear.quad.result, title = "Predictor Effects for Bear Conflict")
bear.quad.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

bear.coef.plot <- plot(post.pa.full, pars = c("dist.2.pa.ps","dist.2.met.ps",
                                              "animal.farm.dens.ps",
                                              "ground.crop.dens.ps",
                                              "pop.dens"), main = "Predictor Effects for General Wildlife Conflict")

saveRDS(bear.quad.preds.plot, "Data/processed/bear_quad_predsplot.rds")
saveRDS(bear.coef.plot, "Data/processed/bear_coef_plot.rds")

# Plot results ------------------------------------------------------------

posterior <- as.matrix(bear.full.mod.quad)
parnames <- names(fixef(bear.full.mod.quad))[2:11]
p <- mcmc_intervals(posterior,
                    pars = parnames,
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist2pa" = "Dist. to PA",
                              "dist2grizz" = "Dist. to extant grizzly bear \npopulations",
                              "livestockOps" = "Dens. of livestock ops.",
                              "rowcropOps" = "Dens. of row-crop ops.",
                              "connectivity" = "Connectivity",
                              "grizzinc" = "Public perceptions \nof grizzly bears",
                              "habsuit" = "Grizzly bear \nhabitat suitability",
                              "humandens" = "Human population dens.",
                              "conflictprob" = "Prob of wildlife conflict",
                              "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2)))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = seq_range(dist2pa, n=300),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, # changing to add_elpd_draws from add_fitted_draws
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2pa_un <- (postdraws$dist2pa * attributes(bear.conflict.df.scl$dist2pa)[[3]])+attributes(bear.conflict.df.scl$dist2pa)[[2]]

# Plot Dist to PA:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2pa_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
dist2pa.plot.b <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2pa_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2pa_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Protected Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = seq_range(dist2grizz, n=300),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2grizz_un <- (postdraws$dist2grizz * attributes(bear.conflict.df.scl$dist2grizz)[[3]])+attributes(bear.conflict.df.scl$dist2grizz)[[2]]

# Plot Dist to GrizzPop:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2grizz_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
dist2grizz.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2grizz_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2grizz_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Extant Grizzly Pops. (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = seq_range(livestockOps, n=300),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
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
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = seq_range(rowcropOps, n=300),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
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
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Row-crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = seq_range(connectivity, n=300),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
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
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Cumulative Current Flow (Amperes)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = seq_range(grizzinc, n=300),
                    habsuit = mean(habsuit),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$grizzinc_un <- (postdraws$grizzinc * attributes(bear.conflict.df.scl$grizzinc)[[3]]) + attributes(bear.conflict.df.scl$grizzinc)[[2]]

# Plot GrizzInc:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(grizzinc_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
grizzinc.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = grizzinc_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=grizzinc_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Prop. of People Supporting Grizzly Pop. Increase")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = seq_range(habsuit, n=300),
                    humandens = mean(humandens),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
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
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Predicted Grizzly Bear Habitat Suitability")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

simdata <- bear.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    dist2grizz = mean(dist2grizz),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    grizzinc = mean(grizzinc),
                    habsuit = mean(habsuit),
                    humandens = seq_range(humandens, n=300),
                    conflictprob = quantile(bear.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(bear.full.mod.quad, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$humandens_un <- (postdraws$humandens * attributes(bear.conflict.df.scl$humandens)[[3]])+attributes(bear.conflict.df.scl$humandens)[[2]]

# Plot Pop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(humandens_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
humandens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = humandens_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=humandens_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="D","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="D", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Human Population Density")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))

# Add Plots together:
biophys.p <-  connectivity.plot + habsuit.plot + dist2pa.plot.b + dist2grizz.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p <- grizzinc.plot + humandens.plot + livestockOps.plot.b + rowcropOps.plot.b + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

saveRDS(biophys.p, "Data/processed/biophys_bear_conf_plots.rds")
saveRDS(social.p, "Data/processed/social_bear_conf_plots.rds")
