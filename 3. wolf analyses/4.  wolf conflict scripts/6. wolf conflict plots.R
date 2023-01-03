
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
wolf.full.mod <- readRDS("data/processed/wolf_full_mod.rds")
#wolf.no.conflict.mod <- readRDS("data/processed/wolf_no_conf.rds")

# Plot Effects of Posterior Coefficients:
library(bayestestR)
# install.packages("see")
#install.packages("insight")
library(see)
library(insight)
library(ggplot2)

wolf.full.result <- p_direction(wolf.full.mod)
wolf.mod.preds.plot <- plot(wolf.full.result, title = "Predictor Effects for Bear Conflict")
wolf.mod.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

wolf.coef.plot <- plot(wolf.full.mod, pars = c("dist2pa","humandens",
                                              "livestockOps",
                                              "rowcropOps",
                                              "ungulate_dens", "gHM", "habsuit", "connectivity", "wolfincrease", "roaddens", "conflictprob"), main = "Predictor Effects for Black Bear Conflict")

saveRDS(wolf.mod.preds.plot, "data/processed/wolf_noconf_predsplot.rds")
saveRDS(wolf.coef.plot, "data/processed/wolf_coef_plot.rds")

# Plot results ------------------------------------------------------------

posterior <- as.matrix(wolf.full.mod)
parnames <- names(fixef(wolf.full.mod))[2:9] # change range based on model variables
p <- mcmc_intervals(posterior,
                    pars = parnames,
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist2pa" = "Dist. to PA",
                              "humandens" = "Human Population Density",
                              "livestockOps" = "Dens. of livestock ops.",
                              "rowcropOps" = "Dens. of row-crop ops.",
                              "connectivity" = "Bear Biophysical Connectivity",
                              "ungulate_dens" = "Ungulate Density",
                              "habsuit" = "Black bear habitat suitability",
                               "gHM" = "Human modification" ,
                              "wolfincrease" = "Public Support of Wolf Population Increase",
                              "roaddens" = "Road Density",
                               "conflictprob" = "Prob of wildlife conflict"))
                              # "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2))) # only use these if using conflict model


# Prep Dist to PA Plot ----------------------------------------------------
simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = seq_range(dist2pa, n=300),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, # changing to add_elpd_draws from add_fitted_draws
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2pa_un <- (postdraws$dist2pa * attributes(wolf.conflict.df.scl$dist2pa)[[3]])+attributes(wolf.conflict.df.scl$dist2pa)[[2]]

# Plot Dist to PA:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2pa_un, conflictprob) %>% # if using conflict model, this is conflictprob
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")

dist2pa.plot.w <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2pa_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2pa_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Distance to Protected Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(dist2pa.plot.w, "data/processed/wolf_dist2pa_mixe_plot.rds")


# Prep Human Density Plot: ----------------------------------------------------


simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = seq_range(humandens, n=300),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod,
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$humandens <- (postdraws$humandens * attributes(wolf.conflict.df.scl$humandens)[[3]])+attributes(wolf.conflict.df.scl$humandens)[[2]]

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
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(pop.dens.plot, "data/processed/wolf_popdens_mixe_plot.rds")


# Prep Livestock plot: ----------------------------------------------------
simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = seq_range(livestockOps, n=300),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$livestockOps_un <- (postdraws$livestockOps * attributes(wolf.conflict.df.scl$livestockOps)[[3]])+attributes(wolf.conflict.df.scl$livestockOps)[[2]]

# Plot Livestock Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(livestockOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
livestockOps.plot.w <- ggplot(data=plot.df) +
  geom_line(aes(x = livestockOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=livestockOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Livestock Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(livestockOps.plot.w, "data/processed/wolf_livestockOps_mixe_plot.rds")


# Prep Row Crop Plot: -----------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = seq_range(rowcropOps, n=300),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$rowcropOps_un <- (postdraws$rowcropOps * attributes(wolf.conflict.df.scl$rowcropOps)[[3]])+attributes(wolf.conflict.df.scl$rowcropOps)[[2]]

# Plot Row Crop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(rowcropOps_un, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
rowcropOps.plot.w <- ggplot(data=plot.df) +
  geom_line(aes(x = rowcropOps_un, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=rowcropOps_un, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab(expression("Density of Row-crop Operations per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(rowcropOps.plot.w, "data/processed/wolf_rowcrops_mixe_plot.rds")

# Prep Connectivity Plot: -------------------------------------------------
simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = seq_range(connectivity, n=300),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$connectivity_un <- (postdraws$connectivity * attributes(wolf.conflict.df.scl$connectivity)[[3]])+attributes(wolf.conflict.df.scl$connectivity)[[2]]

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
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(connectivity.plot, "data/processed/wolf_connectivity_mixe_plot.rds")

# Prep Ungulate density Plot: ---------------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = seq_range(ungulate_dens, n=300),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ungulate_dens <- (postdraws$ungulate_dens * attributes(wolf.conflict.df.scl$ungulate_dens)[[3]]) + attributes(wolf.conflict.df.scl$ungulate_dens)[[2]]

# Plot GrizzInc:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(ungulate_dens, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
ungulate.plot.w <- ggplot(data=plot.df) +
  geom_line(aes(x = ungulate_dens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ungulate_dens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Ungulate Population Density")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(ungulate.plot.w, "data/processed/wolf_ungulate_density_mixe_plot.rds")


# Prep WHS Plot -----------------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = seq_range(habsuit, n=300),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$habsuit_un <- (postdraws$habsuit * attributes(wolf.conflict.df.scl$habsuit)[[3]])+attributes(wolf.conflict.df.scl$habsuit)[[2]]

#Plot wHS:
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
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(habsuit.plot, "data/processed/wolf_bhs_mixe_plot.rds")

# Prep gHM Plot -----------------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = seq_range(gHM, n=300),
                    wolfincrease = mean(wolfincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ghm <- (postdraws$gHM * attributes(wolf.conflict.df.scl$gHM)[[3]])+attributes(wolf.conflict.df.scl$gHM)[[2]]

# Plot Pop Dens:
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
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(human.mod.plot, "data/processed/wolf_gHM_mixe_plot.rds")

# Prep wolf inc Plot -----------------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = seq_range(wolfincrease, n=300),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$wolfinc <- (postdraws$wolfinc * attributes(wolf.conflict.df.scl$wolfinc)[[3]])+attributes(wolf.conflict.df.scl$wolfinc)[[2]]

# Plot Pop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(wolfinc, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
wolf.increase.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = wolfinc, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=wolfinc, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Public Support of Wolf Population Increase (%)")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(wolf.increase.plot, "data/processed/wolf_increase_mixe_plot.rds")

# Prep road dens Plot -----------------------------------------------------------

simdata <- wolf.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    wolfincrease = mean(wolfincrease),
                    roaddens = seq_range(roaddens, n=300),
                    conflictprob = quantile(wolf.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(wolf.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$roaddens <- (postdraws$roaddens * attributes(wolf.conflict.df.scl$roaddens)[[3]])+attributes(wolf.conflict.df.scl$roaddens)[[2]]

# Plot Pop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(roaddens, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
road.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = roaddens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=roaddens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Road Density per km")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(road.dens.plot, "data/processed/wolf_road_density_mixe_plot.rds")


# Add Plots together:
biophys.p <-  connectivity.plot + habsuit.plot + dist2pa.plot.w + ungulate.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p <-  livestockOps.plot.w + rowcropOps.plot.w + human.mod.plot + road.dens.plot + wolf.increase.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

wolf.plot.all <- connectivity.plot + habsuit.plot + dist2pa.plot.w + ungulate.plot + livestockOps.plot.w + rowcropOps.plot.w + human.mod.plot + road.dens.plot + wolf.increase.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

saveRDS(biophys.p, "data/processed/biophys_wolf_conf_plots.rds")
saveRDS(social.p, "data/processed/social_wolf_conf_plots.rds")
saveRDS(wolf.plot.all, "data/processed/all_wolf_conf_plots.rds")

