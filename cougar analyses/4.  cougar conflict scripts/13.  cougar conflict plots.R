
# Mixed effects plots of cougar conflict results -----------------------------

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
cougar.full.mod <- readRDS("data/processed/cougar_full_mod.rds")
#cougar.no.conflict.mod <- readRDS("data/processed/cougar_no_conf.rds")

# Plot Effects of Posterior Coefficients:
library(bayestestR)
# install.packages("see")
#install.packages("insight")
library(see)
library(insight)
library(ggplot2)

cougar.full.result <- p_direction(cougar.full.mod)
cougar.mod.preds.plot <- plot(cougar.full.result, title = "Predictor Effects for Bear Conflict")
cougar.mod.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

cougar.coef.plot <- plot(cougar.full.mod, pars = c("dist2pa","humandens",
                                              "livestockOps",
                                              "rowcropOps",
                                              "ungulate_dens", "gHM", "habsuit", "connectivity", "cougarincrease", "roaddens", "conflictprob"), main = "Predictor Effects for Black Bear Conflict")

saveRDS(cougar.mod.preds.plot, "data/processed/cougar_noconf_predsplot.rds")
saveRDS(cougar.coef.plot, "data/processed/cougar_coef_plot.rds")

# Plot results ------------------------------------------------------------

posterior <- as.matrix(cougar.full.mod)
parnames <- names(fixef(cougar.full.mod))[2:9] # change range based on model variables
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
                              "cougarincrease" = "Public Support of cougar Population Increase",
                              "roaddens" = "Road Density",
                               "conflictprob" = "Prob of wildlife conflict"))
                              # "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2))) # only use these if using conflict model


# Prep Dist to PA Plot ----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = seq_range(dist2pa, n=300),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, # changing to add_elpd_draws from add_fitted_draws
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2pa_un <- (postdraws$dist2pa * attributes(cougar.conflict.df.scl$dist2pa)[[3]])+attributes(cougar.conflict.df.scl$dist2pa)[[2]]

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
saveRDS(dist2pa.plot.w, "data/processed/cougar_dist2pa_mixe_plot.rds")


# Prep Human Density Plot: ----------------------------------------------------


simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = seq_range(humandens, n=300),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod,
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$humandens <- (postdraws$humandens * attributes(cougar.conflict.df.scl$humandens)[[3]])+attributes(cougar.conflict.df.scl$humandens)[[2]]

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
saveRDS(pop.dens.plot, "data/processed/cougar_popdens_mixe_plot.rds")


# Prep Livestock plot: ----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = seq_range(livestockOps, n=300),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$livestockOps_un <- (postdraws$livestockOps * attributes(cougar.conflict.df.scl$livestockOps)[[3]])+attributes(cougar.conflict.df.scl$livestockOps)[[2]]

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
saveRDS(livestockOps.plot.w, "data/processed/cougar_livestockOps_mixe_plot.rds")


# Prep Row Crop Plot: -----------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = seq_range(rowcropOps, n=300),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$rowcropOps_un <- (postdraws$rowcropOps * attributes(cougar.conflict.df.scl$rowcropOps)[[3]])+attributes(cougar.conflict.df.scl$rowcropOps)[[2]]

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
saveRDS(rowcropOps.plot.w, "data/processed/cougar_rowcrops_mixe_plot.rds")

# Prep Connectivity Plot: -------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = seq_range(connectivity, n=300),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$connectivity_un <- (postdraws$connectivity * attributes(cougar.conflict.df.scl$connectivity)[[3]])+attributes(cougar.conflict.df.scl$connectivity)[[2]]

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
saveRDS(connectivity.plot, "data/processed/cougar_connectivity_mixe_plot.rds")

# Prep Ungulate density Plot: ---------------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = seq_range(ungulate_dens, n=300),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ungulate_dens <- (postdraws$ungulate_dens * attributes(cougar.conflict.df.scl$ungulate_dens)[[3]]) + attributes(cougar.conflict.df.scl$ungulate_dens)[[2]]

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
saveRDS(ungulate.plot.w, "data/processed/cougar_ungulate_density_mixe_plot.rds")


# Prep WHS Plot -----------------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = seq_range(habsuit, n=300),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$habsuit_un <- (postdraws$habsuit * attributes(cougar.conflict.df.scl$habsuit)[[3]])+attributes(cougar.conflict.df.scl$habsuit)[[2]]

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
saveRDS(habsuit.plot, "data/processed/cougar_bhs_mixe_plot.rds")

# Prep gHM Plot -----------------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = seq_range(gHM, n=300),
                    cougarincrease = mean(cougarincrease),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ghm <- (postdraws$gHM * attributes(cougar.conflict.df.scl$gHM)[[3]])+attributes(cougar.conflict.df.scl$gHM)[[2]]

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
saveRDS(human.mod.plot, "data/processed/cougar_gHM_mixe_plot.rds")

# Prep cougar inc Plot -----------------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = seq_range(cougarincrease, n=300),
                    roaddens = mean(roaddens),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$cougarinc <- (postdraws$cougarinc * attributes(cougar.conflict.df.scl$cougarinc)[[3]])+attributes(cougar.conflict.df.scl$cougarinc)[[2]]

# Plot Pop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(cougarinc, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
cougar.increase.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = cougarinc, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=cougarinc, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Bear Conflict") + 
  xlab("Public Support of cougar Population Increase (%)")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.wackground = element_rect(fill = "white", colour = "grey50"))
saveRDS(cougar.increase.plot, "data/processed/cougar_increase_mixe_plot.rds")

# Prep road dens Plot -----------------------------------------------------------

simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    habsuit = mean(habsuit),
                    gHM = mean(gHM),
                    cougarincrease = mean(cougarincrease),
                    roaddens = seq_range(roaddens, n=300),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$roaddens <- (postdraws$roaddens * attributes(cougar.conflict.df.scl$roaddens)[[3]])+attributes(cougar.conflict.df.scl$roaddens)[[2]]

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
saveRDS(road.dens.plot, "data/processed/cougar_road_density_mixe_plot.rds")


# Add Plots together:
biophys.p <-  connectivity.plot + habsuit.plot + dist2pa.plot.w + ungulate.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p <-  livestockOps.plot.w + rowcropOps.plot.w + human.mod.plot + road.dens.plot + cougar.increase.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

cougar.plot.all <- connectivity.plot + habsuit.plot + dist2pa.plot.w + ungulate.plot + livestockOps.plot.w + rowcropOps.plot.w + human.mod.plot + road.dens.plot + cougar.increase.plot + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')

saveRDS(biophys.p, "data/processed/biophys_cougar_conf_plots.rds")
saveRDS(social.p, "data/processed/social_cougar_conf_plots.rds")
saveRDS(cougar.plot.all, "data/processed/all_cougar_conf_plots.rds")

