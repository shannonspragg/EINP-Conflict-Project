
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
cougar.mod.preds.plot <- plot(cougar.full.result, title = "Predictor Effects for Cougar Conflict")
cougar.mod.preds.plot
# this is the max probability of effect (MPE), showing the probability of a predictor having a positive or negative effect

cougar.coef.plot <- plot(cougar.full.mod, pars = c("dist2wetland","humandens",
                                              "edge_habitat",
                                              "pipeline_dens",
                                              "ungulatedens","road_dens", "gHM", "habsuit", "connectivity", "conflictprob"), main = "Predictor Effects for Black Cougar Conflict") # "cougarincrease",

saveRDS(cougar.mod.preds.plot, "data/processed/cougar_noconf_predsplot.rds")
saveRDS(cougar.coef.plot, "data/processed/cougar_coef_plot.rds")

# Plot results ------------------------------------------------------------

posterior <- as.matrix(cougar.full.mod)
parnames <- names(fixef(cougar.full.mod))[2:9] # change range based on model variables
p <- mcmc_intervals(posterior,
                    pars = parnames,
                    prob = 0.8) +
  scale_y_discrete(labels = c("dist2wetland" = "Dist. to Wetlands",
                              "humandens" = "Human Population Density",
                              "edge_habitat" = "Edge Habitats",
                              "pipeline_dens" = "Pipeline Density",
                              "ungulatedens" = "Ungulate Density",
                              "road_dens" = "Road Density",
                               "gHM" = "Human modification" ,
                              "habsuit" = "Cougar habitat suitability",
                            #  "cougarincrease" = "Public Support of cougar Population Increase",
                            "connectivity" = "Cougar Biophysical Connectivity",
                            "conflictprob" = "Prob of wildlife conflict",
                            "I(conflictprob^2)" = expression("Prob of wildlife conflict"^2))) # only use these if using conflict model


# Prep Dist to Wetland Plot ----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = seq_range(dist2wetland, n=300),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                 #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, # changing to add_elpd_draws from add_fitted_draws
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$dist2wetland <- (postdraws$dist2wetland * attributes(cougar.conflict.df.scl$dist2wetland)[[3]])+attributes(cougar.conflict.df.scl$dist2wetland)[[2]]

# Plot Dist to Wetland:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(dist2wetland, conflictprob) %>% # if using conflict model, this is conflictprob
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")

dist2wetland.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = dist2wetland, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=dist2wetland, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab("Distance to Wetland Areas (km)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(dist2wetland.plot, "data/processed/cougar_dist2wetland_mixe_plot.rds")


# Prep Human Density Plot: ----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = seq_range(humandens, n=300),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
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
pop.dens.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = humandens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=humandens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") +
  xlab("Human Population Density")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(pop.dens.plot.c, "data/processed/cougar_popdens_mixe_plot.rds")


# Prep Edge Habitat plot: ----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = seq_range(edge_habitat, n=300),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$edge_habitat <- (postdraws$edge_habitat * attributes(cougar.conflict.df.scl$edge_habitat)[[3]])+attributes(cougar.conflict.df.scl$edge_habitat)[[2]]

# Plot Edge habitat:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(edge_habitat, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
edge.hab.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = edge_habitat, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=edge_habitat, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab("Forest Edge Habitat Coverage")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(edge.hab.plot, "data/processed/cougar_edge_hab_mixe_plot.rds")


# Prep Pipeline Dens Plot: -----------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = seq_range(pipeline_dens, n=300),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$pipeline_dens <- (postdraws$pipeline_dens * attributes(cougar.conflict.df.scl$pipeline_dens)[[3]])+attributes(cougar.conflict.df.scl$pipeline_dens)[[2]]

# Plot Pipeline Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(pipeline_dens, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
pipeline.dens.plot <- ggplot(data=plot.df) +
  geom_line(aes(x = pipeline_dens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=pipeline_dens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab(expression("Density of Pipelines per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(pipeline.dens.plot, "data/processed/cougar_pipeline_dens_mixe_plot.rds")


# Prep Ungulate density Plot: ---------------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = seq_range(ungulatedens, n=300),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$ungulatedens <- (postdraws$ungulatedens * attributes(cougar.conflict.df.scl$ungulatedens)[[3]]) + attributes(cougar.conflict.df.scl$ungulatedens)[[2]]

# Plot Ungulate Density:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(ungulatedens, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
ungulate.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = ungulatedens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=ungulatedens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab(expression("Ungulate Population Density per"~km^{2}))+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(ungulate.plot.c, "data/processed/cougar_ungulate_density_mixe_plot.rds")

# Prep road dens Plot -----------------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = seq_range(road_dens, n = 300),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$road_dens <- (postdraws$road_dens * attributes(cougar.conflict.df.scl$road_dens)[[3]])+attributes(cougar.conflict.df.scl$road_dens)[[2]]

# Plot Pop Dens:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(road_dens, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
road.dens.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = road_dens, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=road_dens, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab(expression("Road Density per"~km^{2}))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(road.dens.plot.c, "data/processed/cougar_road_density_mixe_plot.rds")


# Prep gHM Plot -----------------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = seq_range(gHM, n=300),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
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
human.mod.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = gHM, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=gHM, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab("Degree of Human Modification (gHM)")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(human.mod.plot.c, "data/processed/cougar_gHM_mixe_plot.rds")

# Prep CHS Plot -----------------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = seq_range(habsuit, n=300),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = mean(connectivity),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$habsuit <- (postdraws$habsuit * attributes(cougar.conflict.df.scl$habsuit)[[3]])+attributes(cougar.conflict.df.scl$habsuit)[[2]]

#Plot CHS:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(habsuit, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
habsuit.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = habsuit, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=habsuit, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab("Predicted Cougar Habitat Suitability")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(habsuit.plot.c, "data/processed/cougar_chs_mixe_plot.rds")

# Prep Connectivity Plot: -------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2wetland = mean(dist2wetland),
                    humandens = mean(humandens),
                    edge_habitat = mean(edge_habitat),
                    pipeline_dens = mean(pipeline_dens),
                    ungulatedens = mean(ungulatedens),
                    road_dens = mean(road_dens),
                    gHM = mean(gHM),
                    habsuit = mean(habsuit),
                    #   cougarincrease = mean(cougarincrease),
                    connectivity = seq_range(connectivity, n=300),
                    conflictprob = quantile(cougar.conflict.df.scl$conflictprob, probs = c(0.1, 0.5, 0.9)))

postdraws <- tidybayes::add_epred_draws(cougar.full.mod, 
                                        newdata=simdata,
                                        ndraws=1000,
                                        re_formula=NA)

postdraws$connectivity <- (postdraws$connectivity * attributes(cougar.conflict.df.scl$connectivity)[[3]])+attributes(cougar.conflict.df.scl$connectivity)[[2]]

# Plot Biophys Current:
plot.df <- postdraws %>% 
  mutate_at(., vars(conflictprob), as.factor) %>% 
  group_by(connectivity, conflictprob) %>% 
  summarise(., mean = mean(.epred),
            lo = quantile(.epred, 0.2),
            hi = quantile(.epred, 0.8))

levels(plot.df$conflictprob) <-  c("Lower 10%", "Mean", "Upper 10%")
connectivity.plot.c <- ggplot(data=plot.df) +
  geom_line(aes(x = connectivity, y = mean, colour =conflictprob), lwd=1.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi, x=connectivity, fill = conflictprob), alpha = 0.2) +
  scale_colour_viridis(discrete = "TRUE", option="C","General Conflict Prob.")+
  scale_fill_viridis(discrete = "TRUE", option="C", "General Conflict Prob.") +
  ylab("Probability of Cougar Conflict") + 
  xlab("Cumulative Current Flow (Amperes)")+
  # guides(fill=guide_legend(title="Population Density"))+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(connectivity.plot.c, "data/processed/cougar_connectivity_mixe_plot.rds")

# Prep cougar inc Plot -----------------------------------------------------------
simdata <- cougar.conflict.df.scl %>%
  modelr::data_grid(dist2pa = mean(dist2pa),
                    humandens = mean(humandens),
                    livestockOps = mean(livestockOps),
                    rowcropOps = mean(rowcropOps),
                    connectivity = mean(connectivity),
                    ungulate_dens = mean(ungulate_dens),
                    road_dens = mean(road_dens),
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

# Plot Cougar Inc:
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
  ylab("Probability of Cougar Conflict") + 
  xlab("Public Support of cougar Population Increase (%)")+
  theme(text=element_text(size=12,  family="Times New Roman"), legend.text = element_text(size=10),panel.background = element_rect(fill = "white", colour = "grey50"))
saveRDS(cougar.increase.plot, "data/processed/cougar_increase_mixe_plot.rds")


# Add Plots together:
biophys.p.c <-  dist2wetland.plot + edge.hab.plot + habsuit.plot.c + ungulate.plot.c + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect')         

social.p.c <-  human.mod.plot.c + pop.dens.plot.c + pipeline.dens.plot + road.dens.plot.c + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect') # + cougar.increase.plot

cougar.plot.all <- dist2wetland.plot + edge.hab.plot + habsuit.plot.c + ungulate.plot.c +  human.mod.plot.c + pop.dens.plot.c + pipeline.dens.plot + road.dens.plot.c + plot_annotation(tag_levels = 'a', tag_suffix = ")") +  plot_layout(guides = 'collect') # + cougar.increase.plot

saveRDS(biophys.p.c, "data/processed/biophys_cougar_conf_plots.rds")
saveRDS(social.p.c, "data/processed/social_cougar_conf_plots.rds")
saveRDS(cougar.plot.all, "data/processed/all_cougar_conf_plots.rds")

