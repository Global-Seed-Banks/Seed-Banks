
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

# remove NA values 
sb_seeds_area <- sb_prep %>% filter(!is.na(Total_Seeds),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

nrow(sb_seeds_area)

sb_seeds_area_zone <- sb_seeds_area %>% filter(!Habitat_Broad == "Arable")

nrow(sb_seeds_area_zone)


# rich_zones <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (1 | Method/studyID/rowID ),
#                   family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
#                   control = list(adapt_delta = 0.99,
#                                  max_treedepth = 12)
# )


# rich_habs <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Habitat_Broad + ( 1 | Method/studyID/rowID ),
#                  family = poisson(), data = sb, cores = 4, chains = 4, iter = 3000, warmup =1000,
#                  control = list(adapt_delta = 0.99) )

# rich_deg <- brm(Total_Species ~ log_Total_Sample_Area_mm2 *  Habitat_Degraded  + ( 1 | Method/studyID/rowID ) ,
#                 family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 3000, warmup =1000,
#                 control = list(adapt_delta = 0.99))
# 

head(sb_prep_area)
nrow(sb_prep_area)


setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'seed_zone.Rdata')
load( 'seed_hab.Rdata')
load( 'seed_deg.Rdata')


# seeds_zones
# seeds_habs
# seeds_deg

# model summary
summary(seeds_zones)
summary(seeds_habs) 
summary(seeds_deg)


# posterior predictive check
color_scheme_set("darkgray")
pp_seed.zone <- pp_check(seeds_zones)+ xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values


pp_seed.habs <- pp_check(seeds_habs)+ xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") +  xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values


pp_seed.deg <- pp_check(seeds_deg)+ xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") +  xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

(pp_seed.zone | pp_seed.habs |  pp_seed.deg)


# caterpillars/chains
plot(seeds_zones)
plot(seeds_habs)
plot(seeds_deg)

# # check model residuals
# zones
mr.zones <- residuals(seeds_zones)
mr.zones <- as.data.frame(mr.zones)
nrow(mr.zones)

nrow(sb_prep_area_zone)
zones.plot <- cbind(sb_seeds_area_zone, mr.zones$Estimate)
head(zones.plot)
#mr.zones make sure they are factors
zones.plot$Biome_WWF_Zone <- as.factor(zones.plot$Biome_WWF_Zone )
zones.plot$Method <- as.factor(zones.plot$Method )
zones.plot$studyID <- as.factor(zones.plot$studyID )
zones.plot$rowID <- as.factor(zones.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(zones.plot, plot(Biome_WWF_Zone, mr.zones$Estimate))
with(zones.plot, plot(Method, mr.zones$Estimate))
with(zones.plot, plot(studyID, mr.zones$Estimate))
with(zones.plot, plot(rowID, mr.zones$Estimate))


# and habs
mr.habs <- residuals(seeds_habs)
mr.habs <- as.data.frame(mr.habs)
nrow(mr.habs)
nrow(sb_prep_area)
habs.plot <- cbind(sb_seeds_area, mr.habs$Estimate)
habs.plot$Habitat_Broad <- as.factor(habs.plot$Habitat_Broad )
habs.plot$Method <- as.factor(habs.plot$Method )
habs.plot$studyID <- as.factor(habs.plot$studyID )
habs.plot$rowID <- as.factor(habs.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(habs.plot, plot(Habitat_Broad, mr.habs$Estimate))
with(habs.plot, plot(Method, mr.habs$Estimate))
with(habs.plot, plot(studyID, mr.habs$Estimate))
with(habs.plot, plot(rowID, mr.habs$Estimate))

# and deg
mr.deg <- residuals(seeds_deg)
mr.deg <- as.data.frame(mr.deg)
nrow(mr.deg)
nrow(sb_prep_area)
deg.plot <- cbind(sb_seeds_area, mr.deg$Estimate)
deg.plot$Habitat_Degraded <- as.factor(deg.plot$Habitat_degraded )
deg.plot$Method <- as.factor(deg.plot$Method )
deg.plot$studyID <- as.factor(deg.plot$studyID )
deg.plot$rowID <- as.factor(deg.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(deg.plot, plot(Habitat_Degraded, mr.deg$Estimate))
with(deg.plot, plot(Method, mr.deg$Estimate))
with(deg.plot, plot(studyID, mr.deg$Estimate))
with(deg.plot, plot(rowID, mr.deg$Estimate))

head(sb_prep_area)

# WWF ZONES model
# # for plotting fixed effects
seeds_zone_fitted <- cbind(seeds_zones$data,
                          fitted(seeds_zones, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_seeds_area_zone %>% select(Total_Species, 
                                                          Total_Number_Samples, Total_Sample_Area_mm2,
                                                          log_Total_Sample_Area_mm2,
                                                          Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                          Centred_log_Total_Sample_Area_m2,
                                                          Biome_WWF_Zone),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(seeds_zone_fitted)
nrow(seeds_zone_fitted)

seeds_habs_fitted <- cbind(seeds_habs$data,
                          fitted(seeds_habs, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_seeds_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                     Centred_log_Total_Sample_Area_m2,
                                                     Habitat_Broad),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(seeds_habs_fitted)
nrow(seeds_habs_fitted)


summary(seeds_deg)

seeds_deg_fitted <- cbind(seeds_deg$data,
                         fitted(seeds_deg, re_formula = NA
                         )) %>%
  as_tibble() %>% inner_join(sb_seeds_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                     Centred_log_Total_Sample_Area_m2,
                                                     Habitat_Degraded),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(seeds_deg_fitted)
nrow(seeds_deg_fitted)



# fixed effect coefficients
seeds_zone_fixef <- fixef(seeds_zones)
head(seeds_zone_fixef)

# Random effect coefficients
seeds_zone_coef <- coef(seeds_zones)
seeds_zone_coef # dont really need this



# fixed effect coefficients
seeds_habs_fixef <- fixef(seeds_habs)
head(seeds_habs_fixef)

# Random effect coefficients
seeds_habs_coef <- coef(seeds_habs)
seeds_habs_coef



# fixed effect coefficients
seeds_deg_fixef <- fixef(seeds_deg)
head(seeds_deg_fixef)

# Random effect coefficients
seeds_deg_coef <- coef(seeds_deg)
seeds_deg_coef

setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(seeds_zone_fitted, seeds_zone_fixef, seeds_zone_coef, file = 'seeds_zone.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(seeds_habs_fitted, seeds_habs_fixef, seeds_habs_coef, file = 'seeds_habs.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(seeds_deg_fitted, seeds_deg_fixef, seeds_deg_fixef, file = 'seeds_deg.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('seeds.area.poisson.mod_dat.Rdata')
load('seeds_zone.mod_dat.Rdata')

# plot seedsness area zone relationship
seeds_zones

seed_zone_c <- conditional_effects(seeds_zones, effects = 'Centred_log_Total_Sample_Area_m2:Biome_WWF_Zone', re_formula = NA, method = 'fitted')  # conditional effects

seed_zone_c


fig_seeds.zone <- ggplot() + 
  #facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seeds_area_zone ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Seeds, colour = Biome_WWF_Zone,
             ), 
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seeds_zone_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Biome_WWF_Zone),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = seeds_zone_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5, fill = Biome_WWF_Zone),
              alpha = 0.3 ) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Seeds",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "WWF Zone", fill = "WWF Zone", subtitle= "") + guides(col = guide_legend(ncol = 2)) +
#xlim(0,800)+ 
#  coord_cartesian( ylim = c(0,530000))# +
scale_x_log10() + scale_y_log10() 

fig_seeds.zone

# (Total_Sample_Area_mm2/1000000) =m2

# Habitat


setwd(paste0(path2wd, 'Data/'))
load('seeds_habs.mod_dat.Rdata')

# plot seedsness area relationship
colnames(sb_prep_area)

fig_seeds.habs <- ggplot() + 
  # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seeds_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Seeds,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seeds_habs_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Habitat_Broad),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = seeds_habs_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Broad),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "Habitats", fill = "Habitats", subtitle = "") +  guides(col = guide_legend(ncol = 2)) +
# xlim(0,800)+ ylim(0,200)+
scale_x_log10() + scale_y_log10() 

fig_seeds.habs


setwd(paste0(path2wd, 'Data/'))
load('seeds_deg.mod_dat.Rdata')

# plot seedsness area relationship
colnames(sb_prep_area)

fig_seeds.deg <- ggplot() + 
  # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seeds_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Seeds,
                 colour = Habitat_Degraded),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seeds_deg_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Habitat_Degraded),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = seeds_deg_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Degraded),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "Degraded Habitat", fill = "Degraded Habitat", subtitle = "")+  guides(col = guide_legend(ncol = 2)) +
# xlim(0,800)+ ylim(0,200)+
scale_x_log10() + scale_y_log10() 

fig_seeds.deg


Almost_Total_Seeds_Fig <- (fig_seeds.zone | fig_seeds.habs | fig_seeds.deg)


Total_Seeds_Fig <- (Almost_Total_Seeds_Fig) + plot_annotation(title = "Total Seeds",
                  theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=3)


Total_Seeds_Fig


(Total_Species_Fig / Total_Seeds_Fig)



# global effects
seeds_zone.fixed.p <- as_draws_df(seeds_zones, subset = floor(runif(n = 1000, 1, max = 2000)))

seeds_zone.fixed.p
nrow(seeds_zone.fixed.p)
head(seeds_zone.fixed.p)
colnames(seeds_zone.fixed.p)

# select columns of interests and give meaningful names
seeds_zone_global_posterior <-  seeds_zone.fixed.p %>% 
  as_tibble() %>%
  mutate(seeds.bor.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seeds.med.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneMediterraneanandDesert`),
         seeds.temp.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTemperate`),
         seeds.trop.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTropical`),
         seeds.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTundra`),
  ) %>%
  dplyr::select(c(seeds.bor.global, seeds.med.global, seeds.temp.global, seeds.trop.global, seeds.tund.global ))

View(seeds_zone_global_posterior)
head(seeds_zone_global_posterior)

seeds.bor.p <-  seeds_zone_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(seeds.bor.global),
          eff_lower = quantile(seeds.bor.global, probs=0.025),
          eff_upper = quantile(seeds.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
seeds.med.p <-  seeds_zone_global_posterior %>% 
  mutate( response = "MediterraneanandDesert", eff = mean(seeds.med.global),
          eff_lower = quantile(seeds.med.global, probs=0.025),
          eff_upper = quantile(seeds.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
seeds.temp.p <-  seeds_zone_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(seeds.temp.global),
          eff_lower = quantile(seeds.temp.global, probs=0.025),
          eff_upper = quantile(seeds.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
seeds.trop.p <-  seeds_zone_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(seeds.trop.global),
          eff_lower = quantile(seeds.trop.global, probs=0.025),
          eff_upper = quantile(seeds.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
seeds.tund.p <-  seeds_zone_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(seeds.tund.global),
          eff_lower = quantile(seeds.tund.global, probs=0.025),
          eff_upper = quantile(seeds.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.seeds_zone.p <- bind_rows(seeds.bor.p, seeds.med.p,
                                seeds.temp.p, seeds.trop.p,
                                seeds.tund.p)
head(global.seeds_zone.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seeds_zone.p, file = 'global.seeds_zone.posteriors.Rdata')


fig_seeds_zone_global <- ggplot() + 
  geom_point(data = global.seeds_zone.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.seeds_zone.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_seeds_zone_global


# habs
seeds_hab.fixed.p <- as_draws_df(seeds_habs, subset = floor(runif(n = 1000, 1, max = 2000)))

seeds_hab.fixed.p
nrow(seeds_zone.fixed.p)
head(seeds_zone.fixed.p)
colnames(seeds_hab.fixed.p)

# select columns of interests and give meaningful names
seeds_hab_global_posterior <-  seeds_hab.fixed.p %>% 
  as_tibble() %>%
  mutate(seeds.aquatic.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seeds.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadArable`),
         seeds.forest.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadForest`),
         seeds.grassland.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadGrassland`),
         seeds.wetland.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadWetland`),
  ) %>%
  dplyr::select(c(seeds.aquatic.global, seeds.arable.global, seeds.forest.global, seeds.grassland.global, seeds.wetland.global ))

head(seeds_hab_global_posterior)

seeds.aquatic.p <-  seeds_hab_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(seeds.aquatic.global),
          eff_lower = quantile(seeds.aquatic.global, probs=0.025),
          eff_upper = quantile(seeds.aquatic.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
seeds.arable.p <-  seeds_hab_global_posterior %>% 
  mutate( response = "Arable", eff = mean(seeds.arable.global),
          eff_lower = quantile(seeds.arable.global, probs=0.025),
          eff_upper = quantile(seeds.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
seeds.forest.p <-  seeds_hab_global_posterior %>% 
  mutate( response = "Forest", eff = mean(seeds.forest.global),
          eff_lower = quantile(seeds.forest.global, probs=0.025),
          eff_upper = quantile(seeds.forest.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
seeds.grassland.p <-  seeds_hab_global_posterior %>% 
  mutate( response = "Grassland", eff = mean(seeds.grassland.global),
          eff_lower = quantile(seeds.grassland.global, probs=0.025),
          eff_upper = quantile(seeds.grassland.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
seeds.wetland.p <-  seeds_hab_global_posterior %>% 
  mutate( response = "Wetland", eff = mean(seeds.wetland.global),
          eff_lower = quantile(seeds.wetland.global, probs=0.025),
          eff_upper = quantile(seeds.wetland.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.seeds_hab.p <- bind_rows(seeds.aquatic.p, seeds.arable.p,
                               seeds.forest.p, seeds.grassland.p,
                               seeds.wetland.p)
head(global.seeds_hab.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seeds_hab.p, file = 'global.seeds_hab.posteriors.Rdata')


fig_seeds_hab_global <- ggplot() + 
  geom_point(data = global.seeds_hab.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.seeds_hab.p, aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_seeds_hab_global



# degraded habs
seeds_deg.fixed.p <- as_draws_df(seeds_deg, subset = floor(runif(n = 1000, 1, max = 2000)))

seeds_deg.fixed.p
nrow(seeds_zone.fixed.p)
head(seeds_zone.fixed.p)
colnames(seeds_deg.fixed.p)

# select columns of interests and give meaningful names
seeds_deg_global_posterior <-  seeds_deg.fixed.p %>% 
  as_tibble() %>%
  mutate(seeds.notdeg.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seeds.deg.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Habitat_Degraded1`),
  ) %>%
  dplyr::select(c(seeds.notdeg.global, seeds.deg.global))

head(seeds_deg_global_posterior)

seeds.notdeg.p <-  seeds_deg_global_posterior %>% 
  mutate( response = "Not Degraded", eff = mean(seeds.notdeg.global),
          eff_lower = quantile(seeds.notdeg.global, probs=0.025),
          eff_upper = quantile(seeds.notdeg.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seeds.deg.p <-  seeds_deg_global_posterior %>% 
  mutate( response = "Degraded", eff = mean(seeds.deg.global),
          eff_lower = quantile(seeds.deg.global, probs=0.025),
          eff_upper = quantile(seeds.deg.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.seeds_deg.p <- bind_rows(seeds.notdeg.p, seeds.deg.p)
head(global.seeds_deg.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seeds_deg.p, file = 'global.seeds_deg.posteriors.Rdata')


fig_seeds_deg_global <- ggplot() + 
  geom_point(data = global.seeds_deg.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.seeds_deg.p, aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_seeds_deg_global


Total_Seeds_Eff <- (fig_seeds_zone_global | fig_seeds_hab_global | fig_seeds_deg_global)


Total_Species_Eff / Total_Seeds_Eff



