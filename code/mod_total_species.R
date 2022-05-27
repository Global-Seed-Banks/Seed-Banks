
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
sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
    Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)

sb_rich_area_zone <- sb_rich_area %>% filter(!Habitat_Broad == "Arable")


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




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_zone.Rdata')
load( 'rich_hab.Rdata')
load( 'rich_deg.Rdata')


# rich_zones
# rich_habs
# rich_deg

# model summary
summary(rich_zones)
summary(rich_habs) 
summary(rich_deg)


# posterior predictive check
color_scheme_set("darkgray")
pp_rich.zone <- pp_check(rich_zones)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values


pp_rich.habs <- pp_check(rich_habs)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +   xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values


pp_rich.deg <- pp_check(rich_deg)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +  xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

(pp_rich.zone | pp_rich.habs |  pp_rich.deg)



# caterpillars/chains
plot(rich_zones)
plot(rich_habs)
plot(rich_deg)

# # check model residuals
# zones
mr.zones <- residuals(rich_zones)
mr.zones <- as.data.frame(mr.zones)
nrow(mr.zones)


nrow(sb_prep_area_zone)
zones.plot <- cbind(sb_rich_area_zone, mr.zones$Estimate)
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
mr.habs <- residuals(rich_habs)
mr.habs <- as.data.frame(mr.habs)
nrow(mr.habs)
nrow(sb_prep_area)
habs.plot <- cbind(sb_rich_area, mr.habs$Estimate)
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
mr.deg <- residuals(rich_deg)
mr.deg <- as.data.frame(mr.deg)
nrow(mr.deg)
nrow(sb_prep_area)
deg.plot <- cbind(sb_rich_area, mr.deg$Estimate)
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
rich_zone_fitted <- cbind(rich_zones$data,
                             fitted(rich_zones, re_formula = NA
                             )) %>%
  as_tibble() %>% inner_join(sb_rich_area_zone %>% select(Total_Species, 
                                                    Total_Number_Samples, Total_Sample_Area_mm2,
                                                    log_Total_Sample_Area_mm2,
                                                    Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                    Centred_log_Total_Sample_Area_m2,
                                                  Biome_WWF_Zone),
                           #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich_zone_fitted)
nrow(rich_zone_fitted)

rich_habs_fitted <- cbind(rich_habs$data,
                               fitted(rich_habs, re_formula = NA
                               )) %>%
  as_tibble() %>% inner_join(sb_rich_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                     Centred_log_Total_Sample_Area_m2,
                                                     Habitat_Broad),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich_habs_fitted)
nrow(rich_habs_fitted)


summary(rich_deg)

rich_deg_fitted <- cbind(rich_deg$data,
                            fitted(rich_deg, re_formula = NA
                            )) %>%
  as_tibble() %>% inner_join(sb_rich_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                     Centred_log_Total_Sample_Area_m2,
                                                     Habitat_Degraded),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich_deg_fitted)
nrow(rich_deg_fitted)



# fixed effect coefficients
rich_zone_fixef <- fixef(rich_zones)
head(rich_zone_fixef)

# Random effect coefficients
rich_zone_coef <- coef(rich_zones)
rich_zone_coef # dont really need this



# fixed effect coefficients
rich_habs_fixef <- fixef(rich_habs)
head(rich_habs_fixef)

# Random effect coefficients
rich_habs_coef <- coef(rich_habs)
rich_habs_coef



# fixed effect coefficients
rich_deg_fixef <- fixef(rich_deg)
head(rich_deg_fixef)

# Random effect coefficients
rich_deg_coef <- coef(rich_deg)
rich_deg_coef

 setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich_zone_fitted, rich_zone_fixef, rich_zone_coef, file = 'rich_zone.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(rich_habs_fitted, rich_habs_fixef, rich_habs_coef, file = 'rich_habs.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(rich_deg_fitted, rich_deg_fixef, rich_deg_fixef, file = 'rich_deg.mod_dat.Rdata')


# plots
 setwd(paste0(path2wd, 'Data/'))
 #load('rich.area.poisson.mod_dat.Rdata')
 load('rich_zone.mod_dat.Rdata')

# plot richness area zone relationship

fig_rich.zone <- ggplot() + 
  #facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area_zone ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
                # x = Total_Sample_Area_mm2,
                 x = Total_Sample_Area_m2,
                 y = Total_Species, colour = Biome_WWF_Zone,
                 ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_zone_fitted,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
               # x = Total_Sample_Area_mm2,
              x = Total_Sample_Area_m2,
                y = Estimate, colour = Biome_WWF_Zone),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_zone_fitted,
              aes( #x =  (Total_Sample_Area_mm2/1000000), 
                  #x =  Total_Sample_Area_mm2, 
                x = Total_Sample_Area_m2,
                  ymin = Q2.5, ymax = Q97.5, fill = Biome_WWF_Zone),
              alpha = 0.1 ) +
  #coord_cartesian(xlim = c(min(sb_rich_area_zone$Total_Sample_Area_m2), quantile(sb_rich_area_zone$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Zone", fill = "WWF Zone", subtitle= "") + guides(col = guide_legend(nrow = 2))# +
  #xlim(0,800)+ ylim(0,200)+
  #scale_x_log10() + scale_y_log10() 

fig_rich.zone

# (Total_Sample_Area_mm2/1000000) =m2

# Habitat


setwd(paste0(path2wd, 'Data/'))
load('rich_habs.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_area)

fig_rich.habs <- ggplot() + 
 # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_habs_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Habitat_Broad),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_habs_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Broad),
              alpha = 0.1) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  #coord_cartesian(xlim = c(min(sb_rich_area$Total_Sample_Area_m2), quantile(sb_rich_area$Total_Sample_Area_m2, 0.997))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "",  x = expression(paste('Total Sample Area ' , m^2)),
       #x="",
       color = "Habitats", fill = "Habitats", subtitle = "") +  guides(col = guide_legend(ncol = 2))# +
# xlim(0,800)+ ylim(0,200)+
 #scale_x_log10() + scale_y_log10() 

fig_rich.habs


setwd(paste0(path2wd, 'Data/'))
load('rich_deg.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_area)

fig_rich.deg <- ggplot() + 
  # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
                  x = Total_Sample_Area_m2,
                 y = Total_Species,
                 colour = Habitat_Degraded),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_deg_fitted,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
                x = Total_Sample_Area_m2,
                y = Estimate, colour = Habitat_Degraded),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_deg_fitted,
              aes(#x =  (Total_Sample_Area_mm2/1000000), 
                   x =  Total_Sample_Area_m2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Degraded),
              alpha = 0.1) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
    #coord_cartesian(xlim = c(min(sb_rich_area$Total_Sample_Area_m2), quantile(sb_rich_area$Total_Sample_Area_m2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "",  #x = expression(paste('Total Sample Area ' , m^2)),
       x= "",
       color = "Degraded Habitat", fill = "Degraded Habitat", subtitle = "")+  guides(col = guide_legend(ncol = 2)) #+
  # xlim(0,800)+ ylim(0,200)+
 #scale_x_log10() + scale_y_log10() 

fig_rich.deg


Almost_Total_Species_Fig <- (fig_rich.zone | fig_rich.habs | fig_rich.deg)


Total_Species_Fig <- (Almost_Total_Species_Fig) + plot_annotation(title = "Total Species",
                                                              theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=3)


Total_Species_Fig


# (Total_Sample_Area_mm2/1000000) =m2


# conditional effects
rich_zone_c <- conditional_effects(rich_zones, effects = 'Centred_log_Total_Sample_Area_m2:Biome_WWF_Zone', re_formula = NA, method = 'fitted')  # conditional effects

rich_zone_c

# global effects
rich_zone.fixed.p <- as_draws_df(rich_zones, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_zone.fixed.p
nrow(rich_zone.fixed.p)
head(rich_zone.fixed.p)
colnames(rich_zone.fixed.p)

# select columns of interests and give meaningful names
rich_zone_global_posterior <-  rich_zone.fixed.p %>% 
  as_tibble() %>%
  mutate(rich.bor.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         rich.med.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneMediterraneanandDesert`),
         rich.temp.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTemperate`),
         rich.trop.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTropical`),
         rich.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWF_ZoneTundra`),
  ) %>%
  dplyr::select(c(rich.bor.global, rich.med.global, rich.temp.global, rich.trop.global, rich.tund.global ))

 View(rich_zone_global_posterior)
head(rich_zone_global_posterior)

rich.bor.p <-  rich_zone_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(rich.bor.global),
          eff_lower = quantile(rich.bor.global, probs=0.025),
          eff_upper = quantile(rich.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.med.p <-  rich_zone_global_posterior %>% 
  mutate( response = "Mediterranean and Desert", eff = mean(rich.med.global),
          eff_lower = quantile(rich.med.global, probs=0.025),
          eff_upper = quantile(rich.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.temp.p <-  rich_zone_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(rich.temp.global),
          eff_lower = quantile(rich.temp.global, probs=0.025),
          eff_upper = quantile(rich.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.trop.p <-  rich_zone_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(rich.trop.global),
          eff_lower = quantile(rich.trop.global, probs=0.025),
          eff_upper = quantile(rich.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.tund.p <-  rich_zone_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.tund.global),
          eff_lower = quantile(rich.tund.global, probs=0.025),
          eff_upper = quantile(rich.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich_zone.p <- bind_rows(rich.bor.p, rich.med.p,
                                rich.temp.p, rich.trop.p,
                                rich.tund.p)
head(global.rich_zone.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_zone.p, file = 'global.rich_zone.posteriors.Rdata')

head(global.rich_zone.p)

fig_rich_zone_global <- ggplot() + 
  geom_point(data = global.rich_zone.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich_zone.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
   scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_rich_zone_global


# habs
rich_hab.fixed.p <- as_draws_df(rich_habs, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_hab.fixed.p
nrow(rich_zone.fixed.p)
head(rich_zone.fixed.p)
colnames(rich_hab.fixed.p)

# select columns of interests and give meaningful names
rich_hab_global_posterior <-  rich_hab.fixed.p %>% 
  as_tibble() %>%
  mutate(rich.aquatic.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         rich.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadArable`),
         rich.forest.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadForest`),
         rich.grassland.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadGrassland`),
         rich.wetland.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Habitat_BroadWetland`),
  ) %>%
  dplyr::select(c(rich.aquatic.global, rich.arable.global, rich.forest.global, rich.grassland.global, rich.wetland.global ))

head(rich_hab_global_posterior)

rich.aquatic.p <-  rich_hab_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(rich.aquatic.global),
          eff_lower = quantile(rich.aquatic.global, probs=0.025),
          eff_upper = quantile(rich.aquatic.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.arable.p <-  rich_hab_global_posterior %>% 
  mutate( response = "Arable", eff = mean(rich.arable.global),
          eff_lower = quantile(rich.arable.global, probs=0.025),
          eff_upper = quantile(rich.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.forest.p <-  rich_hab_global_posterior %>% 
  mutate( response = "Forest", eff = mean(rich.forest.global),
          eff_lower = quantile(rich.forest.global, probs=0.025),
          eff_upper = quantile(rich.forest.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.grassland.p <-  rich_hab_global_posterior %>% 
  mutate( response = "Grassland", eff = mean(rich.grassland.global),
          eff_lower = quantile(rich.grassland.global, probs=0.025),
          eff_upper = quantile(rich.grassland.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.wetland.p <-  rich_hab_global_posterior %>% 
  mutate( response = "Wetland", eff = mean(rich.wetland.global),
          eff_lower = quantile(rich.wetland.global, probs=0.025),
          eff_upper = quantile(rich.wetland.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich_hab.p <- bind_rows(rich.aquatic.p, rich.arable.p,
                                rich.forest.p, rich.grassland.p,
                                rich.wetland.p)
head(global.rich_hab.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_hab.p, file = 'global.rich_hab.posteriors.Rdata')


fig_rich_hab_global <- ggplot() + 
  geom_point(data = global.rich_hab.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich_hab.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich_hab_global



# degraded habs
rich_deg.fixed.p <- as_draws_df(rich_deg, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_deg.fixed.p
nrow(rich_zone.fixed.p)
head(rich_zone.fixed.p)
colnames(rich_deg.fixed.p)

# select columns of interests and give meaningful names
rich_deg_global_posterior <-  rich_deg.fixed.p %>% 
  as_tibble() %>%
  mutate(rich.notdeg.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         rich.deg.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Habitat_Degraded1`),
  ) %>%
  dplyr::select(c(rich.notdeg.global, rich.deg.global))

head(rich_deg_global_posterior)

rich.notdeg.p <-  rich_deg_global_posterior %>% 
  mutate( response = "0", eff = mean(rich.notdeg.global),
          eff_lower = quantile(rich.notdeg.global, probs=0.025),
          eff_upper = quantile(rich.notdeg.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.deg.p <-  rich_deg_global_posterior %>% 
  mutate( response = "1", eff = mean(rich.deg.global),
          eff_lower = quantile(rich.deg.global, probs=0.025),
          eff_upper = quantile(rich.deg.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich_deg.p <- bind_rows(rich.notdeg.p, rich.deg.p)
head(global.rich_deg.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_deg.p, file = 'global.rich_deg.posteriors.Rdata')


fig_rich_deg_global <- ggplot() + 
  geom_point(data = global.rich_deg.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich_deg.p, aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich_deg_global


Total_Species_Eff <- (fig_rich_zone_global | fig_rich_hab_global | fig_rich_deg_global)

Total_Species_Eff



Total_Species_Fig <- (Almost_Total_Species_Fig / Total_Species_Eff) + plot_annotation(title = "Total Species",
                                                          theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=1, nrow=2)

# LANDSCAPE 10 X 16
Total_Species_Fig



# inset slope effects
rich_zone <- fig_rich.zone +  annotation_custom(ggplotGrob(fig_rich_zone_global), xmin = 11, xmax = 15.75, 
                                        ymin = 70, ymax = 110)

rich_zone


rich_hab <- fig_rich.habs +  annotation_custom(ggplotGrob(fig_rich_hab_global), xmin = 11, xmax = 15.75, 
                                                ymin = 75, ymax = 108)

rich_hab


rich_deg <- fig_rich.deg +  annotation_custom(ggplotGrob(fig_rich_deg_global), xmin = 11, xmax = 15.75, 
                                               ymin = 75, ymax = 108)

rich_deg




