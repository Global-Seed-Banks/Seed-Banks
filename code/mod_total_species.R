
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
sb_prep_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
    Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

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

sb_prep_area_zone <- sb_prep_area %>% filter(!Habitat_Broad == "Arable")
nrow(sb_prep_area_zone)
zones.plot <- cbind(sb_prep_area_zone, mr.zones$Estimate)
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
habs.plot <- cbind(sb_prep_area, mr.habs$Estimate)
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
deg.plot <- cbind(sb_prep_area, mr.deg$Estimate)
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
rich.area.zone_fitted <- cbind(rich.p_zones$data,
                             fitted(rich.p_zones, re_formula = NA
                             )) %>%
  as_tibble() %>% inner_join(sb_prep_area_zone %>% select(Total_Species, 
                                                    Total_Number_Samples, Total_Sample_Area_mm2,
                                                    log_Total_Sample_Area_mm2,
                                                  Biome_WWF_Zone),
                           #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich.area.zone_fitted)
nrow(rich.area.zone_fitted)

rich.area.habs_fitted <- cbind(rich.p_habs$data,
                               fitted(rich.p_habs, re_formula = NA
                               )) %>%
  as_tibble() %>% inner_join(sb_prep_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Habitat_Broad),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich.area.habs_fitted)
nrow(rich.area.habs_fitted)


summary(rich.p_i)

rich_deg_fitted <- cbind(rich_deg$data,
                            fitted(rich.p_i, re_formula = NA
                            )) %>%
  as_tibble() %>% inner_join(sb_prep_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
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
rich._deg_coef

 setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich_zone_fitted, rich_zone_fixef, file = 'rich_zone.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(rich_habs_fitted, rich_habs_fixef, file = 'rich_habs.mod_dat.Rdata')


setwd(paste0(path2wd, 'Data/'))
save(rich_deg_fitted, rich_deg_fixef, file = 'rich_deg.mod_dat.Rdata')


# plots
 setwd(paste0(path2wd, 'Data/'))
 #load('rich.area.poisson.mod_dat.Rdata')
 load('rich_zone.mod_dat.Rdata')

# plot richness area zone relationship


fig_rich.area.zone <- ggplot() + 
  #facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_prep_area_zone ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                # x = Total_Sample_Area_mm2,
                 y = Total_Species, colour = Biome_WWF_Zone,
                 ), 
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_zone_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
               # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Biome_WWF_Zone),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_zone_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                  #x =  Total_Sample_Area_mm2, 
                  ymin = Q2.5, ymax = Q97.5, fill = Biome_WWF_Zone),
              alpha = 0.3 ) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "WWF Zone", fill = "WWF Zone", title= "WWF Zones") + guides(col = guide_legend(ncol = 2))+
  #xlim(0,800)+ ylim(0,200)+
  scale_x_log10() + scale_y_log10() 

fig_rich.area.zone

# (Total_Sample_Area_mm2/1000000) =m2

# Habitat


setwd(paste0(path2wd, 'Data/'))
load('rich_habs.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_area)

fig_rich.area.habs <- ggplot() + 
 # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_prep_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich._habs_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Habitat_Broad),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_habs_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Broad),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "Habitats", fill = "Habitats", title = "Habitats") +  guides(col = guide_legend(ncol = 2)) +
# xlim(0,800)+ ylim(0,200)+
 scale_x_log10() + scale_y_log10() 

fig_rich.area.habs


setwd(paste0(path2wd, 'Data/'))
load('rich_deg.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_area)

fig_rich.area.i <- ggplot() + 
  # facet_wrap(~Habitat_Broad, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_prep_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                 # x = Total_Sample_Area_mm2,
                 y = Total_Species,
                 colour = Habitat_Degraded),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_deg_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
                # x = Total_Sample_Area_mm2,
                y = Estimate, colour = Habitat_Degraded),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_deg_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                   #x =  Total_Sample_Area_mm2, 
                   ymin = Q2.5, ymax = Q97.5,  fill = Habitat_Degraded),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "Degraded Habitat", fill = "Degraded Habitat", title = "Degraded Habitat")+  guides(col = guide_legend(ncol = 2)) +
  # xlim(0,800)+ ylim(0,200)+
 scale_x_log10() + scale_y_log10() 

fig_rich.area.i


(fig_rich.area.zone | fig_rich.area.habs | fig_rich.area.i)



# (Total_Sample_Area_mm2/1000000) =m2

# sample posteriors etc


View(rich.area.habs_fitted)


# global effects
rich.area.zone.fixed.p <- posterior_samples(rich.p_zones, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.area.zone.fixed.p)

# select columns of interests and give meaningful names
rich.area.zone_global_posterior <-  rich.area.zone.fixed.p %>% dplyr::select(`b_Intercept`,
                                                                    `b_log_Total_Sample_Area_mm2`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`) %>%
  mutate(rich.area.bor.global =`b_Intercept`,
         rich.med.global =`b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
         rich.temp.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
         rich.trop.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
         rich.tund.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`,
         rich.area.med.global = (`b_log_Total_Sample_Area_mm2` + `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`),
         rich.area.temp.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`),
         rich.area.trop.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`),
         rich.area.tund.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`),
  ) %>%
  dplyr::select(-c(`b_log_Total_Sample_Area_mm2`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`))

head(rich.area_global_posterior)

rich.area.bor.p <-  rich.area.zone_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(rich.area.bor.global),
          eff_lower = quantile(rich.area.bor.global, probs=0.025),
          eff_upper = quantile(rich.area.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.area.med.p <-  rich.area.zone_global_posterior %>% 
  mutate( response = "MediterraneanandDesert", eff = mean(rich.area.med.global),
          eff_lower = quantile(rich.area.med.global, probs=0.025),
          eff_upper = quantile(rich.area.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.area.temp.p <-  rich.area.zone_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(rich.area.temp.global),
          eff_lower = quantile(rich.area.temp.global, probs=0.025),
          eff_upper = quantile(rich.area.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.area.trop.p <-  rich.area.zone_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(rich.area.trop.global),
          eff_lower = quantile(rich.area.trop.global, probs=0.025),
          eff_upper = quantile(rich.area.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.area.tund.p <-  rich.area.zone_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.area.tund.global),
          eff_lower = quantile(rich.area.tund.global, probs=0.025),
          eff_upper = quantile(rich.area.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich.area.p <- bind_rows(rich.area.bor.p, rich.area.med.p,
                                rich.area.temp.p, rich.area.trop.p,
                                rich.area.tund.p)
head(global.rich.area.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich.area.p, file = 'global.rich.area.zone.poisson.posteriors.Rdata')


fig_rich.area_global_zones <- ggplot() + 
  geom_point(data = global.rich.area.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich.area.p, aes(x = response,ymin = eff_lower,
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

fig_rich.area_global_zones



