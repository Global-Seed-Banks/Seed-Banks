
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
rich_zone_fitted <- cbind(rich_zones$data,
                             fitted(rich_zones, re_formula = NA
                             )) %>%
  as_tibble() %>% inner_join(sb_prep_area_zone %>% select(Total_Species, 
                                                    Total_Number_Samples, Total_Sample_Area_mm2,
                                                    log_Total_Sample_Area_mm2,
                                                  Biome_WWF_Zone),
                           #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich_zone_fitted)
nrow(rich_zone_fitted)

rich_habs_fitted <- cbind(rich_habs$data,
                               fitted(rich_habs, re_formula = NA
                               )) %>%
  as_tibble() %>% inner_join(sb_prep_area %>% select(Total_Species, 
                                                     Total_Number_Samples, Total_Sample_Area_mm2,
                                                     log_Total_Sample_Area_mm2,
                                                     Habitat_Broad),
                             #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich_habs_fitted)
nrow(rich_habs_fitted)


summary(rich_deg)

rich_deg_fitted <- cbind(rich_deg$data,
                            fitted(rich_deg, re_formula = NA
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
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Total Species",  #x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Zone", fill = "WWF Zone", subtitle= "WWF Zones") + guides(col = guide_legend(ncol = 2))+
  #xlim(0,800)+ ylim(0,200)+
  scale_x_log10() + scale_y_log10() 

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
  geom_point(data = sb_prep_area ,
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
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
  # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "Habitats", fill = "Habitats", subtitle = "Habitats") +  guides(col = guide_legend(ncol = 2)) +
# xlim(0,800)+ ylim(0,200)+
 scale_x_log10() + scale_y_log10() 

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
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "", # x = expression(paste('Total Sample Area ' , m^2)),
       x= "",
       color = "Degraded Habitat", fill = "Degraded Habitat", subtitle = "Degraded Habitat")+  guides(col = guide_legend(ncol = 2)) +
  # xlim(0,800)+ ylim(0,200)+
 scale_x_log10() + scale_y_log10() 

fig_rich.deg


Almost_Total_Species_Fig <- (fig_rich.zone | fig_rich.habs | fig_rich.deg)


Total_Species_Fig <- (Almost_Total_Species_Fig) + plot_annotation(title = "Total Species",
                                                              theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + plot_layout(ncol=3)


Total_Species_Fig


# (Total_Sample_Area_mm2/1000000) =m2

# global effects
rich_zone.fixed.p <- as_draws(rich_zones, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_zone.fixed.p
nrow(rich_zone.fixed.p)
head(rich_zone.fixed.p)
colnames(rich_zone.fixed.p)

# select columns of interests and give meaningful names
rich_zone_global_posterior <-  rich_zone.fixed.p %>% 
  purrr::pluck("b_log_Total_Sample_Area_mm2"#,"b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert",
              # "b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate", "b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical",
              #"b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra"
              ) #%>%
  as_tibble() %>%
  mutate(rich.bor.global = (`b_log_Total_Sample_Area_mm2` ),
         rich.med.global = (`b_log_Total_Sample_Area_mm2` + `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`),
         rich.temp.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`),
         rich.trop.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`),
         rich.tund.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`),
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
  mutate( response = "MediterraneanandDesert", eff = mean(rich.med.global),
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


fig_rich_zone_global <- ggplot() + 
  geom_point(data = global.rich_zone.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich_zone.p, aes(x = response,ymin = eff_lower,
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

fig_rich_zone_global



