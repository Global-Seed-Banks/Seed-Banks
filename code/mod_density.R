
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
sb_density_area <- sb_prep %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

nrow(sb_density_area)

sb_density_area_zone <- sb_density_area %>% filter(!Habitat_Broad == "Arable")

nrow(sb_density_area_zone)


head(sb_prep_area)
nrow(sb_prep_area)


# density_zones <- brm(Seed_density_m2 ~  Biome_WWF_Zone + (1 | Method/studyID/rowID ),
#                      family = gaussian(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
#                      control = list(adapt_delta = 0.99,
#                                     max_treedepth = 12)
# )

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'density_zone.Rdata')
load( 'density_hab.Rdata')
load( 'density_deg.Rdata')


# density_zones
# density_habs
# density_deg

# model summary
summary(density_zones)
summary(density_habs) 
summary(density_deg)


# posterior predictive check
color_scheme_set("darkgray")
pp_den.zone <- pp_check(density_zones)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") +# xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values


pp_den.habs <- pp_check(density_habs)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") +#  xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values


pp_den.deg <- pp_check(density_deg)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") +#  xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

(pp_den.zone | pp_den.habs |  pp_den.deg)


# caterpillars/chains
plot(density_zones)
plot(density_habs)
plot(density_deg)

# # check model residuals
# zones
mr.zones <- residuals(density_zones)
mr.zones <- as.data.frame(mr.zones)
nrow(mr.zones)

nrow(sb_density_area_zone)
zones.plot <- cbind(sb_density_area_zone, mr.zones$Estimate)
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
mr.habs <- residuals(density_habs)
mr.habs <- as.data.frame(mr.habs)
nrow(mr.habs)
nrow(sb_density_area)
habs.plot <- cbind(sb_density_area, mr.habs$Estimate)
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
mr.deg <- residuals(density_deg)
mr.deg <- as.data.frame(mr.deg)
nrow(mr.deg)
nrow(sb_density_area)
deg.plot <- cbind(sb_density_area, mr.deg$Estimate)
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



density_zone_c <- conditional_effects(density_zones, effects = 'Biome_WWF_Zone', re_formula = NA, method = 'fitted')  # conditional effects

head(density_zone_c)


Density_Zone_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_density_area_zone,
             aes(x = Biome_WWF_Zone, y = Seed_density_m2, #colour = 	"#C0C0C0"
                 colour = Biome_WWF_Zone
                 ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = density_zone_c$Biome_WWF_Zone,
             aes(x = Biome_WWF_Zone, y = estimate__, colour = Biome_WWF_Zone), size = 3) +
  geom_errorbar(data = density_zone_c$Biome_WWF_Zone,
                aes(x = Biome_WWF_Zone, ymin = lower__, ymax = upper__, colour = Biome_WWF_Zone),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle= 'WWF Zones') +
 # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
   coord_cartesian( ylim = c(0,10000)) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


Density_Zone_Fig



density_hab_c <- conditional_effects(density_habs, effects = 'Habitat_Broad', re_formula = NA, method = 'fitted')  # conditional effects

head(density_hab_c)


Density_Hab_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_density_area,
             aes(x = Habitat_Broad, y = Seed_density_m2, #colour = 	"#C0C0C0"
                 colour = Habitat_Broad
             ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = density_hab_c$Habitat_Broad,
             aes(x = Habitat_Broad, y = estimate__, colour = Habitat_Broad), size = 3) +
  geom_errorbar(data = density_hab_c$Habitat_Broad,
                aes(x = Habitat_Broad, ymin = lower__, ymax = upper__, colour = Habitat_Broad),
                size = 1, width = 0) +
  labs(x = '',
       y = '',#expression(paste('Seed density (',m^2,')')),
       subtitle= 'Habitats') +
  # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
  coord_cartesian( ylim = c(0,10000)) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 


Density_Hab_Fig




density_deg_c <- conditional_effects(density_deg, effects = 'Habitat_Degraded', re_formula = NA, method = 'fitted')  # conditional effects

head(density_deg_c)


Density_Deg_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_density_area,
             aes(x = Habitat_Degraded, y = Seed_density_m2, #colour = 	"#C0C0C0"
                 colour = Habitat_Degraded
             ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = density_deg_c$Habitat_Degraded,
             aes(x = Habitat_Degraded, y = estimate__, colour = Habitat_Degraded), size = 3) +
  geom_errorbar(data = density_deg_c$Habitat_Degraded,
                aes(x = Habitat_Degraded, ymin = lower__, ymax = upper__, colour = Habitat_Degraded),
                size = 1, width = 0) +
  labs(x = '',
       y = '', #expression(paste('Seed density (',m^2,')')),
       subtitle= 'Habitat Degraded') +
  # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
  coord_cartesian( ylim = c(0,10000)) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 


Density_Deg_Fig


Density_Zone_Fig | Density_Hab_Fig | Density_Deg_Fig
