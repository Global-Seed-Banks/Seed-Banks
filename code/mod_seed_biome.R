

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
head(sb_prep)

# remove NA values 
sb_seed_area <- sb_prep %>% filter(!is.na(Total_Seeds),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_WWF_biome = as.factor(Biome_WWF),
          Habitat_Broad = as.factor(Habitat_Broad),
          Biome_Hab = as.factor(Biome_Hab),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_seed_area)



sb_seed_area %>% distinct(Biome_WWF, Biome_WWF_Broad, Biome_WWF_biome) %>% arrange(Biome_WWF_biome)




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'seed_biome.Rdata')


summary(seeds_biome)


# posterior predictive check
color_scheme_set("darkgray")
pp_seed.biome <- pp_check(seeds_biome)+ xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_seed.biome 


# caterpillars/chains
plot(seeds_biome)


# # check model residuals
# zones
mr.biome <- residuals(seeds_biome)
mr.biome <- as.data.frame(mr.biome)
nrow(mr.biome)

nrow(sb_prep_area_zone)
biome.plot <- cbind(sb_seed_area, mr.biome$Estimate)
head(biome.plot)
#mr.biome make sure they are factors
biome.plot$Biome_hab <- as.factor(biome.plot$Biome_Hab )
biome.plot$Method <- as.factor(biome.plot$Method )
biome.plot$studyID <- as.factor(biome.plot$studyID )
biome.plot$rowID <- as.factor(biome.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome.plot, plot(Biome_Hab, mr.biome$Estimate))
with(biome.plot, plot(Method, mr.biome$Estimate))
with(biome.plot, plot(studyID, mr.biome$Estimate))
with(biome.plot, plot(rowID, mr.biome$Estimate))



head(sb_seed_area)
# WWF biomeS model
# # for plotting fixed effects
seed_biome_fitted <- cbind(seeds_biome$data,
                          fitted(seeds_biome, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_seed_area %>% select(Total_Species, 
                                                          Total_Number_Samples, Total_Sample_Area_mm2,
                                                          log_Total_Sample_Area_mm2,
                                                          Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                          Centred_log_Total_Sample_Area_m2,
                                                          Biome_WWF, Biome_Hab),
                             #  by= c("Total_Species2","Biome_WWF_biome","Habitat_Broad","studyID", "rowID")
  )


head(seed_biome_fitted)
nrow(seed_biome_fitted)


# fixed effect coefficients
seed_biome_fixef <- fixef(seeds_biome)
head(seed_biome_fixef)

# Random effect coefficients
seed_biome_coef <- coef(seeds_biome)
seed_biome_coef # dont really need this



setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(seed_biome_fitted, seed_biome_fixef, seed_biome_coef, file = 'seed_biome.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('seed.area.poisson.mod_dat.Rdata')
load('seed_biome.mod_dat.Rdata')

# plot seedness area biome relationship

head(sb_seed_area_biome)

fig_seed.biome <- ggplot() + 
  #facet_wrap(~Biome_WWF_biome, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seed_area ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
               # x = Total_Sample_Area_mm2,
               x = Total_Sample_Area_m2,
               y = Total_Species, colour = Biome_Hab,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seed_biome_fitted,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
              # x = Total_Sample_Area_mm2,
              x = Total_Sample_Area_m2,
              y = Estimate, colour = Biome_Hab),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = seed_biome_fitted,
              aes( #x =  (Total_Sample_Area_mm2/1000000), 
                #x =  Total_Sample_Area_mm2, 
                x = Total_Sample_Area_m2,
                ymin = Q2.5, ymax = Q97.5, fill = Biome_Hab),
              alpha = 0.1 ) +
  #coord_cartesian(xlim = c(min(sb_seed_area_biome$Total_Sample_Area_m2), quantile(sb_seed_area_biome$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Seeds", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "") + guides(col = guide_legend(nrow = 5)) #+
#xlim(0,800)+ ylim(0,200)+
#scale_x_log10() + scale_y_log10() 

fig_seed.biome


seed_biome.fixed.p <- as_draws_df(seeds_biome, subset = floor(runif(n = 1000, 1, max = 2000)))

seed_biome.fixed.p
nrow(seed_biome.fixed.p)
head(seed_biome.fixed.p)
colnames(seed_biome.fixed.p)

# select columns of interests and give meaningful names
seed_biome_global_posterior <-  seed_biome.fixed.p %>% 
  as_tibble() %>%
  mutate(seed.aq.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seed.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_HabArable` ),
         seed.bor.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_HabBorealForestsDTaiga` ),
         seed.des.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_HabDesertsandXericShrublands`),
         seed.fgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabFloodedGrasslandsandSavannas`),
         seed.mang.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabMangroves`),
         seed.medfs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabMediterraneanForestsWoodlandsandScrub`),
         seed.mongs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabMontaneGrasslandsandShrublands`),
         seed.tempbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTemperateBroadleafandMixedForests`),
         seed.tempcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTemperateConiferForests`),
         seed.tempgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTemperateGrasslandsSavannasandShrublands`),
         seed.tropcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTropicalandSubtropicalConiferousForests`),
         seed.tropdbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTropicalandSubtropicalDryBroadleafForests`),
         seed.tropgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         seed.tropmbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTropicalandSubtropicalMoistBroadleafForests`),
         seed.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_HabTundra`),
  ) %>%
  dplyr::select(c(seed.aq.global, seed.arable.global, seed.bor.global, seed.des.global, seed.fgs.global, seed.mang.global, seed.medfs.global,
                  seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, seed.tropcf.global,
                  seed.tropdbf.global, seed.tropgs.global, seed.tropmbf.global,
                  seed.tund.global
                  ))

View(seed_biome_global_posterior)
head(seed_biome_global_posterior)


sb_seed_area_zone$Biome_WWF<- as.factor(sb_seed_area_zone$Biome_WWF)
levels(sb_seed_area_zone$Biome_WWF)

# seed.bor.global, seed.des.global, seed.fgs.global, seed.mang.global, seed.medfs.global,
# seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, seed.tropcf.global,
# seed.tropdbf.global, seed.tropgs.global, seed.tropmbf.global,
# seed.tund.global

seed.aq.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(seed.aq.global),
          eff_lower = quantile(seed.aq.global, probs=0.025),
          eff_upper = quantile(seed.aq.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.arable.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Arable", eff = mean(seed.arable.global),
          eff_lower = quantile(seed.arable.global, probs=0.025),
          eff_upper = quantile(seed.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.bor.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(seed.bor.global),
          eff_lower = quantile(seed.bor.global, probs=0.025),
          eff_upper = quantile(seed.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.des.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(seed.des.global),
          eff_lower = quantile(seed.des.global, probs=0.025),
          eff_upper = quantile(seed.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.fgs.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Flooded Grasslands and Savannas", eff = mean(seed.fgs.global),
          eff_lower = quantile(seed.fgs.global, probs=0.025),
          eff_upper = quantile(seed.fgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.mang.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Mangroves", eff = mean(seed.mang.global),
          eff_lower = quantile(seed.mang.global, probs=0.025),
          eff_upper = quantile(seed.mang.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.medfs.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(seed.medfs.global),
          eff_lower = quantile(seed.medfs.global, probs=0.025),
          eff_upper = quantile(seed.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.mongs.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(seed.mongs.global),
          eff_lower = quantile(seed.mongs.global, probs=0.025),
          eff_upper = quantile(seed.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempbf.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(seed.tempbf.global),
          eff_lower = quantile(seed.tempbf.global, probs=0.025),
          eff_upper = quantile(seed.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempcf.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(seed.tempcf.global),
          eff_lower = quantile(seed.tempcf.global, probs=0.025),
          eff_upper = quantile(seed.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempgs.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(seed.tempgs.global),
          eff_lower = quantile(seed.tempgs.global, probs=0.025),
          eff_upper = quantile(seed.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropcf.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Coniferous Forests", eff = mean(seed.tropcf.global),
          eff_lower = quantile(seed.tropcf.global, probs=0.025),
          eff_upper = quantile(seed.tropcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropdbf.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Dry Broadleaf Forests", eff = mean(seed.tropdbf.global),
          eff_lower = quantile(seed.tropdbf.global, probs=0.025),
          eff_upper = quantile(seed.tropdbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropgs.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(seed.tropgs.global),
          eff_lower = quantile(seed.tropgs.global, probs=0.025),
          eff_upper = quantile(seed.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropmbf.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Moist Broadleaf Forests", eff = mean(seed.tropmbf.global),
          eff_lower = quantile(seed.tropmbf.global, probs=0.025),
          eff_upper = quantile(seed.tropmbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tund.p <-  seed_biome_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(seed.tund.global),
          eff_lower = quantile(seed.tund.global, probs=0.025),
          eff_upper = quantile(seed.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.seed_biome.p <- bind_rows(seed.aq.p, seed.arable.p,
                                seed.bor.p, seed.des.p,
                                 seed.fgs.p, seed.mang.p,
                                 seed.medfs.p, seed.mongs.p, seed.tempbf.p, seed.tempcf.p, seed.tempgs.p, seed.tropcf.p,
                                 seed.tropdbf.p, seed.tropgs.p, seed.tropmbf.p,
                                 seed.tund.p
                                 )
head(global.seed_biome.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seed_biome.p, file = 'global.seed_biome.posteriors.Rdata')

global.seed_biome.p

fig_seed_biome_global <- ggplot() + 
  geom_point(data = global.seed_biome.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.seed_biome.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color = response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  labs(y = "Slope", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", subtitle= "") + 
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_seed_biome_global


# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# overall legend
seed_biome_leg <- g_legend(fig_seed.biome)


(fig_seed.biome + theme(legend.position="none") | fig_seed_biome_global)/ (seed_biome_leg)

