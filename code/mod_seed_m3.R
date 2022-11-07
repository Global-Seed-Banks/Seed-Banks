

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
sb_seed_vol <- sb_prep %>% filter(!is.na(Total_Seeds),
                                   !is.na(Calc_Volume_m3)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_seed_vol)



sb_seed_vol %>% distinct(biome_broad_WWF, biome_broad_WWF_Broad, Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'seed_m3.Rdata')


summary(seeds_biome_broad)


# posterior predictive check
color_scheme_set("darkgray")
pp_seed.biome_broad <- pp_check(seeds_biome_broad)+ xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_seed.biome_broad 


# caterpillars/chains
plot(seeds_biome_broad)


# # check model residuals
# zones
mr.biome_broad <- residuals(seeds_biome_broad)
mr.biome_broad <- as.data.frame(mr.biome_broad)
nrow(mr.biome_broad)

nrow(sb_prep_area_zone)
biome_broad.plot <- cbind(sb_seed_vol, mr.biome_broad$Estimate)
head(biome_broad.plot)
#mr.biome_broad make sure they are factors
biome_broad.plot$biome_broad_hab <- as.factor(biome_broad.plot$biome_broad_Hab )
biome_broad.plot$Method <- as.factor(biome_broad.plot$Method )
biome_broad.plot$studyID <- as.factor(biome_broad.plot$studyID )
biome_broad.plot$rowID <- as.factor(biome_broad.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome_broad.plot, plot(biome_broad_Hab, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(Method, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(studyID, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(rowID, mr.biome_broad$Estimate))



head(sb_seed_vol)
# WWF biome_broadS model
# # for plotting fixed effects
seed_biome_broad_fitted <- cbind(seeds_biome_broad$data,
                          fitted(seeds_biome_broad, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_seed_vol %>% select(Total_Species, 
                                                     Calc_Volume_m3,
                                                     Centred_log_Calc_Volume_m3,
                                                          Biome_Broad_Hab),
                             #  by= c("Total_Species2","Biome_Broad_Hab","Habitat_Broad","studyID", "rowID")
  )


head(seed_biome_broad_fitted)
nrow(seed_biome_broad_fitted)


# fixed effect coefficients
seed_biome_broad_fixef <- fixef(seeds_biome_broad)
head(seed_biome_broad_fixef)

# Random effect coefficients
seed_biome_broad_coef <- coef(seeds_biome_broad)
seed_biome_broad_coef # dont really need this



setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(seed_biome_broad_fitted, seed_biome_broad_fixef, seed_biome_broad_coef, file = 'seed_biome_broad.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('seed.area.poisson.mod_dat.Rdata')
load('seed_biome_broad.mod_dat.Rdata')

# plot seedness area biome_broad relationship

head(sb_seed_vol_biome_broad)

fig_seed.biome_broad <- ggplot() + 
  #facet_wrap(~Biome_Broad_Hab, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seed_vol ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
               # x = Total_Sample_Area_mm2,
               x = Total_Sample_Area_m2,
               y = Total_Species, colour = Biome_Broad_Hab,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seed_biome_broad_fitted,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
              # x = Total_Sample_Area_mm2,
              x = Total_Sample_Area_m2,
              y = Estimate, colour = Biome_Broad_Hab),
            size = 0.75) +
  # uncertainy in fixed effect
  geom_ribbon(data = seed_biome_broad_fitted,
              aes( #x =  (Total_Sample_Area_mm2/1000000), 
                #x =  Total_Sample_Area_mm2, 
                x = Total_Sample_Area_m2,
                ymin = Q2.5, ymax = Q97.5, fill = Biome_Broad_Hab),
              alpha = 0.1 ) +
  #coord_cartesian(xlim = c(min(sb_seed_vol_biome_broad$Total_Sample_Area_m2), quantile(sb_seed_vol_biome_broad$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Number of seeds in the soil seed bank",  x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") + guides(col = guide_legend(nrow = 4)) #+
#xlim(0,800)+ ylim(0,200)+
#scale_x_log10() + scale_y_log10() 

fig_seed.biome_broad





seed_biome_broad.fixed.p <- as_draws_df(seeds_biome_broad, subset = floor(runif(n = 1000, 1, max = 2000)))

seed_biome_broad.fixed.p
nrow(seed_biome_broad.fixed.p)
head(seed_biome_broad.fixed.p)
colnames(seed_biome_broad.fixed.p)

# select columns of interests and give meaningful names
seed_biome_broad_global_posterior <-  seed_biome_broad.fixed.p %>% 
  as_tibble() %>%
  mutate(seed.aq.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seed.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabArable` ),
         seed.bor.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabBorealForestsDTaiga` ),
         seed.des.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabDesertsandXericShrublands`),
        # seed.fgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabFloodedGrasslandsandSavannas`),
        # seed.mang.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMangroves`),
         seed.medfs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMediterraneanForestsWoodlandsandScrub`),
         seed.mongs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMontaneGrasslandsandShrublands`),
         seed.tempbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateBroadleafandMixedForests`),
         seed.tempcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateConiferForests`),
         seed.tempgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateGrasslandsSavannasandShrublands`),
         seed.tropf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalForests`),
         #seed.tropcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalConiferousForests`),
         #seed.tropdbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalDryBroadleafForests`),
         seed.tropgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         #seed.tropmbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalMoistBroadleafForests`),
         seed.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTundra`),
  ) %>%
  dplyr::select(c(seed.aq.global, seed.arable.global, seed.bor.global, seed.des.global, #seed.fgs.global, seed.mang.global, 
                  seed.medfs.global,
                  seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, 
                  #seed.tropcf.global, seed.tropdbf.global,  seed.tropmbf.global,
                  seed.tropf.global, seed.tropgs.global, seed.tund.global
                  ))

View(seed_biome_broad_global_posterior)
head(seed_biome_broad_global_posterior)


sb_seed_vol_zone$biome_broad_WWF<- as.factor(sb_seed_vol_zone$biome_broad_WWF)
levels(sb_seed_vol_zone$biome_broad_WWF)

# seed.bor.global, seed.des.global, seed.fgs.global, seed.mang.global, seed.medfs.global,
# seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, seed.tropcf.global,
# seed.tropdbf.global, seed.tropgs.global, seed.tropmbf.global,
# seed.tund.global

seed.aq.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(seed.aq.global),
          eff_lower = quantile(seed.aq.global, probs=0.025),
          eff_upper = quantile(seed.aq.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.arable.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Arable", eff = mean(seed.arable.global),
          eff_lower = quantile(seed.arable.global, probs=0.025),
          eff_upper = quantile(seed.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.bor.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(seed.bor.global),
          eff_lower = quantile(seed.bor.global, probs=0.025),
          eff_upper = quantile(seed.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.des.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(seed.des.global),
          eff_lower = quantile(seed.des.global, probs=0.025),
          eff_upper = quantile(seed.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# seed.fgs.p <-  seed_biome_broad_global_posterior %>% 
#   mutate( response = "Flooded Grasslands and Savannas", eff = mean(seed.fgs.global),
#           eff_lower = quantile(seed.fgs.global, probs=0.025),
#           eff_upper = quantile(seed.fgs.global, probs=0.975)) %>%
#   dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# 
# seed.mang.p <-  seed_biome_broad_global_posterior %>% 
#   mutate( response = "Mangroves", eff = mean(seed.mang.global),
#           eff_lower = quantile(seed.mang.global, probs=0.025),
#           eff_upper = quantile(seed.mang.global, probs=0.975)) %>%
#   dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.medfs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(seed.medfs.global),
          eff_lower = quantile(seed.medfs.global, probs=0.025),
          eff_upper = quantile(seed.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.mongs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(seed.mongs.global),
          eff_lower = quantile(seed.mongs.global, probs=0.025),
          eff_upper = quantile(seed.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempbf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(seed.tempbf.global),
          eff_lower = quantile(seed.tempbf.global, probs=0.025),
          eff_upper = quantile(seed.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempcf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(seed.tempcf.global),
          eff_lower = quantile(seed.tempcf.global, probs=0.025),
          eff_upper = quantile(seed.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempgs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(seed.tempgs.global),
          eff_lower = quantile(seed.tempgs.global, probs=0.025),
          eff_upper = quantile(seed.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# Tropical and Subtropical Forests
#seed.tropf.global
seed.tropf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Forests", eff = mean(seed.tropf.global),
          eff_lower = quantile(seed.tropf.global, probs=0.025),
          eff_upper = quantile(seed.tropf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# seed.tropcf.p <-  seed_biome_broad_global_posterior %>% 
#   mutate( response = "Tropical and Subtropical Coniferous Forests", eff = mean(seed.tropcf.global),
#           eff_lower = quantile(seed.tropcf.global, probs=0.025),
#           eff_upper = quantile(seed.tropcf.global, probs=0.975)) %>%
#   dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# 
# seed.tropdbf.p <-  seed_biome_broad_global_posterior %>% 
#   mutate( response = "Tropical and Subtropical Dry Broadleaf Forests", eff = mean(seed.tropdbf.global),
#           eff_lower = quantile(seed.tropdbf.global, probs=0.025),
#           eff_upper = quantile(seed.tropdbf.global, probs=0.975)) %>%
#   dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropgs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(seed.tropgs.global),
          eff_lower = quantile(seed.tropgs.global, probs=0.025),
          eff_upper = quantile(seed.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# seed.tropmbf.p <-  seed_biome_broad_global_posterior %>% 
#   mutate( response = "Tropical and Subtropical Moist Broadleaf Forests", eff = mean(seed.tropmbf.global),
#           eff_lower = quantile(seed.tropmbf.global, probs=0.025),
#           eff_upper = quantile(seed.tropmbf.global, probs=0.975)) %>%
#   dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tund.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(seed.tund.global),
          eff_lower = quantile(seed.tund.global, probs=0.025),
          eff_upper = quantile(seed.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.seed_biome_broad.p <- bind_rows(seed.aq.p, seed.arable.p,
                                seed.bor.p, seed.des.p,
                                # seed.fgs.p, seed.mang.p,
                                 seed.medfs.p, seed.mongs.p, seed.tempbf.p, seed.tempcf.p, seed.tempgs.p, 
                                #seed.tropcf.p, seed.tropdbf.p, seed.tropmbf.p,
                                seed.tropf.p, seed.tropgs.p, seed.tund.p
                                 ) %>%
  mutate(  Model = "Number of Seeds",
         `WWF Biome` = response ,
           Estimate = round(eff, 2),
         `Upper CI` = round(eff_upper, 2),
           `Lower CI` = round(eff_lower, 2),
  ) %>% select(-c(eff, eff_lower, eff_upper, response))

head(global.seed_biome_broad.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seed_biome_broad.p, file = 'global.seed_biome_broad.posteriors.Rdata')


setwd(paste0(path2wd, 'Tables/'))
write.csv(global.seed_biome_broad.p, "table_2.csv")

global.seed_biome_broad.p

fig_seed_biome_broad_global <- ggplot() + 
  geom_point(data = global.seed_biome_broad.p, aes(x = `WWF Biome`, y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.seed_biome_broad.p, aes(x = `WWF Biome`,ymin = `Lower CI`,
                                               ymax = `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  labs(y = "Slope", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_seed_biome_broad_global


# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# overall legend
seed_biome_broad_leg <- g_legend(fig_seed.biome_broad)


(fig_seed.biome_broad + theme(legend.position="none") | fig_seed_biome_broad_global)/ (seed_biome_broad_leg) +  plot_layout(ncol=1, nrow=2, heights = c(10,1))

