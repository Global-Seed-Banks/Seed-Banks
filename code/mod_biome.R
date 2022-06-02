

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
          Biome_WWF_biome = as.factor(Biome_WWF_biome),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)

sb_rich_area_biome <- sb_rich_area %>% filter(!Habitat_Broad == "Arable")


sb_rich_area %>% distinct(Biome_WWF, Biome_WWF_Broad, Biome_WWF_biome) %>% arrange(Biome_WWF_biome)




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_biome.Rdata')
load( 'rich_biome_broad.Rdata')

summary(rich_biome)
summary(rich_biome_broad) 



# posterior predictive check
color_scheme_set("darkgray")
pp_rich.biome <- pp_check(rich_biome)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values


pp_rich.bb <- pp_check(rich_biome_broad)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +   xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values


(pp_rich.biome | pp_rich.bb)



# caterpillars/chains
plot(rich_biome)
plot(rich_biome_broad)


head(sb_rich_area_biome)
# WWF biomeS model
# # for plotting fixed effects
rich_biome_fitted <- cbind(rich_biome$data,
                          fitted(rich_biome, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_rich_area_biome %>% select(Total_Species, 
                                                          Total_Number_Samples, Total_Sample_Area_mm2,
                                                          log_Total_Sample_Area_mm2,
                                                          Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                          Centred_log_Total_Sample_Area_m2,
                                                          Biome_WWF),
                             #  by= c("Total_Species2","Biome_WWF_biome","Habitat_Broad","studyID", "rowID")
  )


head(rich_biome_fitted)
nrow(rich_biome_fitted)






# fixed effect coefficients
rich_biome_fixef <- fixef(rich_biome)
head(rich_biome_fixef)

# Random effect coefficients
rich_biome_coef <- coef(rich_biome)
rich_biome_coef # dont really need this



setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich_biome_fitted, rich_biome_fixef, rich_biome_coef, file = 'rich_biome.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('rich.area.poisson.mod_dat.Rdata')
load('rich_biome.mod_dat.Rdata')

# plot richness area biome relationship

head(sb_rich_area_biome)

fig_rich.biome <- ggplot() + 
  #facet_wrap(~Biome_WWF_biome, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area_zone ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
               # x = Total_Sample_Area_mm2,
               x = Total_Sample_Area_m2,
               y = Total_Species, colour = Biome_WWF,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = rich_biome_fitted,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
              # x = Total_Sample_Area_mm2,
              x = Total_Sample_Area_m2,
              y = Estimate, colour = Biome_WWF),
            size = 1) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_biome_fitted,
              aes( #x =  (Total_Sample_Area_mm2/1000000), 
                #x =  Total_Sample_Area_mm2, 
                x = Total_Sample_Area_m2,
                ymin = Q2.5, ymax = Q97.5, fill = Biome_WWF),
              alpha = 0.1 ) +
  #coord_cartesian(xlim = c(min(sb_rich_area_biome$Total_Sample_Area_m2), quantile(sb_rich_area_biome$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF biome", fill = "WWF biome", subtitle= "") + guides(col = guide_legend(nrow = 5)) #+
#xlim(0,800)+ ylim(0,200)+
#scale_x_log10() + scale_y_log10() 

fig_rich.biome

rich_biome.fixed.p <- as_draws_df(rich_biome, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_biome.fixed.p
nrow(rich_biome.fixed.p)
head(rich_biome.fixed.p)
colnames(rich_biome.fixed.p)

# select columns of interests and give meaningful names
rich_biome_global_posterior <-  rich_biome.fixed.p %>% 
  as_tibble() %>%
  mutate(rich.bor.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         rich.des.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_WWFDesertsandXericShrublands`),
         rich.fgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFFloodedGrasslandsandSavannas`),
         rich.mang.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFMangroves`),
         rich.medfs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFMediterraneanForestsWoodlandsandScrub`),
         rich.mongs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFMontaneGrasslandsandShrublands`),
         rich.tempbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTemperateBroadleafandMixedForests`),
         rich.tempcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTemperateConiferForests`),
         rich.tempgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTemperateGrasslandsSavannasandShrublands`),
         rich.tropcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTropicalandSubtropicalConiferousForests`),
         rich.tropdbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTropicalandSubtropicalDryBroadleafForests`),
         rich.tropgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         rich.tropmbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTropicalandSubtropicalMoistBroadleafForests`),
         rich.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_WWFTundra`),
  ) %>%
  dplyr::select(c(rich.bor.global, rich.des.global, rich.fgs.global, rich.mang.global, rich.medfs.global,
                  rich.mongs.global, rich.tempbf.global, rich.tempcf.global, rich.tempgs.global, rich.tropcf.global,
                  rich.tropdbf.global, rich.tropgs.global, rich.tropmbf.global,
                  rich.tund.global
                  ))

View(rich_biome_global_posterior)
head(rich_biome_global_posterior)


sb_rich_area_zone$Biome_WWF<- as.factor(sb_rich_area_zone$Biome_WWF)
levels(sb_rich_area_zone$Biome_WWF)

# rich.bor.global, rich.des.global, rich.fgs.global, rich.mang.global, rich.medfs.global,
# rich.mongs.global, rich.tempbf.global, rich.tempcf.global, rich.tempgs.global, rich.tropcf.global,
# rich.tropdbf.global, rich.tropgs.global, rich.tropmbf.global,
# rich.tund.global

rich.bor.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(rich.bor.global),
          eff_lower = quantile(rich.bor.global, probs=0.025),
          eff_upper = quantile(rich.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.des.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(rich.des.global),
          eff_lower = quantile(rich.des.global, probs=0.025),
          eff_upper = quantile(rich.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.fgs.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Flooded Grasslands and Savannas", eff = mean(rich.fgs.global),
          eff_lower = quantile(rich.fgs.global, probs=0.025),
          eff_upper = quantile(rich.fgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.mang.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Mangroves", eff = mean(rich.mang.global),
          eff_lower = quantile(rich.mang.global, probs=0.025),
          eff_upper = quantile(rich.mang.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.medfs.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(rich.medfs.global),
          eff_lower = quantile(rich.medfs.global, probs=0.025),
          eff_upper = quantile(rich.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.mongs.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(rich.mongs.global),
          eff_lower = quantile(rich.mongs.global, probs=0.025),
          eff_upper = quantile(rich.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempbf.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(rich.tempbf.global),
          eff_lower = quantile(rich.tempbf.global, probs=0.025),
          eff_upper = quantile(rich.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempcf.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(rich.tempcf.global),
          eff_lower = quantile(rich.tempcf.global, probs=0.025),
          eff_upper = quantile(rich.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempgs.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(rich.tempgs.global),
          eff_lower = quantile(rich.tempgs.global, probs=0.025),
          eff_upper = quantile(rich.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropcf.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Coniferous Forests", eff = mean(rich.tropcf.global),
          eff_lower = quantile(rich.tropcf.global, probs=0.025),
          eff_upper = quantile(rich.tropcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropdbf.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Dry Broadleaf Forests", eff = mean(rich.tropdbf.global),
          eff_lower = quantile(rich.tropdbf.global, probs=0.025),
          eff_upper = quantile(rich.tropdbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropgs.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(rich.tropgs.global),
          eff_lower = quantile(rich.tropgs.global, probs=0.025),
          eff_upper = quantile(rich.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropmbf.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Moist Broadleaf Forests", eff = mean(rich.tropmbf.global),
          eff_lower = quantile(rich.tropmbf.global, probs=0.025),
          eff_upper = quantile(rich.tropmbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tund.p <-  rich_biome_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.tund.global),
          eff_lower = quantile(rich.tund.global, probs=0.025),
          eff_upper = quantile(rich.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.rich_biome.p <- bind_rows(rich.bor.p, rich.des.p,
                                 rich.fgs.p, rich.mang.p,
                                 rich.medfs.p, rich.mongs.p, rich.tempbf.p, rich.tempcf.p, rich.tempgs.p, rich.tropcf.p,
                                 rich.tropdbf.p, rich.tropgs.p, rich.tropmbf.p,
                                 rich.tund.p
                                 )
head(global.rich_biome.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_biome.p, file = 'global.rich_biome.posteriors.Rdata')

head(global.rich_biome.p)

fig_rich_biome_global <- ggplot() + 
  geom_point(data = global.rich_biome.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich_biome.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color = response),
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
                               strip.background = element_blank(),legend.position="bottom") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_rich_biome_global






