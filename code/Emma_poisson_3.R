
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
sb_prep_area <- sb_prep %>% filter(!is.na(Total_Species2),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))


head(sb_prep_area)
nrow(sb_prep_area)

# area model
# rich.p_zones <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad ),
#                 family = poisson(), data = sb_dat, cores = 4, chains = 4,
#                 )


setwd(paste0(path2wd, 'Model_Fits/'))
# save model object
#save(rich.area, file = 'gsb_rich_area-poisson.Rdata')
load( 'gsb_rich_area_zone_2.Rdata')

# rich.p_zones

# model summary
summary(rich.p_zones)
summary(rich.p_zones_2)
summary(rich.p_zones_3)

# posterior predictive check
color_scheme_set("darkgray")
pp_rich.area <- pp_check(rich.p_zones)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

pp_rich.area_2 <- pp_check(rich.p_zones_2)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values

pp_rich.area_3 <- pp_check(rich.p_zones_3)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") + xlim(0,300)+ ylim(0,0.040)+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values


(pp_rich.area | pp_rich.area_2 | pp_rich.area_3)


# caterpillars/chains
plot(rich.p_zones)
plot(rich.p_zones_2)
plot(rich.p_zones_3)

# check model residuals
ma <- residuals(rich.p_zones)
ma <- as.data.frame(ma)
ar.plot <- cbind(sb_prep_area, ma$Estimate)

#make sure they are factors
ar.plot$Biome_WWF_Zone <- as.factor(ar.plot$Biome_WWF_Zone )
ar.plot$Habitat_Broad <- as.factor(ar.plot$Habitat_Broad )
#ar.plot$studyID <- as.factor(ar.plot$studyID )
#ar.plot$rowID <- as.factor(ar.plot$rowID )
#plot residuals
par(mfrow=c(1,2))
with(ar.plot, plot(Biome_WWF_Zone, ma$Estimate))
with(ar.plot, plot(Habitat_Broad, ma$Estimate))
#with(ar.plot, plot(studyID, ma$Estimate))
#with(ar.plot, plot(rowID, ma$Estimate))


head(sb_prep_area)
#  
# # for plotting fixed effects
rich.area_fitted <- cbind(rich.p_zones_2$data,
                             fitted(rich.p_zones_2, re_formula = NA
                             )) %>%
  as_tibble() %>% inner_join(sb_prep_area %>% select(Total_Species, Total_Species2,
                                                    Total_Number_Samples, Total_Sample_Area_mm2,
                                                    log_Total_Sample_Area_mm2,
                                                  Biome_WWF_Zone, Habitat_Broad),
                           #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich.area_fitted)
nrow(rich.area_fitted)



# fixed effect coefficients
rich.area_fixef <- fixef(rich.p_zones_2)
head(rich.area_fixef)

# Random effect coefficients
rich.samps_coef <- coef(rich.p_zones_2)
rich.samps_coef

# predict estimates for each habitat within each biome across a sequence of log_total_volumes and total_volumes
# we could also just extract the coefs, but since we are plotting a linear curve in log space this is more accurate
# and better practice
obs_nest.rich.area <- sb_prep_area %>%
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone,
         Habitat_Broad_group = Habitat_Broad) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone, Habitat_Broad_group, Habitat_Broad) %>%
  summarise(log_Total_Sample_Area_mm2 = seq(min(log_Total_Sample_Area_mm2, na.rm = TRUE), quantile(log_Total_Sample_Area_mm2, na.rm = TRUE),  length.out = 1000 ),
            Total_Sample_Area_mm2 = seq(min(Total_Sample_Area_mm2, na.rm = TRUE), max(Total_Sample_Area_mm2, na.rm = TRUE),  length.out = 1000)) %>%
  nest(data = c( Biome_WWF_Zone, Habitat_Broad, log_Total_Sample_Area_mm2, Total_Sample_Area_mm2)) %>%
  mutate(predicted = map(data, ~predict(rich.p_zones_2, newdata= .x, re_formula = ~(log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad) )))


View(obs_nest.rich.area)

 setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich.area_fitted, rich.area_fixef, obs_nest.rich.area, file = 'rich.area.poisson.mod_dat_2.Rdata')

 setwd(paste0(path2wd, 'Data/'))
 #load('rich.area.poisson.mod_dat.Rdata')
 load('rich.area.poisson.mod_dat_2.Rdata')

# plot richness area relationship
colnames(sb_prep_area)

View(rich.area_fitted)
obs_nest.rich.area  %>% unnest(cols = c(data, predicted)) 

fig_rich.area <- ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_prep_area ,
             aes(x = (Total_Sample_Area_mm2/1000000),
                # x = Total_Sample_Area_mm2,
                 y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
  # random slopes
  geom_line(data = obs_nest.rich.area  %>% unnest(cols = c(data, predicted)) ,
            aes(x = (Total_Sample_Area_mm2/1000000),
              #x = Total_Sample_Area_mm2, 
              y= predicted[,1] ,
                group = Habitat_Broad,
                colour = Habitat_Broad),
            size = 1.2) +
  # fixed effect
  geom_line(data = rich.area_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), 
               # x = Total_Sample_Area_mm2,
                y = Estimate),
            size = 1.5) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich.area_fitted,
              aes( x =  (Total_Sample_Area_mm2/1000000), 
                  #x =  Total_Sample_Area_mm2, 
                  ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_area$Total_Sample_Area_mm2), quantile(sb_prep_area$Total_Sample_Area_mm2, 0.97))) +
 # xlim(0,10) + ylim(0,200) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "Habitat Type") #+ #guides(col = guide_legend(ncol = 2))# +
  #scale_x_log10() + scale_y_log10() 

fig_rich.area

# (Total_Sample_Area_mm2/1000000) =m2

# sample posterior etc

setwd(paste0(path2wd, 'Model_Fits/'))
load( 'gsb_rich_area_zone_2.Rdata')


# Habitat level posterior samples
hab_levels <- rich.p_zones_2$data %>% 
  as_tibble() %>% 
  distinct(Habitat_Broad) %>% 
  mutate(level =  Habitat_Broad) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each habitat and wwf zone
hab_rich.area_posterior <- hab_levels %>%
  mutate( area.hab = purrr::map(data, ~posterior_samples(rich.p_zones_2,
                                                         pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
                                                         fixed = TRUE,
                                                         subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.med.hab = purrr::map(data, ~posterior_samples(rich.p_zones_2,
                                                             pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert]', sep=''),
                                                             fixed = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.temp.hab = purrr::map(data, ~posterior_samples(rich.p_zones_2,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.trop.hab = purrr::map(data, ~posterior_samples(rich.p_zones_2,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.tund.hab = purrr::map(data, ~posterior_samples(rich.p_zones_2,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
  )

head(hab_rich.area_posterior)

# habitat effects
rich.area_hab_posterior <- hab_rich.area_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.hab, area.med.hab, area.temp.hab, area.trop.hab, area.tund.hab) %>%
  mutate( rich.area.bor.hab = area.hab,
          rich.area.med.hab = (area.hab + area.med.hab),
          rich.area.temp.hab = (area.hab + area.temp.hab),
          rich.area.trop.hab = (area.hab + area.trop.hab),
          rich.area.tund.hab = (area.hab + area.tund.hab))

# habitat level effects
head(rich.area_hab_posterior)

# global effects
rich.area.fixed.p <- posterior_samples(rich.p_zones_2, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.area.fixed.p)

# select columns of interests and give meaningful names
rich.area_global_posterior <-  rich.area.fixed.p %>% dplyr::select(`b_log_Total_Sample_Area_mm2`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`) %>%
  mutate(rich.area.bor.global =`b_log_Total_Sample_Area_mm2`,
         rich.med.global =`b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
         rich.temp.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
         rich.trop.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
         rich.tund.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`,
         rich.area.med.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`),
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

rich.area.bor.p <-  rich.area_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(rich.area.bor.global),
          eff_lower = quantile(rich.area.bor.global, probs=0.025),
          eff_upper = quantile(rich.area.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.area.med.p <-  rich.area_global_posterior %>% 
  mutate( response = "MediterraneanandDesert", eff = mean(rich.area.med.global),
          eff_lower = quantile(rich.area.med.global, probs=0.025),
          eff_upper = quantile(rich.area.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.area.temp.p <-  rich.area_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(rich.area.temp.global),
          eff_lower = quantile(rich.area.temp.global, probs=0.025),
          eff_upper = quantile(rich.area.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.area.trop.p <-  rich.area_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(rich.area.trop.global),
          eff_lower = quantile(rich.area.trop.global, probs=0.025),
          eff_upper = quantile(rich.area.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.area.tund.p <-  rich.area_global_posterior %>% 
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
save(global.rich.area.p, file = 'global.rich.area.poisson.posteriors.Rdata')


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



# Habitat level
head(rich.area_hab_posterior)

rich.area.bor.hab.p <-  rich.area_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Boreal", eff = mean(rich.area.bor.hab),
          eff_lower = quantile(rich.area.bor.hab, probs=0.025),
          eff_upper = quantile(rich.area.bor.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.area.med.hab.p <-  rich.area_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "MediterraneanandDesert", eff = mean(rich.area.med.hab),
          eff_lower = quantile(rich.area.med.hab, probs=0.025),
          eff_upper = quantile(rich.area.med.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.area.temp.hab.p <-  rich.area_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Temperate", eff = mean(rich.area.temp.hab),
          eff_lower = quantile(rich.area.temp.hab, probs=0.025),
          eff_upper = quantile(rich.area.temp.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.area.trop.hab.p <-  rich.area_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tropical", eff = mean(rich.area.trop.hab),
          eff_lower = quantile(rich.area.trop.hab, probs=0.025),
          eff_upper = quantile(rich.area.trop.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.area.tund.hab.p <-  rich.area_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tundra", eff = mean(rich.area.tund.hab),
          eff_lower = quantile(rich.area.tund.hab, probs=0.025),
          eff_upper = quantile(rich.area.tund.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

hab.rich.area.p <- bind_rows(rich.area.bor.hab.p, rich.area.med.hab.p,
                             rich.area.temp.hab.p, rich.area.trop.hab.p,
                             rich.area.tund.hab.p)
head(hab.rich.area.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(hab.rich.area.p, file = 'habitat.rich.area.poisson.posteriors.Rdata')


fig_rich.area_habitat <- ggplot() + 
  facet_wrap(~response) +
  geom_point(data = hab.rich.area.p, aes(x = Habitat_Broad, y = eff,color=Habitat_Broad),size = 2) +
  geom_errorbar(data = hab.rich.area.p, aes(x = Habitat_Broad,ymin = eff_lower,
                                            ymax = eff_upper, color=Habitat_Broad),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  # scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_habitat

