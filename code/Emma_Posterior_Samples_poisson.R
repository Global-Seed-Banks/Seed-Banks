
library(tidyverse)
library(brms)
library(bayesplot)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

setwd(paste0(path2wd, 'Model_Fits/'))
load( 'gsb_rich_area-poisson.Rdata')


summary(rich.mod)


# Habitat level posterior samples
zone_levels <- rich.mod$data %>% 
  as_tibble() %>% 
  distinct(Biome_WWF_Zone) %>% 
  mutate(level =  Biome_WWF_Zone) %>%
  nest_legacy(level)


# extract 1000 study-level posterior samples for each habitat and wwf zone
zone_rich.mod_posterior <- zone_levels %>%
  mutate( area.I.zone = purrr::map(data, ~posterior_samples(rich.mod,
                                                            pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',Intercept]', sep=''),
                                                            fixed = TRUE,
                                                            subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.zone = purrr::map(data, ~posterior_samples(rich.mod,
                                                          pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
                                                          fixed = TRUE,
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          
  )

head(zone_rich.mod_posterior)

# habitat effects
rich.mod_zone_posterior <- zone_rich.mod_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.I.zone, area.zone) %>%
  mutate( area.trt.zone = (area.I.zone + area.zone)) 

# habitat level effects
head(rich.mod_zone_posterior)

rich.mod.zone.p <-  rich.mod_zone_posterior %>% 
  group_by(Biome_WWF_Zone) %>%
  mutate( response = "Zones",
          eff = mean(area.trt.zone),
          eff_lower = quantile(area.trt.zone, probs=0.025),
          eff_upper = quantile(area.trt.zone, probs=0.975)) %>%
  dplyr::select(c(Biome_WWF_Zone,eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.mod.zone.p

# global effects
rich.mod.fixed.p <- posterior_samples(rich.mod, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.mod.fixed.p)


# select columns of interests and give meaningful names
rich.mod_global_posterior <-  rich.mod.fixed.p %>% dplyr::select(`b_Intercept`,
                                                                   `b_log_Total_Sample_Area_mm2`) %>%
  mutate(rich.mod.I.global =`b_Intercept`,
         rich.mod.global =`b_log_Total_Sample_Area_mm2`,
         rich.mod.trt.global = (rich.mod.I.global + rich.mod.global))  %>%
  dplyr::select(-c(`b_Intercept`,
                   `b_log_Total_Sample_Area_mm2`))

head(rich.mod_global_posterior)

rich.mod.global.p <-  rich.mod_global_posterior %>% 
  mutate( response = "Global", Biome_WWF_Zone = "Global",
          eff = mean(rich.mod.trt.global),
          eff_lower = quantile(rich.mod.trt.global, probs=0.025),
          eff_upper = quantile(rich.mod.trt.global, probs=0.975)) %>%
  dplyr::select(c(Biome_WWF_Zone,eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.mod.global.p

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(rich.mod.global.p,rich.mod.zone.p, file = 'global.rich.area.poisson.zone.posteriors.Rdata')


fig_rich.mod_global_zones <- ggplot() + 
  geom_point(data = rich.mod.global.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = rich.mod.global.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.mod_global_zones











#########################
# Habitat level posterior samples
hab_levels <- rich.mod$data %>% 
  as_tibble() %>% 
  distinct(Habitat_Broad) %>% 
  mutate(level =  Habitat_Broad) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each habitat and wwf zone
hab_rich.mod_posterior <- hab_levels %>%
  mutate( area.hab = purrr::map(data, ~posterior_samples(rich.mod,
                                                               pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
                                                               fixed = TRUE,
                                                               subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.med.hab = purrr::map(data, ~posterior_samples(rich.mod,
                                                               pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert]', sep=''),
                                                               fixed = TRUE,
                                                               subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.temp.hab = purrr::map(data, ~posterior_samples(rich.mod,
                                                             pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate]', sep=''),
                                                             fixed = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.trop.hab = purrr::map(data, ~posterior_samples(rich.mod,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.tund.hab = purrr::map(data, ~posterior_samples(rich.mod,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
  )

head(hab_rich.mod_posterior)

# habitat effects
rich.mod_hab_posterior <- hab_rich.mod_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.hab, area.med.hab, area.temp.hab, area.trop.hab, area.tund.hab) %>%
  mutate( rich.mod.bor.hab = area.hab,
          rich.mod.med.hab = (area.hab + area.med.hab),
          rich.mod.temp.hab = (area.hab + area.temp.hab),
          rich.mod.trop.hab = (area.hab + area.trop.hab),
          rich.mod.tund.hab = (area.hab + area.tund.hab))

# habitat level effects
head(rich.mod_hab_posterior)

# global effects
rich.mod.fixed.p <- posterior_samples(rich.mod, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.mod.fixed.p)

# select columns of interests and give meaningful names
rich.mod_global_posterior <-  rich.mod.fixed.p %>% dplyr::select(`b_log_Total_Sample_Area_mm2`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
                                                                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`) %>%
  mutate(rich.mod.bor.global =`b_log_Total_Sample_Area_mm2`,
         rich.med.global =`b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
         rich.temp.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
         rich.trop.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
         rich.tund.global = `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`,
         rich.mod.med.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`),
         rich.mod.temp.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`),
         rich.mod.trop.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`),
         rich.mod.tund.global = (`b_log_Total_Sample_Area_mm2`+ `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`),
         ) %>%
  dplyr::select(-c(`b_log_Total_Sample_Area_mm2`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical`,
                   `b_log_Total_Sample_Area_mm2:Biome_WWF_ZoneTundra`))

head(rich.mod_global_posterior)

rich.mod.bor.p <-  rich.mod_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(rich.mod.bor.global),
          eff_lower = quantile(rich.mod.bor.global, probs=0.025),
          eff_upper = quantile(rich.mod.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
  # Med
rich.mod.med.p <-  rich.mod_global_posterior %>% 
  mutate( response = "MediterraneanandDesert", eff = mean(rich.mod.med.global),
  eff_lower = quantile(rich.mod.med.global, probs=0.025),
  eff_upper = quantile(rich.mod.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.mod.temp.p <-  rich.mod_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(rich.mod.temp.global),
eff_lower = quantile(rich.mod.temp.global, probs=0.025),
eff_upper = quantile(rich.mod.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
  rich.mod.trop.p <-  rich.mod_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(rich.mod.trop.global),
eff_lower = quantile(rich.mod.trop.global, probs=0.025),
eff_upper = quantile(rich.mod.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
  rich.mod.tund.p <-  rich.mod_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.mod.tund.global),
eff_lower = quantile(rich.mod.tund.global, probs=0.025),
eff_upper = quantile(rich.mod.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich.mod.p <- bind_rows(rich.mod.bor.p, rich.mod.med.p,
                           rich.mod.temp.p, rich.mod.trop.p,
                           rich.mod.tund.p)
head(global.rich.mod.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich.mod.p, file = 'global.rich.mod.posteriors.Rdata')


fig_rich.mod_global_zones <- ggplot() + 
  geom_point(data = global.rich.mod.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich.mod.p, aes(x = response,ymin = eff_lower,
                                        ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.mod_global_zones



# Habitat level
head(rich.mod_hab_posterior)

rich.mod.bor.hab.p <-  rich.mod_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Boreal", eff = mean(rich.mod.bor.hab),
          eff_lower = quantile(rich.mod.bor.hab, probs=0.025),
          eff_upper = quantile(rich.mod.bor.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.mod.med.hab.p <-  rich.mod_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "MediterraneanandDesert", eff = mean(rich.mod.med.hab),
          eff_lower = quantile(rich.mod.med.hab, probs=0.025),
          eff_upper = quantile(rich.mod.med.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.mod.temp.hab.p <-  rich.mod_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Temperate", eff = mean(rich.mod.temp.hab),
          eff_lower = quantile(rich.mod.temp.hab, probs=0.025),
          eff_upper = quantile(rich.mod.temp.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.mod.trop.hab.p <-  rich.mod_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tropical", eff = mean(rich.mod.trop.hab),
          eff_lower = quantile(rich.mod.trop.hab, probs=0.025),
          eff_upper = quantile(rich.mod.trop.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.mod.tund.hab.p <-  rich.mod_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tundra", eff = mean(rich.mod.tund.hab),
          eff_lower = quantile(rich.mod.tund.hab, probs=0.025),
          eff_upper = quantile(rich.mod.tund.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

hab.rich.mod.p <- bind_rows(rich.mod.bor.hab.p, rich.mod.med.hab.p,
                                rich.mod.temp.hab.p, rich.mod.trop.hab.p,
                                rich.mod.tund.hab.p)
head(hab.rich.mod.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(hab.rich.mod.p, file = 'habitat.rich.mod.posteriors.Rdata')


fig_rich.mod_habitat <- ggplot() + 
  facet_wrap(~response) +
  geom_point(data = hab.rich.mod.p, aes(x = Habitat_Broad, y = eff,color=Habitat_Broad),size = 2) +
  geom_errorbar(data = hab.rich.mod.p, aes(x = Habitat_Broad,ymin = eff_lower,
                                               ymax = eff_upper, color=Habitat_Broad),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.mod_habitat


# SAMPLES ###############################
setwd('~/Desktop/')
load( 'gsb_rich_samps-poisson.Rdata')

summary(rich.samps)

# Habitat level posterior samples
hab_levels <- rich.samps$data %>% 
  as_tibble() %>% 
  distinct(Habitat_Broad) %>% 
  mutate(level =  Habitat_Broad) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each habitat and wwf zone
hab_rich.samps_posterior <- hab_levels %>%
  mutate( area.hab = purrr::map(data, ~posterior_samples(rich.samps,
                                                         pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Number_Samples]', sep=''),
                                                         fixed = TRUE,
                                                         subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.med.hab = purrr::map(data, ~posterior_samples(rich.samps,
                                                             pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Number_Samples:Biome_WWF_ZoneMediterraneanandDesert]', sep=''),
                                                             fixed = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.temp.hab = purrr::map(data, ~posterior_samples(rich.samps,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Number_Samples:Biome_WWF_ZoneTemperate]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.trop.hab = purrr::map(data, ~posterior_samples(rich.samps,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Number_Samples:Biome_WWF_ZoneTropical]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.tund.hab = purrr::map(data, ~posterior_samples(rich.samps,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Number_Samples:Biome_WWF_ZoneTundra]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
  )

View(hab_rich.samps_posterior)

# habitat effects
rich.samps_hab_posterior <- hab_rich.samps_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.hab, area.med.hab, area.temp.hab, area.trop.hab, area.tund.hab) %>%
  mutate( rich.samps.bor.hab = area.hab,
          rich.samps.med.hab = (area.hab + area.med.hab),
          rich.samps.temp.hab = (area.hab + area.temp.hab),
          rich.samps.trop.hab = (area.hab + area.trop.hab),
          rich.samps.tund.hab = (area.hab + area.tund.hab))

# habitat level effects
head(rich.samps_hab_posterior)

# global effects
rich.samps.fixed.p <- posterior_samples(rich.samps, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.samps.fixed.p)

# select columns of interests and give meaningful names
rich.samps_global_posterior <-  rich.samps.fixed.p %>% dplyr::select(`b_log_Total_Number_Samples`,
                                                                   `b_log_Total_Number_Samples:Biome_WWF_ZoneMediterraneanandDesert`,
                                                                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTemperate`,
                                                                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTropical`,
                                                                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTundra`) %>%
  mutate(rich.samps.bor.global =`b_log_Total_Number_Samples`,
         rich.med.global =`b_log_Total_Number_Samples:Biome_WWF_ZoneMediterraneanandDesert`,
         rich.temp.global = `b_log_Total_Number_Samples:Biome_WWF_ZoneTemperate`,
         rich.trop.global = `b_log_Total_Number_Samples:Biome_WWF_ZoneTropical`,
         rich.tund.global = `b_log_Total_Number_Samples:Biome_WWF_ZoneTundra`,
         rich.samps.med.global = (`b_log_Total_Number_Samples`+ `b_log_Total_Number_Samples:Biome_WWF_ZoneMediterraneanandDesert`),
         rich.samps.temp.global = (`b_log_Total_Number_Samples`+ `b_log_Total_Number_Samples:Biome_WWF_ZoneTemperate`),
         rich.samps.trop.global = (`b_log_Total_Number_Samples`+ `b_log_Total_Number_Samples:Biome_WWF_ZoneTropical`),
         rich.samps.tund.global = (`b_log_Total_Number_Samples`+ `b_log_Total_Number_Samples:Biome_WWF_ZoneTundra`),
  ) %>%
  dplyr::select(-c(`b_log_Total_Number_Samples`,
                   `b_log_Total_Number_Samples:Biome_WWF_ZoneMediterraneanandDesert`,
                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTemperate`,
                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTropical`,
                   `b_log_Total_Number_Samples:Biome_WWF_ZoneTundra`))

head(rich.samps_global_posterior)

rich.samps.bor.p <-  rich.samps_global_posterior %>% 
  mutate( response = "Boreal", eff = mean(rich.samps.bor.global),
          eff_lower = quantile(rich.samps.bor.global, probs=0.025),
          eff_upper = quantile(rich.samps.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.samps.med.p <-  rich.samps_global_posterior %>% 
  mutate( response = "MediterraneanandDesert", eff = mean(rich.samps.med.global),
          eff_lower = quantile(rich.samps.med.global, probs=0.025),
          eff_upper = quantile(rich.samps.med.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.samps.temp.p <-  rich.samps_global_posterior %>% 
  mutate( response = "Temperate", eff = mean(rich.samps.temp.global),
          eff_lower = quantile(rich.samps.temp.global, probs=0.025),
          eff_upper = quantile(rich.samps.temp.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.samps.trop.p <-  rich.samps_global_posterior %>% 
  mutate( response = "Tropical", eff = mean(rich.samps.trop.global),
          eff_lower = quantile(rich.samps.trop.global, probs=0.025),
          eff_upper = quantile(rich.samps.trop.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.samps.tund.p <-  rich.samps_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.samps.tund.global),
          eff_lower = quantile(rich.samps.tund.global, probs=0.025),
          eff_upper = quantile(rich.samps.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

global.rich.samps.p <- bind_rows(rich.samps.bor.p, rich.samps.med.p,
                                rich.samps.temp.p, rich.samps.trop.p,
                                rich.samps.tund.p)
head(global.rich.samps.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich.samps.p, file = 'global.rich.samps.posteriors.Rdata')


fig_rich.samps_global_zones <- ggplot() + 
  geom_point(data = global.rich.samps.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich.samps.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.samps_global_zones



# Habitat level
head(rich.samps_hab_posterior)

rich.samps.bor.hab.p <-  rich.samps_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Boreal", eff = mean(rich.samps.bor.hab),
          eff_lower = quantile(rich.samps.bor.hab, probs=0.025),
          eff_upper = quantile(rich.samps.bor.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Med
rich.samps.med.hab.p <-  rich.samps_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "MediterraneanandDesert", eff = mean(rich.samps.med.hab),
          eff_lower = quantile(rich.samps.med.hab, probs=0.025),
          eff_upper = quantile(rich.samps.med.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Temp
rich.samps.temp.hab.p <-  rich.samps_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Temperate", eff = mean(rich.samps.temp.hab),
          eff_lower = quantile(rich.samps.temp.hab, probs=0.025),
          eff_upper = quantile(rich.samps.temp.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Trop
rich.samps.trop.hab.p <-  rich.samps_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tropical", eff = mean(rich.samps.trop.hab),
          eff_lower = quantile(rich.samps.trop.hab, probs=0.025),
          eff_upper = quantile(rich.samps.trop.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 
# Tund
rich.samps.tund.hab.p <-  rich.samps_hab_posterior %>% group_by(Habitat_Broad) %>%
  mutate( response = "Tundra", eff = mean(rich.samps.tund.hab),
          eff_lower = quantile(rich.samps.tund.hab, probs=0.025),
          eff_upper = quantile(rich.samps.tund.hab, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

hab.rich.samps.p <- bind_rows(rich.samps.bor.hab.p, rich.samps.med.hab.p,
                             rich.samps.temp.hab.p, rich.samps.trop.hab.p,
                             rich.samps.tund.hab.p)
head(hab.rich.samps.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(hab.rich.samps.p, file = 'habitat.rich.samps.posteriors.Rdata')


fig_rich.samps_habitat <- ggplot() + 
  facet_wrap(~response) +
  geom_point(data = hab.rich.samps.p, aes(x = Habitat_Broad, y = eff,color=Habitat_Broad),size = 2) +
  geom_errorbar(data = hab.rich.samps.p, aes(x = Habitat_Broad,ymin = eff_lower,
                                            ymax = eff_upper, color=Habitat_Broad),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.samps_habitat


