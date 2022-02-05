
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


setwd(paste0(path2wd, 'Data/'))
sb <- read.csv(paste0(path2wd, 'gsb_prep.csv'))


setwd('~/Desktop/')
load( 'gsb_rich_area-poisson.Rdata')


summary(rich.area)

# Habitat level posterior samples
hab_levels <- rich.area$data %>% 
  as_tibble() %>% 
  distinct(Habitat_Broad) %>% 
  mutate(level =  Habitat_Broad) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each habitat and wwf zone
hab_rich.area_posterior <- hab_levels %>%
  mutate( area.hab = purrr::map(data, ~posterior_samples(rich.area,
                                                               pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
                                                               fixed = TRUE,
                                                               subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.med.hab = purrr::map(data, ~posterior_samples(rich.area,
                                                               pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneMediterraneanandDesert]', sep=''),
                                                               fixed = TRUE,
                                                               subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.temp.hab = purrr::map(data, ~posterior_samples(rich.area,
                                                             pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTemperate]', sep=''),
                                                             fixed = TRUE,
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.trop.hab = purrr::map(data, ~posterior_samples(rich.area,
                                                              pars = paste('r_Habitat_Broad[', as.character(.x$level), ',log_Total_Sample_Area_mm2:Biome_WWF_ZoneTropical]', sep=''),
                                                              fixed = TRUE,
                                                              subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
          area.tund.hab = purrr::map(data, ~posterior_samples(rich.area,
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
rich.area.fixed.p <- posterior_samples(rich.area, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

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
save(global.rich.area.p, file = 'global.rich.area.posteriors.Rdata')


fig_rich.area_global_zones <- ggplot() + 
  geom_point(data = global.rich.area.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich.area.p, aes(x = response,ymin = eff_lower,
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
save(hab.rich.area.p, file = 'habitat.rich.area.posteriors.Rdata')


fig_rich.area_habitat <- ggplot() + 
  facet_wrap(~response) +
  geom_point(data = hab.rich.area.p, aes(x = Habitat_Broad, y = eff,color=Habitat_Broad),size = 2) +
  geom_errorbar(data = hab.rich.area.p, aes(x = Habitat_Broad,ymin = eff_lower,
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

fig_rich.area_habitat


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


