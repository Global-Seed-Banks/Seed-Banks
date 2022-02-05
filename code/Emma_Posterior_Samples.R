
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
#load( 'gsb_rich_samps-12179403.Rdata')
load( 'gsb_rich_area-12179402.Rdata')


summary(rich.area)

# STUDY LEVEL POSTERIORS
hab_levels <- rich.area$data %>% 
  as_tibble() %>% 
  distinct(Habitat_Broad) %>% 
  mutate(level =  Habitat_Broad) %>%
  nest_legacy(level)

# extract 1000 study-level posterior samples for each habitat and wwf zone
hab_sample_posterior <- hab_levels %>%
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

View(hab_sample_posterior)



area_hab_posterior <- hab_sample_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.hab, area.med.hab, area.temp.hab, area.trop.hab, area.tund.hab) %>%
  mutate( bor.hab = area.hab,
          med.hab = (area.hab + area.med.hab),
          temp.hab = (area.hab + area.temp.hab),
          trop.hab = (area.hab + area.trop.hab),
          tund.hab = (area.hab + area.tund.hab))


View(area_hab_posterior)







