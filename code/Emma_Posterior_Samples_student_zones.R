
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
sb <- read.csv('sb_prep.csv')


setwd(paste0(path2wd, 'Model_Fits/'))
load( 'gsb_rich_area_zone.Rdata')


summary(rich.area)

rich.area$data

# Habitat level posterior samples
zone_levels <- rich.area$data %>% 
  as_tibble() %>% 
  distinct(Biome_WWF_Zone) %>% 
  mutate(level =  Biome_WWF_Zone) %>%
  nest_legacy(level)


# extract 1000 study-level posterior samples for each habitat and wwf zone
zone_rich.area_posterior <- zone_levels %>%
  mutate( area.I.zone = purrr::map(data, ~posterior_samples(rich.area,
                                                          pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',Intercept]', sep=''),
                                                          fixed = TRUE,
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
    area.zone = purrr::map(data, ~posterior_samples(rich.area,
                                                         pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
                                                         fixed = TRUE,
                                                         subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         
  )

head(zone_rich.area_posterior)

# habitat effects
rich.area_zone_posterior <- zone_rich.area_posterior  %>% 
  dplyr::select(-data) %>% 
  unnest_legacy(area.I.zone, area.zone) %>%
  mutate( area.trt.zone = (area.I.zone + area.zone)) 

# habitat level effects
head(rich.area_zone_posterior)

# global effects
rich.area.fixed.p <- posterior_samples(rich.area, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))

head(rich.area.fixed.p)

# select columns of interests and give meaningful names
rich.area_global_posterior <-  rich.area.fixed.p %>% dplyr::select(`b_Intercept`,
                                                                   `b_log_Total_Sample_Area_mm2`) %>%
  mutate(rich.area.I.global =`b_Intercept`,
    rich.area.global =`b_log_Total_Sample_Area_mm2`,
    rich.area.trt.global = (rich.area.I.global + rich.area.global))  %>%
  dplyr::select(-c(`b_Intercept`,
    `b_log_Total_Sample_Area_mm2`))

head(rich.area_global_posterior)

rich.area.global.p <-  rich.area_global_posterior %>% 
  mutate( response = "Global", Biome_WWF_Zone = "Global",
          eff = mean(rich.area.trt.global),
          eff_lower = quantile(rich.area.trt.global, probs=0.025),
          eff_upper = quantile(rich.area.trt.global, probs=0.975)) %>%
  dplyr::select(c(Biome_WWF_Zone,eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.area.global.p

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich.area.p, file = 'global.rich.area.student.zone.posteriors.Rdata')


fig_rich.area_global_zones <- ggplot() + 
  geom_point(data = rich.area.global.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = rich.area.global.p, aes(x = response,ymin = eff_lower,
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



# for every zone
head(rich.area_zone_posterior)

rich.area.zone.p <-  rich.area_zone_posterior %>% group_by(Biome_WWF_Zone) %>%
  mutate( response = "Zones", eff = mean(area.trt.zone),
          eff_lower = quantile(area.trt.zone, probs=0.025),
          eff_upper = quantile(area.trt.zone, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower, response)) %>% distinct() 

head(rich.area.zone.p)


setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(hab.rich.area.p, file = 'habitat.rich.area.student.zones.posteriors.Rdata')


fig_rich.area_zone <- ggplot() + 
  #facet_wrap(~response) +
  geom_point(data = rich.area.zone.p, aes(x = Biome_WWF_Zone, y = eff, color=Biome_WWF_Zone),size = 2) +
  geom_errorbar(data = rich.area.zone.p, aes(x = Biome_WWF_Zone,ymin = eff_lower,
                                            ymax = eff_upper, color=Biome_WWF_Zone),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_zone

