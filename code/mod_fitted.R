rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)
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
          Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)

sb_rich_area_zone <- sb_rich_area %>% filter(!Habitat_Broad == "Arable")

rich_zones.min <- sb_rich_area_zone %>% 
  group_by(Biome_WWF_Zone) %>%
  filter(Total_Sample_Area_m2 == min(Total_Sample_Area_m2) ) %>%
  mutate(value = "min")

head(rich_zones.min)


rich_zones.max <- sb_rich_area_zone %>% 
  group_by(Biome_WWF_Zone) %>%
  filter(Total_Sample_Area_m2 == max(Total_Sample_Area_m2) ) %>%
  mutate(value = "max")

head(rich_zones.min)


rich_zones.mm <- rich_zones.max %>% rbind(rich_zones.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_zone.Rdata')
load( 'rich_hab.Rdata')
load( 'rich_deg.Rdata')

head(sb_rich_area_zone)

rich_zones_fitted <- sb_rich_area_zone %>% 
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone) %>% 
  summarise(Centred_log_Total_Sample_Area_m2,
            Total_Sample_Area_m2,
  ) %>%
  nest(data = c(Biome_WWF_Zone, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2)) %>%
  mutate(fitted = map(data, ~epred_draws(rich_zones, newdata= .x, re_formula = ~(Biome_WWF_Zone * Centred_log_Total_Sample_Area_m2) ))) 


head(rich_zones_fitted)


rich_zones_fitted_df  <- rich_zones_fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

View(rich_zones_fitted_df)


head(rich_zones_fitted_df)

rich_zones_fit <- rich_zones_fitted_df %>%
  select(-.draw) %>%
  group_by(Biome_WWF_Zone_group, Centred_log_Total_Sample_Area_m2,
           Total_Sample_Area_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct() 

head(rich_zones_fit)

rich_zones_fit.min <- rich_zones_fit %>% 
  group_by(Biome_WWF_Zone_group) %>%
  filter(Total_Sample_Area_m2 == min(Total_Sample_Area_m2) ) %>%
  mutate(value = "min")

head(rich_zones_fit.min)


rich_zones_fit.max <- rich_zones_fit %>% 
  group_by(Biome_WWF_Zone_group) %>%
  filter(Total_Sample_Area_m2 == max(Total_Sample_Area_m2) ) %>%
  mutate(value = "max")

head(rich_zones_fit.min)


rich_zones_fit.mm <- rich_zones_fit.max %>% rbind(rich_zones_fit.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(rich_zones_fit.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')


rich_zone_i <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_zones_fit.mm,
             aes(x = Biome_WWF_Zone , y = P_Estimate, colour = Biome_WWF_Zone, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_zones_fit.mm,
                aes(x = Biome_WWF_Zone , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = Biome_WWF_Zone, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_shape_manual(name = "Average total species",
                     values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ylim(0, 150)+
  labs(x='',
       y = 'Average no. species',
       subtitle= '') 


rich_zone_i



seeds_zones_fitted <- sb_seeds_area_zone %>% 
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone) %>% 
  summarise(Centred_log_Total_Sample_Area_m2,
            Total_Sample_Area_m2,
  ) %>%
  nest(data = c(Biome_WWF_Zone, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2)) %>%
  mutate(fitted = map(data, ~epred_draws(seeds_zones, newdata= .x, re_formula = ~(Biome_WWF_Zone * Centred_log_Total_Sample_Area_m2) ))) 


head(seeds_zones_fitted)


seeds_zones_fitted_df  <- seeds_zones_fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(seeds_zones_fitted_df)


head(seeds_zones_fitted_df)

seeds_zones_fit <- seeds_zones_fitted_df %>%
  select(-.draw) %>%
  group_by(Biome_WWF_Zone_group, Centred_log_Total_Sample_Area_m2,
           Total_Sample_Area_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct() 

head(seeds_zones_fit)

seeds_zones_fit.min <- seeds_zones_fit %>% 
  group_by(Biome_WWF_Zone_group) %>%
  filter(Total_Sample_Area_m2 == min(Total_Sample_Area_m2) ) %>%
  mutate(value = "min")

head(seeds_zones_fit.min)


seeds_zones_fit.max <- seeds_zones_fit %>% 
  group_by(Biome_WWF_Zone_group) %>%
  filter(Total_Sample_Area_m2 == max(Total_Sample_Area_m2) ) %>%
  mutate(value = "max")

head(seeds_zones_fit.min)


seeds_zones_fit.mm <- seeds_zones_fit.max %>% rbind(seeds_zones_fit.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(seeds_zones_fit.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')


seeds_zone_i <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = seeds_zones_fit.mm,
             aes(x = Biome_WWF_Zone , y = P_Estimate, colour = Biome_WWF_Zone, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = seeds_zones_fit.mm,
                aes(x = Biome_WWF_Zone , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = Biome_WWF_Zone, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_shape_manual(name = "Average total species",
                     values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  coord_cartesian( ylim = c(0,150)) +
  labs(x='',
       y = 'Average no. seeds',
       subtitle= '') 


seeds_zone_i



seeds_zone_leg <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = seeds_zones_fit.mm,
             aes(x = Biome_WWF_Zone , y = P_Estimate, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = seeds_zones_fit.mm,
                aes(x = Biome_WWF_Zone , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_shape_manual(name = "Average total species",
                     values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  coord_cartesian( ylim = c(0,150)) +
  labs(x='',
       y = 'Average no. seeds',
       subtitle= '') 


seeds_zone_leg

# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# overall legend
rich_zone_leg <- g_legend(rich_zone)
# site-level legend
av_leg <- g_legend(seeds_zone_leg)

zone_legend <- grid.arrange( arrangeGrob(
  # overall
  rich_zone_leg, 
  # site
  av_leg,
  ncol = 2, nrow = 1
 # heights = c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10),
  #layout_matrix = rbind(c(NA), c(NA), c(NA), c(1), c(2), c(3), c(NA), c(NA))
) )

# LANDSCAPE 12 X 15
(rich_zone + theme(legend.position="none") | seeds_zone) / (rich_zone_i | seeds_zone_i) / (zone_legend ) + plot_annotation(title = "WWF Zones",
                                                                        theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + 
  plot_layout(ncol=1, nrow=3, heights = c(10,10,1))
