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
sb_biome_area <- sb_prep %>% # filter(!is.na(Total_Species),
  #    !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))



setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')
load( 'seed_m2.Rdata')



summary(sb_biome_area)

sb_biome_area_prep <- sb_biome_area %>% distinct(Total_Sample_Area_mm2, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
  mutate(Total_Sample_Area_mm2 = round(Total_Sample_Area_mm2, 2),
         Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 6),
         Centred_log_Total_Sample_Area_m2 = round(Centred_log_Total_Sample_Area_m2, 6),
         ) %>% arrange(Total_Sample_Area_m2) #

summary(sb_biome_area_prep)

sb_biome_area_prep %>%
  summarise(Total_Sample_Area_m2 = seq(0.010000, 816.0000, length.out = 200 ) ) 
              
sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 0.010000)
sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 816.0000)


rich_biome_predict <- sb_biome_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Total_Sample_Area_m2 = seq(0.010000, 816.0000, length.out = 200 ),
            Total_Sample_Area_mm2 = seq(1500, 816000000, length.out = 200),
            Centred_log_Total_Sample_Area_m2 =  seq(-5.4512, 7.7556, length.out = 200) ) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Total_Sample_Area_mm2)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 


View(rich_biome_predict)


head(rich_biome_predict)


rich_biome_predict_df <- rich_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  %>% mutate( log_predicted = .prediction,
              predicted = exp(.prediction)) %>%
  select(-.prediction)


head(rich_biome_predict_df)



rich_biome_fit <- rich_biome_fitted_df %>%
  select(-.draw) %>%
  group_by(Biome_Broad_Hab_group, Centred_log_Total_Sample_Area_m2,
           Total_Sample_Area_m2) %>%
  mutate( Estimate = mean(.epred, na.rm =TRUE ),
          `Upper CI` = quantile(.epred, probs=0.975, na.rm =TRUE ),
          `Lower CI` = quantile(.epred, probs=0.025, na.rm =TRUE )
  ) %>% 
  select(-.epred) %>% distinct() %>% ungroup()

head(rich_biome_fit)

rich_biome_fit.min <- rich_biome_fit %>% 
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>%
  slice(which.min(Total_Sample_Area_m2)) %>%
  mutate(value = "min")

head(rich_biome_fit.min)


rich_biome_fit.max <- rich_biome_fit %>% 
  group_by(Biome_Broad_Hab_group) %>%
  slice(which.max(Total_Sample_Area_m2)) %>%
  mutate(value = "max")

colnames(rich_biome_fit.max)


rich_biome_fit.mm <- rich_biome_fit.min %>% rbind(rich_biome_fit.max) %>%
  mutate(value = fct_relevel(value, c("min","max")))  %>%
  ungroup() %>%
  mutate(Model = "Species Richness",
         `WWF Biome` = Biome_Broad_Hab,
         Centred_log_Total_Sample_Area_m2 = round(Centred_log_Total_Sample_Area_m2, 2),
         Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 2),
         Estimate = round(Estimate, 2),
         `Upper CI` = round(`Upper CI`, 2),
         `Lower CI` = round(`Lower CI`, 2)
  ) %>%
  select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  relocate( Model:`WWF Biome`, .before = Centred_log_Total_Sample_Area_m2 ) %>%
  arrange(`WWF Biome`)


View(rich_biome_fit.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')

setwd(paste0(path2wd, 'Tables/'))
write.csv(rich_biome_fit.mm, "table_4.csv")

rich_biome_i <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_fit.mm,
             aes(x = Biome_Broad_Hab , y = P_Estimate, colour = Biome_Broad_Hab, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_fit.mm,
                aes(x = Biome_Broad_Hab , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = Biome_Broad_Hab, group = value),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
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
       y = 'Average species richness in the soil seed bank',
       subtitle= 'a) Species richness') 


rich_biome_i



seeds_biome_fitted <- sb_biome_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Centred_log_Total_Sample_Area_m2,
            Total_Sample_Area_m2,
  ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2)) %>%
  mutate(fitted = map(data, ~epred_draws(seeds_biome_broad, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 


head(seeds_biome_fitted)


seeds_biome_fitted_df  <- seeds_biome_fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(seeds_biome_fitted_df)


head(seeds_biome_fitted_df)

seeds_biome_fit <- seeds_biome_fitted_df %>%
  select(-.draw) %>%
  group_by(Biome_Broad_Hab_group, Centred_log_Total_Sample_Area_m2,
           Total_Sample_Area_m2) %>%
  mutate( Estimate = mean(.epred, na.rm =TRUE),
          `Upper CI` = quantile(.epred, probs=0.975, na.rm =TRUE),
          `Lower CI` = quantile(.epred, probs=0.025, na.rm =TRUE)
  ) %>% 
  select(-.epred) %>% distinct() 

head(seeds_biome_fit)

seeds_biome_fit.min <- seeds_biome_fit %>% 
  group_by(Biome_Broad_Hab_group) %>%
  slice(which.min(Total_Sample_Area_m2)) %>%
  mutate(value = "min")

head(seeds_biome_fit.min)


seeds_biome_fit.max <- seeds_biome_fit %>% 
  group_by(Biome_Broad_Hab_group) %>%
  slice(which.max(Total_Sample_Area_m2)) %>%
  mutate(value = "max")

colnames(seeds_biome_fit.min)


seeds_biome_fit.mm <- seeds_biome_fit.min %>% rbind(seeds_biome_fit.max) %>%
  mutate(value = fct_relevel(value, c("min","max")))  %>%
  ungroup() %>%
  mutate(Model = "Number of Seeds",
         `WWF Biome` = Biome_Broad_Hab,
         Centred_log_Total_Sample_Area_m2 = round(Centred_log_Total_Sample_Area_m2, 2),
         Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 2),
         Estimate = round(Estimate, 2),
         `Upper CI` = round(`Upper CI`, 2),
         `Lower CI` = round(`Lower CI`, 2)
  ) %>%
  select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  relocate( Model:`WWF Biome`, .before = Centred_log_Total_Sample_Area_m2 ) %>%
  arrange(`WWF Biome`)

head(seeds_biome_fit.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')


setwd(paste0(path2wd, 'Tables/'))
write.csv(seeds_biome_fit.mm, "table_5.csv")

seeds_biome_i <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = seeds_biome_fit.mm,
             aes(x = `WWF Biome`  , y = Estimate, colour = `WWF Biome` , shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = seeds_biome_fit.mm,
                aes(x = `WWF Biome`  , ymin = `Lower CI`, ymax = `Upper CI`,  colour = `WWF Biome` , group = value),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
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
       y = 'Average number of seeds in the soil seed bank',
       subtitle= 'b) Number of seeds') 


seeds_biome_i



seeds_biome_leg <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = seeds_biome_fit.mm,
             aes(x = Biome_Broad_Hab , y = P_Estimate, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = seeds_biome_fit.mm,
                aes(x = Biome_Broad_Hab , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  group = value),
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


seeds_biome_leg

# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# overall legend
rich_biome_leg <- g_legend(fig_rich.biome_broad)
# site-level legend
av_leg <- g_legend(seeds_biome_leg)

biome_legend <- grid.arrange( arrangeGrob(
  # overall
  rich_biome_leg, 
  # site
  av_leg,
  ncol = 1, nrow = 2
  # heights = c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10),
  #layout_matrix = rbind(c(NA), c(NA), c(NA), c(1), c(2), c(3), c(NA), c(NA))
) )

# LANDSCAPE 12 X 15
(rich_biome_i | seeds_biome_i) / (biome_legend ) + #plot_annotation(title = "WWF Biome",
  #        theme = theme(plot.title = element_text(hjust = 0.5, size= 18))) + 
  plot_layout(ncol=1, nrow=2, heights = c(10,2))