
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
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

sb_ratio <- sb_prep %>% 
  filter(!is.na(ratio_seeds_species),
         !ratio_seeds_species == 0,
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome = as.factor(Biome),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  

head(sb_ratio)



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'ratio_aq.Rdata')
load( 'ratio_ar.Rdata')
load( 'ratio_forest.Rdata')
load( 'ratio_grass.Rdata')
load( 'ratio_med_de.Rdata')
load( 'ratio_po_alp.Rdata')
load( 'ratio_wetland.Rdata')


#aquatic

sb_aq_ra <- sb_ratio %>% filter(Realm == "Aquatic") 


summary(mod_aq_ra)

aq_ra <- conditional_effects(mod_aq_ra, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

aq_ra

aq_ra_raf <-
  as.data.frame(aq_ra$`Habitat_degraded`)

head(aq_ra_raf)

aq_ra_ce <- aq_ra_raf %>%
  select( Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Aquatic",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model,  Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

aq_ra_ce
sb_aq_ra

fig_aq_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_aq_ra,
             aes(x = Habitat_degraded, y = ratio_seeds_species, 
                 shape= Habitat_degraded, color=Realm
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = aq_ra_ce,
             aes(x =  Habitat_degraded, y = Estimate, color=Model,
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = aq_ra_ce,
                aes(x =   Habitat_degraded, ymin = `Lower CI`, ymax = `Upper CI`, color=Model),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #  y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "g) Aquatic" ) +
  scale_color_manual( values= c(    "#447fdd"))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank(), axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_aq_ra


#arable

sb_arable_ra <- sb_ratio %>%
  filter(Realm == "Arable")  %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))


summary(mod_ar_ra)

arable_ra <- conditional_effects(mod_ar_ra, effects = 'Biome', re_formula = NA, method = 'fitted')  # conditional effects

arable_ra

arable_ra_raf <-
  as.data.frame(arable_ra$`Biome`)

head(arable_ra_raf)

arable_ra_ce <- arable_ra_raf %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate( Model = "Arable",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Estimate, `Upper CI`, `Lower CI`) 

arable_ra_ce
sb_arable_ra

fig_arable_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_arable_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 colour = Realm , 
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = arable_ra_ce,
             aes(x =  Biome, y = Estimate, colour =  Model),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = arable_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Model),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #  y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "e) Arable" ) +
  scale_color_manual( values= c( "#99610a"  ))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_arable_ra



#forests
sb_forest_ra <- sb_ratio %>%
  filter(Realm == "Forest") 


summary(mod_forest_ra)

forest_ra <- conditional_effects(mod_forest_ra, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

forest_ra

forest_ra_raf <-
  as.data.frame(forest_ra$`Biome:Habitat_degraded`)

head(forest_ra_raf)

forest_ra_ce <- forest_ra_raf %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Forest",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

forest_ra_ce
sb_forest_ra

fig_forest_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_forest_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = forest_ra_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = forest_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
      # y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "b) Forests" ) +
  scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_forest_ra



# Grass

sb_grass_ra <- sb_ratio %>%
  filter(Realm == "Grassland") 


summary(mod_grass_ra)

grass_ra <- conditional_effects(mod_grass_ra, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

grass_ra

grass_ra_raf <-
  as.data.frame(grass_ra$`Biome:Habitat_degraded`)

head(grass_ra_raf)

grass_ra_ce <- grass_ra_raf %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Grassland",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

grass_ra_ce
sb_grass_ra

fig_grass_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_grass_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = grass_ra_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = grass_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #  y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "c) Grasslands" ) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_grass_ra



# Med

sb_med_de_ra <- sb_ratio %>%
  filter(Realm == "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))

sb_med_de_ra

summary(mod_med_de_ra)

med_de_ra <- conditional_effects(mod_med_de_ra, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

med_de_ra

med_de_ra_raf <-
  as.data.frame(med_de_ra$`Biome:Habitat_degraded`)

head(med_de_ra_raf)

med_de_ra_ce <- med_de_ra_raf %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Mediterranean Forests, Woodlands and Scrub",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))

med_de_ra_ce
sb_med_de_ra

fig_med_de_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_med_de_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = med_de_ra_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = med_de_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '',
        y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "d) Mediterranean and Deserts" ) +
  scale_color_manual( values= c(    "#da7901",  "#fab255"))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_med_de_ra




# tund

sb_tund_ra <- sb_ratio %>%
  filter(Realm == "Tundra") 

sb_tund_ra

summary(mod_tund_ra)

tund_ra <- conditional_effects(mod_tund_ra, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

tund_ra

tund_ra_raf <-
  as.data.frame(tund_ra$`Habitat_degraded`)

head(tund_ra_raf)

tund_ra_ce <- tund_ra_raf %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Tundra",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

tund_ra_ce
sb_tund_ra

fig_tund_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_tund_ra,
             aes(x = Habitat_degraded, y = ratio_seeds_species, 
                 colour = Realm ,  shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2) +
  geom_point(data = tund_ra_ce,
             aes(x =  Habitat_degraded, y = Estimate, colour =  Model, 
                 shape = Habitat_degraded ),size = 3) +
  geom_errorbar(data = tund_ra_ce,
                aes(x =   Habitat_degraded, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Model),
                size = 1, width = 0,  ) +
  labs(x = '',
        y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "a) Tundra" ) +
  scale_color_manual( values= c(   "#94b594" ))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_tund_ra




# wetlsnd

sb_wetland_ra <- sb_ratio %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  filter(Realm == "Wetland") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))

sb_wetland_ra

summary(mod_wetland_ra)

wetland_ra <- conditional_effects(mod_wetland_ra, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

wetland_ra

wetland_ra_raf <-
  as.data.frame(wetland_ra$`Biome:Habitat_degraded`)

head(wetland_ra_raf)

wetland_ra_ce <- wetland_ra_raf %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Wetland",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

wetland_ra_ce
sb_wetland_ra

fig_wetland_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 colour = Realm , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_ra_ce,
             aes(x =  Biome, y = Estimate, colour =  Model, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = wetland_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Model, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  scale_color_manual( values= c(  "#20B2AA" ))+
  labs(x = '',
        y = expression(paste('Ratio (Seeds/Species)')) ,
       subtitle=  "f) Wetlands" ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
   coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_wetland_ra


legend_ra <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_ra,
             aes(x = Biome, y = ratio_seeds_species, 
                 group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_ra_ce,
             aes(x =  Biome, y = Estimate,  group= Habitat_degraded, 
                 shape = Habitat_degraded ), alpha = 0.7,
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = wetland_ra_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
 # scale_color_manual( values= c(  "#20B2AA" ))+
  labs(x = '',
       y = expression(paste('Ratio (Seeds/Species)')) , shape = "Habitat",
       subtitle=  "f) Wetlands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,350)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


legend_ra


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_ra <- g_legend(legend_ra)

ratio_fig <- (fig_tund_ra + fig_forest_ra + fig_grass_ra) /
  ( fig_med_de_ra + fig_arable_ra) /
  ( fig_wetland_ra + fig_aq_ra  )/ (legend_ra) + plot_layout(heights = c(10, 10,  10,  1))

ratio_fig
