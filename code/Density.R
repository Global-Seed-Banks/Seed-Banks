

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

sb_density <- sb_prep %>% 
  filter(!is.na(Seed_density_m2),
         !Seed_density_m2 == 0,
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome = as.factor(Biome),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  

head(sb_density)



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# Biomes run on cluster, load in Biome objects here
load( 'den_aq.Rdata')
load( 'den_ar.Rdata')
load( 'den_forest.Rdata')
load( 'den_grass.Rdata')
load( 'den_med_de.Rdata')
load( 'den_po_alp.Rdata')
load( 'den_wetland.Rdata')


#aquatic


#forests
sb_aq_d <- sb_density %>% filter(Realm == "Aquatic") 


summary(mod_aq_d)

aq_d <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

aq_d

aq_d_df <-
  as.data.frame(aq_d$`Habitat_degraded`)

head(aq_d_df)

aq_d_ce <- aq_d_df %>%
  select( Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Biome = "Aquatic",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome,  Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

aq_d_ce

write.csv(aq_d_ce,  "Data/aq_d_ce.csv")

sb_aq_d

fig_aq_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_aq_d,
             aes(x = Habitat_degraded, y = Seed_density_m2, 
                  shape= Habitat_degraded, color=Realm
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = aq_d_ce,
             aes(x =  Habitat_degraded, y = Estimate, color=Biome,
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = aq_d_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, color=Biome),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "g) Aquatic" ) +
  scale_color_manual( values= c(    "#447fdd"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,15000, 200000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                # axis.text.y=element_blank(), 
                                 axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_aq_d


#arable

sb_arable_d <- sb_density %>%
  filter(Realm == "Arable") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))


summary(mod_ar_d)

arable_d <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted')  # conditional effects

arable_d

arable_d_df <-
  as.data.frame(arable_d$`Biome`)

head(arable_d_df)

arable_d_ce <- arable_d_df %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate( Realm = "Arable",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Realm, Estimate, `Upper_CI`, `Lower_CI`) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

arable_d_ce

write.csv(arable_d_ce,  "Data/arable_d_ce.csv")


sb_arable_d

fig_arable_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_arable_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , 
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = arable_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = arable_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "e) Arable" ) +
  scale_color_manual( values= c("#99610a" , "#E2C59F", "#AA3929" ))+
  coord_cartesian( ylim = c(0,20000)) +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_arable_d



#forests
sb_forest_d <- sb_density %>%
  filter(Realm == "Forest") 


summary(mod_forest_d)

forest_d <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

forest_d

forest_d_df <-
  as.data.frame(forest_d$`Biome:Habitat_degraded`)

head(forest_d_df)

forest_d_ce <- forest_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Forest",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Realm, Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

forest_d_ce

write.csv(forest_d_ce,  "Data/forest_d_ce.csv")

sb_forest_d

fig_forest_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_forest_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
                 ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = forest_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                shape = Habitat_degraded ),
  position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = forest_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "b) Forests" ) +
 scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ))+
  coord_cartesian( ylim = c(0,8000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_forest_d



# Grass

sb_grass_d <- sb_density %>%
  filter(Realm == "Grassland") 


summary(mod_grass_d)

grass_d <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

grass_d

grass_d_df <-
  as.data.frame(grass_d$`Biome:Habitat_degraded`)

head(grass_d_df)

grass_d_ce <- grass_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Grassland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Realm, Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

grass_d_ce

write.csv(grass_d_ce,  "Data/grass_d_ce.csv")

sb_grass_d

fig_grass_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_grass_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = grass_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = grass_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "c) Grasslands" ) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"))+
  coord_cartesian( ylim = c(0,10000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_grass_d



# Med

sb_med_de_d <- sb_density %>%
  filter(Realm == "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))

sb_med_de_d

summary(mod_med_de_d)

med_de_d <- conditional_effects(mod_med_de_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

med_de_d

med_de_d_df <-
  as.data.frame(med_de_d$`Biome:Habitat_degraded`)

head(med_de_d_df)

med_de_d_ce <- med_de_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Mediterranean Forests, Woodlands and Scrub",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Realm, Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))%>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

med_de_d_ce

write.csv(med_de_d_ce,  "Data/med_de_d_ce.csv")


sb_med_de_d

fig_med_de_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_med_de_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = med_de_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = med_de_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "d) Mediterranean and Deserts" ) +
  scale_color_manual( values= c(    "#fab255",  "#da7901"))+
  coord_cartesian( ylim = c(0,8000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_med_de_d




# tund

sb_tund_d <- sb_density %>%
  filter(Realm == "Tundra") %>%  mutate( Biome = "Tundra and Boreal")

sb_tund_d

summary(mod_tund_d)

tund_d <- conditional_effects(mod_tund_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

tund_d

tund_d_df <-
  as.data.frame(tund_d$`Habitat_degraded`)

head(tund_d_df)

tund_d_ce <- tund_d_df %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Biome = "Tundra and Boreal",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

tund_d_ce

write.csv(tund_d_ce,  "Data/tund_d_ce.csv")

sb_tund_d

fig_tund_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_tund_d,
             aes(x = Habitat_degraded, y = Seed_density_m2, 
                 colour = Biome ,  shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2,
             , 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1) ) +
  geom_point(data = tund_d_ce,
             aes(x =  Habitat_degraded, y = Estimate, colour =  Biome, 
                 shape = Habitat_degraded ),size = 3) +
  geom_errorbar(data = tund_d_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 1, width = 0,  ) +
  labs(x = '',y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Tundra" ) +
  scale_color_manual( values= c(   "#94b594" ))+
  coord_cartesian( ylim = c(0,5000)) +
  scale_y_continuous(breaks=c(0,1000,2500,4000,5000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_tund_d




# wetlsnd

sb_wetland_d <- sb_density %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  filter(Realm == "Wetland") 

sb_wetland_d

summary(mod_wetland_d)

wetland_d <- conditional_effects(mod_wetland_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

wetland_d

wetland_d_df <-
  as.data.frame(wetland_d$`Biome:Habitat_degraded`)

head(wetland_d_df)

wetland_d_ce <- wetland_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Wetland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Biome, Realm, Habitat_degraded, Estimate, `Upper_CI`, `Lower_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

wetland_d_ce
write.csv(wetland_d_ce,  "Data/wetland_d_ce.csv")

sb_wetland_d

fig_wetland_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = wetland_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352"))+
  labs(x = '', y= '',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "f) Wetlands" ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_wetland_d




legend_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_d,
             aes(x = Biome, y = Seed_density_m2, 
                  group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_d_ce,
             aes(x =  Biome, y = Estimate,  group= Habitat_degraded, 
                 shape = Habitat_degraded), alpha = 0.7 ,
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = wetland_d_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')), shape = "Habitat",
       subtitle=  "f) Wetlands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,15000)) +
  scale_y_continuous(breaks=c(0,2500,5000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


legend_d


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_d <- g_legend(legend_d)


density_fig <- (fig_tund_d + fig_forest_d + fig_grass_d) /
               ( fig_med_de_d + fig_arable_d) /
                ( fig_wetland_d + fig_aq_d  )/ (legend_d) + plot_layout(heights = c(10, 10,  10,  1))

density_fig

