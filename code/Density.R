





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

sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2),
                                   #Number_sites == 1 
) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome = as.factor(Biome),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)
sb_rich_area %>% select(Method) %>% distinct()



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'den_aq.Rdata')
load( 'den_ar.Rdata')
load( 'den_forest.Rdata')
load( 'den_grass.Rdata')
load( 'den_med_de.Rdata')
load( 'den_po_alp.Rdata')
load( 'den_wetland.Rdata')


#aquatic


#forests
sb_aq_d <- sb %>% 
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
  filter(Realm == "Aquatic") 


summary(mod_aq_d)

aq_d <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

aq_d

aq_d_df <-
  as.data.frame(aq_d$`Habitat_degraded`)

head(aq_d_df)

aq_d_ce <- aq_d_df %>%
  select( Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Aquatic",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model,  Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

aq_d_ce
sb_aq_d

fig_aq_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_aq_d,
             aes(x = Habitat_degraded, y = Seed_density_m2, 
                  shape= Habitat_degraded, color=Realm
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = aq_d_ce,
             aes(x =  Habitat_degraded, y = Estimate, color=Model,
                 shape = Habitat_degraded ),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = aq_d_ce,
                aes(x =   Habitat_degraded, ymin = `Lower CI`, ymax = `Upper CI`, color=Model),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Aquatic" ) +
  scale_color_manual( values= c(    "#447fdd"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_aq_d


#arable

sb_arable_d <- sb %>% 
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
  filter(Realm == "Arable")  %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))


summary(mod_ar_d)

arable_d <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted')  # conditional effects

arable_d

arable_d_df <-
  as.data.frame(arable_d$`Biome`)

head(arable_d_df)

arable_d_ce <- arable_d_df %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate( Model = "Arable",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Estimate, `Upper CI`, `Lower CI`) 

arable_d_ce
sb_arable_d

fig_arable_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_arable_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Realm , 
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = arable_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Model),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = arable_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Model),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Arable" ) +
  scale_color_manual( values= c( "#99610a"  ))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_arable_d



#forests
sb_forest_d <- sb %>% 
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
  filter(Realm == "Forest") 


summary(mod_forest_d)

forest_d <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

forest_d

forest_d_df <-
  as.data.frame(forest_d$`Biome:Habitat_degraded`)

head(forest_d_df)

forest_d_ce <- forest_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Forest",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

forest_d_ce
sb_forest_d

fig_forest_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_forest_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
                 ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = forest_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                shape = Habitat_degraded ),
  position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = forest_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Forests" ) +
  scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_forest_d



# Grass

sb_grass_d <- sb %>% 
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
  filter(Realm == "Grassland") 


summary(mod_grass_d)

grass_d <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

grass_d

grass_d_df <-
  as.data.frame(grass_d$`Biome:Habitat_degraded`)

head(grass_d_df)

grass_d_ce <- grass_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Grassland",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

grass_d_ce
sb_grass_d

fig_grass_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_grass_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = grass_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = grass_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Grassland" ) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.y=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_grass_d



# Med

sb_med_de_d <- sb %>% 
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
  mutate( Model = "Mediterranean Forests, Woodlands and Scrub",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))

med_de_d_ce
sb_med_de_d

fig_med_de_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_med_de_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = med_de_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = med_de_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Mediterranean and Desert" ) +
  scale_color_manual( values= c(    "#da7901",  "#fab255"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_med_de_d




# tund

sb_tund_d <- sb %>% 
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
  filter(Realm == "Tundra") 

sb_tund_d

summary(mod_tund_d)

tund_d <- conditional_effects(mod_tund_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted')  # conditional effects

tund_d

tund_d_df <-
  as.data.frame(tund_d$`Biome:Habitat_degraded`)

head(tund_d_df)

tund_d_ce <- tund_d_df %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Model = "Tundra",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

tund_d_ce
sb_tund_d

fig_tund_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_tund_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = tund_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = tund_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Tundra" ) +
  scale_color_manual( values= c( "#1e3d14",   "#94b594" ))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_tund_d




# wetlsnd

sb_wetland_d <- sb %>% 
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
  mutate( Model = "Tundra",
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
          `Upper CI` = round(upper__ , 2),
  ) %>% select(Model, Biome, Habitat_degraded, Estimate, `Upper CI`, `Lower CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

wetland_d_ce
sb_wetland_d

fig_wetland_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25, jitter.height=0.45, dodge.width = 0.75)) +
  geom_point(data = wetland_d_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = wetland_d_ce,
                aes(x =   Biome, ymin = `Lower CI`, ymax = `Upper CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 0.75),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Wetlands" ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_wetland_d




density_fig <- (fig_tund_d + fig_forest_d + fig_grass_d) /
               ( fig_med_de_d + fig_arable_d) /
                ( fig_wetland_d + fig_aq_d  )

density_fig
