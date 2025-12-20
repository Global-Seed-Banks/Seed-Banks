

#rm(list = ls())


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
                  "emmaladouceur" = "~/Dropbox/GSB/",
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

aq_d_90 <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
aq_d_50 <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects

aq_d_df_90 <-
  as.data.frame(aq_d_90$`Habitat_degraded`)
aq_d_df_50 <-
  as.data.frame(aq_d_50$`Habitat_degraded`)

aq_d_90_ce <- aq_d_df_90 %>%
  select( Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Aquatic", Biome = "Aquatic",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome,  Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

aq_d_50_ce <- aq_d_df_50 %>%
  select( Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Aquatic", Biome = "Aquatic",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome,  Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 



fig_aq_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = aq_s, aes(yintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  geom_point(data = sb_aq_d,
             aes(x = Habitat_degraded, y = Seed_density_m2, 
                  shape= Habitat_degraded, color=Realm
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_errorbar(data = aq_d_90_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, color=Biome),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = aq_d_50_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, color=Biome),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  geom_point(data = aq_d_90_ce,
             aes(x =  Habitat_degraded, y = Estimate, color=Biome,
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 4) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "g) Aquatic" ) +
  scale_color_manual( values= c(    "#447fdd"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,5000, 10000,15000,20000,15000, 200000,25000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                # axis.text.y=element_blank(), 
                                 axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


fig_aq_d


# arable

sb_arable_d <- sb_density %>%
  filter(Realm == "Arable") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))


summary(mod_ar_d)

arable_d_90 <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
arable_d_50 <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects


arable_d_df_90 <-
  as.data.frame(arable_d_90$`Biome`)
arable_d_df_50 <-
  as.data.frame(arable_d_50$`Biome`)


arable_d_90_ce <- arable_d_df_90 %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate( Realm = "Arable", Habitat_degraded = "1",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

arable_d_50_ce <- arable_d_df_50 %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate( Realm = "Arable", Habitat_degraded = "1",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))



fig_arable_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = ar_s, aes(yintercept = mean), size = 1.2, color ="#472c0b", alpha = 0.7, linetype="longdash") +
  geom_point(data = sb_arable_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , 
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = arable_d_90_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome),
             position = position_dodge(width = 1), size = 4) +
  geom_errorbar(data = arable_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = arable_d_50_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "e) Arable" ) +
  scale_color_manual( values= c("#99610a" , "#E2C59F", "#AA3929" ))+
  scale_x_discrete( labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical") )+
  coord_cartesian( ylim = c(0,20000)) +
  scale_y_continuous(breaks=c(0,5000, 10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) 


fig_arable_d



#forests
sb_forest_d <- sb_density %>%
  filter(Realm == "Forest") 


summary(mod_forest_d)

forest_d_90 <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
forest_d_50 <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects



forest_d_df_90 <-
  as.data.frame(forest_d_90$`Biome:Habitat_degraded`)
forest_d_df_50 <-
  as.data.frame(forest_d_50$`Biome:Habitat_degraded`)


forest_d_90_ce <- forest_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Forest",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))


forest_d_50_ce <- forest_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Forest",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))
forest_d_ce



fig_forest_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = t_s, aes(yintercept = mean), size = 1.2, color = "#0c7156", alpha = 0.7) +
  geom_point(data = sb_forest_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
                 ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = forest_d_90_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                shape = Habitat_degraded ),
  position = position_dodge(width = 1), size = 4) +
  geom_errorbar(data = forest_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = forest_d_50_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "b) Forests" ) +
 scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ))+
  scale_x_discrete(labels = c("Boreal", "Temperate", "Tropical & \nSubtropical"))+
  coord_cartesian( ylim = c(0,8000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) 


fig_forest_d



# Grass

sb_grass_d <- sb_density %>%
  filter(Realm == "Grassland") 


summary(mod_grass_d)

grass_d_90 <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted',prob=0.90)  # conditional effects
grass_d_50 <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects


grass_d_df_90 <-
  as.data.frame(grass_d_90$`Biome:Habitat_degraded`)
grass_d_df_50 <-
  as.data.frame(grass_d_50$`Biome:Habitat_degraded`)

grass_d_90_ce <- grass_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Grassland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome,  Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))


grass_d_50_ce <- grass_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Grassland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome,  Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))


fig_grass_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = t_s, aes(yintercept = mean), size = 1.2, color = "#0c7156", alpha = 0.7) +
  geom_point(data = sb_grass_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = grass_d_90_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 4) +
  geom_errorbar(data = grass_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = grass_d_50_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '', y='',
      # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "c) Grasslands & Savannas" ) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"))+
  coord_cartesian( ylim = c(0,10000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.y=element_blank()
  ) + 
  scale_x_discrete( labels = c("Temperate & \nBoreal", "Tropical & \nSubtropical") )


fig_grass_d



# Med

sb_med_de_d <- sb_density %>%
  filter(Realm == "Mediterranean and Desert") %>%
  #mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

sb_med_de_d

summary(mod_med_de_d)

med_de_d_90 <- conditional_effects(mod_med_de_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
med_de_d_50 <- conditional_effects(mod_med_de_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects



med_de_d_df_90 <-
  as.data.frame(med_de_d_90$`Biome:Habitat_degraded`)
med_de_d_df_50 <-
  as.data.frame(med_de_d_50$`Biome:Habitat_degraded`)


med_de_d_90_ce <- med_de_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Mediterranean and Desert",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))%>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

med_de_d_50_ce <- med_de_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Mediterranean and Desert",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))%>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))



fig_med_de_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = t_s, aes(yintercept = mean), size = 1.2, color = "#0c7156", alpha = 0.7) +
  geom_point(data = sb_med_de_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = med_de_d_90_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 4) +
  geom_errorbar(data = med_de_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = med_de_d_50_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "d) Mediterranean & Desert" ) +
  scale_color_manual( values= c(    "#da7901",  "#fab255"))+
  scale_x_discrete(  labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands") )+
  coord_cartesian( ylim = c(0,8000)) +
  scale_y_continuous(breaks=c(0,2500,5000,7000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) 


fig_med_de_d




# tund

sb_tund_d <- sb_density %>%
  filter(Realm == "Tundra") %>%  mutate( Biome = "Tundra and Boreal")

sb_tund_d

summary(mod_tund_d)

tund_d_90 <- conditional_effects(mod_tund_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
tund_d_50 <- conditional_effects(mod_tund_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects



tund_d_df_90 <-
  as.data.frame(tund_d_90$`Habitat_degraded`)
tund_d_df_50 <-
  as.data.frame(tund_d_50$`Habitat_degraded`)


tund_d_90_ce <- tund_d_df_90 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Tundra", Biome = "Tundra",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 

tund_d_50_ce <- tund_d_df_50 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Tundra", Biome = "Tundra",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) 




fig_tund_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = t_s, aes(yintercept = mean), size = 1.2, color = "#0c7156", alpha = 0.7) +
  geom_point(data = sb_tund_d,
             aes(x = Habitat_degraded, y = Seed_density_m2, 
                 colour = Biome ,  shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1) ) +
  geom_point(data = tund_d_90_ce,
             aes(x =  Habitat_degraded, y = Estimate, colour =  Biome, 
                 shape = Habitat_degraded ),size = 4) +
  geom_errorbar(data = tund_d_90_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 0.75, width = 0,  ) +
  geom_errorbar(data = tund_d_50_ce,
                aes(x =   Habitat_degraded, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome),
                size = 2, width = 0,  ) +
  labs(x = '',y='',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Tundra" ) +
  scale_color_manual( values= c( "#94b594",    "#94b594" ))+
  coord_cartesian( ylim = c(0,5000)) +
  scale_y_continuous(breaks=c(0,1000,2500,4000,5000,10000,15000,20000,15000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 axis.text.x=element_blank()
  ) 


fig_tund_d




# wetlsnd

sb_wetland_d <- sb_density %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  filter(Realm == "Wetland") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

sb_wetland_d

summary(mod_wetland_d)

wetland_d_90 <- conditional_effects(mod_wetland_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.90)  # conditional effects
wetland_d_50 <- conditional_effects(mod_wetland_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob=0.50)  # conditional effects



wetland_d_df_90 <-
  as.data.frame(wetland_d_90$`Biome:Habitat_degraded`)
wetland_d_df_50 <-
  as.data.frame(wetland_d_50$`Biome:Habitat_degraded`)


wetland_d_50_ce <- wetland_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Wetland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

wetland_d_90_ce <- wetland_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate( Realm = "Wetland",
          Estimate = round(estimate__ , 2),
          `Lower_CI` = round(lower__ , 2),
          `Upper_CI` = round(upper__ , 2),
  ) %>% select(Realm, Biome, Habitat_degraded, Estimate, `Lower_CI`, `Upper_CI`) %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))




fig_wetland_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = w_s, aes(yintercept = mean), size = 1.2, color = "#208cc0", alpha = 0.7, linetype="dotted") +
  geom_point(data = sb_wetland_d,
             aes(x = Biome, y = Seed_density_m2, 
                 colour = Biome , group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_d_90_ce,
             aes(x =  Biome, y = Estimate, colour =  Biome, group= Habitat_degraded, 
                 shape = Habitat_degraded ),
             position = position_dodge(width = 1), size = 4) +
  geom_errorbar(data = wetland_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 0.75, width = 0, position = position_dodge(width = 1),  ) +
  geom_errorbar(data = wetland_d_50_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, colour =  Biome, 
                    group= Habitat_degraded),
                size = 2, width = 0, position = position_dodge(width = 1),  ) +
  scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352"))+
  scale_x_discrete( labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  labs(x = '', y= '',
       #y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "f) Wetlands & Flooded Grasslands" ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="none",
                                 #axis.text.x=element_blank()
  ) 


fig_wetland_d




legend_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_wetland_d,
             aes(x = Biome, y = Seed_density_m2, 
                  group= Habitat_degraded , shape= Habitat_degraded,
             ), 
             size = 1.5, alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = wetland_d_90_ce,
             aes(x =  Biome, y = Estimate,  group= Habitat_degraded, 
                 shape = Habitat_degraded), alpha = 0.7 ,
             position = position_dodge(width = 1), size = 3) +
  geom_errorbar(data = wetland_d_90_ce,
                aes(x =   Biome, ymin = `Lower_CI`, ymax = `Upper_CI`, 
                    group= Habitat_degraded),
                size = 1, width = 0, position = position_dodge(width = 1),  ) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')), shape = "State",
       subtitle=  "f) Wetlands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,15000)) +
  scale_y_continuous(breaks=c(0,2500,5000,10000,15000,20000,25000))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


legend_d



legend_line <- ggplot() + 
 # geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = group_means, aes(yintercept = mean, color = Group, linetype=Group, group=Group), size = 1.2, alpha = 0.7) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),  
       subtitle=  "f) Wetlands" ) +
  scale_color_manual( name = "Realm Mean",labels = c("Natural Terrestrial", "Arable", "Wetlands", "Aquatic"),
                      values= c( "#0c7156","#472c0b",   "#208cc0","#003967") )+
  scale_linetype_manual(name="Realm Mean",labels = c("Natural Terrestrial", "Arable", "Wetlands", "Aquatic"),
                          values= c("solid", "longdash", "dotted", "twodash" ) )+
  #scale_color_manual( values= c(  "#20B2AA"))+
  theme_bw(base_size=18) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 legend.key.width = unit(3,"cm")
                                 #axis.text.x=element_blank()
  ) 


legend_line


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_d <- g_legend(legend_d)
legend_line <- g_legend(legend_line)


density_fig <- (fig_tund_d + fig_forest_d + fig_grass_d) /
               ( fig_med_de_d + fig_arable_d) /
                ( fig_wetland_d + fig_aq_d  )/ (legend_d)/ (legend_line) + plot_layout(heights = c(10, 10,  10,  1.5, 1.5))

density_fig

#data
# Rename columns in each before joining
forest_d_90_clean <- forest_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

forest_d_50_clean <- forest_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
forest_d_ce <- forest_d_90_clean %>%
  left_join(forest_d_50_clean)

grass_d_ce
grass_d_90_clean <- grass_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

grass_d_50_clean <- grass_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
grass_d_ce <- grass_d_90_clean %>%
  left_join(grass_d_50_clean)


med_de_d_90_clean <- med_de_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

med_de_d_50_clean <- med_de_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
med_de_d_ce <- med_de_d_90_clean %>%
  left_join(med_de_d_50_clean)



arable_d_90_clean <- arable_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

arable_d_50_clean <- arable_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
arable_d_ce <- arable_d_90_clean %>%
  left_join(arable_d_50_clean)


wetland_d_90_clean <- wetland_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

wetland_d_50_clean <- wetland_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
wetland_d_ce <- wetland_d_90_clean %>%
  left_join(wetland_d_50_clean)


aq_d_90_clean <- aq_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

aq_d_50_clean <- aq_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
aq_d_ce <- aq_d_90_clean %>%
  left_join(aq_d_50_clean)

tund_d_90_clean <- tund_d_90_ce %>%
  rename(
    Lower_CI_90 = Lower_CI,
    Upper_CI_90 = Upper_CI
  )

tund_d_50_clean <- tund_d_50_ce %>%
  rename(
    Lower_CI_50 = Lower_CI,
    Upper_CI_50 = Upper_CI)

# Join them on Biome and Habitat_degraded
tund_d_ce <- tund_d_90_clean %>%
  left_join(tund_d_50_clean)

table_2 <- tund_d_ce %>% bind_rows(forest_d_ce, grass_d_ce, med_de_d_ce, arable_d_ce, wetland_d_ce, aq_d_ce) %>%
  mutate(Estimate = round(Estimate, 0),
         Lower_90 = round(Lower_CI_90, 0),
         Upper_90 = round(Upper_CI_90, 0),
         Lower_50 = round(Lower_CI_50, 0),
         Upper_50 = round(Upper_CI_50, 0),
  ) %>%
  unite("50% Credible Interval", Lower_50:Upper_50, sep="-") %>%
  unite("90% Credible Interval", Lower_90:Upper_90, sep="-") %>%
  mutate(`Intervals` = paste0("(", `50% Credible Interval`, " , " , `90% Credible Interval`, ")") ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Intervals) %>%
  unite("Density and Intervals", Estimate:`Intervals`, sep=" ") %>% ungroup() 
  

table_2
write.csv(table_2, "~/Dropbox/GSB/Data/Table_Fig_3.csv")

