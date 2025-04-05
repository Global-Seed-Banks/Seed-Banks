
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
library(ggridges)

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


sb_aq_d <- sb_density %>% filter(Realm == "Aquatic") 

sb_arable_d <- sb_density %>%
  filter(Realm == "Arable") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

sb_forest_d <- sb_density %>%
  filter(Realm == "Forest")

sb_grass_d <- sb_density %>%
  filter(Realm == "Grassland") 


sb_med_de_d <- sb_density %>%
  filter(Realm == "Mediterranean and Desert") %>%
  #mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))



sb_tund_d <- sb_density %>%
  filter(Realm == "Tundra") %>%  mutate( Biome = "Tundra and Boreal")

sb_wetland_d <- sb_density %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  filter(Realm == "Wetland") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))


# Global Mean

# List of models :  mod_aq_d, mod_ar_d,  mod_forest_d, mod_grass_d, mod_med_de_d, mod_tund_d, mod_wetland_dn 
# List of data: sb_aq_d, sb_arable_d, sb_forest_d, sb_grass_d, sb_med_de_d, sb_tund_d, sb_wetland_d

head(sb_aq_d)
summary(mod_ar_d)


aq_fitted <- cbind(mod_aq_d$data,
                             fitted(mod_aq_d, re_formula = NA
                             )) %>% 
  as_tibble() %>% inner_join(sb_aq_d %>% distinct(Seed_density_m2,Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Aquatic") %>% mutate(Group = "Aquatic")

head(aq_fitted)

ggplot(aq_fitted, aes(x = Estimate, y = Habitat_degraded, fill = Habitat_degraded, color = Habitat_degraded)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +  # Theme for ridgeline plot
  labs(title = "Density of Posterior Samples by Habitat Degradation", x = "Posterior Sample Value", y = "Habitat Degraded") +
  theme(legend.position = "none") 

ar_fitted <- cbind(mod_ar_d$data,
                   fitted(mod_ar_d, re_formula = NA
                   )) %>% 
  as_tibble() %>% inner_join(sb_arable_d %>% distinct(Seed_density_m2,  Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Arable", Habitat_degraded = "1") %>% mutate(Group = "Arable")

ar_fitted


f_fitted <- cbind(mod_forest_d$data,
                   fitted(mod_forest_d, re_formula = NA
                   )) %>% 
  as_tibble() %>% inner_join(sb_forest_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Forest") %>% mutate(Group = "Terrestrial")


g_fitted <- cbind(mod_grass_d$data,
                  fitted(mod_grass_d, re_formula = NA
                  )) %>% 
  as_tibble() %>% inner_join(sb_grass_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Grass") %>% mutate(Group = "Terrestrial")


m_fitted <- cbind(mod_med_de_d$data,
                  fitted(mod_med_de_d, re_formula = NA
                  )) %>% 
  as_tibble() %>% inner_join(sb_med_de_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Med") %>% mutate(Group = "Terrestrial")

t_fitted <- cbind(mod_tund_d$data,
                  fitted(mod_tund_d, re_formula = NA
                  )) %>% 
  as_tibble() %>% inner_join(sb_tund_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Tund") %>% mutate(Group = "Terrestrial")


w_fitted <- cbind(mod_wetland_d$data,
                  fitted(mod_wetland_d, re_formula = NA
                  )) %>% 
  as_tibble() %>% inner_join(sb_wetland_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Wet") %>% mutate(Group = "Wetlands")


fitted_values <- aq_fitted %>% bind_rows(ar_fitted, f_fitted, g_fitted, m_fitted, t_fitted, w_fitted) %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Arable", 
    Realm == "Arable" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Arable", 
    Realm == "Arable" & Biome ==  "Tropical" ~  "Tropical Arable", 
    Realm == "Forest" & Biome ==  "Temperate" ~  "Temperate Forests",
    Realm == "Forest" & Biome ==  "Tropical" ~  "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome ==  "Boreal" ~  "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome ==  "Tropical" ~  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Deserts and Xeric Shrublands" ~  "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Mediterranean Forests, Woodlands and Scrub" ~  "Mediterranean Forests, Woodlands & Scrub",
    Realm ==  "Tundra" ~  Realm,
    Realm ==  "Wetland" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Wetlands",
    Realm ==  "Wetland" & Biome ==  "Temperate and Boreal"~  "Temperate & Boreal Wetlands",
    Realm ==  "Wetland"  & Biome ==  "Tropical" ~  "Tropical Wetlands",
  )) %>% 
  mutate(Realm_Biome = fct_relevel(Realm_Biome, 
                                   "Tundra", "Boreal Forests/Taiga",  "Temperate Forests",  "Tropical & Subtropical Forests",
                                   "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                   "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands", 
                                    "Temperate & Boreal Arable", "Mediterranean & Desert Arable",    "Tropical Arable",
                                   "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
                                   "Aquatic",
  )) %>%   mutate(Realm = fct_relevel(Realm, 
                                            "Tundra", "Forest", "Grassland", "Mediterranean and Desert", "Arable", "Wetland", "Aquatic"
  )) 
  

head(fitted_values)
fitted_values %>% filter(Group == "Arable") 


aq_s <-fitted_values %>% filter(Group == "Aquatic") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower = quantile(Estimate, 0.05),
    upper = quantile(Estimate, 0.95)
  )
aq_s

aq_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group %in% c("Aquatic")), aes(x = Estimate, y = Habitat_degraded, fill = Habitat_degraded, color = Habitat_degraded), alpha = 0.7, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "black") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  xlim(0,15000)+
  scale_color_manual( values= c( "#447fdd",    "#447fdd", "#20B2AA", "#4E84C4", "#293352" ))+
  scale_fill_manual( values= c( "#447fdd",    "#447fdd", "#20B2AA", "#4E84C4", "#293352" ))+
  labs(title = "Aquatic", x = "Posterior Sample Value", y = "Habitat Degraded") +
  theme(legend.position = "bottom") 

aq_fig

t_s <-fitted_values %>% filter(Group == "Terrestrial") %>% 
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower = quantile(Estimate, 0.05),
    upper = quantile(Estimate, 0.95)
  )
t_s

w_s <-fitted_values %>%  filter(Group == "Wetlands") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower = quantile(Estimate, 0.05),
    upper = quantile(Estimate, 0.95)
  )
w_s

head(fitted_values %>% filter(Group == "Arable"))

t_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group != "Aquatic") %>% filter(Group != "Arable"), aes(x = Estimate, y = Realm, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_vline(data = t_s, aes(xintercept = mean), size = 1.2, color = "black") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  scale_y_discrete(limits=rev)+
  xlim(0,15000)+
  scale_color_manual( values= c( "#94b594", "#1e3d14", "#788f33","#228B22","#d8b847", "#b38711",  "#da7901","#fab255",  "#20B2AA", "#4E84C4", "#293352" ))+
 # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c( "#94b594", "#1e3d14", "#788f33","#228B22","#d8b847", "#b38711",   "#da7901", "#fab255", "#20B2AA", "#4E84C4", "#293352"))+
  labs(title = "Natural Areas", x = "Posterior Sample Value", y = "") +
  theme(legend.position = "bottom") 

t_fig

ar_s <-fitted_values %>% filter(Group == "Arable") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower = quantile(Estimate, 0.05),
    upper = quantile(Estimate, 0.95)
  )
ar_s

ar_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Arable"), aes(x = Estimate, y = Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth = 600) +
  theme_ridges() +  # Theme for ridgeline plot
   geom_vline(data = ar_s, aes(xintercept = mean), size = 1.2, color = "black") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  scale_y_discrete(limits=rev)+
  xlim(0,15000)+
  scale_color_manual( values= c( "#99610a" , "#E2C59F", "#AA3929" ))+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c( "#99610a" , "#E2C59F", "#AA3929"))+
  labs(title = "Arable", x = "Posterior Sample Value", y = "") +
  theme(legend.position = "bottom") 

ar_fig




aq_fig + t_fig + ar_fig
