
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
                             fitted(mod_aq_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                             )) %>% 
  as_tibble() %>% inner_join(sb_aq_d %>% distinct(Seed_density_m2, Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Aquatic") %>% mutate(Group = "Aquatic")

head(aq_fitted)
#View(aq_fitted)

ggplot(aq_fitted, aes(x = Estimate, y = Habitat_degraded, fill = Habitat_degraded, color = Habitat_degraded)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +  # Theme for ridgeline plot
  labs(title = "Density of Posterior Samples by Habitat Degradation", x = "Posterior Sample Value", y = "Habitat Degraded") +
  theme(legend.position = "none") 

ar_fitted <- cbind(mod_ar_d$data,
                   fitted(mod_ar_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                   )) %>% 
  as_tibble() %>% inner_join(sb_arable_d %>% distinct(Seed_density_m2,  Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Arable", Habitat_degraded = "1") %>% mutate(Group = "Arable")

ar_fitted


f_fitted <- cbind(mod_forest_d$data,
                   fitted(mod_forest_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                   )) %>% 
  as_tibble() %>% inner_join(sb_forest_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Forest") %>% mutate(Group = "Terrestrial")


g_fitted <- cbind(mod_grass_d$data,
                  fitted(mod_grass_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_grass_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Grass") %>% mutate(Group = "Terrestrial")


m_fitted <- cbind(mod_med_de_d$data,
                  fitted(mod_med_de_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_med_de_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Med") %>% mutate(Group = "Terrestrial")

t_fitted <- cbind(mod_tund_d$data,
                  fitted(mod_tund_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_tund_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Tund") %>% mutate(Group = "Terrestrial")


w_fitted <- cbind(mod_wetland_d$data,
                  fitted(mod_wetland_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
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
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
    ) %>% distinct()

aq_s

aq_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group %in% c("Aquatic")), aes(x = Estimate, y = Habitat_degraded, fill = Habitat_degraded, color = Habitat_degraded), alpha = 0.7, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  geom_point(data = fitted_values %>% filter(Group %in% c("Aquatic"))  %>% filter(Habitat_degraded == "0") %>%
               group_by(Habitat_degraded) %>% summarise(mean = mean(Estimate))
               , aes(x = mean, y = Habitat_degraded, shape = Habitat_degraded), color = "#447fdd", size=5, alpha=0.9)+
  geom_point(data = fitted_values %>% filter(Group %in% c("Aquatic"))  %>% filter(Habitat_degraded == "1") %>%
               group_by(Habitat_degraded) %>% summarise(mean = mean(Estimate))
             , aes(x = mean, y = Habitat_degraded, shape = Habitat_degraded), color = "#C0C0C0", size=5, alpha=0.9)+
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  xlim(0,15000)+
  scale_color_manual( values= c("#447fdd" , "#447fdd" ),
                      labels = c("Undisturbed", "Degraded"))+
  scale_fill_manual( values= c( "#447fdd", "#C0C0C0"),
                     labels = c("Undisturbed", "Degraded"))+
  scale_shape_manual( values= c( 16,17),
                     labels = c("Undisturbed", "Degraded"))+
  labs(subtitle = "Aquatic", 
    #x = expression(paste('Seed density (',m^2,')')), 
    x="", y = "State"
    ) +
  # --- Theme ---
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

aq_fig


head(fitted_values)

t_s <-fitted_values %>% filter(Group == "Terrestrial") %>% 
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
t_s

w_s <-fitted_values %>%  filter(Group == "Wetlands") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
w_s

head(fitted_values %>% filter(Group == "Arable"))



ar_s <-fitted_values %>% filter(Group == "Arable") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
ar_s

ar_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Arable"), aes(x = Estimate, y = Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth = 600) +
  geom_point(data = fitted_values %>% filter(Group %in% c("Arable")) %>%
               group_by(Biome, Realm_Biome) %>% summarise(mean = mean(Estimate))
             , aes(x = mean, y = Biome,  color = Realm_Biome), size=5, shape = 15, alpha=0.9)+
   geom_vline(data = ar_s, aes(xintercept = mean), size = 1.2, color ="#472c0b", alpha = 0.7, linetype="longdash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  scale_y_discrete(limits=rev)+
  xlim(0,15000)+
  scale_color_manual( values= c( "#99610a" , "#E2C59F", "#AA3929" ),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical") )+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c( "#99610a" , "#E2C59F", "#AA3929"),
                     labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"))+
  labs(subtitle = "Arable", 
       x = expression(paste('Seed density (',m^2,')')), y = "Biome") +
  # --- Theme ---
  theme_bw(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

ar_fig




aq_fig + t_fig + ar_fig


group_means <- t_s  %>% bind_rows(ar_s,w_s, aq_s ) %>%
  mutate(mean = round(mean, 0),
         median = round(median, 0),
         lower_90 = round(lower_90, 0),
         upper_90 = round(upper_90, 0),
         lower_50 = round(lower_50, 0),
         upper_50 = round(upper_50, 0),
  ) %>%
  unite("50% Credible Interval", lower_50:upper_50, sep="-") %>%
  unite("90% Credible Interval", lower_90:upper_90, sep="-") 


head(group_means)
write.csv(group_means, "~/Dropbox/GSB/Data/Table_Fig_3_Realm_Means.csv")

head(fitted_values)
fitted_values %>% select(Group) %>% distinct()

#,  fill = "#C0C0C0"
t_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "1") , 
                      aes(x = Estimate, y = Realm_Biome,  color = Realm_Biome), alpha = 0.7, bandwidth =600, fill = "#C0C0C0") +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "0") ,
                      aes(x = Estimate, y = Realm_Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_vline(data = t_s, aes(xintercept = mean), size = 1.2, color = "#0c7156") +
  geom_point(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "1") %>%
               group_by(Biome, Realm_Biome) %>% summarise(mean = mean(Estimate)), 
                      aes(x = mean, y = Realm_Biome), alpha = 0.9, color = "#C0C0C0", shape=17, size=5) +
  geom_point(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "0") %>%
               group_by(Biome, Realm_Biome) %>% summarise(mean = mean(Estimate)) ,
                      aes(x = mean, y = Realm_Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.9, shape=16, size=5) +
  # geom_vline(data = w_s, aes(xintercept = mean), size = 1.2, color = "#208cc0",linetype="dotted") +
  # geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  # geom_vline(data = ar_s, aes(xintercept = mean), size = 1.2, color ="#472c0b", alpha = 0.7, linetype="longdash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  scale_y_discrete(limits=rev)+
  xlim(0,9000)+
  scale_color_manual( values= c( "#94b594", "#1e3d14", "#788f33","#228B22","#d8b847", "#b38711",  "#da7901","#fab255" ),
                      labels = c("Tundra", "Forest: Boreal", "Forest: Temperate", "Forest: Tropical & \nSubtropical",
                                 "Grasslands & Savannas: \nTemperate & Boreal", "Grasslands & Savannas: \nTropical & Subtropical", "Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"))+
  # # # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
   scale_fill_manual( values= c( "#94b594", "#1e3d14", "#788f33","#228B22","#d8b847", "#b38711",   "#da7901", "#fab255"),
                      labels = c("Tundra", "Forest: Boreal", "Forest: Temperate", "Forest: Tropical & \nSubtropical",
                      "Grasslands & Savannas: \nTemperate & Boreal", "Grasslands & Savannas: \nTropical & Subtropical", "Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"))+
   labs(subtitle = "Natural Terrestrial Areas", 
    x = expression(paste('Seed density (',m^2,')')),
       y = "") +
  labs(#subtitle = "a) Natural Terrestrial", 
    x = expression(paste('Seed density (',m^2,')')), y = "Biome") +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

t_fig




w_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "1")  , 
                      aes(x = Estimate, y = Realm_Biome,  color = Realm_Biome), alpha = 0.7, bandwidth =600, fill = "#C0C0C0") +
  geom_density_ridges(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "0")  , 
                      aes(x = Estimate, y = Realm_Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth =600) +
  geom_point(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "1") %>%
               group_by(Biome, Realm_Biome) %>% summarise(mean = mean(Estimate)), 
             aes(x = mean, y = Realm_Biome), alpha = 0.9, color = "#C0C0C0", shape=17, size=5) +
  geom_point(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "0") %>%
               group_by(Biome, Realm_Biome) %>% summarise(mean = mean(Estimate)) ,
             aes(x = mean, y = Realm_Biome, fill = Realm_Biome, color = Realm_Biome), alpha = 0.9, shape=16, size=5) +
   geom_vline(data = w_s, aes(xintercept = mean), size = 1.2, color = "#208cc0",linetype="dotted") +
 # geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  scale_y_discrete(limits=rev)+
  xlim(0,15000)+
  scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352","#447fdd" ),
                      labels=c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c(  "#20B2AA", "#4E84C4", "#293352","#447fdd"),
                     labels=c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  labs(subtitle = "Wetlands", 
    x = expression(paste('Seed density (',m^2,')')),
       y = "Biome") +
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

w_fig


(t_fig + ar_fig) / (w_fig + aq_fig)+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))


(t_fig) / ( ar_fig + w_fig + aq_fig)+
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))



t_fig 

( ar_fig + w_fig + aq_fig)

