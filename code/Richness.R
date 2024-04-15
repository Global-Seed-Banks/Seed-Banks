


rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

sb_rich_area <- sb_prep %>% 
  filter(!is.na(Total_species),
         # !Total_species == 0,
         !is.na(Centred_log_total_sample_area_m2) #,
         #Number_sites == 1 
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome) 


sb_rich_area %>% select(Realm) %>% distinct()

setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'rich_aq.Rdata')
load( 'rich_ar.Rdata')
load( 'rich_forest.Rdata')
load( 'rich_grass.Rdata')
load( 'rich_med_de.Rdata')
load( 'rich_po_alp.Rdata')
load( 'rich_wetland.Rdata')



# Tundra
sb_tund_r <- sb_rich_area %>% filter(Realm == "Tundra")%>% filter(Habitat_broad == "Grassland")

head(sb_tund_r)

summary(mod_tund_r)

pp_check(mod_tund_r)
plot(mod_tund_r)
conditional_effects(mod_tund_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
tund_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_tund_r %>% group_by( Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded ) %>%
  nest(data = c( Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


tund_predict_df <- tund_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Biome = "Tundra and Boreal")

summary(tund_predict_df)
nrow(tund_predict_df) 
tund_predict_df %>% select(.draw, Habitat_degraded) %>% mutate(max_draw = max(.draw))

fig_tund_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = tund_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Habitat_degraded , y = predicted,  fill= Biome, shape=Habitat_degraded),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = tund_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Habitat_degraded , y = predicted,  fill= Biome, 
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(  "#94b594" 
  )) +   coord_cartesian( ylim = c(0,40)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a) Tundra" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_tund_r



# Forest
sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>%  ungroup()

nrow(sb_forest_r)

summary(sb_forest_r)

summary(mod_forest_r)

pp_check(mod_forest_r)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "a) Species ~ area") + xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

plot(mod_forest_r)

pairs(mod_forest_r)

#residuals
head(sb_forest_r)
forest_r <- residuals(mod_forest_r)
forest_r <- as.data.frame(forest_r)
head(forest_r)
forest_r_plot <- cbind(sb_forest_r, forest_r$Estimate)
head(forest_r_plot)

par(mfrow=c(2,3))
with(forest_r_plot, plot(as.factor(Biome), forest_r$Estimate))
with(forest_r_plot, plot(Habitat_degraded, forest_r$Estimate))
with(forest_r_plot, plot(Centred_log_total_sample_area_m2, forest_r$Estimate))
with(forest_r_plot, plot(Centred_log_number_sites, forest_r$Estimate))
with(forest_r_plot, plot(StudyID, forest_r$Estimate))
with(forest_r_plot, plot(RowID, forest_r$Estimate))


# make sure purr not loaded, and Biome is a character NOT A FACTOR
forest_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


forest_predict_df <- forest_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

head(forest_predict_df)

# look at old paper tables for inspo on formatting nicely
forest_rich <- forest_predict_df %>% select(Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate( Realm = "Forest" ,  Estimate = mean(predicted),
          Lower_CI = quantile(predicted, probs=0.025),
          Upper_CI = quantile(predicted, probs=0.975))  %>%
  mutate( Habitat_state = case_when( Habitat_degraded == 0 ~ "Undisturbed habitat",
                                     Habitat_degraded == 1 ~ "Degraded habitat",
  )) %>%
  dplyr::select(c( Realm, Biome, Habitat_state, Total_sample_area_m2, Estimate, Lower_CI, Upper_CI)) %>% distinct() 

forest_rich

write.csv(forest_rich,  "Data/forest_rich.csv")


fig_forest_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
stat_halfeye(data = forest_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
             aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded, shape=Habitat_degraded),
  point_interval = mean_qi,  .width = c(0.50, 0.9), 
  alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = forest_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded,
                   shape= Habitat_degraded,
                   ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(  "#1e3d14",   "#788f33", "#228B22"
  )) +   coord_cartesian( ylim = c(0,60)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "b) Forests" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_forest_r


# Grasslands
sb_grass_r <- sb_rich_area %>% filter(Realm == "Grassland") %>%  ungroup() 
nrow(sb_grass_r)
head(sb_grass_r)

summary(mod_grass_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
grass_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


grass_predict_df <- grass_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

fig_grass_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded, shape=Habitat_degraded),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded,
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c( "#d8b847", "#b38711"
  )) +   coord_cartesian( ylim = c(0,50)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "c) Grasslands" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_grass_r



# Med Desert
sb_med_de_r <- sb_rich_area %>%  
  filter(Realm == "Mediterranean and Desert")

head(sb_med_de_r)

summary(mod_med_de_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
med_de_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


med_de_predict_df <- med_de_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

fig_med_de_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = med_de_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded, shape=Habitat_degraded),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = med_de_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded,
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(   "#da7901",  "#fab255"
  )) +   coord_cartesian( ylim = c(0,100)) +
  labs(x = '', #y='',
        y = 'Species richness \n in the soil seedbank',
       subtitle=  "d) Mediterranean and Deserts" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_med_de_r


# Arable
sb_ar_r <- sb_rich_area %>% filter(Realm == "Arable")   %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

head(sb_ar_r)

summary(mod_ar_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
ar_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_ar_r %>% group_by(Biome) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome,  Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


ar_predict_df <- ar_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Realm = "Arable") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

fig_ar_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = ar_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Biome , y = predicted,  fill= Biome),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = ar_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, 
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c("#99610a" , "#E2C59F", "#AA3929"
  )) +   coord_cartesian( ylim = c(0,40)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "e) Arable" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_ar_r




# Wetland
sb_wetland_r <- sb_rich_area %>% filter(Realm == "Wetland") %>% 
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

nrow(sb_wetland_r)
head(sb_wetland_r)
sb_wetland_r %>% select(Biome) %>% distinct()


summary(mod_wetland_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
wetland_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


wetland_predict_df <- wetland_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Wetland") %>% 
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

fig_wetland_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = wetland_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded, shape=Habitat_degraded),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = wetland_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded,
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(  "#20B2AA", "#4E84C4", "#293352"
  )) +   coord_cartesian( ylim = c(0,70)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "f) Wetlands" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_wetland_r

#Aquatic

sb_aq_r <- sb_rich_area %>% filter(Realm == "Aquatic") %>%  ungroup() 

head(sb_aq_r)

summary(mod_aq_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
aq_predict <-   tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_aq_r %>% group_by( Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded ) %>%
  nest(data = c( Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_aq_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


aq_predict_df <- aq_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Biome = "Aquatic")

fig_aq_r <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  stat_halfeye(data = aq_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
               aes(x = Habitat_degraded , y = predicted,  fill= Biome, shape=Habitat_degraded),
               point_interval = mean_qi,  .width = c(0.50, 0.9), 
               alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = aq_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Habitat_degraded , y = predicted,  fill= Biome, 
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(  "#447fdd"
  )) +   coord_cartesian( ylim = c(0,35)) +
  labs(x = '', y='',
       # y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "g) Aquatic" ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") 

fig_aq_r




legend_g <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  # stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
  #              aes(x = Biome , y = predicted,  group = Habitat_degraded, shape=Habitat_degraded),
  #              point_interval = mean_qi,  .width = c(0.50, 0.9), 
  #              alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,   group = Habitat_degraded,
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=10, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c( "#d8b847", "#b38711"
  )) +   coord_cartesian( ylim = c(0,90)) +
  labs(x = '', y='', shape = (expression(paste( italic(gamma), '-richness 15 (',m^2,')',sep = ''))),
       subtitle=  "c) Grasslands" ) +
scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  guides(shape = guide_legend(override.aes = list(size = 15)))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") 

legend_g


legend_a <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  # stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
  #              aes(x = Biome , y = predicted,  group = Habitat_degraded, shape=Habitat_degraded),
  #              point_interval = mean_qi,  .width = c(0.50, 0.9), 
  #              alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = grass_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,   group = Habitat_degraded,
                   shape= Habitat_degraded,
               ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c( "#d8b847", "#b38711"
  )) +   coord_cartesian( ylim = c(0,90)) +
  labs(x = '', y='', shape = (expression(paste( italic(alpha), '-richness 0.01 (',m^2,')',sep = ''))),
       subtitle=  "c) Grasslands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom",
                               legend.spacing.x = unit(1, 'cm') ) 

legend_a

# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_g <- g_legend(legend_g)
legend_a <- g_legend(legend_a)



richness_fig <- (fig_tund_r + fig_forest_r + fig_grass_r) /
  ( fig_med_de_r + fig_ar_r) /
  ( fig_wetland_r + fig_aq_r  )/ (legend_g) / (legend_a) + plot_layout(heights = c(10, 10,  10, 2.5, 1))

richness_fig
