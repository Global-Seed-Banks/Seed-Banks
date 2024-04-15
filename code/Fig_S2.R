
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
         !is.na(Centred_log_Total_sample_area_m2) #,
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

x_labs <- c(0.01, 0.05, 1, 5, 20)

# tundra
sb_tundra_r <- sb_rich_area %>% filter(Realm == "Tundra") %>%  ungroup()

tundra_fitted <-   tidyr::crossing( 
  sb_tundra_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded ) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_tund_r, newdata= .x, re_formula = NA  ))) 

tundra_fitted_df <- tundra_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Habitat_degraded_group) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate() %>%
  mutate( Realm = "Tundra")

head(tundra_fitted_df)



fig_s2a <- ggplot() + 
 # facet_wrap(~Habitat_degraded) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_tundra_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Realm, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = tundra_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Realm, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = tundra_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Realm),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(   "#94b594" 
  ))+
  scale_fill_manual( values= c(  "#94b594" 
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "a) Tundra") + guides(col = guide_legend(nrow = 4)) 

fig_s2a

# Forest
sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>%  ungroup()

forest_fitted <-   tidyr::crossing( 
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_forest_r, newdata= .x, re_formula = NA  ))) 

forest_fitted_df <- forest_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_group) %>%
  arrange(Biome, Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate()

head(forest_fitted_df)


fig_s2b <- ggplot() + 
  facet_wrap(~Biome) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_forest_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Biome, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = forest_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Biome, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = forest_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Biome),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(  "#1e3d14",   "#788f33", "#228B22"
  ))+
  scale_fill_manual( values= c(  "#1e3d14",   "#788f33", "#228B22"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
                     ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "b) Forests") + guides(col = guide_legend(nrow = 4)) 

fig_s2b



# grassland
sb_grassland_r <- sb_rich_area %>% filter(Realm == "Grassland") %>%  ungroup()

grassland_fitted <-   tidyr::crossing( 
  sb_grassland_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_grass_r, newdata= .x, re_formula = NA  ))) 

grassland_fitted_df <- grassland_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_group) %>%
  arrange(Biome, Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate()

head(grassland_fitted_df)


fig_s2c <- ggplot() + 
  facet_wrap(~Biome) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_grassland_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Biome, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = grassland_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Biome, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = grassland_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Biome),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c( "#d8b847", "#b38711"
  ))+
  scale_fill_manual( values= c(  "#d8b847", "#b38711"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "c) Grasslands") + guides(col = guide_legend(nrow = 4)) 

fig_s2c



# med_de_
sb_med_de_r <- sb_rich_area %>%  
  filter(Realm == "Mediterranean and Desert")

med_de_fitted <-   tidyr::crossing( 
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_med_de_r, newdata= .x, re_formula = NA  ))) 

med_de_fitted_df <- med_de_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_group) %>%
  arrange(Biome, Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate() %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

head(med_de__fitted_df)


fig_s2d <- ggplot() + 
  facet_wrap(~Biome) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_med_de_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Biome, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = med_de_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Biome, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = med_de_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Biome),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(  "#da7901",  "#fab255"
  ))+
  scale_fill_manual( values= c(   "#da7901",  "#fab255"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "d) Mediterranean and Desert") + guides(col = guide_legend(nrow = 4)) 

fig_s2d

# Arable
sb_ar_r <- sb_rich_area %>%  
  filter(Realm == "Arable")%>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical")) %>% arrange(Biome)

ar_fitted <-   tidyr::crossing( 
  sb_ar_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_ar_r, newdata= .x, re_formula = NA  ))) 

ar_fitted_df <- ar_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_group) %>%
  arrange(Biome, Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate() %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical")) %>% arrange(Biome)

head(ar_fitted_df)


fig_s2e <- ggplot() + 
  facet_wrap(~Biome) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_ar_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Biome, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = ar_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Biome, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = ar_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Biome),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(  "#99610a" , "#E2C59F", "#AA3929"
  ))+
  scale_fill_manual( values= c(  "#99610a" , "#E2C59F", "#AA3929"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "e) Arable") + guides(col = guide_legend(nrow = 4)) 

fig_s2e


# wetland
sb_wetland_r <- sb_rich_area %>% filter(Realm == "Wetland") %>%  ungroup()%>% 
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical")) %>% arrange(Biome)

wetland_fitted <-   tidyr::crossing( 
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_wetland_r, newdata= .x, re_formula = NA  ))) 

wetland_fitted_df <- wetland_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_group) %>%
  arrange(Biome, Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate()%>% 
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical")) %>% arrange(Biome)

head(wetland_fitted_df)


fig_s2f <- ggplot() + 
  facet_wrap(~Biome) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_wetland_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Biome, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = wetland_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Biome, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = wetland_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Biome),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(  "#20B2AA", "#4E84C4", "#293352"
  ))+
  scale_fill_manual( values= c(  "#20B2AA", "#4E84C4", "#293352"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "f) Wetland") + guides(col = guide_legend(nrow = 4)) 

fig_s2f


# aquatic
sb_aquatic_r <- sb_rich_area %>% filter(Realm == "Aquatic") %>%  ungroup()

aquatic_fitted <-   tidyr::crossing( 
  sb_aquatic_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c( seq( min(Total_sample_area_m2), max(Total_sample_area_m2), 
                                                    length.out = n()
                                                    #length.out = 100
    ) ) ), 
  Number_sites = c(1, 20, 100),
)  %>%
  mutate( Log_number_sites = log(Number_sites),
          Log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = Log_number_sites - mean(Log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = Log_total_sample_area_m2 - mean(Log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( Log_number_sites, Log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded ) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(fitted = purrr::map(data, ~fitted(mod_aq_r, newdata= .x, re_formula = NA  ))) 

aquatic_fitted_df <- aquatic_fitted  %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Habitat_degraded_group) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>% 
  filter(Number_sites == 1) %>% mutate() %>%
  mutate( Realm = "Aquatic")

head(aquatic_fitted_df)



fig_s2g <- ggplot() + 
  #facet_wrap(~Habitat_degraded) +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_aquatic_r,
             aes(x = Total_sample_area_m2,
                 y = Total_species, colour = Realm, shape = Habitat_degraded,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = aquatic_fitted_df, 
            aes( x = Total_sample_area_m2,
                 y = fitted[,1] , colour = Realm, group = Habitat_degraded , linetype = Habitat_degraded  ),
            size = 1 
  ) +
  geom_ribbon(data = aquatic_fitted_df ,
              aes(
                x = Total_sample_area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Habitat_degraded , fill = Realm),
              alpha = 0.2) +
  coord_cartesian( ylim = c(1,100),  xlim = c(0.01,20) ) +
  scale_color_manual( values= c(  "#447fdd"
  ))+
  scale_fill_manual( values= c(  "#447fdd"
  ))+
  scale_y_continuous(trans = 'log', breaks= c(1, 5,10,20,50, 100)
  ) +  scale_x_continuous(trans = 'log', breaks=c(0.01, 0.05, 1, 5, 20) , labels = x_labs) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "Biome", fill = "Biome", subtitle= "g) Aquatic") + guides(col = guide_legend(nrow = 4)) 

fig_s2g




# custom legend
legend.data <- wetland_fitted_df %>%   mutate(Habitat_degraded = factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, c("0","1")))

line.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=20 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = legend.data %>% select(Habitat_degraded) %>% distinct(Habitat_degraded),
               aes(x = 0,
                   xend = 15,
                   y = 0,
                   yend = 15,  linetype = Habitat_degraded ), 
               size = 1.5, alpha= 0.5  )  +
  scale_linetype_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  "solid", "dashed") ) +
  theme(legend.key.width = unit(2,"cm")) +  guides(linetype=guide_legend(title=""))

line.leg

shape.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=20 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_point(data = legend.data %>% select(Habitat_degraded) %>% distinct(Habitat_degraded),
             aes(x=0, y=0, shape = Habitat_degraded), alpha= 0.5 ,size =3)+
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  guides(shape = guide_legend(title = "" )  )+
  theme(legend.key.width = unit(2,"cm")) #+  guides(linetype=guide_legend(title="Habitat"))

shape.leg

# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect legend
line_legend <- g_legend(line.leg)
shape_legend <- g_legend(shape.leg)

(fig_s2a) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2b) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2c) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2d) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2e) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2f) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))
(fig_s2g) / (shape_legend) / (line_legend) + plot_layout(heights = c(10, 1, 1))

