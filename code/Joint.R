

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



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'rich_aq.Rdata')
load( 'rich_ar.Rdata')
load( 'rich_forest.Rdata')
load( 'rich_grass.Rdata')
load( 'rich_med_de.Rdata')
load( 'rich_po_alp.Rdata')
load( 'rich_wetland.Rdata')


setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'den_aq.Rdata')
load( 'den_ar.Rdata')
load( 'den_forest.Rdata')
load( 'den_grass.Rdata')
load( 'den_med_de.Rdata')
load( 'den_po_alp.Rdata')
load( 'den_wetland.Rdata')

# Tundra


sb_tund_r <- sb_rich_area %>% filter(Realm == "Tundra") %>%  ungroup() 

head(sb_tund_r)

summary(mod_tund_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
tund_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_tund_r %>% group_by( Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
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


head(tund_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


tund_r1_df <- tund_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Biome = "Tundra and Boreal") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(tund_r1_df)


tund_d_ce <- read.csv(paste0(path2wd, 'Data/tund_d_ce.csv'))

tund_d_ce

tund_joint <- tund_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
                     d_Upper_CI = `Upper_CI`,
                     d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(tund_r1_df)

tund_joint


fig_tund_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = tund_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 3) +
  geom_errorbar(data = tund_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Biome  )) +
  geom_errorbarh(data = tund_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Biome )) +
  scale_color_manual( values= c(  "#94b594"  )) + 
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "a) Tundra"
        ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_tund_joint


# forstss

sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>%  ungroup() 

head(sb_forest_r)

summary(mod_forest_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
forest_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c( Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata= .x, re_formula = NA  ))) 


head(forest_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


forest_r1_df <- forest_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Realm = "Forest") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(forest_r1_df)


forest_d_ce <- read.csv(paste0(path2wd, 'Data/forest_d_ce.csv'))

forest_d_ce

forest_joint <- forest_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
                                   d_Upper_CI = `Upper_CI`,
                                   d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(forest_r1_df)

forest_joint


fig_forest_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = forest_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 3) +
  geom_errorbar(data = forest_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Biome  )) +
  geom_errorbarh(data = forest_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Biome )) +
  scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "b) Forests"
  ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_forest_joint


# Grasslands

sb_grass_r <- sb_rich_area %>% filter(Realm == "Grassland") %>%  ungroup() 

head(sb_grass_r)

summary(mod_grass_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
grass_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c( Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata= .x, re_formula = NA  ))) 


head(grass_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


grass_r1_df <- grass_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Realm = "Grassland") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(grass_r1_df)


grass_d_ce <- read.csv(paste0(path2wd, 'Data/grass_d_ce.csv'))

grass_d_ce

grass_joint <- grass_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_CI = `Upper_CI`,
         d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(grass_r1_df)

grass_joint


fig_grass_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = grass_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded
             ), size = 3) +
  geom_errorbar(data = grass_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Biome  )) +
  geom_errorbarh(data = grass_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Biome )) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "b) Grasslands"
  ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_grass_joint


# Med Deserts


sb_med_de_r <- sb_rich_area %>%
  filter(Realm == "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))

head(sb_med_de_r)

summary(mod_med_de_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
med_de_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c( Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata= .x, re_formula = NA  ))) 


head(med_de_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


med_de_r1_df <- med_de_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Realm = "Mediterranean Forests, Woodlands and Scrub") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts")) %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(med_de_r1_df)


med_de_d_ce <- read.csv(paste0(path2wd, 'Data/med_de_d_ce.csv'))

med_de_d_ce

med_de_joint <- med_de_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_CI = `Upper_CI`,
         d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(med_de_r1_df)

med_de_joint


fig_med_de_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = med_de_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded
             ), size = 3) +
  geom_errorbar(data = med_de_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Biome  )) +
  geom_errorbarh(data = med_de_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Biome )) +
  labs( y = expression(paste('Average seed density / ',m^2,'')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle=  "d) Mediterranean and Deserts" ) +
  scale_color_manual( values= c(    "#da7901",  "#fab255"))+
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_med_de_joint

# Arable


sb_ar_r <- sb_rich_area %>% filter(Realm == "Arable") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))

head(sb_ar_r)

summary(mod_ar_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
ar_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_ar_r %>% group_by( Biome) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c( Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata= .x, re_formula = NA  ))) 


head(ar_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


ar_r1_df <- ar_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  dplyr::group_by(Biome) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Biome, Biome, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(ar_r1_df)


ar_d_ce <- read.csv(paste0(path2wd, 'Data/arable_d_ce.csv'))

ar_d_ce

ar_joint <- ar_d_ce %>% 
  mutate(d_Estimate = Estimate,
         d_Upper_CI = `Upper_CI`,
         d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(ar_r1_df)

ar_joint


fig_ar_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = ar_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Realm, 
             ), size = 3) +
  geom_errorbar(data = ar_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Realm  )) +
  geom_errorbarh(data = ar_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Realm )) +
  scale_color_manual( values= c( "#99610a"  ))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "e) Arable"
  ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_ar_joint


# Wetlands

sb_wetland_r <- sb_rich_area %>% filter(Realm == "Wetland") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))

head(sb_wetland_r)

summary(mod_wetland_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
wetland_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c( Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata= .x, re_formula = NA  ))) 


head(wetland_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


wetland_r1_df <- wetland_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Realm = "Wetland") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(wetland_r1_df)


wetland_d_ce <- read.csv(paste0(path2wd, 'Data/wetland_d_ce.csv'))

wetland_d_ce

wetland_joint <- wetland_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_CI = `Upper_CI`,
         d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(wetland_r1_df)

wetland_joint


fig_wetland_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = wetland_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Realm, shape = Habitat_degraded
             ), size = 3) +
  geom_errorbar(data = wetland_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Realm  )) +
  geom_errorbarh(data = wetland_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Realm )) +
  scale_color_manual( values= c(  "#20B2AA" ))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness / ',m^2,'',sep = ''))) ,
        subtitle = "f) Wetlands"
  ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
       axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_wetland_joint

# Aquatic

sb_aq_r <- sb_rich_area %>% filter(Realm == "Aquatic") %>%  ungroup() 

head(sb_aq_r)

summary(mod_aq_r)

# make sure purr not loaded, and Biome is a character NOT A FACTOR
aq_r1 <-   tidyr::crossing( 
  Number_sites = c(1),
  sb_aq_r %>% group_by( Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq(1.000000, length.out = 1) ) ), 
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


head(aq_r1)
# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 


aq_r1_df <- aq_r1  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Biome = "Aquatic") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `r_Lower_CI` = quantile(predicted, probs=0.025, na.rm =TRUE ),
  ) %>%  select(c(Biome, Habitat_degraded, r_Estimate, r_Upper_CI, r_Lower_CI)) %>% distinct() %>% ungroup()

head(aq_r1_df)


aq_d_ce <- read.csv(paste0(path2wd, 'Data/aq_d_ce.csv'))

aq_d_ce

aq_joint <- aq_d_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_CI = `Upper_CI`,
         d_Lower_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(aq_r1_df)

aq_joint


fig_aq_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = aq_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 3) +
  geom_errorbar(data = aq_joint,
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI`,  colour = Biome  )) +
  geom_errorbarh(data = aq_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`,  colour = Biome )) +
  scale_color_manual( values= c(    "#447fdd"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness / ',m^2,'',sep = ''))) ,
        subtitle = "g) Aquatic"
  ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_aq_joint




#Legend
fig_legend_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = forest_joint %>% filter(Biome == "Tropical"),
             aes(x = r_Estimate, y = d_Estimate,  shape = Habitat_degraded,
             ), size = 3) +
  geom_errorbar(data = forest_joint %>% filter(Biome == "Tropical"),
                aes(x = r_Estimate , ymin = `d_Lower_CI`, ymax =  `d_Upper_CI` )) +
  geom_errorbarh(data = forest_joint %>% filter(Biome == "Tropical"),
                 aes(y = d_Estimate , xmin = `r_Lower_CI`, xmax =  `r_Upper_CI`)) +
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "b) Forests", shape = "Habitat"
  ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title="Biome", ncol = 3))

# 8.50 X 14
fig_legend_joint




# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

fig_legend_joint <- g_legend(fig_legend_joint)


joint_fig <- (fig_tund_joint + fig_forest_joint + fig_grass_joint) /
  ( fig_med_de_joint + fig_ar_joint) /
  ( fig_wetland_joint + fig_aq_joint  )/ (fig_legend_joint) + plot_layout(heights = c(10, 10,  10,  1))

joint_fig



