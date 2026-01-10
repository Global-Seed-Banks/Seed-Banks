

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
                  "emmaladouceur" = "~/Dropbox/GSB/",
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
  mutate(Realm = "Tundra",  Biome = "Tundra") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(tund_r1_df)


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


tund_joint <- tund_d_90_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
                     d_Upper_90_CI = `Upper_CI`,
                     d_Lower_90_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% 
  left_join(
    tund_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  ) %>%  left_join(tund_r1_df)

tund_joint


fig_tund_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = tund_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 5) +
  geom_errorbar(data = tund_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = tund_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0) +
  geom_errorbar(data = tund_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = tund_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c(  "#94b594"  )) + 
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x = "", 
       # x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "a Tundra"
        ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
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
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(forest_r1_df)



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

forest_joint <- forest_d_90_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
                                   d_Upper_90_CI = `Upper_CI`,
                                   d_Lower_90_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  %>% 
  left_join(
    forest_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  ) %>% left_join(forest_r1_df)

forest_joint


fig_forest_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = forest_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 5) +
  geom_errorbar(data = forest_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = forest_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0) +
  geom_errorbar(data = forest_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = forest_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c(  "#1e3d14", "#788f33","#228B22" ),
                      labels = c("Boreal", "Temperate", "Tropical & \nSubtropical")
                      )+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "b Forests"
  ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
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
  mutate( Realm = "Grasslands") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(grass_r1_df)


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


grass_joint <- grass_d_90_ce %>% 
  select(-Realm) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_90_CI = `Upper_CI`,
         d_Lower_90_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% 
  left_join(
    grass_d_50_ce %>% 
      select(-Realm) %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  )  %>% left_join(grass_r1_df)

grass_joint


fig_grass_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = grass_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded
             ), size = 5) +
  geom_errorbar(data = grass_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = grass_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0) +
  geom_errorbar(data = grass_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = grass_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c(  "#d8b847", "#b38711"),
                      labels = c("Temperate & \nBoreal", "Tropical & \nSubtropical"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "c Grasslands & Savannas"
  ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
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
  mutate( Realm = "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")) %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(med_de_r1_df)



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



med_de_joint <- med_de_d_90_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_90_CI = `Upper_CI`,
         d_Lower_90_CI = `Lower_CI`) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% 
  left_join(
    med_de_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  )  %>% left_join(med_de_r1_df)%>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

med_de_joint


fig_med_de_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = med_de_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded
             ), size = 5) +
  geom_errorbar(data = med_de_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = med_de_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0) +
  geom_errorbar(data = med_de_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = med_de_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  labs( y = "Seed density m-2",
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle=  "d Mediterranean & Desert" ) +
  scale_color_manual( values= c(    "#da7901",  "#fab255"),
                      labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"))+
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.title.y = element_text(face = "bold"), 
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
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(ar_r1_df)


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



ar_joint <- arable_d_90_ce %>% 
  mutate(d_Estimate = Estimate,
         d_Upper_90_CI = `Upper_CI`,
         d_Lower_90_CI = `Lower_CI`)  %>% 
  left_join(
    arable_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  ) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(ar_r1_df)%>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

ar_joint


fig_ar_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = ar_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, 
             ), size = 5, shape=15) +
  geom_errorbar(data = ar_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = ar_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0) +
  geom_errorbar(data = ar_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = ar_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c( "#99610a" , "#E2C59F", "#AA3929"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "e Arable"
  ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
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
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(wetland_r1_df)



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



wetland_joint <- wetland_d_90_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_90_CI = `Upper_CI`,
         d_Lower_90_CI = `Lower_CI`) %>% 
  left_join(
    wetland_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  )  %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(wetland_r1_df)%>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

wetland_joint


fig_wetland_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = wetland_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded
             ), size = 5) +
  geom_errorbar(data = wetland_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = wetland_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ),height=0) +
  geom_errorbar(data = wetland_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = wetland_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352" ), 
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= "Species richness m-2",
        subtitle = "f Wetlands & Flooded Grasslands"
  ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.title.x = element_text(face = "bold"), 
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
  mutate( Realm = "Aquatic", Biome = "Aquatic") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate( r_Estimate = round( mean(predicted, na.rm =TRUE ) ,0),
          `r_Upper_90_CI` = quantile(predicted, probs=0.95, na.rm =TRUE ),
          `r_Lower_90_CI` = quantile(predicted, probs=0.05, na.rm =TRUE ),
          `r_Upper_50_CI` = quantile(predicted, probs=0.25, na.rm =TRUE ),
          `r_Lower_50_CI` = quantile(predicted, probs=0.75, na.rm =TRUE ),
  ) %>%  
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>% distinct() %>% ungroup()

head(aq_r1_df)




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



aq_joint <- aq_d_90_ce %>% 
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(d_Estimate = Estimate,
         d_Upper_90_CI = `Upper_CI`,
         d_Lower_90_CI = `Lower_CI`)  %>% 
  left_join(
    aq_d_50_ce %>% 
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(d_Estimate = Estimate,
             d_Upper_50_CI = `Upper_CI`,
             d_Lower_50_CI = `Lower_CI`) %>% 
      select(-c(Estimate, `Upper_CI`, `Lower_CI` ))  
  ) %>% 
  select(-c(Estimate, `Upper_CI`, `Lower_CI` )) %>% left_join(aq_r1_df)

aq_joint


fig_aq_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = aq_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded,
             ), size = 5) +
  geom_errorbar(data = aq_joint,
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI`,  colour = Biome  ), width = 0 ) +
  geom_errorbarh(data = aq_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`,  colour = Biome ), height = 0 ) +
  geom_errorbar(data = aq_joint,
                aes(x = r_Estimate , ymin = `d_Lower_50_CI`, ymax =  `d_Upper_50_CI`,  colour = Biome  ), size=2, width=0) +
  geom_errorbarh(data = aq_joint,
                 aes(y = d_Estimate , xmin = `r_Lower_50_CI`, xmax =  `r_Upper_50_CI`,  colour = Biome ), size=2,height=0) +
  scale_color_manual( values= c(    "#447fdd"))+
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= "Species richness m-2",
        subtitle = "g Aquatic"
  ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.title.x = element_text(face = "bold"), 
        axis.title.y=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title=""), shape="none")

# 8.50 X 14
fig_aq_joint




#Legend
fig_legend_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = forest_joint ,
             aes(x = r_Estimate, y = d_Estimate,  shape = Biome,
             ), size = 3, color="grey") +
  geom_errorbar(data = forest_joint %>% filter(Biome == "Tropical"),
                aes(x = r_Estimate , ymin = `d_Lower_90_CI`, ymax =  `d_Upper_90_CI` )) +
  geom_errorbarh(data = forest_joint %>% filter(Biome == "Tropical"),
                 aes(y = d_Estimate , xmin = `r_Lower_90_CI`, xmax =  `r_Upper_90_CI`)) +
  labs( y = expression(paste('Average seed density (',m^2,')')),
        x= (expression(paste('Average species richness (',m^2,')',sep = ''))) ,
        subtitle = "b) Forests", shape = "State"
  ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat", "Arable"), values = c(  16, 17, 15) ) +
  theme_classic(base_size=16) +
  theme( plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
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
#16X16

table_joint <- tund_joint %>% bind_rows(forest_joint, grass_joint, med_de_joint, 
                                      ar_joint, wetland_joint, aq_joint) 

head(table_joint)
print(table_joint, n=Inf)

write.csv(table_joint, "~/Dropbox/GSB/Data/joint.csv")



table_s7c <- tund_r1_df %>% bind_rows(forest_r1_df, grass_r1_df, med_de_r1_df, 
                                    ar_r1_df, wetland_r1_df, aq_r1_df) %>%
  mutate(Estimate = round(r_Estimate, 0),
         Lower_90 = round(r_Lower_90_CI, 0),
         Upper_90 = round(r_Upper_90_CI, 0), 
         Lower_50 = round(r_Lower_50_CI, 0),
         Upper_50 = round(r_Upper_50_CI, 0)
         ) %>%
  unite("90_CI", Lower_90:Upper_90, sep=",") %>%
  unite("50_CI", Lower_50:Upper_50, sep=",") %>%
  select(-c(r_Estimate, r_Lower_90_CI, r_Upper_90_CI, r_Upper_50_CI, r_Lower_50_CI )) %>%
  mutate(CI = paste0("(", `50_CI`, " , ", `90_CI`, ")") ) %>%
    select(-c(`90_CI`, `50_CI`)) %>%
  unite("1", Estimate:CI, sep=" ")

print(table_s7c, n=Inf)

scales_div <- table_s7 %>% left_join(table_s7c) %>%
  mutate(Realm = as.factor(Realm)) %>%
  mutate(Realm = fct_relevel(Realm, "Tundra", "Forest", "Grasslands", "Mediterranean and Desert",
                                  "Arable", "Wetland", "Aquatic"
  )) %>% arrange(Realm) %>% ungroup() %>% select(Realm, Biome, Habitat_degraded, `0.01`, `1`, `15`)

print(scales_div, n=Inf)


write.csv(scales_div, "~/Dropbox/GSB/Data/Table_Fig_2_Fig_4.csv")


