

#rm(list = ls())


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
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata= .x, re_formula = NA, probs = c(0.025, 0.975)  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 

tund_predict_df <- tund_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate( Realm = "Tundra", Biome = "Tundra") %>% filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  mutate(Group = "Terrestrial")

head(tund_predict_df)
colnames(tund_predict_df)
nrow(tund_predict_df)

tund_div <- tund_predict_df %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>% 
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Terrestrial")

head(tund_div)



#Forest
sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>%  ungroup()

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
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  mutate(Realm = "Forest")  %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Terrestrial")

head(forest_predict_df)

forest_div <- forest_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Terrestrial")

forest_div

#Grass
sb_grass_r <- sb_rich_area %>% filter(Realm == "Grassland") %>%  ungroup()

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
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  mutate(Realm = "Grassland")  %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Terrestrial")

head(grass_predict_df)

grass_div <- grass_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Terrestrial")

grass_div



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
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))%>%
  mutate(Realm = "Mediterranean and Desert")%>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Terrestrial")

med_de_div <- med_de_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Terrestrial")

med_de_div


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
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))%>%
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  #mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                             `1` = "Degraded habitat"  )) %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Arable")

ar_div <- ar_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Arable")

ar_div



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
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical")) %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Wetlands")

wetland_div <- wetland_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Wetlands")

wetland_div


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
  mutate( Biome = "Aquatic", Realm = "Aquatic") %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate( Total_sample_area_m2 = recode_factor(
    Total_sample_area_m2,  `0.01` = "α",`15` = "γ" )) %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) %>%
  filter(!is.na(Total_sample_area_m2)) %>%        # ensure no NA in y
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)   ) %>%
  mutate(Group = "Aquatic")

aq_div <- aq_predict_df %>% filter(Number_sites == 1) %>% 
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs=0.025),
         Upper_95 = quantile(predicted, probs=0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)
  ) %>%
  select(-predicted) %>% distinct()%>%
  mutate(Group = "Aquatic")

aq_div


predict_dat <- tund_predict_df %>% bind_rows(aq_predict_df) %>%
  bind_rows(ar_predict_df) %>%
  bind_rows(wetland_predict_df) %>%
  bind_rows(med_de_predict_df) %>%
  bind_rows(grass_predict_df) %>%
  bind_rows(forest_predict_df) %>%
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
  )) %>% mutate(Estimate = predicted)
  # mutate(Realm_Biome = fct_relevel(Realm_Biome, 
  #                                  "Tundra", "Boreal Forests/Taiga",  "Temperate Forests",  "Tropical & Subtropical Forests",
  #                                  "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
  #                                  "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands", 
  #                                  "Temperate & Boreal Arable", "Mediterranean & Desert Arable",    "Tropical Arable",
  #                                  "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
  #                                  "Aquatic",
  # )) %>%   mutate(Realm = fct_relevel(Realm, 
  #                                     "Tundra", "Forest", "Grassland", "Mediterranean and Desert", "Arable", "Wetland", "Aquatic"
  # )) %>% mutate(Estimate = predicted) %>% 
  # mutate(Realm_Biome = factor(Realm_Biome, levels = biome_levels_pd))

head(predict_dat)
View(predict_dat)

div_dat <- tund_div %>% bind_rows(forest_div, grass_div, med_de_div, ar_div, wetland_div, aq_div) %>%
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
  )) %>% mutate(Estimate = Estimate_) 
  # mutate(Realm_Biome = fct_relevel(Realm_Biome,
  #                                  "Tundra", "Boreal Forests/Taiga",  "Temperate Forests",  "Tropical & Subtropical Forests",
  #                                  "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  #                                  "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands",
  #                                  "Temperate & Boreal Arable", "Mediterranean & Desert Arable",    "Tropical Arable",
  #                                  "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
  #                                  "Aquatic",
  # )) %>%   mutate(Realm = fct_relevel(Realm, 
  #                                      "Tundra", "Forest", "Grassland", "Mediterranean and Desert", "Arable", "Wetlands", "Aquatic"
  #  )) %>% mutate(Estimate = Estimate_)  %>% 
  # mutate(Realm_Biome = factor(Realm_Biome, levels = biome_levels_dd))

head(div_dat)
View(div_dat)


tdiv_dat <- div_dat %>% filter(Group == "Terrestrial") #%>% 
  # mutate(Realm_Biome = fct_relevel(Realm_Biome, "Tundra", "Boreal Forests/Taiga","Temperate Forests","Tropical & Subtropical Forests", 
  #                                "Temperate & Boreal Grasslands, Savannas & Shrublands",    "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
  #                                "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands")) %>% 
  # mutate(Realm_Biome = factor(Realm_Biome, levels = biome_levels_tdd))


biome_levels <- c(
  "Tundra",
  "Boreal Forests/Taiga",
  "Temperate Forests",
  "Tropical & Subtropical Forests",
  "Temperate & Boreal Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Mediterranean Forests, Woodlands & Scrub",
  "Deserts & Xeric Shrublands",
  "Temperate & Boreal Arable",
  "Mediterranean & Desert Arable",
  "Tropical Arable",
  "Temperate & Boreal Wetlands",
  "Mediterranean & Desert Wetlands",
  "Tropical Wetlands",
  "Aquatic"
)

add_y <- function(df) {
  df %>%
    mutate(
      Realm_Biome = factor(Realm_Biome, levels = rev(biome_levels)),  # 👈 KEY
      y_base = as.numeric(Realm_Biome)
    )
}

predict_dat <- add_y(predict_dat)
div_dat     <- add_y(div_dat)
tdiv_dat    <- add_y(tdiv_dat)

y_offset <- 0.18
y_text_offset <- 0.18

y_offset_1 <- 0.36
y_offset_2 <- 0.18
y_offset_3 <- 0.54

head(div_dat)

t_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = y_base,   height = ..density.., color = Realm_Biome),
                      alpha = 0.35, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = y_base + y_offset_1,   height = ..density.., color = Realm_Biome),  alpha = 0.7, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = y_base + y_offset_2,   height = ..density.., color = Realm_Biome,  fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = y_base + y_offset_3,   height = ..density.., color = Realm_Biome, fill = Realm_Biome),  alpha = 0.7, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  #points
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
             aes(x = Estimate, y = y_base), alpha = 0.9, color = "#C0C0C0", shape=17, size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = y_base + y_offset_1), alpha = 0.9, color = "#C0C0C0", shape=17, size=4) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
             aes(x = Estimate, y = y_base + y_offset_2, color=Realm_Biome),  shape=16, size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = y_base + y_offset_3, color=Realm_Biome),  shape=16, size=4) +
  #errors
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50 ), height = 0.15, linewidth = 1.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50 ), height = 0.15, linewidth = 1.5, colour = "#C0C0C0" )+
  
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base + y_offset_2, xmin = Lower_90, xmax = Upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base + y_offset_2, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_3, xmin = Lower_90, xmax = Upper_90 , color=Realm_Biome), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_3, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  # xlim(0,50)+
  scale_color_manual( values= c( "#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594" ))+
  scale_fill_manual( values= c( "#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594"))+
  labs(subtitle = "Natural Terrestrial Areas",
    x = "Species richness in the soil seedbank",
    y = "Biome") +
  geom_text(data = tdiv_dat %>% filter(Group == "Terrestrial") %>% distinct(Realm_Biome, y_base),
            aes( y = y_base + y_text_offset, label = Realm_Biome ), x =  120, colour = "grey60", vjust = 0)+
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


t_div_fig



ar_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Arable")  %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = y_base,   height = ..density.., color = Realm_Biome,  fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = y_base + y_offset_1,   height = ..density.., color = Realm_Biome, fill = Realm_Biome),  
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>%  filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base, color=Realm_Biome),   shape=15, size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = y_base + y_offset_1, color=Realm_Biome), shape=15, size=4) +
  geom_errorbarh( data = div_dat %>% filter(Group == "Arable")  %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base , xmin = Lower_90, xmax = Upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Arable")  %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90 , color=Realm_Biome), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Arable")  %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base , xmin = Lower_50, xmax = Upper_50, color=Realm_Biome ), height = 0.15, linewidth = 1.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Arable")  %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  
  #scale_y_discrete(limits=rev)+
  # xlim(0,50)+
  scale_color_manual( values= c( "#AA3929",  "#E2C59F", "#99610a"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical") )+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c("#AA3929",  "#E2C59F", "#99610a" ),
                     labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"))+
  labs(subtitle = "Arable",
    x = "Species richness in the soil seedbank",
    y = "Biome") +
  # geom_text(data = div_dat %>% filter(Group == "Arable") %>% distinct(Realm_Biome, y_base) ,
  #           aes( y = y_base + y_text_offset, label = Realm_Biome ), x =  55, colour = "grey60", vjust = 0)+
  geom_text(data = div_dat %>% filter(Group == "Arable") %>%distinct(Realm_Biome, y_base) %>%
              mutate(label = case_when(
                Realm_Biome == "Temperate & Boreal Arable" ~ "Temperate & Boreal",
                Realm_Biome ==  "Mediterranean & Desert Arable" ~ "Mediterranean & Desert",
                Realm_Biome == "Tropical Arable" ~ "Tropical",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset, label = label), x = 55, colour = "grey60", vjust = 0
  )+
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


ar_div_fig

div_dat %>% filter(Group == "Wetlands") %>% distinct(Realm_Biome)

w_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = y_base,   height = ..density.., color = Realm_Biome),
                      alpha = 0.7, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = y_base + y_offset_1,   height = ..density.., color = Realm_Biome),  alpha = 0.7, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = y_base + y_offset_2,   height = ..density.., color = Realm_Biome,  fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = y_base + y_offset_3,   height = ..density.., color = Realm_Biome, fill = Realm_Biome),  alpha = 0.35, scale = 0.9,rel_min_height = 0.001, stat = "density_ridges", position = "identity") +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base), alpha = 0.9, color = "#C0C0C0", shape=17, size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = y_base + y_offset_1),  color = "#C0C0C0", shape=17, size=4) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base+ y_offset_2, color=Realm_Biome),  shape=16, size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = y_base+ y_offset_3, color=Realm_Biome),  shape=16, size=4) +
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base + y_offset_2, xmin = Lower_90, xmax = Upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = y_base + y_offset_2, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_3, xmin = Lower_90, xmax = Upper_90 , color=Realm_Biome), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = y_base + y_offset_3, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  #scale_y_discrete(limits=rev)+
  # xlim(0,50)+
  scale_color_manual( values= c(  "#293352", "#4E84C4", "#20B2AA" ))+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c( "#293352", "#4E84C4", "#20B2AA"))+
  labs(subtitle = "Wetlands & Flooded Grasslands",
    x = "Species richness in the soil seedbank",
    y = "Biome") +
  # geom_text(data = div_dat %>% filter(Group == "Wetlands") %>% distinct(Realm_Biome, y_base),
  #           aes( y = y_base + y_text_offset, label = Realm_Biome ), x =  75, colour = "grey60", vjust = 0)+
  geom_text(data = div_dat %>% filter(Group == "Wetlands") %>%distinct(Realm_Biome, y_base) %>%
      mutate(label = case_when(
        Realm_Biome == "Temperate & Boreal Wetlands" ~ "Temperate & Boreal",
        Realm_Biome ==  "Mediterranean & Desert Wetlands" ~ "Mediterranean & Desert",
        Realm_Biome == "Tropical Wetlands" ~ "Tropical",
        TRUE ~ Realm_Biome
      )),
    aes(y = y_base + y_text_offset, label = label), x = 75, colour = "grey60", vjust = 0
  )+
  theme_bw(base_size = 18) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    #axis.title.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )


w_div_fig


aq_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Aquatic") %>%  filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = Habitat_degraded,   height = ..density..), color="#C0C0C0", fill="#C0C0C0",
                      alpha = 0.35, scale = 0.9,  stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = Habitat_degraded,   height = ..density..),  color="#C0C0C0", fill="#C0C0C0",
                      alpha = 0.7, scale = 0.9, stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Aquatic") %>%  filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") , 
                      aes(x = Estimate, y = Habitat_degraded,   height = ..density..), color="#447fdd", fill="#447fdd",
                      alpha = 0.35, scale = 0.9,  stat = "density_ridges", position = "identity") +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") , 
                      aes(x = Estimate, y = Habitat_degraded,   height = ..density..),  color="#447fdd", fill="#447fdd",
                      alpha = 0.7, scale = 0.9,stat = "density_ridges", position = "identity") +
  
  geom_point(data = div_dat %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α") ,
             aes(x = Estimate, y = Habitat_degraded, color=Habitat_degraded, shape=Habitat_degraded), alpha = 0.9,  size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Aquatic")  %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = Habitat_degraded, color=Habitat_degraded, shape=Habitat_degraded), alpha = 0.9, size=4) +
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = Habitat_degraded, xmin = Lower_90, xmax = Upper_90 ), height = 0, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = Habitat_degraded, xmin = Lower_90, xmax = Upper_90 ), height = 0, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = Habitat_degraded, xmin = Lower_50, xmax = Upper_50 ), height = 0, linewidth = 1.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = Habitat_degraded, xmin = Lower_50, xmax = Upper_50 ), height = 0, linewidth = 1.5, colour = "#C0C0C0" )+
  
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = Habitat_degraded , xmin = Lower_90, xmax = Upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = Habitat_degraded, xmin = Lower_90, xmax = Upper_90 , color=Realm_Biome), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α") ,
                  aes(y = Habitat_degraded , xmin = Lower_50, xmax = Upper_50, color=Realm_Biome ), height = 0.15, linewidth = 1.5 )+
  geom_errorbarh( data = div_dat %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ") ,
                  aes(y = Habitat_degraded, xmin = Lower_50, xmax = Upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  #scale_y_discrete(limits=rev)+
  # xlim(0,50)+
  scale_color_manual( values= c("#447fdd" ,"#C0C0C0", "#447fdd","#447fdd" ))+
  scale_fill_manual( values= c( "#447fdd", "#C0C0C0"))+
  scale_shape_manual( values= c( 16, 17),
                      labels = c("Undisturbed", "Degraded"))+
  labs(subtitle = "Aquatic",
    #x = "Species richness in the soil seedbank",
    x="",
    y = "State") +
  theme_bw(base_size = 18) +
  #ggplot2::annotate("text", x = 60, y = 1.1, hjust = 0, size = 5, label = paste("Forests"), color = "black", alpha = 0.7) +
  geom_text(data = div_dat %>% filter(Group == "Aquatic") %>% distinct(Habitat_degraded) ,
            aes( y = as.numeric(factor(Habitat_degraded)) + 0.15, label = Habitat_degraded ), x =  60, colour = "grey60", vjust = 0)+
  theme(
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    #axis.title.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

aq_div_fig

(t_div_fig + ar_div_fig)/(w_div_fig + aq_div_fig) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))
#13X18


(t_div_fig)/ (ar_div_fig + w_div_fig + aq_div_fig) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))



legend_g <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = div_dat %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α") ,
             aes(x = Estimate, y = Habitat_degraded, shape=Habitat_degraded, group=Habitat_degraded), alpha = 0.9,  size=4) +
  geom_point(data = div_dat %>% filter(Group == "Aquatic")  %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = Habitat_degraded, shape=Habitat_degraded, group=Habitat_degraded), alpha = 0.9, size=4) +
  scale_fill_manual( values= c( "#d8b847", "#b38711"
  )) +   coord_cartesian( ylim = c(0,90)) +
  labs(x = '', y='', shape = (expression(paste( italic(gamma), '-richness 15 (',m^2,')',sep = ''))),
       subtitle=  "c) Grasslands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") 

legend_g


legend_a <- ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = div_dat %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α") ,
             aes(x = Estimate, y = Habitat_degraded, shape=Habitat_degraded, group=Habitat_degraded),  size=2.5) +
  geom_point(data = div_dat %>% filter(Group == "Aquatic")  %>% filter(Total_sample_area_m2 == "γ") ,
             aes(x = Estimate, y = Habitat_degraded, shape=Habitat_degraded, group=Habitat_degraded),  size=2.5) +
  scale_fill_manual( values= c( "#d8b847", "#b38711"
  )) +   coord_cartesian( ylim = c(0,90)) +
  labs(x = '', y='', shape = (expression(paste( italic(alpha), '-richness 0.01 (',m^2,')',sep = ''))),
       subtitle=  "c) Grasslands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat"), values = c(  16, 17) ) +
  guides(shape = guide_legend(override.aes = list(size = 2.5)))+
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

# richness_fig <- (fig_tund_r + fig_forest_r + fig_grass_r) /
#   ( fig_med_de_r + fig_ar_r) /
#   ( fig_wetland_r + fig_aq_r  )/ (legend_g) / (legend_a) + plot_layout(heights = c(10, 10,  10, 2.5, 1))
# 
# richness_fig

leg <- grid.arrange( arrangeGrob( legend_g , legend_a), ncol = 1, nrow = 9,
                     heights = c(0.10, 0.10, 0.10,0.10, 1, 0.10, 1, 0.10, 0.10),
                     layout_matrix = rbind(c(NA), c(NA), c(NA), c(NA), c(1), c(NA), c(2), c(NA), c(NA))
)



t_div_fig

low_row <- (ar_div_fig + w_div_fig + aq_div_fig)

richness_fig <- ( low_row / legend_a/legend_g) + plot_layout(heights = c(10, 1, 1))
richness_fig




