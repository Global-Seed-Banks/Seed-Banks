


# packages
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)


user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'rich_aq.Rdata')
load( 'rich_ar.Rdata')
load( 'rich_forest.Rdata')
load( 'rich_grass.Rdata')
load( 'rich_med_de.Rdata')
load( 'rich_po_alp.Rdata')
load( 'rich_wetland.Rdata')

# tundra
tund_r_draws <- as_draws_df(mod_tund_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(tund_r_draws)
colnames(tund_r_draws)

tund_r_p <-  tund_r_draws %>% 
  as_tibble() %>%
  mutate( 
    tund_r_tund_deg = (`b_Centred_log_total_sample_area_m2` ),
    tund_r_tund_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(tund_r_tund_deg,tund_r_tund_und
  )) %>% mutate(Realm ="Tundra") %>% gather(Group, Estimate, tund_r_tund_deg:tund_r_tund_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = "Tundra"
  ) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                                        Group = grepl("_und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(tund_r_p)


# Forest
forest_r_draws <- as_draws_df(mod_forest_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(forest_r_draws)
colnames(forest_r_draws)

forest_r_p <-  forest_r_draws %>% 
  as_tibble() %>%
  mutate( 
    forest_r_boreal_deg = (`b_Centred_log_total_sample_area_m2` ),
    forest_r_boreal_und = (`b_Centred_log_total_sample_area_m2` +`b_Habitat_degraded0`),
    forest_r_temperate_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTemperate` ),
    forest_r_temperate_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTemperate:Habitat_degraded0` ),
    forest_r_tropical_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical` ),
    forest_r_tropical_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(forest_r_boreal_deg,forest_r_boreal_und, forest_r_temperate_deg, forest_r_temperate_und,
                  forest_r_tropical_deg, forest_r_tropical_und
  )) %>% mutate(Realm ="Forest") %>% gather(Group, Estimate, forest_r_boreal_deg:forest_r_tropical_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = case_when( Group = grepl("boreal", Group) ~ "Boreal", 
                             Group = grepl("temperate", Group) ~ "Temperate", 
                             Group = grepl("tropical", Group) ~ "Tropical", 
                             )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                             Group = grepl("und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(forest_r_p)


# grass
grass_r_draws <- as_draws_df(mod_grass_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(grass_r_draws)
colnames(grass_r_draws)

grass_r_p <-  grass_r_draws %>% 
  as_tibble() %>%
  mutate( 
    grass_r_temperate_deg = (`b_Centred_log_total_sample_area_m2` ),
    grass_r_temperate_und = (`b_Centred_log_total_sample_area_m2` +`b_Habitat_degraded0`),
    grass_r_tropical_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical` ),
    grass_r_tropical_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(grass_r_temperate_deg, grass_r_temperate_und,
                  grass_r_tropical_deg, grass_r_tropical_und
  )) %>% mutate(Realm ="Grassland") %>% gather(Group, Estimate, grass_r_temperate_deg:grass_r_tropical_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = case_when( 
                             Group = grepl("temperate", Group) ~ "Temperate and Boreal", 
                             Group = grepl("tropical", Group) ~ "Tropical", 
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                                        Group = grepl("und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(grass_r_p)

# med_de
med_de_r_draws <- as_draws_df(mod_med_de_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(med_de_r_draws)
colnames(med_de_r_draws)

med_de_r_p <-  med_de_r_draws %>% 
  as_tibble() %>%
  mutate( 
    med_de_r_desert_deg = (`b_Centred_log_total_sample_area_m2` ),
    med_de_r_desert_und = (`b_Centred_log_total_sample_area_m2` +`b_Habitat_degraded0`),
    med_de_r_mediterra_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeMediterraneanForestsWoodlandsandScrub` ),
    med_de_r_mediterra_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeMediterraneanForestsWoodlandsandScrub:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(med_de_r_desert_deg, med_de_r_desert_und,
                  med_de_r_mediterra_deg, med_de_r_mediterra_und
  )) %>% mutate(Realm ="Mediterranean and Desert") %>% gather(Group, Estimate, med_de_r_desert_deg:med_de_r_mediterra_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = case_when( 
    Group = grepl("desert", Group) ~ "Deserts and Xeric Shrublands", 
    Group = grepl("mediterra", Group) ~ "Mediterranean Forests, Woodlands and Scrub", 
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                                        Group = grepl("und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(med_de_r_p)


# ar
ar_r_draws <- as_draws_df(mod_ar_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(ar_r_draws)
colnames(ar_r_draws)

ar_r_p <-  ar_r_draws %>% 
  as_tibble() %>%
  mutate( 
    ar_r_med_de = (`b_Centred_log_total_sample_area_m2` ),
    ar_r_temperate = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTemperateandBoreal` ),
    ar_r_tropical = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical` ),
  ) %>%
  dplyr::select(c(ar_r_med_de, ar_r_temperate,
                  ar_r_tropical
  )) %>% mutate(Realm ="Arable") %>% gather(Group, Estimate, ar_r_med_de:ar_r_tropical) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = case_when( 
    Group = grepl("med_de", Group) ~ "Mediterranean and Desert", 
    Group = grepl("temperate", Group) ~ "Temperate and Boreal", 
    Group = grepl("tropical", Group) ~ "Tropical", 
  )) %>%
  mutate( Habitat_degraded = "1") %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(ar_r_p)

# wetland
wetland_r_draws <- as_draws_df(mod_wetland_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(wetland_r_draws)
colnames(wetland_r_draws)

wetland_r_p <-  wetland_r_draws %>% 
  as_tibble() %>%
  mutate( 
    wetland_r_mediterra_deg = (`b_Centred_log_total_sample_area_m2` ),
    wetland_r_mediterra_und = (`b_Centred_log_total_sample_area_m2` +`b_Habitat_degraded0`),
    wetland_r_temperate_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTemperateandBoreal` ),
    wetland_r_temperate_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTemperateandBoreal:Habitat_degraded0` ),
    wetland_r_tropical_deg = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical` ),
    wetland_r_tropical_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:BiomeTropical:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(wetland_r_mediterra_deg, wetland_r_mediterra_und, 
                  wetland_r_temperate_deg, wetland_r_temperate_und,
                  wetland_r_tropical_deg, wetland_r_tropical_und
  )) %>% mutate(Realm ="Wetland") %>% gather(Group, Estimate, wetland_r_mediterra_deg:wetland_r_tropical_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = case_when( 
    Group = grepl("mediterra", Group) ~ "Mediterranean and Desert", 
    Group = grepl("temperate", Group) ~ "Temperate and Boreal", 
    Group = grepl("tropical", Group) ~ "Tropical", 
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                                        Group = grepl("und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(wetland_r_p)



#  aquatic
aq_r_draws <- as_draws_df(mod_aq_r, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

head(aq_r_draws)
colnames(aq_r_draws)

aq_r_p <-  aq_r_draws %>% 
  as_tibble() %>%
  mutate( 
    aq_r_aq_deg = (`b_Centred_log_total_sample_area_m2` ),
    aq_r_aq_und = (`b_Centred_log_total_sample_area_m2` + `b_Centred_log_total_sample_area_m2:Habitat_degraded0` ),
  ) %>%
  dplyr::select(c(aq_r_aq_deg,aq_r_aq_und
  )) %>% mutate(Realm ="Aquatic") %>% gather(Group, Estimate, aq_r_aq_deg:aq_r_aq_und) %>%
  group_by(Realm, Group) %>% 
  mutate(slope = mean(Estimate),
         slope_lower = quantile(Estimate, probs=0.025),
         slope_upper = quantile(Estimate, probs=0.975)) %>%
  mutate( Biome = "Aquatic"
  ) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1", 
                                        Group = grepl("_und", Group) ~ "0", 
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower,slope_upper)) %>% distinct() 

head(aq_r_p)


r_slopes <- tund_r_p %>% bind_rows(forest_r_p, grass_r_p, med_de_r_p, ar_r_p,
                                   wetland_r_p, aq_r_p) %>% 
  mutate( slope = round(slope,4), slope_lower = round(slope_lower,4), slope_upper = round(slope_upper,4),)

table_1 <- r_slopes


head(table_1)
head(table_2)
head(table_3)
head(table_4)
head(table_5)
head(table_6)

table_1_dat <- table_1 %>%  unite("Richness-area slope", slope_lower:slope_upper, sep="-") %>%
  mutate(`Richness-area slope` = paste0("(", `Richness-area slope`, ")") ) %>%
  #select(-c(X, Model))  %>%
  unite("Richness-area slope", slope:`Richness-area slope`, sep=" ") %>% ungroup() %>%
  select(-Group)

table_1_dat

head(table_2)

table_2_dat <- table_2 %>%   unite("Seed density", Lower_CI:Upper_CI, sep="-") %>%
  mutate(`Seed density` = paste0("(", `Seed density`, ")") ) %>%
 # select(-c(X, Model)) %>%
  unite("Seed density", Estimate:`Seed density`, sep=" ")

table_2_dat

table_3_dat <- table_3 %>%   unite("Seed-species ratio", Lower_CI:Upper_CI, sep="-") %>%
  mutate(`Seed-species ratio` = paste0("(", `Seed-species ratio`, ")") ) %>%
  #select(-c(X, Model)) %>%
  unite("Seed-species ratio", Estimate:`Seed-species ratio`, sep=" ")

table_3_dat

table_S6 <- table_1_dat %>% left_join(table_2_dat) %>% left_join(table_3_dat)

print(table_S6, n=Inf)

write.csv(table_S6, "table_S6.csv")


