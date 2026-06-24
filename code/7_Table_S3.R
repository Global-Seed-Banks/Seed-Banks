# ==============================================================================
# Seed Bank Database — Table S3: Richness-Area Model Slopes
# ==============================================================================

# ==============================================================================

# packages
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)

setwd('~/Dropbox/GSB/')

# models run on cluster, load in model objects here
load('Model_Fits/Habs/rich_aq.Rdata')
load('Model_Fits/Habs/rich_ar.Rdata')
load('Model_Fits/Habs/rich_forest.Rdata')
load('Model_Fits/Habs/rich_grass.Rdata')
load('Model_Fits/Habs/rich_med_de.Rdata')
load('Model_Fits/Habs/rich_po_alp.Rdata')
load('Model_Fits/Habs/rich_wetland.Rdata')

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)
         ) %>%
  mutate( Biome = "Tundra"
  ) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                                        Group = grepl("_und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = case_when( Group = grepl("boreal", Group) ~ "Boreal",
                             Group = grepl("temperate", Group) ~ "Temperate",
                             Group = grepl("tropical", Group) ~ "Tropical",
                             )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                             Group = grepl("und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = case_when(
                             Group = grepl("temperate", Group) ~ "Temperate and Boreal",
                             Group = grepl("tropical", Group) ~ "Tropical",
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                                        Group = grepl("und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = case_when(
    Group = grepl("desert", Group) ~ "Deserts and Xeric Shrublands",
    Group = grepl("mediterra", Group) ~ "Mediterranean Forests, Woodlands and Scrub",
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                                        Group = grepl("und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = case_when(
    Group = grepl("med_de", Group) ~ "Mediterranean and Desert",
    Group = grepl("temperate", Group) ~ "Temperate and Boreal",
    Group = grepl("tropical", Group) ~ "Tropical",
  )) %>%
  mutate( Habitat_degraded = "1") %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = case_when(
    Group = grepl("mediterra", Group) ~ "Mediterranean and Desert",
    Group = grepl("temperate", Group) ~ "Temperate and Boreal",
    Group = grepl("tropical", Group) ~ "Tropical",
  )) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                                        Group = grepl("und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

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
         slope_lower_90 = quantile(Estimate, probs=0.05),
         slope_upper_90 = quantile(Estimate, probs=0.95),
         slope_lower_50 = quantile(Estimate, probs=0.25),
         slope_upper_50 = quantile(Estimate, probs=0.75)) %>%
  mutate( Biome = "Aquatic"
  ) %>%
  mutate( Habitat_degraded = case_when( Group = grepl("deg", Group) ~ "1",
                                        Group = grepl("_und", Group) ~ "0",
  )) %>%
  dplyr::select(c(Realm, Biome, Habitat_degraded, Group, slope,slope_lower_50, slope_upper_50, slope_lower_90, slope_upper_90)) %>% distinct()

head(aq_r_p)


r_slopes <- tund_r_p %>% bind_rows(forest_r_p, grass_r_p, med_de_r_p, ar_r_p,
                                   wetland_r_p, aq_r_p) %>%
  mutate( slope = round(slope,4), slope_lower_50 = round(slope_lower_50,4), slope_upper_50 = round(slope_upper_50,4), slope_lower_90 = round(slope_lower_90,4), slope_upper_90 = round(slope_upper_90,4),

          )

# NOTE: `table_1` here is just `r_slopes` under a different name - it is
# NOT the manuscript's Table 1 (that's built separately in
# seedbank_data_prep_clean.R). Kept as in the source script; see the header
# note above.
table_1 <- r_slopes

head(table_1)

# ------------------------------------------------------------------------------
# TABLE S3 - richness-area model slopes by Realm/Biome/Ecoregion/Degraded
# ------------------------------------------------------------------------------
table_s3 <- table_1 %>%  unite("Slope 50% Credible Interval", slope_lower_50:slope_upper_50, sep="-") %>%
  unite("Slope 90% Credible Interval", slope_lower_90:slope_upper_90, sep="-") %>%
  mutate(`Intervals` = paste0("(", `Slope 50% Credible Interval`, " , " ,`Slope 90% Credible Interval`, ")") ) %>%
  ungroup() %>%
  select(-c(Group, `Slope 50% Credible Interval`, `Slope 90% Credible Interval`)) %>%
  unite("Slope and Intervals", slope:`Intervals`, sep=" ") %>% ungroup()

table_s3

# FIXED: was a hardcoded absolute path ("~/Dropbox/GSB/Data/..."); now
# relative to the setwd() set above.
write.csv(table_s3, "Data/Table_S3.csv")

