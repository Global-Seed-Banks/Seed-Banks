# ==============================================================================
# Seed bank database - species richness ~ sample area figures (Figure 3-ish)
#
# What this script does:
#   1. Loads the prepared seed bank dataset and the brms richness~area models
#      that were fit per Realm on the cluster (rich_aq, rich_ar, rich_forest,
#      rich_grass, rich_med_de, rich_po_alp [Tundra], rich_wetland).
#   2. For each Realm, generates posterior predictions across a small range
#      of sample areas (alpha = 0.01 m2, gamma = 15 m2) and number of sites,
#      then summarises those into point estimates + 50/90/95% intervals.
#   3. Builds one ridgeline/point-and-error-bar figure per group (Terrestrial
#      non-arable, Arable, Wetlands/Transitional, Aquatic), extracts a couple
#      of standalone legends, and assembles everything into a composite
#      richness_fig.
#
# NOTE: one real bug is fixed inline (flagged "# FIXED:") - a stray space in
# a string comparison ("Undisturbed ha bitat") in the aq_div_fig block, which
# would silently match zero rows. One thing is flagged but NOT fixed because
# it's ambiguous: the final richness_fig only combines panels b/c/d
# (Arable/Wetlands/Aquatic) - panel "a" (t_div_fig, Terrestrial non-arable)
# is built but never added to the combined figure. That may be intentional
# (e.g. exported as its own full-page panel elsewhere) - flagged where it
# happens rather than guessed at.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LIBRARIES
# ------------------------------------------------------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)
library(ggridges)

# ------------------------------------------------------------------------------
# 2. SETUP - working directory & load prepared data
# ------------------------------------------------------------------------------

setwd('~/Dropbox/GSB/')
sb_prep <- read.csv('Data/sb_prep.csv')
nrow(sb_prep)

# ------------------------------------------------------------------------------
# 3. DERIVE MODELLING DATASET (sb_rich_area)
# ------------------------------------------------------------------------------
# Drop rows with no richness or no sample area, and make sure every grouping/
# random-effect column is a factor before it goes into model predictions.

sb_rich_area <- sb_prep %>%
  filter(!is.na(Total_species),
         !is.na(Centred_log_total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded),
         Habitat_broad = as.factor(Habitat_broad),
         StudyID = as.factor(StudyID),
         RowID = as.factor(RowID),
         Method = as.factor(Method)) %>%
  arrange(Biome)

sb_rich_area %>% select(Realm) %>% distinct()

# ------------------------------------------------------------------------------
# 4. LOAD FITTED MODELS
# ------------------------------------------------------------------------------
# These brms models were fit on the cluster and saved as .Rdata - loaded here
# rather than refit. Loaded with paths relative to the GSB root (set above)
# so the working directory stays put for the rest of the script (figures get
# written relative to that same root later on).
# NOTE: rich_po_alp.Rdata (polar/alpine) is the Tundra model - the object
# inside it is named mod_tund_r, used below.

load('Model_Fits/Habs/rich_aq.Rdata')
load('Model_Fits/Habs/rich_ar.Rdata')
load('Model_Fits/Habs/rich_forest.Rdata')
load('Model_Fits/Habs/rich_grass.Rdata')
load('Model_Fits/Habs/rich_med_de.Rdata')
load('Model_Fits/Habs/rich_po_alp.Rdata')
load('Model_Fits/Habs/rich_wetland.Rdata')

# ------------------------------------------------------------------------------
# 5. PER-REALM PREDICTIONS & DIVERSITY SUMMARIES
# ------------------------------------------------------------------------------
# Same recipe repeated per Realm:
#   a. subset sb_rich_area to that Realm (+ any Realm-specific quirks)
#   b. cross Number_sites x a small range of Total_sample_area_m2, re-derive
#      the centred/logged predictors the model actually uses
#   c. get posterior predicted draws from the fitted model (Number_sites = 1
#      cases only are kept downstream)
#   d. recode Total_sample_area_m2 into "alpha"/"gamma" labels and
#      Habitat_degraded into readable labels, for plotting
#   e. summarise the predicted draws into point estimate + 50/90/95% interval
#      ("*_div" data frames feed the points/error bars; "*_predict_df" feed
#      the ridgeline densities)

# --- 5a. Tundra ---------------------------------------------------------------
# Tundra realm restricted to the "Grassland" broad habitat subset.
sb_tund_r <- sb_rich_area %>% filter(Realm == "Tundra") %>% filter(Habitat_broad == "Grassland")
head(sb_tund_r)

# Quick model diagnostics - safe to skip on reruns.
summary(mod_tund_r)
pp_check(mod_tund_r)
plot(mod_tund_r)
conditional_effects(mod_tund_r)

# NOTE: purrr must not be loaded with its functions masking dplyr/here, and
# Biome must stay a character (not a factor) for crossing()/nest() below.
tund_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_tund_r %>% group_by(Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata = .x, re_formula = NA, probs = c(0.025, 0.975))))

tund_predict_df <- tund_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Tundra", Biome = "Tundra") %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  mutate(Group = "Terrestrial")
head(tund_predict_df)

tund_div <- tund_predict_df %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Terrestrial")
head(tund_div)

# --- 5b. Forest ----------------------------------------------------------------
sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>% ungroup()

forest_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata = .x, re_formula = NA)))

forest_predict_df <- forest_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  mutate(Realm = "Forest") %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Terrestrial")
head(forest_predict_df)

forest_div <- forest_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Terrestrial")
forest_div

# --- 5c. Grassland --------------------------------------------------------------
sb_grass_r <- sb_rich_area %>% filter(Realm == "Grassland") %>% ungroup()

grass_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata = .x, re_formula = NA)))

grass_predict_df <- grass_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  mutate(Realm = "Grassland") %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Terrestrial")
head(grass_predict_df)

grass_div <- grass_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Terrestrial")
grass_div

# --- 5d. Mediterranean and Desert ------------------------------------------------
sb_med_de_r <- sb_rich_area %>% filter(Realm == "Mediterranean and Desert")
head(sb_med_de_r)
summary(mod_med_de_r)

med_de_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata = .x, re_formula = NA)))

med_de_predict_df <- med_de_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")) %>%
  mutate(Realm = "Mediterranean and Desert") %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Terrestrial")

med_de_div <- med_de_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Terrestrial")
med_de_div

# --- 5e. Arable -------------------------------------------------------------------
# Arable has no Habitat_degraded distinction (all arable = degraded), so it's
# left out of the grouping/relevel calls here and hardcoded to "1" below.
sb_ar_r <- sb_rich_area %>% filter(Realm == "Arable") %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))
head(sb_ar_r)
summary(mod_ar_r)

ar_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_ar_r %>% group_by(Biome) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata = .x, re_formula = NA)))

ar_predict_df <- ar_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Realm = "Arable") %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical")) %>%
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `1` = "Degraded habitat")) %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Arable")

ar_div <- ar_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Arable")
ar_div

# --- 5f. Wetland -------------------------------------------------------------------
sb_wetland_r <- sb_rich_area %>% filter(Realm == "Wetland") %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))
nrow(sb_wetland_r)
head(sb_wetland_r)
sb_wetland_r %>% select(Biome) %>% distinct()
summary(mod_wetland_r)

wetland_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata = .x, re_formula = NA)))

wetland_predict_df <- wetland_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Wetland") %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical")) %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Wetlands")

wetland_div <- wetland_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Wetlands")
wetland_div

# --- 5g. Aquatic ------------------------------------------------------------------
sb_aq_r <- sb_rich_area %>% filter(Realm == "Aquatic") %>% ungroup()
head(sb_aq_r)
summary(mod_aq_r)

aq_predict <- tidyr::crossing(
  Number_sites = c(1, 20, 100),
  sb_aq_r %>% group_by(Habitat_degraded) %>%
    # FIXED: dplyr::summarise() now requires exactly one row per group; this
    # was returning 2 (the seq() of length 2), which errors in current dplyr
    # ("must be size 1, not 2"). reframe() is the dplyr-recommended drop-in
    # replacement for the old "multi-row summarise" pattern.
    dplyr::reframe(Total_sample_area_m2 = c(seq(0.010000, 15.000000, length.out = 2)))
) %>%
  mutate(log_number_sites = log(Number_sites),
         log_total_sample_area_m2 = log(Total_sample_area_m2),
         Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
         Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_aq_r, newdata = .x, re_formula = NA)))

aq_predict_df <- aq_predict %>%
  select(-data) %>% unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = "Aquatic", Realm = "Aquatic") %>%
  mutate(Total_sample_area_m2 = as.factor(Total_sample_area_m2)) %>%
  filter(Number_sites == 1) %>%
  mutate(Total_sample_area_m2 = recode_factor(Total_sample_area_m2, `0.01` = "α", `15` = "γ")) %>%
  mutate(Habitat_degraded = recode_factor(Habitat_degraded, `0` = "Undisturbed habitat", `1` = "Degraded habitat")) %>%
  filter(!is.na(Total_sample_area_m2)) %>%
  mutate(Total_sample_area_m2 = factor(Total_sample_area_m2)) %>%
  mutate(Group = "Aquatic")

aq_div <- aq_predict_df %>% filter(Number_sites == 1) %>%
  select(Realm, Biome, Habitat_degraded, Total_sample_area_m2, predicted) %>%
  group_by(Realm, Biome, Habitat_degraded, Total_sample_area_m2) %>%
  mutate(Estimate_ = mean(predicted),
         Lower_95 = quantile(predicted, probs = 0.025),
         Upper_95 = quantile(predicted, probs = 0.975),
         Lower_90 = quantile(predicted, probs = 0.05),
         Upper_90 = quantile(predicted, probs = 0.95),
         Lower_50 = quantile(predicted, probs = 0.25),
         Upper_50 = quantile(predicted, probs = 0.75)) %>%
  select(-predicted) %>% distinct() %>%
  mutate(Group = "Aquatic")
aq_div

# ------------------------------------------------------------------------------
# 6. COMBINE ALL REALMS & DERIVE Realm_Biome
# ------------------------------------------------------------------------------
# Same Realm x Biome -> Realm_Biome collapsing used in the maps script, so the
# legend/category labels stay consistent across figures.

predict_dat <- tund_predict_df %>%
  bind_rows(aq_predict_df, ar_predict_df, wetland_predict_df, med_de_predict_df, grass_predict_df, forest_predict_df) %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Arable",
    Realm == "Arable" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Arable",
    Realm == "Arable" & Biome == "Tropical" ~ "Tropical Arable",
    Realm == "Forest" & Biome == "Temperate" ~ "Temperate Forests",
    Realm == "Forest" & Biome == "Tropical" ~ "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome == "Boreal" ~ "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome == "Tropical" ~ "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Deserts and Xeric Shrublands" ~ "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Mediterranean Forests, Woodlands and Scrub" ~ "Mediterranean Forests, Woodlands & Scrub",
    Realm == "Tundra" ~ Realm,
    Realm == "Wetland" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Wetlands",
    Realm == "Wetland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Wetlands",
    Realm == "Wetland" & Biome == "Tropical" ~ "Tropical Wetlands",
  )) %>%
  mutate(Estimate = predicted)
  # NOTE: an earlier fct_relevel(Realm_Biome, ...) / fct_relevel(Realm, ...) pass
  # used to live here to fix display order, but it's dead code now - ordering
  # is instead handled below by biome_levels + add_y(), so it's been removed.
head(predict_dat)


div_dat <- tund_div %>%
  bind_rows(forest_div, grass_div, med_de_div, ar_div, wetland_div, aq_div) %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Arable",
    Realm == "Arable" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Arable",
    Realm == "Arable" & Biome == "Tropical" ~ "Tropical Arable",
    Realm == "Forest" & Biome == "Temperate" ~ "Temperate Forests",
    Realm == "Forest" & Biome == "Tropical" ~ "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome == "Boreal" ~ "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome == "Tropical" ~ "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Deserts and Xeric Shrublands" ~ "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Mediterranean Forests, Woodlands and Scrub" ~ "Mediterranean Forests, Woodlands & Scrub",
    Realm == "Tundra" ~ Realm,
    Realm == "Wetland" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Wetlands",
    Realm == "Wetland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Wetlands",
    Realm == "Wetland" & Biome == "Tropical" ~ "Tropical Wetlands",
  )) %>%
  mutate(Estimate = Estimate_)
  # NOTE: same as above - the dead fct_relevel(Realm_Biome, ...) / fct_relevel(Realm, ...)
  # block that used to follow here has been removed; biome_levels + add_y() below
  # now handle display order.
head(div_dat)


tdiv_dat <- div_dat %>% filter(Group == "Terrestrial")

# Canonical display order for Realm_Biome across all figures.
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

# Turns Realm_Biome into an ordered factor (reversed, since ggplot/ridges
# plot category 1 at the bottom) and adds the numeric y-position used to
# stack/offset points, error bars, and ridgeline densities in the figures below.
add_y <- function(df) {
  df %>%
    mutate(
      Realm_Biome = factor(Realm_Biome, levels = rev(biome_levels)),
      y_base = as.numeric(Realm_Biome)
    )
}
predict_dat <- add_y(predict_dat)
div_dat     <- add_y(div_dat)
tdiv_dat    <- add_y(tdiv_dat)

# Vertical offsets used throughout the figures below to stack the four
# alpha/gamma x degraded/undisturbed combinations within each Realm_Biome row.
y_offset <- 0.18
y_text_offset <- 0.18
y_text_offset_t <- 0.10
y_offset_1 <- 0.36
y_offset_2 <- 0.18
y_offset_3 <- 0.54

head(div_dat)
div_dat %>% filter(Group == "Terrestrial") %>% ungroup() %>% select(Realm_Biome) %>% distinct()

# ------------------------------------------------------------------------------
# 7. FIGURES
# ------------------------------------------------------------------------------
# Each figure overlays, per Realm_Biome row: alpha (0.01 m2) and gamma (15 m2)
# sample-area density ridges/points/error-bars, split by degraded (grey,
# triangle) vs undisturbed (coloured, circle) habitat where that distinction
# applies.

# --- 7a. Terrestrial (non-arable) ----------------------------------------------
# NOTE: bandwidth = 5 added to every geom_density_ridges() call below (and in
# ar_div_fig/w_div_fig/aq_div_fig) to match the smoother, more consistent ridge
# look used in the density-figures script (Fig_3), which fixes one bandwidth
# across all its panels instead of letting each ridge pick its own via the
# default rule of thumb. 5 is ~3% of this figure's richness range (0-150),
# the same proportion Fig_3 uses (bandwidth 600 over a 0-20000 range) - worth
# a visual sanity check once you can render this, since it's a smoothing
# choice rather than a hard rule.
#
# NOTE: point sizes below were bumped from 2.5 (alpha) / 4 (gamma) to
# 2 (alpha) / 6 (gamma) throughout this script (all four panels + both
# legends) to make the alpha-vs-gamma size distinction read more clearly.
t_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base, height = ..density.., color = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset_1, height = ..density.., color = Realm_Biome), alpha = 0.7, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base + y_offset_2, height = ..density.., color = Realm_Biome, fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset_3, height = ..density.., color = Realm_Biome, fill = Realm_Biome), alpha = 0.7, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  # points
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base), alpha = 0.9, color = "#C0C0C0", shape = 17, size = 2) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset_1), alpha = 0.9, color = "#C0C0C0", shape = 17, size = 6) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base + y_offset_2, color = Realm_Biome), shape = 16, size = 2) +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset_3, color = Realm_Biome), shape = 16, size = 6) +
  # error bars
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base + y_offset_2, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base + y_offset_2, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_3, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_3, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594")) +
  scale_fill_manual(values = c("#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594")) +
  labs(subtitle = "a Terrestrial - Non-Arable",
       x = "Species richness in the soil seedbank",
       y = "Ecosystem") +
  geom_text(data = div_dat %>% filter(Group == "Terrestrial") %>%
              group_by(Realm_Biome) %>%
              summarise(y_base = first(y_base), .groups = "drop") %>%
              mutate(label = case_when(
                Realm_Biome == "Tundra" ~ "Tundra",
                Realm_Biome == "Boreal Forests/Taiga" ~ "Forests \nBoreal",
                Realm_Biome == "Temperate Forests" ~ "Forests \nTemperate",
                Realm_Biome == "Tropical & Subtropical Forests" ~ "Forests \nTropical & Subtropical",
                Realm_Biome == "Temperate & Boreal Grasslands, Savannas & Shrublands" ~ "Grasslands & Savannas \nTemperate & Boreal",
                Realm_Biome == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Grasslands & Savannas \nTropical & Subtropical",
                Realm_Biome == "Deserts & Xeric Shrublands" ~ "Mediterranean & Desert \nDeserts & Xeric Shrublands",
                Realm_Biome == "Mediterranean Forests, Woodlands & Scrub" ~ "Mediterranean & Desert \nForests, Woodlands & Scrub",
                TRUE ~ Realm_Biome
              )) %>%
              # NOTE: this label sat right on top of its ridge's tail at the
              # shared offset, so it gets a taller nudge than the rest.
              mutate(label_offset_t = if_else(Realm_Biome == "Mediterranean Forests, Woodlands & Scrub",
                                               y_text_offset_t + 0.10, y_text_offset_t)),
            aes(y = y_base + label_offset_t, label = label), x = 120, colour = "grey60", vjust = 0, size = 6) +
  theme_bw(base_size = 18) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150)) +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(),
        legend.position = "none")
t_div_fig

# --- 7b. Arable ------------------------------------------------------------------
ar_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base, height = ..density.., color = Realm_Biome, fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset_1, height = ..density.., color = Realm_Biome, fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base, color = Realm_Biome), shape = 15, size = 2) +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset_1, color = Realm_Biome), shape = 15, size = 6) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#AA3929", "#E2C59F", "#99610a"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  scale_fill_manual(values = c("#AA3929", "#E2C59F", "#99610a"),
                     labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  labs(subtitle = "b Terrestrial - Arable",
       x = "Species richness in the soil seedbank",
       y = "Ecosystem") +
  geom_text(data = div_dat %>% filter(Group == "Arable") %>% group_by(Realm_Biome) %>%
              summarise(y_base = first(y_base), .groups = "drop") %>%
              mutate(label = case_when(
                Realm_Biome == "Temperate & Boreal Arable" ~ "Arable \nTemperate & Boreal",
                Realm_Biome == "Mediterranean & Desert Arable" ~ "Arable \nMediterranean & Desert",
                Realm_Biome == "Tropical Arable" ~ "Arable \nTropical",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset, label = label), x = 55, colour = "grey60", vjust = 0, size = 6) +
  theme_bw(base_size = 18) +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(),
        legend.position = "none")
ar_div_fig

# --- 7c. Wetlands / Transitional --------------------------------------------------
div_dat %>% filter(Group == "Wetlands") %>% distinct(Realm_Biome)

w_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base, height = ..density.., color = Realm_Biome),
                      alpha = 0.7, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset_1, height = ..density.., color = Realm_Biome), alpha = 0.7, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", fill = "#C0C0C0", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base + y_offset_2, height = ..density.., color = Realm_Biome, fill = Realm_Biome),
                      alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset_3, height = ..density.., color = Realm_Biome, fill = Realm_Biome), alpha = 0.35, scale = 0.9, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base), alpha = 0.9, color = "#C0C0C0", shape = 17, size = 2) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset_1), color = "#C0C0C0", shape = 17, size = 6) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base + y_offset_2, color = Realm_Biome), shape = 16, size = 2) +
  geom_point(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset_3, color = Realm_Biome), shape = 16, size = 6) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_1, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base + y_offset_2, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base + y_offset_2, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_3, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset_3, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  scale_color_manual(values = c("#293352", "#4E84C4", "#20B2AA")) +
  scale_fill_manual(values = c("#293352", "#4E84C4", "#20B2AA")) +
  labs(subtitle = "c Transitional",
       x = "Species richness",
       y = "Ecosystem") +
  geom_text(data = div_dat %>% filter(Group == "Wetlands") %>% group_by(Realm_Biome) %>%
              summarise(y_base = first(y_base), .groups = "drop") %>%
              mutate(label = case_when(
                Realm_Biome == "Temperate & Boreal Wetlands" ~ "Wetlands \nTemperate & Boreal",
                Realm_Biome == "Mediterranean & Desert Wetlands" ~ "Wetlands \nMediterranean & Desert",
                Realm_Biome == "Tropical Wetlands" ~ "Wetlands \nTropical",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset, label = label), x = 75, colour = "grey60", vjust = 0, size = 6) +
  theme_bw(base_size = 18) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 100)) +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(),
        legend.position = "none")
w_div_fig

# --- 7d. Aquatic -------------------------------------------------------------------
# Aquatic uses Habitat_degraded itself (not Realm_Biome) as the y-axis
# category, since there's only one biome ("Aquatic") to plot.
hab_levels <- c("Degraded habitat", "Undisturbed habitat")
add_y_h <- function(df) {
  df %>%
    mutate(Habitat_degraded = factor(Habitat_degraded, levels = rev(hab_levels)),
           y_base = as.numeric(Habitat_degraded))
}
predict_dat_a <- predict_dat %>% filter(Group == "Aquatic")
div_dat_a <- div_dat %>% filter(Group == "Aquatic")
predict_dat_a <- add_y_h(predict_dat_a)
div_dat_a     <- add_y_h(div_dat_a)

aq_div_fig <- ggplot() +
  geom_density_ridges(data = predict_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base, height = ..density..), color = "#C0C0C0", fill = "#C0C0C0",
                      alpha = 0.35, scale = 1.6, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset, height = ..density..), color = "#C0C0C0", fill = "#C0C0C0",
                      alpha = 0.7, scale = 1.6, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                      aes(x = Estimate, y = y_base, height = ..density..), color = "#447fdd", fill = "#447fdd",
                      alpha = 0.35, scale = 1.6, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_density_ridges(data = predict_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                      aes(x = Estimate, y = y_base + y_offset, height = ..density..), color = "#447fdd", fill = "#447fdd",
                      alpha = 0.7, scale = 1.6, rel_min_height = 0.001, stat = "density_ridges", position = "identity", bandwidth = 5) +
  geom_point(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = y_base, color = Habitat_degraded, shape = Habitat_degraded), alpha = 0.9, size = 2) +
  geom_point(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = y_base + y_offset, color = Habitat_degraded, shape = Habitat_degraded), alpha = 0.9, size = 6) +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset, xmin = Lower_90, xmax = Upper_90), height = 0.15, linewidth = 0.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Degraded habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset, xmin = Lower_50, xmax = Upper_50), height = 0.15, linewidth = 1.5, colour = "#C0C0C0") +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  # FIXED: this filter was previously `Habitat_degraded == "Undisturbed ha bitat"`
  # (stray space) - it matched zero rows, so this error bar silently never drew.
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset, xmin = Lower_90, xmax = Upper_90, color = Realm_Biome), height = 0.15, linewidth = 0.5) +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "α"),
                  aes(y = y_base, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  geom_errorbarh(data = div_dat_a %>% filter(Group == "Aquatic") %>% filter(Habitat_degraded == "Undisturbed habitat") %>% filter(Total_sample_area_m2 == "γ"),
                  aes(y = y_base + y_offset, xmin = Lower_50, xmax = Upper_50, color = Realm_Biome), height = 0.15, linewidth = 1.5) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_color_manual(values = c("#447fdd", "#C0C0C0", "#447fdd", "#447fdd")) +
  scale_fill_manual(values = c("#447fdd", "#C0C0C0")) +
  scale_shape_manual(values = c(16, 17), labels = c("Undisturbed", "Degraded")) +
  labs(subtitle = "d Aquatic",
       x = "",
       y = "State") +
  theme_bw(base_size = 18) +
  geom_text(data = div_dat_a %>% filter(Group == "Aquatic") %>% group_by(Habitat_degraded) %>%
              summarise(y_base = first(y_base), .groups = "drop"),
            aes(y = y_base + y_text_offset_t, label = Habitat_degraded), x = 60, colour = "grey60", vjust = 0, size = 6) +
  theme(plot.subtitle = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(),
        legend.position = "none")
aq_div_fig

# ------------------------------------------------------------------------------
# 8. LEGEND-EXTRACTION PLOTS
# ------------------------------------------------------------------------------
# Two throwaway plots whose only purpose is to generate a shape legend for
# the gamma-richness (legend_g) and alpha-richness (legend_a) point styles
# (Undisturbed/Degraded/Arable), pulled out below via g_legend() and stacked
# under the combined figure instead of repeating a legend on every panel.
#
# NOTE: color was previously a fixed "grey" on all three shape-mapped layers
# below, which made every legend key (Undisturbed/Degraded/Arable) render
# grey regardless of its shape - inconsistent with the rest of the figures,
# where grey is reserved specifically for "Degraded". Fixed here: the
# Aquatic-data layer (-> "Undisturbed habitat" key) and the Terrestrial-data
# layer (-> "Arable" key) are now black; the Arable-data layer (-> "Degraded
# habitat" key) stays grey. This mapping depends on the default alphabetical
# factor order of Group ("Aquatic" < "Arable" < "Terrestrial"), which is what
# scale_shape_manual()'s values/labels below are already keyed to.

legend_g <- ggplot() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = div_dat %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Habitat_degraded), alpha = 0.9, size = 6, color = "black") +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Habitat_degraded), alpha = 0.9, size = 6, color = "black") +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Group), alpha = 0.9, size = 6, color = "grey") +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = Habitat_degraded), shape = 15, alpha = 0.9, size = 6) +
  scale_fill_manual(values = c("#d8b847", "#b38711")) +
  coord_cartesian(ylim = c(0, 90)) +
  labs(x = '', y = '', shape = (expression(paste(italic(gamma), '-richness 15 m-2', sep = ''))),
       subtitle = "c) Grasslands") +
  scale_shape_manual(labels = c("Undisturbed habitat", "Degraded habitat", "Arable"), values = c(16, 17, 15)) +
  guides(shape = guide_legend(override.aes = list(size = 6))) +
  theme_bw(base_size = 18) +
  theme(plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(), legend.position = "bottom")
legend_g

legend_a <- ggplot() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = div_dat %>% filter(Group == "Aquatic") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Habitat_degraded), alpha = 0.9, size = 2, color = "black") +
  geom_point(data = div_dat %>% filter(Group == "Terrestrial") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Habitat_degraded), alpha = 0.9, size = 2, color = "black") +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "α"),
             aes(x = Estimate, y = Habitat_degraded, shape = Group, group = Group), alpha = 0.9, size = 2, color = "grey") +
  geom_point(data = div_dat %>% filter(Group == "Arable") %>% filter(Total_sample_area_m2 == "γ"),
             aes(x = Estimate, y = Habitat_degraded), shape = 15, alpha = 0.9, size = 2) +
  scale_fill_manual(values = c("#d8b847", "#b38711")) +
  coord_cartesian(ylim = c(0, 90)) +
  labs(x = '', y = '', shape = (expression(paste(italic(alpha), '-richness 0.01 m-2', sep = ''))),
       subtitle = "c) Grasslands") +
  scale_shape_manual(labels = c("Undisturbed habitat", "Degraded habitat", "Arable"), values = c(16, 17, 15)) +
  guides(shape = guide_legend(override.aes = list(size = 2))) +
  theme_bw(base_size = 18) +
  theme(plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.background = element_blank(), legend.position = "bottom",
        legend.spacing.x = unit(1, 'cm'))
legend_a

# Pull just the legend grob out of a ggplot object.
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend_g <- g_legend(legend_g)
legend_a <- g_legend(legend_a)

# ------------------------------------------------------------------------------
# 9. ASSEMBLE FINAL FIGURE & EXPORT
# ------------------------------------------------------------------------------
# richness_fig = the bottom row: b (Arable), c (Wetlands), d (Aquatic) panels
# side by side, with the two extracted legends stacked underneath.

low_row <- (ar_div_fig + w_div_fig + aq_div_fig)
richness_fig <- (low_row / legend_a / legend_g) + plot_layout(heights = c(10, 1, 1))
richness_fig

# Fig_2 = t_div_fig ("a", Terrestrial - Non-Arable) as a single full-width
# panel centred on top, with richness_fig (the b/c/d row + legends) as the
# bottom block. t_div_fig gets a bit more relative height since it has many
# more biome rows than any individual panel in the row below.
Fig_2 <- (t_div_fig / richness_fig) + plot_layout(heights = c(1.2, 1))
Fig_2

# Save figures as .png in Figures/ (created if it doesn't exist yet).

ggsave("Figures/Fig_2_2.png", plot = Fig_2, width = 20, height = 18, units = "in", dpi = 300)
