# ==============================================================================
# Seed Bank Database — Figure 4: Joint Richness x Density by Realm
# ==============================================================================
# For each realm (Tundra, Forest, Grassland, Mediterranean & Desert, Arable,
# Wetland, Aquatic), plots posterior mean species richness (x-axis) against
# posterior mean seed density (y-axis), one point per Biome x Habitat_degraded
# combination, with 50%/90% credible-interval error bars in both directions.
# Panels are combined into a single multi-panel figure (joint_fig) with one
# shared "State" shape legend, mirroring the panel-grid + shared-legend
# pattern used in Fig_2 and Fig_3.
# Table_S5
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

# ------------------------------------------------------------------------------
# 2. SETUP & DATA LOADING
# ------------------------------------------------------------------------------
setwd('~/Dropbox/GSB/')
sb_prep <- read.csv('Data/sb_prep.csv')
nrow(sb_prep)

sb_rich_area <- sb_prep %>%
  filter(
    !is.na(Total_species),
    !is.na(Centred_log_total_sample_area_m2)
  ) %>%
  mutate(
    Habitat_degraded = as.factor(Habitat_degraded),
    Habitat_broad = as.factor(Habitat_broad),
    StudyID = as.factor(StudyID),
    RowID = as.factor(RowID),
    Method = as.factor(Method)
  ) %>%
  arrange(Biome)

# ------------------------------------------------------------------------------
# 3. LOAD MODEL FITS (richness + density, all realms)
# ------------------------------------------------------------------------------
# FIXED: previously setwd() into Model_Fits/Habs/ to load these (twice, once
# for the richness models and once for the density models), which left every
# later relative path (e.g. the write.csv() calls near the bottom) pointed at
# the wrong folder. Loading with relative paths instead keeps the working
# directory at ~/Dropbox/GSB/ for the rest of the script.
load('Model_Fits/Habs/rich_aq.Rdata')
load('Model_Fits/Habs/rich_ar.Rdata')
load('Model_Fits/Habs/rich_forest.Rdata')
load('Model_Fits/Habs/rich_grass.Rdata')
load('Model_Fits/Habs/rich_med_de.Rdata')
load('Model_Fits/Habs/rich_po_alp.Rdata')
load('Model_Fits/Habs/rich_wetland.Rdata')

load('Model_Fits/Habs/den_aq.Rdata')
load('Model_Fits/Habs/den_ar.Rdata')
load('Model_Fits/Habs/den_forest.Rdata')
load('Model_Fits/Habs/den_grass.Rdata')
load('Model_Fits/Habs/den_med_de.Rdata')
load('Model_Fits/Habs/den_po_alp.Rdata')
load('Model_Fits/Habs/den_wetland.Rdata')

# ------------------------------------------------------------------------------
# 4. TUNDRA (panel a)
# ------------------------------------------------------------------------------
sb_tund_r <- sb_rich_area %>% filter(Realm == "Tundra") %>% ungroup()
head(sb_tund_r)
summary(mod_tund_r)

# make sure purrr is not loaded under a masking name, and Biome is a
# character, NOT a factor, before this runs.
tund_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_tund_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata = .x, re_formula = NA)))
head(tund_r1)

tund_r1_df <- tund_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Tundra", Biome = "Tundra") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(tund_r1_df)

tund_d_90 <- conditional_effects(mod_tund_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
tund_d_50 <- conditional_effects(mod_tund_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
tund_d_df_90 <- as.data.frame(tund_d_90$`Habitat_degraded`)
tund_d_df_50 <- as.data.frame(tund_d_50$`Habitat_degraded`)

tund_d_90_ce <- tund_d_df_90 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Tundra", Biome = "Tundra",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

tund_d_50_ce <- tund_d_df_50 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Tundra", Biome = "Tundra",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

tund_joint <- tund_d_90_ce %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(
    tund_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  left_join(tund_r1_df)
tund_joint

fig_tund_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = tund_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = tund_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = tund_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = tund_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = tund_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#94b594")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = "",
    subtitle = "a Tundra"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_tund_joint

# ------------------------------------------------------------------------------
# 5. FOREST (panel b)
# ------------------------------------------------------------------------------
sb_forest_r <- sb_rich_area %>% filter(Realm == "Forest") %>% ungroup()
head(sb_forest_r)
summary(mod_forest_r)

forest_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata = .x, re_formula = NA)))
head(forest_r1)

forest_r1_df <- forest_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Forest") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(forest_r1_df)

forest_d_90 <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
forest_d_50 <- conditional_effects(mod_forest_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
forest_d_df_90 <- as.data.frame(forest_d_90$`Biome:Habitat_degraded`)
forest_d_df_50 <- as.data.frame(forest_d_50$`Biome:Habitat_degraded`)

forest_d_90_ce <- forest_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Forest",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

forest_d_50_ce <- forest_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Forest",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

# FIXED: removed a stray bare `forest_d_ce` line that referenced an object
# never defined under that name (the real tables are forest_d_90_ce and
# forest_d_50_ce, built above) - this would error if the script were run
# top to bottom.

forest_joint <- forest_d_90_ce %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(
    forest_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  left_join(forest_r1_df)
forest_joint

fig_forest_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = forest_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = forest_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = forest_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = forest_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = forest_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#1e3d14", "#788f33", "#228B22"),
                      labels = c("Boreal", "Temperate", "Tropical & \nSubtropical")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = expression(paste('Average species richness (', m^2, ')', sep = '')),
    subtitle = "b Forests"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_forest_joint

# ------------------------------------------------------------------------------
# 6. GRASSLAND (panel c)
# ------------------------------------------------------------------------------
sb_grass_r <- sb_rich_area %>% filter(Realm == "Grassland") %>% ungroup()
head(sb_grass_r)
summary(mod_grass_r)

grass_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata = .x, re_formula = NA)))
head(grass_r1)

grass_r1_df <- grass_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Grasslands") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(grass_r1_df)

grass_d_90 <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
grass_d_50 <- conditional_effects(mod_grass_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
grass_d_df_90 <- as.data.frame(grass_d_90$`Biome:Habitat_degraded`)
grass_d_df_50 <- as.data.frame(grass_d_50$`Biome:Habitat_degraded`)

grass_d_90_ce <- grass_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Grassland",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

grass_d_50_ce <- grass_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Grassland",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

grass_joint <- grass_d_90_ce %>%
  select(-Realm) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(
    grass_d_50_ce %>%
      select(-Realm) %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  left_join(grass_r1_df)
grass_joint

fig_grass_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = grass_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = grass_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = grass_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = grass_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = grass_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#d8b847", "#b38711"),
                      labels = c("Temperate & \nBoreal", "Tropical & \nSubtropical")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = expression(paste('Average species richness (', m^2, ')', sep = '')),
    subtitle = "c Grasslands & Savannas"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_grass_joint

# ------------------------------------------------------------------------------
# 7. MEDITERRANEAN & DESERT (panel d)
# ------------------------------------------------------------------------------
sb_med_de_r <- sb_rich_area %>%
  filter(Realm == "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))
head(sb_med_de_r)
summary(mod_med_de_r)

med_de_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata = .x, re_formula = NA)))
head(med_de_r1)

med_de_r1_df <- med_de_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")) %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(med_de_r1_df)

med_de_d_90 <- conditional_effects(mod_med_de_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
med_de_d_50 <- conditional_effects(mod_med_de_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
med_de_d_df_90 <- as.data.frame(med_de_d_90$`Biome:Habitat_degraded`)
med_de_d_df_50 <- as.data.frame(med_de_d_50$`Biome:Habitat_degraded`)

med_de_d_90_ce <- med_de_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Mediterranean and Desert",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

med_de_d_50_ce <- med_de_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Mediterranean and Desert",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))

med_de_joint <- med_de_d_90_ce %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(
    med_de_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  left_join(med_de_r1_df) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))
med_de_joint

fig_med_de_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = med_de_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = med_de_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = med_de_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = med_de_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = med_de_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  labs(
    y = "Seed density m-2",
    x = expression(paste('Average species richness (', m^2, ')', sep = '')),
    subtitle = "d Mediterranean & Desert"
  ) +
  scale_color_manual(values = c("#da7901", "#fab255"),
                      labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands")) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_med_de_joint

# ------------------------------------------------------------------------------
# 8. ARABLE (panel e)
# ------------------------------------------------------------------------------
sb_ar_r <- sb_rich_area %>%
  filter(Realm == "Arable") %>%
  mutate(Biome = case_when(
    grepl("Deserts", Biome) ~ "Mediterranean and Desert",
    grepl("Temperate", Biome) ~ "Temperate and Boreal",
    grepl("Boreal", Biome) ~ "Temperate and Boreal",
    grepl("Mediterranean", Biome) ~ "Mediterranean and Desert",
    TRUE ~ Biome
  ))
head(sb_ar_r)
summary(mod_ar_r)

ar_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_ar_r %>% group_by(Biome) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata = .x, re_formula = NA)))
head(ar_r1)

ar_r1_df <- ar_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  dplyr::group_by(Biome) %>%
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(ar_r1_df)

arable_d_90 <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted', prob = 0.90)
arable_d_50 <- conditional_effects(mod_ar_d, effects = 'Biome', re_formula = NA, method = 'fitted', prob = 0.50)
arable_d_df_90 <- as.data.frame(arable_d_90$Biome)
arable_d_df_50 <- as.data.frame(arable_d_50$Biome)

arable_d_90_ce <- arable_d_df_90 %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Arable", Habitat_degraded = "1",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

arable_d_50_ce <- arable_d_df_50 %>%
  select(Biome, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Arable", Habitat_degraded = "1",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

ar_joint <- arable_d_90_ce %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  left_join(
    arable_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(ar_r1_df) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))
ar_joint

fig_ar_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = ar_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome), size = 5, shape = 15) +
  geom_errorbar(data = ar_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = ar_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = ar_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = ar_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#99610a", "#E2C59F", "#AA3929"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = expression(paste('Average species richness (', m^2, ')', sep = '')),
    subtitle = "e Arable"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_ar_joint

# ------------------------------------------------------------------------------
# 9. WETLANDS (panel f)
# ------------------------------------------------------------------------------
sb_wetland_r <- sb_rich_area %>%
  filter(Realm == "Wetland") %>%
  mutate(Biome = case_when(
    grepl("Deserts", Biome) ~ "Mediterranean and Desert",
    grepl("Temperate", Biome) ~ "Temperate and Boreal",
    grepl("Boreal", Biome) ~ "Temperate and Boreal",
    grepl("Mediterranean", Biome) ~ "Mediterranean and Desert",
    TRUE ~ Biome
  ))
head(sb_wetland_r)
summary(mod_wetland_r)

wetland_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata = .x, re_formula = NA)))
head(wetland_r1)

wetland_r1_df <- wetland_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Wetland") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(wetland_r1_df)

wetland_d_90 <- conditional_effects(mod_wetland_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
wetland_d_50 <- conditional_effects(mod_wetland_d, effects = 'Biome:Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
wetland_d_df_90 <- as.data.frame(wetland_d_90$`Biome:Habitat_degraded`)
wetland_d_df_50 <- as.data.frame(wetland_d_50$`Biome:Habitat_degraded`)

wetland_d_50_ce <- wetland_d_df_50 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Wetland",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

wetland_d_90_ce <- wetland_d_df_90 %>%
  select(Biome, Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Wetland",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  arrange(Biome, desc(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))

wetland_joint <- wetland_d_90_ce %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  left_join(
    wetland_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(wetland_r1_df) %>%
  mutate(Biome = fct_relevel(Biome, "Temperate and Boreal", "Mediterranean and Desert", "Tropical"))
wetland_joint

fig_wetland_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = wetland_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = wetland_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = wetland_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = wetland_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = wetland_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#20B2AA", "#4E84C4", "#293352"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = "Species richness m-2",
    subtitle = "f Wetlands & Flooded Grasslands"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_wetland_joint

# ------------------------------------------------------------------------------
# 10. AQUATIC (panel g)
# ------------------------------------------------------------------------------
sb_aq_r <- sb_rich_area %>% filter(Realm == "Aquatic") %>% ungroup()
head(sb_aq_r)
summary(mod_aq_r)

aq_r1 <- tidyr::crossing(
  Number_sites = c(1),
  sb_aq_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(1.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_aq_r, newdata = .x, re_formula = NA)))
head(aq_r1)

aq_r1_df <- aq_r1 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Aquatic", Biome = "Aquatic") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()
head(aq_r1_df)

aq_d_90 <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.90)
aq_d_50 <- conditional_effects(mod_aq_d, effects = 'Habitat_degraded', re_formula = NA, method = 'fitted', prob = 0.50)
aq_d_df_90 <- as.data.frame(aq_d_90$Habitat_degraded)
aq_d_df_50 <- as.data.frame(aq_d_50$Habitat_degraded)

aq_d_90_ce <- aq_d_df_90 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Aquatic", Biome = "Aquatic",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

aq_d_50_ce <- aq_d_df_50 %>%
  select(Habitat_degraded, estimate__, lower__, upper__) %>%
  mutate(
    Realm = "Aquatic", Biome = "Aquatic",
    Estimate = round(estimate__, 2),
    Lower_CI = round(lower__, 2),
    Upper_CI = round(upper__, 2)
  ) %>%
  select(Realm, Biome, Habitat_degraded, Estimate, Lower_CI, Upper_CI) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

aq_joint <- aq_d_90_ce %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(
    d_Estimate = Estimate,
    d_Upper_90_CI = Upper_CI,
    d_Lower_90_CI = Lower_CI
  ) %>%
  left_join(
    aq_d_50_ce %>%
      mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
      mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
      mutate(
        d_Estimate = Estimate,
        d_Upper_50_CI = Upper_CI,
        d_Lower_50_CI = Lower_CI
      ) %>%
      select(-c(Estimate, Upper_CI, Lower_CI))
  ) %>%
  select(-c(Estimate, Upper_CI, Lower_CI)) %>%
  left_join(aq_r1_df)
aq_joint

fig_aq_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = aq_joint,
             aes(x = r_Estimate, y = d_Estimate, colour = Biome, shape = Habitat_degraded), size = 5) +
  geom_errorbar(data = aq_joint,
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI, colour = Biome), width = 0) +
  geom_errorbarh(data = aq_joint,
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI, colour = Biome), height = 0) +
  geom_errorbar(data = aq_joint,
                aes(x = r_Estimate, ymin = d_Lower_50_CI, ymax = d_Upper_50_CI, colour = Biome), size = 2, width = 0) +
  geom_errorbarh(data = aq_joint,
                 aes(y = d_Estimate, xmin = r_Lower_50_CI, xmax = r_Upper_50_CI, colour = Biome), size = 2, height = 0) +
  scale_color_manual(values = c("#447fdd")) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = "Species richness m-2",
    subtitle = "g Aquatic"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = ""), shape = "none")
fig_aq_joint

# ------------------------------------------------------------------------------
# 11. SHARED LEGEND
# ------------------------------------------------------------------------------
# fig_legend_joint is a throwaway scaffold plot - only its guide box is kept
# (via g_legend() below), so its axis labels/theme never actually get
# displayed. Built only to extract one shared "State" shape legend used
# across all seven panels above.
#
# NOTE: color was previously a single fixed "grey" applied to one geom_point
# layer covering all three Biome levels used here (Boreal/Temperate/
# Tropical, borrowed from forest_joint purely as a convenient 3-level
# factor), which made every legend key (Undisturbed/Degraded/Arable) render
# grey regardless of shape - inconsistent with the rest of the figures,
# where grey is reserved specifically for "Degraded". Fixed here: the
# Boreal-data layer (-> "Undisturbed habitat" key) and the Tropical-data
# layer (-> "Arable" key) are now black; the Temperate-data layer (->
# "Degraded habitat" key) stays grey. This mapping depends on the default
# alphabetical factor order of Biome ("Boreal" < "Temperate" < "Tropical"),
# which is what scale_shape_manual()'s values/labels below are already
# keyed to.
fig_legend_joint <- ggplot() +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_point(data = forest_joint %>% filter(Biome == "Boreal"),
             aes(x = r_Estimate, y = d_Estimate, shape = Biome), size = 3, color = "black") +
  geom_point(data = forest_joint %>% filter(Biome == "Temperate"),
             aes(x = r_Estimate, y = d_Estimate, shape = Biome), size = 3, color = "grey") +
  geom_point(data = forest_joint %>% filter(Biome == "Tropical"),
             aes(x = r_Estimate, y = d_Estimate, shape = Biome), size = 3, color = "black") +
  geom_errorbar(data = forest_joint %>% filter(Biome == "Tropical"),
                aes(x = r_Estimate, ymin = d_Lower_90_CI, ymax = d_Upper_90_CI)) +
  geom_errorbarh(data = forest_joint %>% filter(Biome == "Tropical"),
                 aes(y = d_Estimate, xmin = r_Lower_90_CI, xmax = r_Upper_90_CI)) +
  labs(
    y = expression(paste('Average seed density (', m^2, ')')),
    x = expression(paste('Average species richness (', m^2, ')', sep = '')),
    subtitle = "b) Forests", shape = "State"
  ) +
  scale_shape_manual(labels = c("Undisturbed habitat", "Degraded habitat", "Arable"), values = c(16, 17, 15)) +
  theme_classic(base_size = 16) +
  theme(
    plot.subtitle = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour = "black", fill = "white"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(title = "Biome", ncol = 3))
fig_legend_joint

# Extract just the legend grob from the scaffold plot.
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
fig_legend_joint <- g_legend(fig_legend_joint)

# ------------------------------------------------------------------------------
# 12. ASSEMBLE FINAL FIGURE & EXPORT
# ------------------------------------------------------------------------------
Fig_4 <- (fig_tund_joint + fig_forest_joint + fig_grass_joint) /
  (fig_med_de_joint + fig_ar_joint) /
  (fig_wetland_joint + fig_aq_joint) /
  (fig_legend_joint) +
  plot_layout(heights = c(10, 10, 10, 1))
Fig_4

ggsave("Figures/Fig_4.png", plot = Fig_4, width = 16, height = 16, units = "in", dpi = 300)

# ------------------------------------------------------------------------------
# 13. SUMMARY TABLES & EXPORT
# ------------------------------------------------------------------------------
table_joint <- tund_joint %>%
  bind_rows(forest_joint, grass_joint, med_de_joint, ar_joint, wetland_joint, aq_joint)
head(table_joint)
print(table_joint, n = Inf)

# FIXED: was a hardcoded absolute path ("~/Dropbox/GSB/Data/..."); now
# relative to the setwd() set in section 2.
write.csv(table_joint, "Data/joint.csv")

# ------------------------------------------------------------------------------
# TABLE S5 (continued) - "0.01" and "15" m2 columns (table_s5)
# ------------------------------------------------------------------------------
# Builds the same per-realm richness-prediction pipeline used above for the
# "1" m2 column (the "_r1"/"_r1_df" blocks, e.g. tund_r1/tund_r1_df), just
# evaluated at Total_sample_area_m2 = 0.01 and 15 instead of 1. Reuses the
# same richness models (mod_*_r, already loaded above) and the same
# per-realm filtered datasets (sb_tund_r, sb_forest_r, etc., already built
# above for the "1" m2 blocks) - this is NOT new modelling, just the existing
# prediction pipeline run at two more sample-area values. LOW RISK: unlike
# Table S4, this reuses code/models already proven to run in this script (the
# "_r1" blocks above) - only the seq(...) value changes.

# ============================== "0.01" m2 ==============================
# --- Tundra (panel a) - 0.01 m2 ---
tund_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_tund_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata = .x, re_formula = NA)))

tund_r0_01_df <- tund_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Tundra", Biome = "Tundra") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Forest (panel b) - 0.01 m2 ---
forest_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata = .x, re_formula = NA)))

forest_r0_01_df <- forest_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Forest") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Grassland (panel c) - 0.01 m2 ---
grass_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata = .x, re_formula = NA)))

grass_r0_01_df <- grass_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Grasslands") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Mediterranean & Desert (panel d) - 0.01 m2 ---
med_de_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata = .x, re_formula = NA)))

med_de_r0_01_df <- med_de_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")) %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Arable (panel e) - 0.01 m2 ---
ar_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_ar_r %>% group_by(Biome) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata = .x, re_formula = NA)))

ar_r0_01_df <- ar_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  dplyr::group_by(Biome) %>%
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Wetland (panel f) - 0.01 m2 ---
wetland_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata = .x, re_formula = NA)))

wetland_r0_01_df <- wetland_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Wetland") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Aquatic (panel g) - 0.01 m2 ---
aq_r0_01 <- tidyr::crossing(
  Number_sites = c(1),
  sb_aq_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(0.01, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_aq_r, newdata = .x, re_formula = NA)))

aq_r0_01_df <- aq_r0_01 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Aquatic", Biome = "Aquatic") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# ============================== "15" m2 ==============================
# --- Tundra (panel a) - 15 m2 ---
tund_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_tund_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_tund_r, newdata = .x, re_formula = NA)))

tund_r15_df <- tund_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Tundra", Biome = "Tundra") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Forest (panel b) - 15 m2 ---
forest_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata = .x, re_formula = NA)))

forest_r15_df <- forest_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Forest") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Grassland (panel c) - 15 m2 ---
grass_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_grass_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_grass_r, newdata = .x, re_formula = NA)))

grass_r15_df <- grass_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Grasslands") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Mediterranean & Desert (panel d) - 15 m2 ---
med_de_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_med_de_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_med_de_r, newdata = .x, re_formula = NA)))

med_de_r15_df <- med_de_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Mediterranean and Desert") %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands")) %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Arable (panel e) - 15 m2 ---
ar_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_ar_r %>% group_by(Biome) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_ar_r, newdata = .x, re_formula = NA)))

ar_r15_df <- ar_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  dplyr::group_by(Biome) %>%
  mutate(Realm = "Arable", Habitat_degraded = "1") %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Wetland (panel f) - 15 m2 ---
wetland_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_wetland_r %>% group_by(Biome, Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome) %>%
  nest(data = c(Habitat_degraded, Biome, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_wetland_r, newdata = .x, re_formula = NA)))

wetland_r15_df <- wetland_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Wetland") %>%
  dplyr::group_by(Biome, Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Aquatic (panel g) - 15 m2 ---
aq_r15 <- tidyr::crossing(
  Number_sites = c(1),
  sb_aq_r %>% group_by(Habitat_degraded) %>%
    dplyr::summarise(Total_sample_area_m2 = c(seq(15.000000, length.out = 1)))
) %>%
  mutate(
    log_number_sites = log(Number_sites),
    log_total_sample_area_m2 = log(Total_sample_area_m2),
    Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE)
  ) %>%
  select(-c(log_number_sites, log_total_sample_area_m2)) %>%
  arrange(Total_sample_area_m2, Number_sites) %>%
  mutate(Habitat_degraded_group = Habitat_degraded) %>%
  group_by(Habitat_degraded_group, Habitat_degraded) %>%
  nest(data = c(Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_aq_r, newdata = .x, re_formula = NA)))

aq_r15_df <- aq_r15 %>%
  select(-data) %>%
  unnest(cols = c(predicted)) %>%
  mutate(predicted = .prediction) %>%
  select(-c(.prediction, .draw, .row, .chain, .iteration)) %>%
  ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1")) %>%
  mutate(Realm = "Aquatic", Biome = "Aquatic") %>%
  dplyr::group_by(Habitat_degraded) %>%
  mutate(
    r_Estimate = round(mean(predicted, na.rm = TRUE), 0),
    r_Upper_90_CI = quantile(predicted, probs = 0.95, na.rm = TRUE),
    r_Lower_90_CI = quantile(predicted, probs = 0.05, na.rm = TRUE),
    r_Upper_50_CI = quantile(predicted, probs = 0.25, na.rm = TRUE),
    r_Lower_50_CI = quantile(predicted, probs = 0.75, na.rm = TRUE)
  ) %>%
  select(c(Realm, Biome, Habitat_degraded, r_Estimate, r_Upper_90_CI, r_Lower_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  distinct() %>%
  ungroup()

# --- Combine all realms for each sample area into one table per area ---
table_s5_001 <- tund_r0_01_df %>%
  bind_rows(forest_r0_01_df, grass_r0_01_df, med_de_r0_01_df, ar_r0_01_df, wetland_r0_01_df, aq_r0_01_df) %>%
  mutate(
    Estimate = round(r_Estimate, 0),
    Lower_90 = round(r_Lower_90_CI, 0),
    Upper_90 = round(r_Upper_90_CI, 0),
    Lower_50 = round(r_Lower_50_CI, 0),
    Upper_50 = round(r_Upper_50_CI, 0)
  ) %>%
  unite("90_CI", Lower_90:Upper_90, sep = ",") %>%
  unite("50_CI", Lower_50:Upper_50, sep = ",") %>%
  select(-c(r_Estimate, r_Lower_90_CI, r_Upper_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  mutate(CI = paste0("(", `50_CI`, " , ", `90_CI`, ")")) %>%
  select(-c(`90_CI`, `50_CI`)) %>%
  unite("0.01", Estimate:CI, sep = " ")
print(table_s5_001, n = Inf)

table_s5_15 <- tund_r15_df %>%
  bind_rows(forest_r15_df, grass_r15_df, med_de_r15_df, ar_r15_df, wetland_r15_df, aq_r15_df) %>%
  mutate(
    Estimate = round(r_Estimate, 0),
    Lower_90 = round(r_Lower_90_CI, 0),
    Upper_90 = round(r_Upper_90_CI, 0),
    Lower_50 = round(r_Lower_50_CI, 0),
    Upper_50 = round(r_Upper_50_CI, 0)
  ) %>%
  unite("90_CI", Lower_90:Upper_90, sep = ",") %>%
  unite("50_CI", Lower_50:Upper_50, sep = ",") %>%
  select(-c(r_Estimate, r_Lower_90_CI, r_Upper_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  mutate(CI = paste0("(", `50_CI`, " , ", `90_CI`, ")")) %>%
  select(-c(`90_CI`, `50_CI`)) %>%
  unite("15", Estimate:CI, sep = " ")
print(table_s5_15, n = Inf)

# `table_s5` - joins the "0.01" and "15" columns on Realm/Biome/Habitat_degraded,
# matching the keys table_s5c (the "1" column, built further below) joins on.
table_s5 <- table_s5_001 %>%
  left_join(table_s5_15, by = c("Realm", "Biome", "Habitat_degraded"))
print(table_s5, n = Inf)

table_s5c <- tund_r1_df %>%
  bind_rows(forest_r1_df, grass_r1_df, med_de_r1_df, ar_r1_df, wetland_r1_df, aq_r1_df) %>%
  mutate(
    Estimate = round(r_Estimate, 0),
    Lower_90 = round(r_Lower_90_CI, 0),
    Upper_90 = round(r_Upper_90_CI, 0),
    Lower_50 = round(r_Lower_50_CI, 0),
    Upper_50 = round(r_Upper_50_CI, 0)
  ) %>%
  unite("90_CI", Lower_90:Upper_90, sep = ",") %>%
  unite("50_CI", Lower_50:Upper_50, sep = ",") %>%
  select(-c(r_Estimate, r_Lower_90_CI, r_Upper_90_CI, r_Upper_50_CI, r_Lower_50_CI)) %>%
  mutate(CI = paste0("(", `50_CI`, " , ", `90_CI`, ")")) %>%
  select(-c(`90_CI`, `50_CI`)) %>%
  unite("1", Estimate:CI, sep = " ")
print(table_s5c, n = Inf)

# table_s5c above is only the "1" m2 column of TABLE S5 (Realm/Biome/
# Habitat_degraded/0.01/1/15). `table_s5` (the "0.01" and "15" m2 columns,
# joined below) is now built above - see the "TABLE S5 (continued)" block.
#
# `table_s5` does NOT exist anywhere in the originally provided scripts -
# it has been newly constructed here by replicating the exact "_r1"/"_r1_df"
# prediction pipeline already used above for the "1" m2 column, just
# evaluated at Total_sample_area_m2 = 0.01 and 15 instead of 1, reusing the
# same already-loaded richness models (mod_*_r) and per-realm datasets. This
# is low-risk relative to other inferred work in this project, since it does
# not invent any new modelling logic - it only reruns proven, working code
# at two more input values. Still worth spot-checking the resulting numbers
# against the published Table S5 once run.
table_s5 <- table_s5 %>%
  left_join(table_s5c) %>%
  mutate(Realm = as.factor(Realm)) %>%
  mutate(Realm = fct_relevel(Realm, "Tundra", "Forest", "Grasslands", "Mediterranean and Desert",
                              "Arable", "Wetland", "Aquatic")) %>%
  arrange(Realm) %>%
  ungroup() %>%
  select(Realm, Biome, Habitat_degraded, `0.01`, `1`, `15`)
print(table_s5, n = Inf)

# FIXED: was a hardcoded absolute path ("~/Dropbox/GSB/Data/..."); now
# relative to the setwd() set in section 2.
write.csv(table_s5, "Data/Table_S5.csv")
