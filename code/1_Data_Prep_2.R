# ==============================================================================
# Seed bank database (sb_pub.csv) - data preparation and summary tables
#
# What this script does:
#   1. Loads the raw seed bank database and drops records with no usable
#      response metric.
#   2. Derives the transformed/centred variables used in downstream models
#      (logs, ratios, sample area/volume conversions, centring).
#   3. Reclassifies Habitat/Biome/Biome_zone combinations into the
#      Realm/Biome scheme used in the paper, and writes the prepared
#      dataset ("sb_prep.csv").
#   4. Reloads the prepared dataset and produces the summary numbers used
#      in Table 1, Table S1, Table S2, Table S3, and the methods-section
#      min/max examples (with CSV exports for each).
#

# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. LOAD RAW DATA
# ------------------------------------------------------------------------------
# Path is hardcoded here rather than switched on username - simplest option
# for a single-user script. Update this path if the project folder moves.

sb <- read.csv('~/Dropbox/GSB/Data paper stuff/Database checking/sb_pub.csv')

# Drop any record that has no value for ALL four response metrics
# (a record only needs one of these to be usable).
sb <- sb[!is.na(sb$Total_seeds) | !is.na(sb$Total_species) |
           !is.na(sb$Seed_density_m2) | !is.na(sb$Seed_density_litre), ]

head(sb)
colnames(sb)
summary(sb)

# Table S1 numbers, before any derived variables are added
nrow(sb) # n records

nrow(sb %>% select(StudyID) %>% distinct())                  # n studies
nrow(sb %>% select(StudyID, Lat_deg, Lon_deg) %>% distinct()) # n unique locations (study x lat/lon)

head(sb)

# ------------------------------------------------------------------------------
# 2. DERIVE METRICS (sb_calc)
# ------------------------------------------------------------------------------
# Adds logged/ratio/area-volume/centred versions of the raw variables that
# are used as predictors and responses in the models.

sb_calc <- sb %>%
  mutate(
    log_total_seeds      = log(Total_seeds),
    log_total_species    = log(Total_species),
    ratio_seeds_species  = Total_seeds / Total_species,

    # Total sampling effort = number of samples x size of each sample
    Total_sample_volume_mm3 = Total_number_samples * Sample_volume_mm3,
    Total_sample_area_mm2   = Total_number_samples * Sample_area_mm2,

    log_total_number_samples = log(Total_number_samples),
    log_number_sites         = log(Number_sites),

    # Convert sampling effort from mm to standard m units
    Total_sample_area_m2   = Total_sample_area_mm2 / 1000000,
    Total_sample_volume_m3 = Total_sample_volume_mm3 / 1000000000,
    log_total_sample_area_m2 = log(Total_sample_area_m2),

    # Mean-centred versions (used as model predictors)
    Centred_total_number_samples     = Total_number_samples - mean(Total_number_samples, na.rm = TRUE),
    Centred_number_sites             = Number_sites - mean(Number_sites, na.rm = TRUE),
    Centred_log_total_number_samples = log_total_number_samples - mean(log_total_number_samples, na.rm = TRUE),
    Centred_log_number_sites         = log_number_sites - mean(log_number_sites, na.rm = TRUE),
    Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE),
  )

head(sb_calc)
summary(sb_calc)

# Sanity check: what Biome_zone / Biome_broad / Habitat_broad combinations exist?
sb_calc %>%
  select(Biome_zone, Biome_broad, Habitat_broad) %>%
  distinct() %>%
  arrange(Habitat_broad, Biome_broad, Biome_zone)

# Sanity check: confirm which biomes/zones show up under "Grassland"
sb_calc %>%
  filter(Habitat_broad == "Grassland") %>%
  select(Habitat_broad, Biome_zone, Biome_broad) %>%
  distinct()

# ------------------------------------------------------------------------------
# 3. RECLASSIFY HABITAT/BIOME INTO Realm + Biome (sb_mod)
# ------------------------------------------------------------------------------
# The raw WWF biome/habitat fields don't line up cleanly with the
# Realm x Biome grouping used in the paper, so we rebuild it here in stages:
#   a) Biome_broad_hab: Arable/Aquatic habitats override the underlying biome
#   b) Biome: collapse fine-grained biomes into the broader categories used
#      in the paper (Desert, Mediterranean, Tropical, Temperate, etc.)
#   c) Realm: assign each record to a top-level Realm (Forest/Grassland/
#      Wetland/Arable/Aquatic/Tundra/Mediterranean and Desert)
#   d) A few Realm-specific overrides to merge Temperate + Boreal, and
#      Desert + Mediterranean, biomes within Arable/Wetland/Grassland realms

sb_mod <- sb_calc %>%
  # (a) Arable/Aquatic habitat overrides whatever biome was recorded
  mutate(Biome_broad_hab = case_when(
    Habitat_broad %in% c("Arable", "Aquatic") ~ Habitat_broad,
    TRUE ~ Biome_broad
  )) %>%
  mutate(Lat_deg_abs = abs(Lat_deg)) %>%
  # (b) First pass at collapsing biomes into broader categories
  mutate(Biome = case_when(
    grepl("Deserts", Biome_broad_hab) ~ "Deserts and Xeric Shrublands",
    grepl("Mediterranean", Biome_broad_hab) ~ "Mediterranean Forests, Woodlands and Scrub",
    grepl("Tropical", Biome_broad) ~ "Tropical",
    Biome_zone == "Mediterranean and Desert" & grepl("Montane", Biome_broad_hab) ~ "Mediterranean Forests, Woodlands and Scrub",
    Habitat_broad == "Grassland" & Biome_zone == "Boreal" ~ "Temperate",
    TRUE ~ Biome_zone
  )) %>%
  # second pass: catch tropical biomes sitting within the Mediterranean/Desert zone
  mutate(Biome = case_when(
    Biome_zone == "Mediterranean and Desert" & grepl("Tropical", Biome_broad_hab) ~ "Mediterranean Forests, Woodlands and Scrub",
    TRUE ~ Biome
  )) %>%
  # (c) Assign top-level Realm
  mutate(Realm = case_when(
    Habitat_broad == "Forest" & Biome_zone == "Mediterranean and Desert" ~ Biome_zone,
    Habitat_broad == "Grassland" & Biome == "Tropical" ~ "Grassland",
    Habitat_broad == "Grassland" & Biome_zone == "Mediterranean and Desert" ~ Biome_zone,
    Biome_zone == "Tundra" ~ Biome_zone,
    Habitat_broad == "Grassland" & Biome_zone == "Boreal" ~ Habitat_broad,
    TRUE ~ Habitat_broad
  )) %>%
  # (d) Realm-specific biome merges: Temperate+Boreal and Desert+Mediterranean
  mutate(Biome = case_when(
    Realm == "Arable" & grepl("Deserts", Biome) ~ "Mediterranean and Desert",
    Realm == "Arable" & grepl("Temperate", Biome) ~ "Temperate and Boreal",
    Realm == "Arable" & grepl("Boreal", Biome) ~ "Temperate and Boreal",
    Realm == "Arable" & grepl("Mediterranean", Biome) ~ "Mediterranean and Desert",
    TRUE ~ Biome
  )) %>%
  mutate(Biome = case_when(
    Realm == "Wetland" & grepl("Deserts", Biome) ~ "Mediterranean and Desert",
    Realm == "Wetland" & grepl("Temperate", Biome) ~ "Temperate and Boreal",
    Realm == "Wetland" & grepl("Boreal", Biome) ~ "Temperate and Boreal",
    Realm == "Wetland" & grepl("Mediterranean", Biome) ~ "Mediterranean and Desert",
    TRUE ~ Biome
  )) %>%
  mutate(Biome = case_when(
    Realm == "Grassland" & grepl("Temperate", Biome) ~ "Temperate and Boreal",
    Realm == "Grassland" & grepl("Boreal", Biome) ~ "Temperate and Boreal",
    TRUE ~ Biome
  )) %>%
  # FIXED: this arrange() was not piped into the chain above (no leading
  # %>%), so it ran as its own standalone statement - `Realm`/`Biome` aren't
  # objects in the global environment, so that would error as soon as it ran.
  arrange(Realm, Biome)

head(sb_mod)

# QA checks on the Realm/Biome mapping produced above
sb_mod %>% select(Realm, Biome, Habitat_broad, Biome_zone, Biome_broad_hab) %>%
  distinct() %>% arrange(Realm, Biome, Habitat_broad, Biome_zone)

sb_mod %>% select(Realm, Biome) %>% distinct() %>% arrange(Realm, Biome)

sb_mod %>% select(Biome_broad, Habitat_broad) %>% distinct()

sb_mod %>% select(Habitat_broad, Biome_zone, Biome_broad) %>%
  distinct() %>% arrange(Habitat_broad, Biome_zone, Biome_broad)

# QA check: inspect Forest records that ended up classified as "Boreal"
sb_mod %>%
  filter(Realm == "Forest") %>%
  select(Habitat_broad, Biome_zone, Biome_broad_hab, Biome_broad, Realm, Biome, Habitat_degraded, Total_species) %>%
  arrange(Habitat_broad, Biome_zone, Biome_broad_hab, Biome_broad, Realm, Biome, Habitat_degraded, Total_species) %>%
  arrange(Realm, Biome, Habitat_degraded, Total_species) %>%
  filter(Biome == "Boreal")

colnames(sb_mod)

# Pull out aquatic study details for manual review/citation
aq <- sb_mod %>%
  filter(Biome_broad_hab == "Aquatic") %>%
  select(StudyID, Title, Doi, Country, Realm, Biome, Biome_broad) %>%
  distinct()
aq
write.csv(aq, "Data/aqua_deets.csv")

# Write the fully processed/classified dataset for re-use below and in
# downstream modelling scripts
write.csv(sb_mod, "Data/sb_prep.csv")

# ------------------------------------------------------------------------------
# 4. RELOAD PREPARED DATA
# ------------------------------------------------------------------------------
# Reload from disk (rather than continuing to use sb_mod in memory) so the
# summaries below reflect exactly what's in the saved file.

sb_prep <- read.csv('Data/sb_prep.csv')
head(sb_prep)
colnames(sb_prep)
summary(sb_prep)

# Table S1 numbers, recomputed on the prepared dataset
nrow(sb_prep) # n records
nrow(sb_prep %>% select(StudyID) %>% distinct()) # n studies

# FLAG: this references the original raw `sb` object, not `sb_prep` -
# likely leftover from copy/pasting the block above.
head(sb)

# QA: biome/habitat combinations and degradation status, excluding Arable/Aquatic
sb_prep %>%
  select(Biome_broad, Habitat_broad, Biome_broad_hab, Habitat_degraded, StudyID, RowID) %>%
  distinct() %>%
  arrange(Biome_broad, Habitat_broad, Biome_broad_hab, Habitat_degraded, StudyID, RowID)

sb_prep %>%
  select(Biome_zone, Biome_broad, Habitat_broad, Biome_broad_hab) %>%
  distinct() %>%
  arrange(Biome_zone, Biome_broad, Habitat_broad, Biome_broad_hab) %>%
  filter(!Biome_broad_hab == "Arable") %>%
  filter(!Biome_broad_hab == "Aquatic")

# ------------------------------------------------------------------------------
# 5. TABLE S1 - observation counts by metric (Realm/Biome grouping)
# ------------------------------------------------------------------------------
# Reshape to long format: one row per record x metric, for the four response
# metrics (Total_seeds, Total_species, Seed_density_m2, ratio_seeds_species).

sb_gathered <- sb_prep %>%
  select(RowID, StudyID, Centred_log_total_sample_area_m2,
         Realm, Biome, Number_sites, Total_seeds, Total_species,
         Seed_density_m2, ratio_seeds_species) %>%
  gather(metric, response, Total_seeds:ratio_seeds_species) %>%
  filter(!is.na(response))

head(sb_gathered)

# Table S1 observation counts
nrow(sb_gathered %>% filter(!metric == "ratio_seeds_species")) # richness + abundance + density combined
nrow(sb_gathered %>% filter(metric == "Total_species"))
nrow(sb_gathered %>% filter(metric == "Total_seeds"))
nrow(sb_gathered %>% filter(metric == "Seed_density_m2"))
nrow(sb_gathered %>% filter(metric == "ratio_seeds_species"))

# Number of observations actually usable in each model (after dropping rows
# missing the sample-area predictor, or with a response of zero)
nrow(sb_gathered %>% filter(metric == "Total_species")   %>% filter(!is.na(Centred_log_total_sample_area_m2)))
nrow(sb_gathered %>% filter(metric == "Total_seeds")     %>% filter(!is.na(Centred_log_total_sample_area_m2)))
nrow(sb_gathered %>% filter(metric == "Seed_density_m2") %>% filter(response != 0))
nrow(sb_gathered %>% filter(metric == "ratio_seeds_species") %>% filter(response != 0))

nrow(sb_gathered %>% filter(response == 0))
# 15/8087 records have a response of zero
round((15 / 8087) * 100, 2) # 0.19% of data are zeros

# Distribution of records by number of sites sampled (excludes the ratio metric,
# which is study- rather than site-level)
sites_count <- sb_gathered %>%
  filter(!metric == "ratio_seeds_species") %>%
  select(Number_sites) %>%
  dplyr::group_by(Number_sites) %>%
  count()
head(sites_count) # e.g. 4763 observations within 1 site

head(sb_prep)

# ------------------------------------------------------------------------------
# 6. TABLE S2 - record counts by WWF biome
# ------------------------------------------------------------------------------

sb_biomes <- sb_prep %>%
  filter(Biome_broad_hab != "Aquatic", Biome_broad_hab != "Arable") %>%
  select(Biome_WWF, Biome_WWF_Broad) %>%
  dplyr::group_by(Biome_WWF, Biome_WWF_Broad) %>%
  count() %>%
  mutate(Biome_WWF_Broad = as.factor(Biome_WWF_Broad)) %>%
  mutate(Biome_WWF_Broad = fct_relevel(Biome_WWF_Broad, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests", "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands"
  ))) %>%
  arrange(Biome_WWF_Broad)

print(sb_biomes)
write.csv(sb_biomes, "Data/sb_biomes.csv")

head(sb_gathered)
head(sb_prep)

# ------------------------------------------------------------------------------
# 7. TABLE S3 - record counts by Biome_broad_hab, split by metric type
# ------------------------------------------------------------------------------
# Re-gather using Biome_broad_hab (rather than Realm/Biome) as the grouping
# variable for this table.

sb_gathered <- sb_prep %>%
  select(RowID, StudyID, Centred_log_total_sample_area_m2,
         Biome_broad_hab, Number_sites, Total_seeds, Total_species,
         Seed_density_m2, ratio_seeds_species) %>%
  gather(metric, response, Total_seeds:ratio_seeds_species) %>%
  filter(!is.na(response))

head(sb_gathered)

# Columns 1-2 of Table S3: metrics that need the sample-area predictor
# (richness, abundance)
biome_count_ss <- sb_gathered %>%
  filter(!is.na(Centred_log_total_sample_area_m2)) %>%
  select(Biome_broad_hab, metric) %>%
  filter(metric %in% c("Total_seeds", "Total_species")) %>%
  dplyr::group_by(Biome_broad_hab, metric) %>%
  count() %>%
  spread(metric, n) %>%
  mutate(Biome_broad_hab = as.factor(Biome_broad_hab)) %>%
  mutate(Biome_broad_hab = fct_relevel(Biome_broad_hab, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests", "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
    "Aquatic", "Arable"
  ))) %>%
  arrange(Biome_broad_hab)

biome_count_ss
write.csv(biome_count_ss, "Data/biome_count_ss.csv")

# Columns 3-4 of Table S3: metrics that don't need sample area, with zero
# responses excluded (density, seeds:species ratio)
biome_count_dr <- sb_gathered %>%
  filter(response != 0) %>%
  select(Biome_broad_hab, metric) %>%
  filter(metric %in% c("Seed_density_m2", "ratio_seeds_species")) %>%
  dplyr::group_by(Biome_broad_hab, metric) %>%
  count() %>%
  spread(metric, n) %>%
  mutate(Biome_broad_hab = as.factor(Biome_broad_hab)) %>%
  mutate(Biome_broad_hab = fct_relevel(Biome_broad_hab, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests", "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
    "Aquatic", "Arable"
  ))) %>%
  arrange(Biome_broad_hab)

biome_count_dr
write.csv(biome_count_dr, "Data/biome_count_dr.csv")

# Number of records per "number of sites" category, for the richness model
sb_gathered %>%
  filter(metric == "Total_species", !is.na(Centred_log_total_sample_area_m2)) %>%
  mutate(N_site_cats = case_when(
    Number_sites == 1 ~ "1 site",
    Number_sites >= 2 & Number_sites <= 20 ~ "2-20 sites",
    Number_sites >= 21 & Number_sites <= 100 ~ "21-100 sites",
    TRUE ~ "> 100 sites"
  )) %>%
  select(N_site_cats) %>%
  dplyr::group_by(N_site_cats) %>%
  count()

# Same, for the seed abundance model (uses slightly different site-count bins)
sb_gathered %>%
  filter(metric == "Total_seeds", !is.na(Centred_log_total_sample_area_m2)) %>%
  mutate(N_site_cats = case_when(
    Number_sites >= 1 & Number_sites <= 19 ~ "1-20 site",
    Number_sites >= 20 & Number_sites <= 99 ~ "20-99 sites",
    Number_sites >= 100 ~ "100 and more sites",
    TRUE ~ "Other"
  )) %>%
  select(N_site_cats) %>%
  dplyr::group_by(N_site_cats) %>%
  count()

# Turn off scientific notation so large sample counts print in full below
options(scipen = 999)
colnames(sb_prep)
head(sb_prep)

# ------------------------------------------------------------------------------
# 8. TABLE 1 - observation counts by ecosystem (Realm/Biome/Ecoregion/Degraded)
# ------------------------------------------------------------------------------
# Reproduces the manuscript's Table 1: for each ecosystem (a Biome x
# Ecoregion x Degraded combination), the number of species-richness and
# seed-density observations, rolled up into the three top-level Realms used
# in the text (Terrestrial / Transitional / Aquatic), plus a totals row.
#
# Naming note: this script's `Realm` column (Forest/Grassland/Tundra/
# Mediterranean and Desert/Arable/Wetland/Aquatic) is what the manuscript
# calls "Biome", and this script's `Biome` column (Boreal/Temperate/Tropical/
# Temperate and Boreal/etc.) is what the manuscript calls "Ecoregion" - both
# are relabelled below to match Table 1 as published.

table_1_base <- sb_prep %>%
  mutate(
    # Manuscript's top-level "Realm": Wetland records are "Transitional",
    # Aquatic records stay "Aquatic", everything else is "Terrestrial"
    Realm_ms = case_when(
      Realm == "Wetland" ~ "Transitional",
      Realm == "Aquatic" ~ "Aquatic",
      TRUE ~ "Terrestrial"
    ),
    # Manuscript's "Degraded" column: Arable has no undisturbed/degraded
    # distinction in the database, so it's left blank (NA) instead of Yes/No
    Degraded_ms = case_when(
      Realm == "Arable" ~ NA_character_,
      Habitat_degraded == 1 ~ "Yes",
      Habitat_degraded == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    # Relabel this script's Realm values to the wording used in Table 1
    Biome_ms = case_when(
      Realm == "Grassland" ~ "Grasslands and Savannas",
      Realm == "Wetland" ~ "Wetlands and Flooded Grasslands",
      TRUE ~ Realm
    )
  )

# Aquatic is reported as a single Biome-level row in Table 1 (no Ecoregion
# split), so it's grouped separately below without `Biome` (this script's
# Ecoregion proxy) in the group_by, and Ecoregion is hardcoded to "Aquatic"
# afterwards. Every other Realm keeps the full Realm/Biome/Ecoregion/Degraded
# split.
table_1_terrestrial <- table_1_base %>%
  filter(Realm_ms != "Aquatic") %>%
  group_by(Realm_ms, Biome_ms, Biome, Degraded_ms) %>%
  summarise(
    `Nobs species richness` = sum(!is.na(Total_species)),
    `Nobs seed density`     = sum(!is.na(Seed_density_m2)),
    .groups = "drop"
  ) %>%
  rename(Realm = Realm_ms, Biome = Biome_ms, Ecoregion = Biome, Degraded = Degraded_ms)

table_1_aquatic <- table_1_base %>%
  filter(Realm_ms == "Aquatic") %>%
  group_by(Realm_ms, Biome_ms, Degraded_ms) %>%
  summarise(
    `Nobs species richness` = sum(!is.na(Total_species)),
    `Nobs seed density`     = sum(!is.na(Seed_density_m2)),
    .groups = "drop"
  ) %>%
  rename(Realm = Realm_ms, Biome = Biome_ms, Degraded = Degraded_ms) %>%
  mutate(Ecoregion = "Aquatic") %>%
  select(Realm, Biome, Ecoregion, Degraded, `Nobs species richness`, `Nobs seed density`)

table_1 <- bind_rows(table_1_terrestrial, table_1_aquatic) %>%
  mutate(
    # Custom order for Realm/Biome to match the published table; Ecoregion
    # and Degraded are left as plain character columns since their published
    # order is just alphabetical (e.g. "Deserts..." before "Mediterranean...",
    # "No" before "Yes")
    Realm = factor(Realm, levels = c("Terrestrial", "Transitional", "Aquatic")),
    Biome = factor(Biome, levels = c(
      "Tundra", "Forest", "Grasslands and Savannas",
      "Mediterranean and Desert", "Arable",
      "Wetlands and Flooded Grasslands", "Aquatic"
    ))
  ) %>%
  arrange(Realm, Biome, Ecoregion, Degraded)

print(table_1, n = Inf)

# Append the "Total number of observations" row at the bottom of the table,
# matching the published layout
table_1_totals <- tibble(
  Realm = "Total number of observations", Biome = NA, Ecoregion = NA, Degraded = NA,
  `Nobs species richness` = sum(table_1$`Nobs species richness`),
  `Nobs seed density`     = sum(table_1$`Nobs seed density`)
)

table_1_full <- bind_rows(
  table_1 %>% mutate(across(c(Realm, Biome), as.character)),
  table_1_totals
)
table_1_full

# Sanity check against the published table: should be 28 rows (27 ecosystem
# rows + 1 totals row), with totals of 2872 species-richness observations
# and 2630 seed-density observations
nrow(table_1_full)
sum(table_1$`Nobs species richness`)
sum(table_1$`Nobs seed density`)

write.csv(table_1_full, "Data/Table_1.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 9. METHODS-SECTION MIN/MAX EXAMPLES (sb_deets / sb_minmax / responses)
# ------------------------------------------------------------------------------
# Goal: find the min and max value of each key variable, together with the
# country/study/record it came from, so we can cite real examples in the
# methods section (e.g. "the largest study sampled X sites in Y country").

study_refs <- sb %>% select(RowID, StudyID, Authors, Year, Title, Journal, Doi, URL) %>% distinct()

sb_deets <- sb_prep %>%
  group_by(Biome_broad_hab, Country, StudyID, RowID) %>%
  summarise(
    `min-Total_number_samples` = min(as.numeric(Total_number_samples), na.rm = TRUE),
    `max-Total_number_samples` = max(as.numeric(Total_number_samples), na.rm = TRUE),
    `min-Number_sites` = min(as.numeric(Number_sites), na.rm = TRUE),
    `max-Number_sites` = max(as.numeric(Number_sites), na.rm = TRUE),
    `min-Sample_area_mm2` = min(as.numeric(Sample_area_mm2), na.rm = TRUE),
    `max-Sample_area_mm2` = max(as.numeric(Sample_area_mm2), na.rm = TRUE),
    `max-ratio_seeds_species` = max(as.numeric(ratio_seeds_species), na.rm = TRUE),
    `min-ratio_seeds_species` = min(as.numeric(ratio_seeds_species), na.rm = TRUE),
    # FIXED: these three referenced Total_Sample_Area_m2/Sample_Volume_mm3/
    # Total_Sample_Volume_m3, but sb_calc actually created
    # Total_sample_area_m2/Sample_volume_mm3/Total_sample_volume_m3 (lowercase
    # "sample"/"volume") - corrected to match the real column names.
    `max-Total_Sample_Area_m2` = max(as.numeric(Total_sample_area_m2), na.rm = TRUE),
    `min-Total_Sample_Area_m2` = min(as.numeric(Total_sample_area_m2), na.rm = TRUE),
    `min-Sample_Volume_mm3` = min(as.numeric(Sample_volume_mm3), na.rm = TRUE),
    `max-Sample_Volume_mm3` = max(as.numeric(Sample_volume_mm3), na.rm = TRUE),
    `min-Total_Sample_Volume_m3` = min(as.numeric(Total_sample_volume_m3), na.rm = TRUE),
    `max-Total_Sample_Volume_m3` = max(as.numeric(Total_sample_volume_m3), na.rm = TRUE),
    `min-Total_species` = min(as.numeric(Total_species), na.rm = TRUE),
    `max-Total_species` = max(as.numeric(Total_species), na.rm = TRUE),
    `min-Seed_density_m2` = min(as.numeric(Seed_density_m2), na.rm = TRUE),
    `max-Seed_density_m2` = max(as.numeric(Seed_density_m2), na.rm = TRUE),
    `min-Total_seeds` = min(as.numeric(Total_seeds), na.rm = TRUE),
    `max-Total_seeds` = max(as.numeric(Total_seeds), na.rm = TRUE),
  ) %>%
  # Reshape min-X/max-X columns into long format, then split "min-X" into
  # separate minmax/name columns
  pivot_longer(`min-Total_number_samples`:`max-Total_seeds`) %>%
  separate(name, into = c("minmax", "name"), sep = "-") %>%
  ungroup() %>%
  filter(!is.infinite(value))

View(sb_deets)

# Records where the minimum value of a variable is zero (e.g. studies that
# recorded zero seeds/species) - 15 such rows
# FLAG: study_refs is not defined anywhere in this script; it must be
# created/loaded elsewhere before this line will run.
sb_zero <- sb_deets %>%
  filter(minmax == "min") %>%
  filter(value == 0) %>%
  left_join(study_refs)
sb_zero
nrow(sb_zero)
View(sb_zero)

# For each variable, find the record(s) holding the global max, and the
# global min excluding zero (since zero mins are handled separately above).
# Then count how many records tie for that min/max per variable.
sb_minmax <- bind_rows(
  sb_max <- sb_deets %>%
    filter(minmax == "max") %>%
    group_by(name) %>%
    filter(value == max(value)),

  sb_min <- sb_deets %>%
    filter(minmax == "min") %>%
    filter(value > 0) %>%
    group_by(name) %>%
    filter(value == min(value))
) %>%
  arrange(name, minmax, value) %>%
  group_by(name) %>%
  left_join(
    bind_rows(
      sb_max %>% group_by(name, minmax) %>% count(),
      sb_min %>% group_by(name, minmax) %>% count()
    )
  )

View(sb_minmax)
min_max <- sb_minmax
print(min_max)
View(min_max)
head(min_max)
colnames(sb_prep)
min_max %>% select(name) %>% distinct()

# Pull the full record details back in for the variables we want to cite as
# examples in the methods section
# FLAG: study_refs is still undefined anywhere in this script - must be
# created/loaded elsewhere before this line will run.
responses <- sb_minmax %>%
  filter(name %in% c("Seed_density_m2", "Total_seeds", "Total_species",
                      "ratio_seeds_species", "Total_Sample_Area_m2",
                      "Total_number_samples", "Number_sites")) %>%
  left_join(sb_prep) %>%
  # FIXED: Total_Sample_Area_m2/Total_Sample_Volume_m3 -> Total_sample_area_m2/
  # Total_sample_volume_m3, to match the columns actually in sb_prep.
  select(Biome_broad_hab, Country, StudyID, RowID, name, minmax, value, n,
         Total_species, Seed_density_m2, Total_seeds, ratio_seeds_species,
         Total_number_samples, Number_sites, Total_sample_area_m2, Total_sample_volume_m3) %>%
  left_join(study_refs)

colnames(responses)
View(responses)

# Variables where more than one record ties for the min/max (n > 1) need a
# judgement call on which example to cite
studies_mmin_multi <- responses %>% filter(n > 1)
View(studies_mmin_multi)

# Variables with a single, unambiguous min/max record
min_max_count <- responses %>% filter(n == 1)
View(min_max_count)

# Look up one specific study's full record for write-up
View(sb_prep %>% filter(StudyID == "H059"))

# FLAG: both lines write the same object (sb_deets) - likely meant to write
# two different summaries (e.g. sb_deets vs sb_minmax/responses).
write.csv(sb_deets, "Data/sb_details_summary.csv")
write.csv(sb_deets, "Data/sb_details_biome_summary.csv")
