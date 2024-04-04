

library(brms)
library("googlesheets4")
library(tidyverse)
library("ggplot2")
library("sf")                 
library("rnaturalearth")
library("rnaturalearthdata")
library(viridis)
library(MetBrewer)
library(hrbrthemes)
library(mapdata)
library(ggrepel)
library(sp) # For converting to decimal degrees
library(maps)
library(mapproj)
library(ggmap)
library(rworldmap)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'Data/sb_prep.csv')) %>%
  filter(!is.na(Total_species),
         # !Total_species == 0,
         !is.na(Centred_log_total_sample_area_m2) #,
         #Number_Sites == 1 
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_broad_hab = as.factor(Biome_broad_hab),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome_broad_hab)

# !!!!! Relevel !!!!!
sb$Habitat_degraded <- relevel(sb$Habitat_degraded, ref = "1")

head(sb)

nrow(sb %>% filter(Habitat_degraded == 0)) # 2105 rows
nrow(sb %>% filter(Habitat_degraded == 1)) # 991 rows
is.character(sb$Habitat_degraded)

sb %>% select(Biome_broad_hab) %>% distinct()

# Biome_broad_hab
# 1                                                       Aquatic
# 2                                                        Arable
# 3                                          Boreal Forests/Taiga
# 4                                  Deserts and Xeric Shrublands
# 5                    Mediterranean Forests, Woodlands and Scrub
# 6                             Montane Grasslands and Shrublands
# 7                         Temperate Broadleaf and Mixed Forests
# 8                                     Temperate Conifer Forests
# 9                 Temperate Grasslands, Savannas and Shrublands
# 10                             Tropical and Subtropical Forests
# 11 Tropical and Subtropical Grasslands, Savannas and Shrublands
# 12                                                       Tundra

  
sb %>% select(Biome_broad_hab, Habitat_degraded) %>% distinct() %>% arrange(Biome_broad_hab, Habitat_degraded)

print(sb %>%  arrange(Biome_broad_hab, Habitat_degraded) %>% group_by(Biome_broad_hab, Habitat_degraded) %>% summarise(n = n()), n=Inf)

# ~ 25 studies with method nested in study
print(sb %>% select(StudyID, Method) %>% distinct() %>% group_by(StudyID) %>% filter(n() > 1), n=Inf)

# Aquatic
sb_aq_r <- sb %>% filter(Biome_broad_hab == "Aquatic") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome_broad) ~ "Arid",
                           grepl("Temperate", Biome_broad) ~ "Temperate",
                           grepl("Mediterranean", Biome_broad) ~ "Mediterranean",
                           grepl("Tropical", Biome_broad, ignore.case = TRUE) ~"Tropical"))

sb_aq_r %>% select(Biome_broad, Biome_broad_hab, Biome) %>% distinct()

mod_aq_r <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Biome * Habitat_degraded  + Centred_log_number_sites + ( 1  | StudyID/RowID ),
               family = poisson(), data = sb_aq_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
           prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
              control = list(adapt_delta = 0.99999,
                    max_treedepth = 13)
)

summary(mod_aq_r)
pp_check(mod_aq_r)
conditional_effects(mod_aq_r)

setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# # # save data objects to avoid doing this every time
save(mod_aq_r, file = 'mod_aq_r.Rdata')


#Arable
sb_ar_r <- sb %>% filter(Biome_broad_hab == "Arable") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome_broad) ~ "Arid",
                           grepl("Temperate", Biome_broad) ~ "Temperate",
                           grepl("Mediterranean", Biome_broad) ~ "Mediterranean",
                           grepl("Boreal", Biome_broad) ~ "Boreal",
                           grepl("Montane", Biome_broad) ~ "Tropical",
                           grepl("Tropical", Biome_broad, ignore.case = TRUE) ~"Tropical"))

View(sb_ar_r %>% select(Biome_broad, Biome_broad_hab, Biome, Biome_zone) %>% distinct())


mod_ar_r <- brm(Total_species ~ Centred_log_total_sample_area_m2  * Biome + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                family = poisson(), data = sb_ar_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                # prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                # control = list(adapt_delta = 0.99999,
                #                max_treedepth = 13)
)

summary(mod_ar_r)
pp_check(mod_ar_r)
conditional_effects(mod_ar_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_ar_r, file = 'mod_ar_r.Rdata')

# Boreal Forests/Taiga
sb_bo_r <- sb %>% filter(Biome_zone == "Boreal")

sb_bo_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

mod_bo_r <- brm(Total_species ~ Centred_log_total_sample_area_m2  * Habitat_broad * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                family = poisson(), data = sb_bo_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_bo_r)
pp_check(mod_bo_r)
conditional_effects(mod_bo_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_bo_r, file = 'mod_bo_r.Rdata')

# Deserts and Xeric Shrublands
sb_de_r <- sb %>% filter(Biome_broad_hab == "Deserts and Xeric Shrublands")

sb_de_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

mod_de_r <- brm(Total_species ~ Centred_log_total_sample_area_m2   *  Habitat_broad * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                family = poisson(), data = sb_de_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_de_r)
pp_check(mod_de_r)
conditional_effects(mod_de_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_de_r, file = 'mod_de_r.Rdata')


# Mediterranean Forests, Woodlands and Scrub
sb_med_r <- sb %>% filter(Biome_broad_hab == "Mediterranean Forests, Woodlands and Scrub")

sb_med_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

mod_med_r <- brm(Total_species ~ Centred_log_total_sample_area_m2  * Habitat_broad * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                family = poisson(), data = sb_med_r, cores = 4, chains = 4, iter = 8000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_med_r)
pp_check(mod_med_r)
conditional_effects(mod_med_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_med_r, file = 'mod_med_r.Rdata')



# Temperate 
sb_temp_r <- sb %>% filter(Biome_zone == "Temperate") %>%
  filter(!Biome_broad_hab == "Arable") %>% filter(!Biome_broad_hab == "Aquatic") 

sb_temp_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

mod_temp_r <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Habitat_broad  * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                family = poisson(), data = sb_temp_r, cores = 4, chains = 4, iter = 8000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_temp_r)
pp_check(mod_temp_r)
conditional_effects(mod_temp_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tbroad_r, file = 'mod_tbroad_r.Rdata')



# Tropical 
sb_trop_r <- sb %>% filter(Biome_zone == "Tropical")  %>%
  filter(!Biome_broad_hab == "Arable") %>% filter(!Biome_broad_hab == "Aquatic") 

sb_trop_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

mod_trop_r <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Habitat_broad  * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                   family = poisson(), data = sb_trop_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_trop_r)
pp_check(mod_trop_r)
conditional_effects(mod_trop_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_trop_r, file = 'mod_tropf_r.Rdata')



# Tropical and Subtropical Grasslands, Savannas and Shrublands
sb_tund_r <- sb %>% filter(Biome_broad_hab == "Tundra")  %>%
  filter(!Biome_broad_hab == "Arable") %>% filter(!Biome_broad_hab == "Aquatic") 

sb_tund_r %>% select(Biome_zone, Biome_broad_hab, Habitat_broad) %>% distinct()

sb_tund_r %>% filter(Habitat_broad == "Forest")

mod_tund_r <- brm(Total_species ~ Centred_log_total_sample_area_m2   * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                   family = poisson(), data = sb_tund_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_tund_r)
pp_check(mod_tund_r)
conditional_effects(mod_tund_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tund_r, file = 'mod_tu_r.Rdata')




