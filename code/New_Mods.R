

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
  filter(!is.na(Total_Species),
         # !Total_Species == 0,
         !is.na(Centred_log_Total_Sample_Area_m2) #,
         #Number_Sites == 1 
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) %>% arrange(Biome_Broad_Hab)

head(sb)

nrow(sb %>% filter(Habitat_Degraded == 0)) # 2105 rows
nrow(sb %>% filter(Habitat_Degraded == 1)) # 991 rows
is.character(sb$Habitat_Degraded)

sb %>% select(Biome_Broad_Hab) %>% distinct()

# Biome_Broad_Hab
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

  
sb %>% select(Biome_Broad_Hab, Habitat_Degraded) %>% distinct() %>% arrange(Biome_Broad_Hab, Habitat_Degraded)

print(sb %>%  arrange(Biome_Broad_Hab, Habitat_Degraded) %>% group_by(Biome_Broad_Hab, Habitat_Degraded) %>% summarise(n = n()), n=Inf)

# ~ 25 studies with method nested in study
print(sb %>% select(studyID, Method) %>% distinct() %>% group_by(studyID) %>% filter(n() > 1), n=Inf)

# Aquatic
sb_aq_r <- sb %>% filter(Biome_Broad_Hab == "Aquatic")

View(sb_aq_r %>% filter(Habitat_Degraded == 1) %>% select(studyID,Country, Total_Species, Total_Sample_Area_m2) %>% arrange(Total_Sample_Area_m2))

mod_aq_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Habitat_Degraded  + Centred_log_Number_Sites + ( 1  | studyID/Method ),
               family = poisson(), data = sb_aq_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
           prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
              control = list(adapt_delta = 0.99999,
                    max_treedepth = 13)
)

summary(mod_aq_r)
pp_check(mod_aq_r)
conditional_effects(mod_aq_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_aq_r, file = 'mod_aq_r.Rdata')


#Arable
sb_ar_r <- sb %>% filter(Biome_Broad_Hab == "Arable")

View(sb_ar_r %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_ar_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2  + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                family = poisson(), data = sb_ar_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_ar_r)
pp_check(mod_ar_r)
conditional_effects(mod_ar_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_ar_r, file = 'mod_ar_r.Rdata')

# Boreal Forests/Taiga
sb_bo_r <- sb %>% filter(Biome_Broad_Hab == "Boreal Forests/Taiga")

View(sb_bo_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_bo_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
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
sb_de_r <- sb %>% filter(Biome_Broad_Hab == "Deserts and Xeric Shrublands")

View(sb_de_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_de_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
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
sb_med_r <- sb %>% filter(Biome_Broad_Hab == "Mediterranean Forests, Woodlands and Scrub")

View(sb_med_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_med_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
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


# Montane Grasslands and Shrublands
sb_mo_r <- sb %>% filter(Biome_Broad_Hab == "Montane Grasslands and Shrublands")

View(sb_mo_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_mo_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                 family = poisson(), data = sb_mo_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                 prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                 control = list(adapt_delta = 0.99999,
                                max_treedepth = 13)
)

summary(mod_mo_r)
pp_check(mod_mo_r)
conditional_effects(mod_mo_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_mo_r, file = 'mod_mo_r.Rdata')



# Temperate Broadleaf and Mixed Forests
sb_tbroad_r <- sb %>% filter(Biome_Broad_Hab == "Temperate Broadleaf and Mixed Forests")

View(sb_tbroad_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tbroad_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                family = poisson(), data = sb_tbroad_r, cores = 4, chains = 4, iter = 8000, warmup = 1000,
                prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                control = list(adapt_delta = 0.99999,
                               max_treedepth = 13)
)

summary(mod_tbroad_r)
pp_check(mod_tbroad_r)
conditional_effects(mod_tbroad_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tbroad_r, file = 'mod_tbroad_r.Rdata')



# Temperate Conifer Forests
sb_tconf_r <- sb %>% filter(Biome_Broad_Hab == "Temperate Conifer Forests")

#View(sb_tconf_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tconf_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                    family = poisson(), data = sb_tconf_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                    prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                    control = list(adapt_delta = 0.99999,
                                   max_treedepth = 13)
)

summary(mod_tconf_r)
pp_check(mod_tconf_r)
conditional_effects(mod_tconf_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tconf_r, file = 'mod_tconf_r.Rdata')



# Temperate Grasslands, Savannas and Shrublands
sb_tempg_r <- sb %>% filter(Biome_Broad_Hab == "Temperate Grasslands, Savannas and Shrublands")

#View(sb_tempg_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tempg_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                   family = poisson(), data = sb_tempg_r, cores = 4, chains = 4, iter = 8000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_tempg_r)
pp_check(mod_tempg_r)
conditional_effects(mod_tempg_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tempg_r, file = 'mod_tempg_r.Rdata')



# Tropical and Subtropical Forests
sb_tropf_r <- sb %>% filter(Biome_Broad_Hab == "Tropical and Subtropical Forests")

#View(sb_tropf_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tropf_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                   family = poisson(), data = sb_tropf_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_tropf_r)
pp_check(mod_tropf_r)
conditional_effects(mod_tropf_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tropf_r, file = 'mod_tropf_r.Rdata')



# Tropical and Subtropical Grasslands, Savannas and Shrublands
sb_tropg_r <- sb %>% filter(Biome_Broad_Hab == "Tropical and Subtropical Grasslands, Savannas and Shrublands")

#View(sb_tropg_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tropg_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                   family = poisson(), data = sb_tropg_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_tropg_r)
pp_check(mod_tropg_r)
conditional_effects(mod_tropg_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tropg_r, file = 'mod_tropg_r.Rdata')



# Tropical and Subtropical Grasslands, Savannas and Shrublands
sb_tu_r <- sb %>% filter(Biome_Broad_Hab == "Tundra")

#View(sb_tu_r %>% filter(Habitat_Degraded == 1) %>% select(Country, Total_Species, Total_Sample_Area_m2))

mod_tu_r <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2   * Habitat_Degraded + Centred_log_Number_Sites + ( 1  | studyID/Method ),
                   family = poisson(), data = sb_tu_r, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                   prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                   control = list(adapt_delta = 0.99999,
                                  max_treedepth = 13)
)

summary(mod_tu_r)
pp_check(mod_tu_r)
conditional_effects(mod_tu_r)

setwd(paste0(path2wd, 'Model_Fits/New/'))
# # # save data objects to avoid doing this every time
save(mod_tu_r, file = 'mod_tu_r.Rdata')




