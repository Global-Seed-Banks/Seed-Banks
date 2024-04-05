
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Banks'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

sb_dat <- sb %>% 
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
          Method = as.factor(Method)) %>% arrange(Biome_broad_hab) %>%
  filter(Habitat_broad == "Wetland") %>%
  mutate(Biome = case_when(
    grepl("Deserts", Biome_broad_hab) ~ "Deserts",
    grepl("Mediterranean", Biome_broad_hab) ~ "Mediterranean",
    grepl("Mediterranean", Biome_zone) & grepl("Tropical", Biome_broad_hab) ~ "Mediterranean",
  )) %>%
  mutate(Biome = case_when(
    is.na(Biome) ~ Biome_zone,
    TRUE ~ Biome
  ))


sb_dat$Habitat_degraded <- relevel(sb_dat$Habitat_degraded, ref = "1")


mod_wetland_r <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Biome  * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                  family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 12000, warmup = 1000,
                  prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                  control = list(adapt_delta = 0.99999,
                                 max_treedepth = 15)
)




save(mod_wetland_r,
     file=Sys.getenv('OFILE'))



