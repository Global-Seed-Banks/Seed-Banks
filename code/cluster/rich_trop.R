
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
 filter(Biome_zone == "Tropical")  %>%
  filter(!Biome_broad_hab == "Arable") %>% filter(!Biome_broad_hab == "Aquatic") 


sb_dat$Habitat_degraded <- relevel(sb_dat$Habitat_degraded, ref = "1")


mod_trop_r <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Habitat_broad  * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/RowID ),
                  family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 6000, warmup = 1000,
                  prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                  control = list(adapt_delta = 0.99999,
                                 max_treedepth = 13)
)




save(mod_trop_r,
     file=Sys.getenv('OFILE'))



