
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          # Biome_WWF_Broad = as.factor(Biome_WWF_Broad),
          # Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) %>% 
  mutate(Biome_Broad_Hab = case_when(Habitat_Broad %in% c("Arable", "Aquatic") ~ Habitat_Broad ,
                                     TRUE ~ Biome_WWF_Broad))


density_biome_broad <- brm(Seed_density_m2 ~  Biome_Broad_Hab + (1 | Method/studyID/rowID ),
                    family = student(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
                    control = list(adapt_delta = 0.999,
                                   max_treedepth = 12)
)


save(density_biome_broad,
     file=Sys.getenv('OFILE'))



