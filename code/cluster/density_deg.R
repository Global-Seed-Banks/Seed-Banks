
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
    Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method))
# %>% filter(!Habitat_Broad == "Arable")


density_deg <- brm(Seed_density_m2 ~  Habitat_Degraded  + ( 1 | Method/studyID/rowID ) ,
                  family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 3000, warmup =1000,
                  control = list(adapt_delta = 0.99))


save(density_deg,
     file=Sys.getenv('OFILE'))



