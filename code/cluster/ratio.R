
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(ratio_seeds_species)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          ratio_seeds_species = as.numeric(ratio_seeds_species),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) 


ratio <- brm(ratio_seeds_species ~  Biome_Broad_Hab + (1 | studyID/Method ),
                    family = lognormal(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
                    control = list(adapt_delta = 0.99999,
                                   max_treedepth = 12)
)


save(ratio,
     file=Sys.getenv('OFILE'))



