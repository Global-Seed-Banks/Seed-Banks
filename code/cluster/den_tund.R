
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Banks'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Seed_density_m2),
                        !Seed_density_m2 == 0,
                        ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_broad_hab = as.factor(Biome_broad_hab),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) 


density_m2 <- brm(Seed_density_m2 ~  Biome_broad_hab * Habitat_degraded + ( 1 | StudyID/Method ),
                    #family = Gamma(link="log"), 
                  family= lognormal(),
                  data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000, 
                    control = list(adapt_delta = 0.999,
                                   max_treedepth = 12)
)


save(density_m2,
     file=Sys.getenv('OFILE'))



