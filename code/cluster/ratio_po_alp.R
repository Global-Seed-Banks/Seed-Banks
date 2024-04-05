
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Banks'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

sb_dat <- sb %>% 
  filter(!is.na(Seed_density_m2),
         !Seed_density_m2 == 0,
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_broad_hab = as.factor(Biome_broad_hab),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome_broad_hab) %>%
  filter(Realm == "Tundra") 

sb_dat$Habitat_degraded <- relevel(sb_dat$Habitat_degraded, ref = "1")


mod_tund_ra <- brm(ratio_seeds_species ~ Biome *  Habitat_degraded + (1 | StudyID ),
                 family = lognormal(), data = sb_dat, cores = 4, chains = 4, iter = 10000, warmup = 1000,
                 control = list(adapt_delta = 0.9999)
)




save(mod_tund_ra,
     file=Sys.getenv('OFILE'))



