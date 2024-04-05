
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
  filter(Realm == "Arable") %>%
  mutate(Biome = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
                           grepl("Temperate", Biome) ~ "Temperate and Boreal",
                           grepl("Boreal", Biome) ~ "Temperate and Boreal",
                           grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))


mod_ar_ra <- brm(ratio_seeds_species ~  Biome + (1 | StudyID/RowID ),
                 family = lognormal(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
                 control = list(adapt_delta = 0.99999,
                                max_treedepth = 12)
)



save(mod_ar_ra,
     file=Sys.getenv('OFILE'))



