
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Total_Species),
                        !is.na(Centred_log_Total_Sample_Area_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) %>% 
  mutate(Biome_Broad_Hab = case_when(Habitat_Broad %in% c("Arable", "Aquatic") ~ Habitat_Broad ,
                                     TRUE ~ Biome_WWF_Broad))

rich_biome_broad <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + (1 | Method/studyID/rowID ),
                    family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
                    control = list(adapt_delta = 0.9999,
                                   max_treedepth = 13)
)


save(rich_biome_broad,
     file=Sys.getenv('OFILE'))



