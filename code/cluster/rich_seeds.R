
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Total_Species),
                        !is.na(Centred_log_Total_Sample_Area_m2),
                        !log_Total_Seeds == "-Inf",
                        !is.na(Total_Seeds),
                        !is.na(log_Total_Species)) %>%
  # treat all random effects as factors
  mutate( 
          Centred_log_Total_Seeds = log_Total_Seeds - mean(log_Total_Seeds, na.rm = TRUE),
          Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
           Habitat_Broad = as.factor(Habitat_Broad),
          log_Total_Seeds = as.numeric(log_Total_Seeds),
          Total_Species = as.numeric(Total_Species),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) 

rich_seeds <- brm(Total_Species ~ Centred_log_Total_Seeds * Biome_Broad_Hab +  Centred_log_Number_Sites + (1 | Method/studyID ),
                    family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
                    control = list(adapt_delta = 0.9999,
                                   max_treedepth = 13)
)


save(rich_seeds,
     file=Sys.getenv('OFILE'))



