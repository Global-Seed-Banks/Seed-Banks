
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Banks'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Total_species),
                       # !Total_Species == 0,
                        !is.na(Centred_log_total_sample_area_m2) #,
                        #Number_Sites == 1 
                        ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_broad_hab = as.factor(Biome_broad_hab),
           Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) 



rich_m2 <- brm(Total_species ~ Centred_log_total_sample_area_m2 * Biome_broad_hab * Habitat_degraded + Centred_log_number_sites + ( 1  | StudyID/Method ),
                    family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 8000, warmup = 1000,
                    prior = c(prior( student_t(3, 0.5, 1) , class = b,  lb = 0)),
                    control = list(adapt_delta = 0.99999,
                                   max_treedepth = 13)
)




save(rich_m2,
     file=Sys.getenv('OFILE'))



