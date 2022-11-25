
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# Total_Species   Seed_density_m2   Total_Seeds
sb_dat <- sb %>% filter(!is.na(Total_Species),
                        #!Total_Species == 0,
                        !is.na(Centred_log_Total_Sample_Area_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
           Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method)) 

set_inits <- function(seed = 1) {
  
  set.seed(seed)
  list(
    Intercept = rnorm(n = 1, mean = 100, sd = 20),
    sigma     = runif(n = 1, min = 9, max = 17),
    beta      = runif(n = 1, min = 35, max = 40)
  )
  
}

# try it out
set_inits(seed = 0)

my_inits <- list(
  # different seed values will return different results
  set_inits(seed = 1),
  set_inits(seed = 2),
  set_inits(seed = 3),
  set_inits(seed = 4)
)

rich_m2 <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + ( 1 | Habitat_Degraded/Method/studyID/rowID ),
                    family = exgaussian(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000, inits = my_inits,
                    control = list(adapt_delta = 0.9999,
                                   max_treedepth = 13)
)


save(rich_m2,
     file=Sys.getenv('OFILE'))



