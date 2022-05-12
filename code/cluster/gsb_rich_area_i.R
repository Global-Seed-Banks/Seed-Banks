
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

sb_dat <- sb %>% filter(!is.na(Total_Species),
                        !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method))
# %>% filter(!Habitat_Broad == "Arable")


rich.p_i <- brm(Total_Species ~ Habitat_Broad  + Habitat_Degraded + log_Total_Sample_Area_mm2 * Habitat_Broad * Biome_WWF_Zone + ( 1 | Method/studyID/rowID ),
                family = poisson(), data = sb, cores = 4, chains = 4, iter = 2000, warmup =1000)



save(rich.p_i,
     file=Sys.getenv('OFILE'))



