
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


#sb <- sb 

sb_dat <- sb %>% filter(!is.na(Total_Species2),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method))
# %>% filter(!Habitat_Broad == "Arable")

# rich.p_zones <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad ),
#                 family = poisson(), data = sb_dat, cores = 4, chains = 4,
#                 )

rich.p_zones_2 <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad/studyID/rowID ),
                    family = poisson(), data = sb_dat, cores = 4, chains = 4,
)


save(rich.p_zones_2,
     file=Sys.getenv('OFILE'))



