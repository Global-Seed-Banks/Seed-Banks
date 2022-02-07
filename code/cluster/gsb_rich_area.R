
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


rich.area <- brm(Total_Species2 ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad/studyID/rowID ),
                family = poisson(), data = sb, cores = 4, chains = 4,
                iter = 5000, warmup = 1000
                )

# rich.area <- brm(log_Total_Species2 ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone  | Habitat_Broad/studyID/rowID ),
#                  family = student(), data = sb, cores = 4, chains = 4,
# )


save(rich.area,
     file=Sys.getenv('OFILE'))



