
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


# rich.p_habs <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Habitat_Broad + ( 1 | Method/studyID/rowID ),
#                 family = poisson(), data = sb, cores = 4, chains = 4, iter = 2000, warmup =1000)

rich.p_habs_2 <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Habitat_Broad + ( log_Total_Sample_Area_mm2 * Habitat_Broad  | Method/studyID/rowID ),
                family = poisson(), data = sb, cores = 4, chains = 4, iter = 2000, warmup =1000)

save(rich.p_habs_2,
     file=Sys.getenv('OFILE'))



