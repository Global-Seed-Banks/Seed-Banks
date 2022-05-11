
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


rich.p_habs <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Habitat_Broad + (log_Total_Sample_Area_mm2 * Habitat_Broad | Biome_WWF_Zone/studyID/rowID ),
                family = poisson(), data = sb, cores = 4, chains = 4, iter = 4000, warmup =1000,
                control = list(adapt_delta = 0.9))



save(rich.p_habs,
     file=Sys.getenv('OFILE'))



