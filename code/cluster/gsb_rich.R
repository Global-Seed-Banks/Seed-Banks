
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_slim.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


rich.mod <- brm(Total_Species ~ Sample_Effort * Biome_WWF_Zone + (Sample_Effort * Biome_WWF_Zone  | pap.loc ), 
                family = student(), data = sb, cores = 4, chains = 4)


save(rich.mod,
     file=Sys.getenv('OFILE'))



