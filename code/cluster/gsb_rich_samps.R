
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))


rich.samps <- brm(Total_Species2 ~ log_Total_Number_Samples * Biome_WWF_Zone + (log_Total_Number_Samples * Biome_WWF_Zone  | Habitat_Broad/studyID/samp.loc ), 
                family = poisson(), data = sb, cores = 4, chains = 4,
                iter = 10000, warmup = 1000
               # control = list(adapt_delta = 0.99) 
                )


save(rich.samps,
     file=Sys.getenv('OFILE'))



