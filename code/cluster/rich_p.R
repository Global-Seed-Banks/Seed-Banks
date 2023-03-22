
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
# data
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# model object
load(paste0(path,'/rich_m2_i.Rdata') )

sb_rich_area <- sb %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))


rich.fitted <- tidyr::crossing( sb_rich_area %>% select(Biome_Broad_Hab) %>% distinct(),
                                Number_Sites = c(1, 20, 100),
                                sb_rich_area %>% summarise(  Total_Sample_Area_m2 = c( seq( min(Total_Sample_Area_m2), max(Total_Sample_Area_m2), length.out = 2792) ) ), # ~4 samples per m2 - 64 x 3 = 192 combos
)  %>%
  mutate( log_Number_Sites = log(Number_Sites),
          log_Total_Sample_Area_m2 = log(Total_Sample_Area_m2),
          Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
          Centred_log_Total_Sample_Area_m2 = log_Total_Sample_Area_m2 - mean(log_Total_Sample_Area_m2, na.rm = TRUE) ) %>%
  select(-c( log_Number_Sites, log_Total_Sample_Area_m2 ) ) %>%
  arrange( Total_Sample_Area_m2, Number_Sites ) %>%
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Centred_log_Number_Sites, Number_Sites)) %>%
  mutate(fitted = map(data, ~epred_draws(rich_m2, newdata= .x,  re_formula =  NA  ))) 


rich.fitted.df  <- rich.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) 

rich.fitted.p <- rich.fitted.df %>%
  ungroup() %>%
  select(-.draw) %>%
  group_by(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Number_Sites, Centred_log_Number_Sites) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs = 0.025),
          P_Estimate_upper = quantile(.epred, probs = 0.975) ) %>%
  select(-.epred) %>% distinct()


save(rich.fitted.p,
     file=Sys.getenv('OFILE'))



