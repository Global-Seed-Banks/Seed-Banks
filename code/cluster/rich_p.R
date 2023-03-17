
library(tidyverse)
library(brms)


path <- '/gpfs1/data/idiv_chase/emmala/Seed-Bank-Map'
# data
sb <- read.csv(paste0(path, '/sb_prep.csv'), header=T, fill=TRUE, sep=",", na.strings=c(""," ","NA","NA ","na"))

# model object
load(paste0(path,'/rich_m2_i.Rdata') )

sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))


rich.fitted <- sb_rich_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Centred_log_Number_Sites, Number_Sites) %>% 
  summarise( Centred_log_Total_Sample_Area_m2 = seq( (0.01), (15),length.out = 1000),
             Total_Sample_Area_m2 = seq ( (0.01), (15), length.out = 1000),
  ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Centred_log_Number_Sites, Number_Sites)) %>%
  mutate(fitted = map(data, ~epred_draws(rich_m2, newdata= .x, re_formula =  NA  ))) 

rich.fitted.df  <- rich.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration)) 

rich.fitted.sum <- rich.fitted.df %>%
  ungroup() %>%
  select(-.draw) %>%
  mutate( extent = ifelse(Number_Sites = 1 , 'one',
                          ifelse(Number_Sites >= 2 & Number_Sites <= 20,  'mid',
                                 ifelse(Number_Sites  >= 21 , 'high', 
                                        'other'))) ) %>%
  group_by(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, extent) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs = 0.025),
          P_Estimate_upper = quantile(.epred, probs = 0.975) ) %>%
  select(-.epred) %>% distinct()


save(rich.fitted.sum,
     file=Sys.getenv('OFILE'))



