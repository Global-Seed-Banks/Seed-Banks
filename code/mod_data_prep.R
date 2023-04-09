

library(tidyverse)
library(brms)
library(bayesplot)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/Data/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'gsb_slim.csv'))


head(sb)
colnames(sb)
summary(sb)


sb_calc <- sb %>% mutate( log_Total_Seeds = log(Total_Seeds),
                          log_Total_Species = log(Total_Species),
                            #  log10_Total_Seeds = log10(Total_Seeds),
                            #  log10_Total_Species = log10(Total_Species),
                          ratio_seeds_species = (Total_Seeds / Total_Species),
                          #     log_ratio_seeds_species = log(ratio_seeds_species),
                          #     log10_ratio_seeds_species = log10(ratio_seeds_species),
                          Total_Sample_Volume_mm3 = (Total_Number_Samples * Sample_Volume_mm3),
                          Total_Sample_Area_mm2 = (Total_Number_Samples * Sample_Area_mm2),
                          log_Total_Number_Samples = log(Total_Number_Samples),
                          log_Number_Sites = log(Number_Sites),
                          # log_Total_Sample_Area_mm2 = log(Total_Sample_Area_mm2),
                          # log10_Total_Number_Samples = log10(Total_Number_Samples),
                          # log10_Total_Sample_Area_mm2 = log10(Total_Sample_Area_mm2),
                          Total_Sample_Area_m2 = (Total_Sample_Area_mm2 / 1000000),
                          Total_Sample_Volume_m3 = (Total_Sample_Volume_mm3 / 1000000000),
                          log_Total_Sample_Area_m2 = log(Total_Sample_Area_m2),
                          # log_Calc_Volume_m3 = log(Calc_Volume_m3),
                          # log10_Total_Sample_Area_m2 = log10(Total_Sample_Area_m2),
                          # log10_Calc_Volume_m3 = log10(Calc_Volume_m3),
                          # Centred_Total_Number_Samples = Total_Number_Samples - mean(Total_Number_Samples, na.rm = TRUE),
                          # Centred_Calc_Volume_mm3 = Calc_Volume_mm3 - mean(Calc_Volume_mm3, na.rm = TRUE),
                          # Centred_Calc_Volume_m3 = Calc_Volume_m3 - mean(Calc_Volume_m3, na.rm = TRUE),
                          # Centred_Total_Sample_Area_mm2 = Total_Sample_Area_mm2 - mean(Total_Sample_Area_mm2, na.rm = TRUE),
                          Centred_Total_Number_Samples = Total_Number_Samples - mean(Total_Number_Samples, na.rm = TRUE),
                          Centred_Number_Sites = Number_Sites - mean(Number_Sites, na.rm = TRUE),
                          Centred_log_Total_Number_Samples = log_Total_Number_Samples - mean(log_Total_Number_Samples, na.rm = TRUE),
                          Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
                          # Centred_log_Total_Seeds = log_Total_Seeds - mean(log_Total_Seeds, na.rm = TRUE),
                          # Centred_log_Total_Sample_Area_mm2 = log_Total_Sample_Area_mm2 - mean(log_Total_Sample_Area_mm2, na.rm = TRUE),
                          # Centred_log_Calc_Volume_m3 = log_Calc_Volume_m3 - mean(log_Calc_Volume_m3, na.rm = TRUE),
                          # Centred_log_Total_Sample_Area_mm2 = log_Total_Sample_Area_mm2 - mean(log_Total_Sample_Area_mm2, na.rm = TRUE),
                          Centred_log_Total_Sample_Area_m2 = log_Total_Sample_Area_m2 - mean(log_Total_Sample_Area_m2, na.rm = TRUE),
                          #Centred_log10_Total_Sample_Area_m2 = log10_Total_Sample_Area_m2 - mean(log10_Total_Sample_Area_m2, na.rm = TRUE)
) 



head(sb_calc)
summary(sb_calc)


# write over biomes when habitat is arable or aquatic
sb_mod <- sb_calc %>% 
  mutate(Biome_Broad_Hab = case_when(Habitat_Broad %in% c("Arable", "Aquatic") ~ Habitat_Broad ,
                                                                                             TRUE ~ Biome_WWF_Broad))
# check out biome and habitat combos
sb_mod %>% distinct( Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)

sb_check <- sb_mod %>% filter(Biome_Broad_Hab %in% c( "Aquatic", "Mediterranean Forests, Woodlands and Scrub", 
                                          "Montane Grasslands and Shrublands" , "Tundra")) %>%
  select(Biome_Broad_Hab, Number_Sites, Total_Sample_Area_m2, Total_Species) %>%
  filter(Total_Sample_Area_m2 <= 1 ,
         !is.na(Total_Species)) %>%
  arrange(Biome_Broad_Hab,  Number_Sites, Total_Sample_Area_m2, Total_Species)

View(sb_check)


# sb_mod %>% distinct(Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)
# 
# sb_mod %>% select(Biome_Broad_Hab, Total_Sample_Area_m2, Total_Seeds, Total_Species) %>%
#   filter(Biome_Broad_Hab == "Tundra") %>% distinct() %>% arrange(desc(Total_Species))
#   
# nrow(sb_mod %>% filter(Total_Seeds == 0))

#setwd(paste0(path2wd, 'Data/'))
write.csv(sb_mod,  "sb_prep.csv")


# reload new dat and get some summaries
sb_prep <- read.csv(paste0(path2wd, 'sb_prep.csv'))

sb_clean <- read.csv(paste0(path2wd, 'gsb_cleaned.csv'))

head(sb_clean)

study_refs <- sb_clean %>% 
  select( studyID, Authors, Year, Title, Journal, Doi, URL, studylong) %>%
  distinct()

head(study_refs)
nrow(study_refs)

head(sb_prep)

biome_count <- sb_prep %>% select(Biome_Broad_Hab,  Total_Species) %>%
  dplyr::group_by(Biome_Broad_Hab) %>%
  count() 

biome_count

sites_count <- sb_prep %>% select(Number_Sites) %>%
  dplyr::group_by(Number_Sites) %>%
  count() 

View(sites_count)

options(scipen = 999)

colnames(sb_prep)

# get a summary of min and max values
# keep country and study details so we can cite them as examples

sb_deets <- sb_prep %>% group_by(Biome_Broad_Hab, Country, studyID) %>%
  summarise(`min-Total_Number_Samples` = min(as.numeric(Total_Number_Samples), na.rm = TRUE),
            `max-Total_Number_Samples` = max(as.numeric(Total_Number_Samples),na.rm = TRUE),
            `min-Number_Sites` = min(as.numeric(Number_Sites), na.rm = TRUE),
            `max-Number_Sites` = max(as.numeric(Number_Sites),na.rm = TRUE),
            `min-Sample_Area_mm2` = min(as.numeric(Sample_Area_mm2),na.rm = TRUE),
            `max-Sample_Area_mm2` = max(as.numeric(Sample_Area_mm2), na.rm = TRUE),
             `max-ratio_seeds_species` = max(as.numeric(ratio_seeds_species), na.rm = TRUE),
            `min-ratio_seeds_species` = min(as.numeric(ratio_seeds_species), na.rm = TRUE),
            `max-Total_Sample_Area_m2` = max(as.numeric(Total_Sample_Area_m2), na.rm = TRUE),
            `min-Total_Sample_Area_m2` = min(as.numeric(Total_Sample_Area_m2), na.rm = TRUE),
            `min-Sample_Volume_mm3` = min(as.numeric(Sample_Volume_mm3), na.rm = TRUE),
            `max-Sample_Volume_mm3` = max(as.numeric(Sample_Volume_mm3),na.rm = TRUE),
            `min-Total_Sample_Volume_m3` = min(as.numeric(Total_Sample_Volume_m3), na.rm = TRUE),
            `max-Total_Sample_Volume_m3` = max(as.numeric(Total_Sample_Volume_m3),na.rm = TRUE),
            `min-Total_Species` = min(as.numeric(Total_Species), na.rm = TRUE),
            `max-Total_Species` = max(as.numeric(Total_Species),na.rm = TRUE),
            `min-Seed_density_m2` = min(as.numeric(Seed_density_m2), na.rm = TRUE),
            `max-Seed_density_m2` = max(as.numeric(Seed_density_m2),na.rm = TRUE),
            `min-Total_Seeds` = min(as.numeric(Total_Seeds), na.rm = TRUE),
            `max-Total_Seeds` = max(as.numeric(Total_Seeds),na.rm = TRUE),
  ) %>%
  pivot_longer(`min-Total_Number_Samples` : `max-Total_Seeds`) %>%
  separate(name, into = c("minmax", "name"), sep="-") %>% 
  #spread(minmax, value) %>%
  ungroup() %>%
  filter(# value > 0,
         !is.infinite(value) ) 

View(sb_deets)

sb_zero <- sb_deets %>%
  filter(minmax == "min") %>%
  filter(value == 0)

sb_zero


sb_minmax <-  bind_rows(
  
  sb_max <- sb_deets %>%
    filter(minmax == "max") %>%
  group_by(name) %>%
  filter(value == max(value)),
  
   sb_min <- sb_deets %>%
    filter(minmax == "min") %>%
    filter(value > 0) %>%
    group_by(name) %>%
  filter(value == min(value))
  ) %>%
  arrange(name, minmax, value ) %>%
  group_by(name) %>%
  left_join( bind_rows(
    sb_max %>% group_by(name, minmax) %>%
      count() ,
    sb_min %>% group_by(name, minmax) %>%
      count() 
  ) ) 

 min_max <- sb_minmax %>% filter(!n > 1)
  
 print(min_max)
 
colnames(sb_prep)

responses <- min_max %>% #filter(name %in% c( "Seed_density_m2", "Total_Seeds", "Total_Species", "ratio_seeds_species") ) %>%
  left_join(sb_prep) %>% select(
    Biome_Broad_Hab, Country, studyID, name, minmax, value, n,
    Total_Species, Seed_density_m2, Total_Seeds, ratio_seeds_species,
    Total_Number_Samples, Number_Sites, Total_Sample_Area_m2, Total_Sample_Volume_m3, )


colnames(responses)
View(responses)

studies_mmin_multi <- sb_minmax %>% filter(n > 1)
 View(studies_mmin_multi)

min_max_count <- sb_minmax %>% filter(n > 1) %>% 
   select( minmax, value, n) %>%
   distinct()

View(min_max_count)

write.csv(sb_deets,  "sb_details_summary.csv")
write.csv(sb_deets,  "sb_details_biome_summary.csv")

