

library(tidyverse)
library(brms)
library(bayesplot)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'Data/gsb_db.csv'))

# remove rows with NA across all 4 metrics
sb<-sb[!is.na(sb$Total_Seeds) | !is.na(sb$Total_Species) | !is.na(sb$Seed_density_m2) | !is.na(sb$Seed_density_litre),]

head(sb)
colnames(sb)
summary(sb)

# table s1 numbers
nrow(sb) # n records

nrow( sb %>% select( # n studies
  studyID
) %>% distinct() )

nrow( sb %>% select( # n unique locations
  studyID,
  Lat_Deg ,  Lon_Deg
) %>% distinct() )

head(sb)

# quantify metrics we want
sb_calc <- sb %>% mutate( log_Total_Seeds = log(Total_Seeds),
                          log_Total_Species = log(Total_Species),
                          ratio_seeds_species = (Total_Seeds / Total_Species),
                          Total_Sample_Volume_mm3 = (Total_Number_Samples * Sample_Volume_mm3),
                          Total_Sample_Area_mm2 = (Total_Number_Samples * Sample_Area_mm2),
                          log_Total_Number_Samples = log(Total_Number_Samples),
                          log_Number_Sites = log(Number_Sites),
                          Total_Sample_Area_m2 = (Total_Sample_Area_mm2 / 1000000),
                          Total_Sample_Volume_m3 = (Total_Sample_Volume_mm3 / 1000000000),
                          log_Total_Sample_Area_m2 = log(Total_Sample_Area_m2),
                          Centred_Total_Number_Samples = Total_Number_Samples - mean(Total_Number_Samples, na.rm = TRUE),
                          Centred_Number_Sites = Number_Sites - mean(Number_Sites, na.rm = TRUE),
                          Centred_log_Total_Number_Samples = log_Total_Number_Samples - mean(log_Total_Number_Samples, na.rm = TRUE),
                          Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
                          Centred_log_Total_Sample_Area_m2 = log_Total_Sample_Area_m2 - mean(log_Total_Sample_Area_m2, na.rm = TRUE),
) 

head(sb_calc)
summary(sb_calc)

# write over biomes when habitat is arable or aquatic
# test Blowes et al biome method fopr comparison
sb_mod <- sb_calc %>% 
  mutate(Biome_Broad_Hab = case_when(Habitat_Broad %in% c("Arable", "Aquatic") ~ Habitat_Broad ,
                                    TRUE ~ Biome_WWF_Broad)) %>%
  mutate(Lat_Deg_abs = abs(Lat_Deg)) 


write.csv(sb_mod,  "sb_prep.csv")


# reload new dat and get some summaries for methods/ tables etc whatever
sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))


head(sb)
colnames(sb)
summary(sb)

# table s1 numbers
nrow(sb_prep) # n records

nrow( sb_prep %>% select( # n studies
  studyID
) %>% distinct() )


head(sb)

# table s1
sb_gathered <- sb_prep %>% select(
  rowID, 
  studyID,
  Centred_log_Total_Sample_Area_m2, 
  Biome_Broad_Hab, Number_Sites, Total_Seeds, Total_Species, Seed_density_m2, ratio_seeds_species) %>%
  gather(metric, response, Total_Seeds:ratio_seeds_species) %>%
  filter(!is.na(response),
        # !is.na(Centred_log_Total_Sample_Area_m2),
       # !response == 0 ,
        #response == 0 
       ) #%>% #filter(metric == "Seed_density_m2") %>%
   #filter(metric == "ratio_seeds_species")

head(sb_gathered)

# table s1 observations
nrow( sb_gathered %>% filter(!metric == "ratio_seeds_species")) # observations including richness, abundance and density
nrow( sb_gathered %>% filter(metric == "Total_Species"))
nrow( sb_gathered %>% filter(metric == "Total_Seeds"))
nrow( sb_gathered %>% filter(metric == "Seed_density_m2"))
nrow( sb_gathered %>% filter(metric == "ratio_seeds_species"))

# number of observations used in each model
nrow( sb_gathered %>% filter(metric == "Total_Species") %>% filter(!is.na(Centred_log_Total_Sample_Area_m2) ))
nrow( sb_gathered %>% filter(metric == "Total_Seeds")  %>% filter(!is.na(Centred_log_Total_Sample_Area_m2)  ))
nrow( sb_gathered %>% filter(metric == "Seed_density_m2")  %>% filter(response != 0 ))
nrow( sb_gathered %>% filter(metric == "ratio_seeds_species")  %>% filter(response != 0 ))

nrow(sb_gathered %>% filter(response == 0 ))
# % 0's 15/8087
round( ((15/8087)* 100) , 2) # 0.19 % of data are zeros


sites_count <- sb_gathered %>% filter(!metric == "ratio_seeds_species") %>%
  select(Number_Sites) %>%
  dplyr::group_by(Number_Sites) %>%
  count() 

head(sites_count) # of data points within number of sites
# 4763 observations within 1 site

head(sb_prep)

# table s2
# table s2 biomes 
sb_biomes <- sb_prep %>% filter(Biome_Broad_Hab != "Aquatic", Biome_Broad_Hab != "Arable") %>%
  select(Biome_WWF, Biome_WWF_Broad) %>% #distinct() %>%
  dplyr::group_by(Biome_WWF, Biome_WWF_Broad) %>%
  count() %>%
  mutate( Biome_WWF_Broad = as.factor(Biome_WWF_Broad)) %>%
  mutate(`Biome_WWF_Broad` = fct_relevel(`Biome_WWF_Broad`, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands"#,
    #"Aquatic", "Arable"
  )
  )) %>% arrange(`Biome_WWF_Broad` )

print(sb_biomes)
# table s2
write.csv(sb_biomes,  "sb_biomes.csv")

head(sb_gathered)
# table s3
sb_gathered <- sb_prep %>% select(
  rowID, 
  studyID,
  Centred_log_Total_Sample_Area_m2, 
  Biome_Broad_Hab, Number_Sites, Total_Seeds, Total_Species, Seed_density_m2, ratio_seeds_species) %>%
 # filter(Seed_density_m2 != 0, ratio_seeds_species != 0) %>%
  gather(metric, response, Total_Seeds:ratio_seeds_species) %>%
  filter(!is.na(response),
          #!is.na(Centred_log_Total_Sample_Area_m2),
         # !response == 0 ,
         #response == 0 
  ) 
head(sb_gathered)

# first two columns (need sample area) table s3
biome_count_ss <- sb_gathered %>% # number of data points within every biome
  filter(!is.na(Centred_log_Total_Sample_Area_m2)) %>%
  select(Biome_Broad_Hab, metric) %>%
  filter(metric %in% c("Total_Seeds" , "Total_Species") ) %>%
  dplyr::group_by(Biome_Broad_Hab, metric) %>%
  count() %>% spread(metric, n) %>%
  mutate( Biome_Broad_Hab = as.factor(Biome_Broad_Hab)) %>%
  mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
    "Aquatic", "Arable")
  )) %>% arrange(Biome_Broad_Hab)

biome_count_ss

write.csv(biome_count_ss,  "biome_count_ss.csv")

# last two columns- dont need sample area and remove 0's table s2
biome_count_dr <- sb_gathered %>% # number of data points within every biome
  filter(response != 0) %>%
  select(Biome_Broad_Hab, metric) %>%
  filter(metric %in% c("Seed_density_m2" , "ratio_seeds_species") ) %>%
  dplyr::group_by(Biome_Broad_Hab, metric) %>%
  count() %>% spread(metric, n) %>%
  mutate( Biome_Broad_Hab = as.factor(Biome_Broad_Hab)) %>%
  mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab, c(
    "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
    "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
    "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
    "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
    "Aquatic", "Arable")
  )) %>% arrange(Biome_Broad_Hab)

biome_count_dr

write.csv(biome_count_dr,  "biome_count_dr.csv")


sb_gathered %>% filter(metric == "Total_Species",
                       !is.na(Centred_log_Total_Sample_Area_m2) ) %>%
  mutate( N_site_cats = 
                        case_when( (Number_Sites == 1) ~ "1 site",
                                  (Number_Sites >= 2 & Number_Sites <= 20) ~ "2-20 sites",
                                  (Number_Sites >= 21 & Number_Sites <= 100) ~ "21-100 sites",
                                  TRUE ~ "> 100 sites" ) ) %>%
  select(N_site_cats) %>%
  dplyr::group_by(N_site_cats) %>%
  count() 
                       

sb_gathered %>% filter(metric == "Total_Seeds",
                       !is.na(Centred_log_Total_Sample_Area_m2) ) %>%
  # mutate( N_site_cats = 
  #           case_when( (Number_Sites == 1) ~ "1 site",
  #                      (Number_Sites >= 2 & Number_Sites <= 20) ~ "2-20 sites",
  #                      (Number_Sites >= 21 & Number_Sites <= 100) ~ "21-100 sites",
  #                      TRUE ~ "> 100 sites" ) ) %>%
  mutate( N_site_cats = 
            case_when( (Number_Sites >= 1 & Number_Sites <= 19) ~ "1-20 site",
                       (Number_Sites >= 20 & Number_Sites <= 99 ) ~ "20-99 sites",
                       (Number_Sites >= 100 ) ~ "100 and more sites",
                       TRUE ~ "Other" ) ) %>%
  select(N_site_cats) %>%
  dplyr::group_by(N_site_cats) %>%
  count() 

options( scipen = 999 )

colnames(sb_prep)

# get a summary of min and max values
# keep country and study details so we can cite them as examples
# cute stories to tell at beginning of methods
sb_deets <- sb_prep %>% group_by(Biome_Broad_Hab, Country, studyID, rowID) %>%
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
  ungroup() %>%
  filter(# value > 0,
         !is.infinite(value) )  #%>% 
 # spread(minmax, value) %>%
  # left_join(study_refs)


View(sb_deets)

sb_zero <- sb_deets %>%
  filter(minmax == "min") %>%
  filter(value == 0) %>% left_join(study_refs)

sb_zero
nrow(sb_zero)
View(sb_zero)
# 15 rows, data points

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

View(sb_minmax)

 min_max <- sb_minmax #%>% filter(!n > 1)
  
print(min_max)

View(min_max)
 head(min_max)
colnames(sb_prep)

min_max %>% select(name) %>% distinct()

responses <- min_max %>% filter(name %in% c( "Seed_density_m2", "Total_Seeds", "Total_Species",
                                             "ratio_seeds_species", "Total_Sample_Area_m2" ,
                                             "Total_Number_Samples", "Number_Sites"
                                             ) ) %>%
  left_join(sb_prep) %>% select(
    Biome_Broad_Hab, Country, studyID, rowID, name, minmax, value, n,
    Total_Species, Seed_density_m2, Total_Seeds, ratio_seeds_species,
    Total_Number_Samples, Number_Sites, Total_Sample_Area_m2, Total_Sample_Volume_m3, ) %>% left_join(study_refs)


colnames(responses)
View(responses)

studies_mmin_multi <- responses %>% filter(n > 1)
 View(studies_mmin_multi)

min_max_count <- responses %>% filter(n == 1) #%>% 
   # select( minmax, value, n) %>%
   # distinct()

View(min_max_count)

View(sb_prep %>% filter(studyID == #"G084",
                        "H059" ))

write.csv(sb_deets,  "sb_details_summary.csv")
write.csv(sb_deets,  "sb_details_biome_summary.csv")

