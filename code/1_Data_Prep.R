

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

sb <- read.csv(paste0(path2wd, 'Data paper stuff/Database checking/sb_pub.csv'))

# remove rows with NA across all 4 metrics
sb<-sb[!is.na(sb$Total_seeds) | !is.na(sb$Total_species) | !is.na(sb$Seed_density_m2) | !is.na(sb$Seed_density_litre),]

head(sb)
colnames(sb)
summary(sb)

# table s1 numbers
nrow(sb) # n records

nrow( sb %>% select( # n studies
  StudyID
) %>% distinct() )

nrow( sb %>% select( # n unique locations
  StudyID,
  Lat_deg ,  Lon_deg
) %>% distinct() )

head(sb)

# quantify metrics we want
sb_calc <- sb %>% mutate( log_total_seeds = log(Total_seeds),
                          log_total_species = log(Total_species),
                          ratio_seeds_species = (Total_seeds / Total_species),
                          Total_sample_volume_mm3 = (Total_number_samples * Sample_volume_mm3),
                          Total_sample_area_mm2 = (Total_number_samples * Sample_area_mm2),
                          log_total_number_samples = log(Total_number_samples),
                          log_number_sites = log(Number_sites),
                          Total_sample_area_m2 = (Total_sample_area_mm2 / 1000000),
                          Total_sample_volume_m3 = (Total_sample_volume_mm3 / 1000000000),
                          log_total_sample_area_m2 = log(Total_sample_area_m2),
                          Centred_total_number_samples = Total_number_samples - mean(Total_number_samples, na.rm = TRUE),
                          Centred_number_sites = Number_sites - mean(Number_sites, na.rm = TRUE),
                          Centred_log_total_number_samples = log_total_number_samples - mean(log_total_number_samples, na.rm = TRUE),
                          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
                          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE),
) 

head(sb_calc)
summary(sb_calc)

# write over biomes when habitat is arable or aquatic
sb_mod <- sb_calc %>% 
  mutate(Biome_broad_hab = case_when(Habitat_broad %in% c("Arable", "Aquatic") ~ Habitat_broad ,
            
                                    TRUE ~ Biome_broad)) %>%
  mutate(Lat_deg_abs = abs(Lat_deg)) %>%
  mutate(Biome = case_when(grepl("Deserts", Biome_broad) ~ "Deserts",
                           grepl("Mediterranean", Biome_broad) ~ "Mediterranean",
                           grepl("Tropical", Biome_broad) ~ "Tropical",
                           Biome_zone == "Mediterranean and Desert" & grepl("Montane", Biome_broad_hab) ~ "Mediterranean",
                           TRUE ~ Biome_zone)) %>% 
  mutate(Realm = case_when(
    Habitat_broad == "Forest" & Biome_zone ==  "Mediterranean and Desert" ~ Biome_zone,
    Habitat_broad == "Grassland" & Biome == "Tropical" ~ "Grassland", 
    Habitat_broad == "Grassland" & Biome_zone ==  "Mediterranean and Desert" ~  Biome_zone,
    Biome_zone ==  "Tundra" ~  Biome_zone,
    Habitat_broad == "Grassland" & Biome_zone ==  "Boreal" ~  "Tundra",
    TRUE ~ Habitat_broad)) %>% arrange(Realm, Biome) 

head(sb_mod)


sb_mod %>% select(Realm, Biome) %>% distinct() %>% arrange(Realm, Biome) 

sb_mod %>% select(Biome_broad, Habitat_broad) %>% distinct()
sb_mod %>% select(Habitat_broad, Biome_zone, Biome_broad) %>% distinct() %>% arrange(Habitat_broad, Biome_zone, Biome_broad) 

sb_mod %>% filter(Biome_broad_hab == "Aquatic") %>%
  #filter(Realm == "Arable") %>%
  #filter(Realm == "Forest") %>%
   #filter(Realm == "Grassland") %>%
  #filter(Realm == "Mediterranean and Desert") %>%
  # filter(Realm == "Wetland") %>%
  select(Habitat_broad, Biome_zone, Biome_broad_hab, Biome_broad, Realm, Biome, Habitat_degraded) %>% #distinct() %>% 
  arrange(Habitat_broad, Biome_zone, Biome_broad_hab, Biome_broad, Realm, Biome, Habitat_degraded) %>%
  arrange(Realm, Biome, Habitat_degraded) %>% 
  group_by(Realm,
           #Biome, 
           Habitat_degraded
           ) %>% 
  summarise(n = n()) #%>%
  # mutate(Biome2 = case_when(grepl("Deserts", Biome) ~ "Mediterranean and Desert",
  #                          grepl("Temperate", Biome) ~ "Temperate and Boreal",
  #                          grepl("Boreal", Biome) ~ "Temperate and Boreal",
  #                          grepl("Mediterranean", Biome) ~ "Mediterranean and Desert", TRUE ~ Biome))
  


colnames(sb_mod)

aq <- sb_mod %>% filter(Biome_broad_hab == "Aquatic") %>% select(StudyID, Title, Doi, Country, Realm, Biome, Biome_broad) %>% distinct()
aq  
write.csv(aq,  "aqua_deets.csv")

  # filter(Biome_broad_hab == "Aquatic") %>%
  # mutate(Biome = case_when(grepl("Deserts", Biome_broad) ~ "Deserts",
  #                          grepl("Temperate", Biome_broad) ~ "Temperate",
  #                          grepl("Mediterranean", Biome_broad) ~ "Mediterranean",
  #                          grepl("Tropical", Biome_broad, ignore.case = TRUE) ~"Tropical"))
  
  # filter(Biome_broad_hab == "Arable") %>%
  # mutate(Biome = case_when(grepl("Deserts", Biome_broad) ~ "Deserts",
  #                          grepl("Temperate", Biome_broad) ~ "Temperate",
  #                          grepl("Mediterranean", Biome_broad) ~ "Mediterranean",
  #                          grepl("Boreal", Biome_broad) ~ "Boreal",
  #                          grepl("Montane", Biome_broad) ~ "Tropical",
  #                          grepl("Tropical", Biome_broad, ignore.case = TRUE) ~"Tropical"))
  
  # filter(Habitat_broad == "Forest") %>%
  # filter(!Biome_zone %in% c( "Tundra", "Mediterranean and Desert") )

  # filter(Habitat_broad == "Grassland") %>%
  # filter(!Biome_zone %in% c( "Tundra",  "Mediterranean and Desert", "Boreal"))

  
# filter(Biome_zone == "Mediterranean and Desert") %>%
# filter(Habitat_broad %in% c( "Grassland", "Forest") )%>%
# mutate(Biome = case_when(grepl("Deserts", Biome_broad_hab) ~ "Deserts",
#                          grepl("Mediterranean", Biome_broad_hab) ~ "Mediterranean",
#                          grepl("Montane", Biome_broad_hab) ~ "Mediterranean",
#                          grepl("Tropical", Biome_broad_hab) ~ "Mediterranean",
#                          ))


# filter(Habitat_broad == "Grassland") %>%
#   filter(Biome_zone %in% c( "Tundra", "Boreal"))

# filter(Habitat_broad == "Wetland") %>%
# mutate(Biome = case_when(
#                          grepl("Deserts", Biome_broad_hab) ~ "Deserts",
#                          grepl("Mediterranean", Biome_broad_hab) ~ "Mediterranean",
#                          grepl("Mediterranean", Biome_zone) & grepl("Tropical", Biome_broad_hab) ~ "Mediterranean",
# )) %>%
#   mutate(Biome = case_when(
#    is.na(Biome) ~ Biome_zone,
#    TRUE ~ Biome
#   ))



write.csv(sb_mod,  "sb_prep.csv")


# reload new dat and get some summaries for methods/ tables etc whatever
sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))


head(sb_prep)
colnames(sb_prep)
summary(sb_prep)

# table s1 numbers
nrow(sb_prep) # n records

nrow( sb_prep %>% select( # n studies
  studyID
) %>% distinct() )


head(sb)


sb_prep %>% select(Biome_broad, Habitat_broad, Biome_broad_hab, Habitat_degraded, StudyID, RowID) %>% distinct() %>% 
  arrange(Biome_broad, Habitat_broad, Biome_broad_hab, Habitat_degraded,  StudyID, RowID)

sb_prep %>% select(Biome_zone, Biome_broad, Habitat_broad, Biome_broad_hab, ) %>% distinct() %>% 
  arrange(Biome_zone, Biome_broad, Habitat_broad, Biome_broad_hab) %>% filter(!Biome_broad_hab == "Arable")  %>% filter(!Biome_broad_hab == "Aquatic")


# table s1
sb_gathered <- sb_prep %>% select(
  rowID, 
  studyID,
  Centred_log_Total_Sample_Area_m2, 
  Biome_Broad_Hab, Number_sites, Total_seeds, Total_species, Seed_density_m2, ratio_seeds_species) %>%
  gather(metric, response, Total_seeds:ratio_seeds_species) %>%
  filter(!is.na(response),
        # !is.na(Centred_log_Total_Sample_Area_m2),
       # !response == 0 ,
        #response == 0 
       ) #%>% #filter(metric == "Seed_density_m2") %>%
   #filter(metric == "ratio_seeds_species")

head(sb_gathered)

# table s1 observations
nrow( sb_gathered %>% filter(!metric == "ratio_seeds_species")) # observations including richness, abundance and density
nrow( sb_gathered %>% filter(metric == "Total_species"))
nrow( sb_gathered %>% filter(metric == "Total_seeds"))
nrow( sb_gathered %>% filter(metric == "Seed_density_m2"))
nrow( sb_gathered %>% filter(metric == "ratio_seeds_species"))

# number of observations used in each model
nrow( sb_gathered %>% filter(metric == "Total_species") %>% filter(!is.na(Centred_log_Total_Sample_Area_m2) ))
nrow( sb_gathered %>% filter(metric == "Total_seeds")  %>% filter(!is.na(Centred_log_Total_Sample_Area_m2)  ))
nrow( sb_gathered %>% filter(metric == "Seed_density_m2")  %>% filter(response != 0 ))
nrow( sb_gathered %>% filter(metric == "ratio_seeds_species")  %>% filter(response != 0 ))

nrow(sb_gathered %>% filter(response == 0 ))
# % 0's 15/8087
round( ((15/8087)* 100) , 2) # 0.19 % of data are zeros


sites_count <- sb_gathered %>% filter(!metric == "ratio_seeds_species") %>%
  select(Number_sites) %>%
  dplyr::group_by(Number_sites) %>%
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
  Biome_Broad_Hab, Number_sites, Total_seeds, Total_species, Seed_density_m2, ratio_seeds_species) %>%
 # filter(Seed_density_m2 != 0, ratio_seeds_species != 0) %>%
  gather(metric, response, Total_seeds:ratio_seeds_species) %>%
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
  filter(metric %in% c("Total_seeds" , "Total_species") ) %>%
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


sb_gathered %>% filter(metric == "Total_species",
                       !is.na(Centred_log_Total_Sample_Area_m2) ) %>%
  mutate( N_site_cats = 
                        case_when( (Number_sites == 1) ~ "1 site",
                                  (Number_sites >= 2 & Number_sites <= 20) ~ "2-20 sites",
                                  (Number_sites >= 21 & Number_sites <= 100) ~ "21-100 sites",
                                  TRUE ~ "> 100 sites" ) ) %>%
  select(N_site_cats) %>%
  dplyr::group_by(N_site_cats) %>%
  count() 
                       

sb_gathered %>% filter(metric == "Total_seeds",
                       !is.na(Centred_log_Total_Sample_Area_m2) ) %>%
  # mutate( N_site_cats = 
  #           case_when( (Number_sites == 1) ~ "1 site",
  #                      (Number_sites >= 2 & Number_sites <= 20) ~ "2-20 sites",
  #                      (Number_sites >= 21 & Number_sites <= 100) ~ "21-100 sites",
  #                      TRUE ~ "> 100 sites" ) ) %>%
  mutate( N_site_cats = 
            case_when( (Number_sites >= 1 & Number_sites <= 19) ~ "1-20 site",
                       (Number_sites >= 20 & Number_sites <= 99 ) ~ "20-99 sites",
                       (Number_sites >= 100 ) ~ "100 and more sites",
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
  summarise(`min-Total_number_samples` = min(as.numeric(Total_number_samples), na.rm = TRUE),
            `max-Total_number_samples` = max(as.numeric(Total_number_samples),na.rm = TRUE),
            `min-Number_sites` = min(as.numeric(Number_sites), na.rm = TRUE),
            `max-Number_sites` = max(as.numeric(Number_sites),na.rm = TRUE),
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
            `min-Total_species` = min(as.numeric(Total_species), na.rm = TRUE),
            `max-Total_species` = max(as.numeric(Total_species),na.rm = TRUE),
            `min-Seed_density_m2` = min(as.numeric(Seed_density_m2), na.rm = TRUE),
            `max-Seed_density_m2` = max(as.numeric(Seed_density_m2),na.rm = TRUE),
            `min-Total_seeds` = min(as.numeric(Total_seeds), na.rm = TRUE),
            `max-Total_seeds` = max(as.numeric(Total_seeds),na.rm = TRUE),
  ) %>%
  pivot_longer(`min-Total_number_samples` : `max-Total_seeds`) %>%
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

responses <- min_max %>% filter(name %in% c( "Seed_density_m2", "Total_seeds", "Total_species",
                                             "ratio_seeds_species", "Total_Sample_Area_m2" ,
                                             "Total_number_samples", "Number_sites"
                                             ) ) %>%
  left_join(sb_prep) %>% select(
    Biome_Broad_Hab, Country, studyID, rowID, name, minmax, value, n,
    Total_species, Seed_density_m2, Total_seeds, ratio_seeds_species,
    Total_number_samples, Number_sites, Total_Sample_Area_m2, Total_Sample_Volume_m3, ) %>% left_join(study_refs)


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

