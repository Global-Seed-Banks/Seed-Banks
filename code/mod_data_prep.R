

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


# get a summary of min and max values
sb_deets <- sb_mod %>% group_by(Biome_Broad_Hab) %>%
summarise(`min-Total_Number_Samples` = min(as.numeric(Total_Number_Samples), na.rm = TRUE),
          `max-Total_Number_Samples` = max(as.numeric(Total_Number_Samples),na.rm = TRUE),
          `min-Number_Sites` = min(as.numeric(Number_Sites), na.rm = TRUE),
          `max-Number_Sites` = max(as.numeric(Number_Sites),na.rm = TRUE),
          `min-Sample_Area_mm2` = min(as.numeric(Sample_Area_mm2),na.rm = TRUE),
          `max-Sample_Area_mm2` = max(as.numeric(Sample_Area_mm2), na.rm = TRUE),
          # `max-Total_Sample_Area_mm2` = max(as.numeric(Total_Sample_Area_mm2), na.rm = TRUE),
          # `min-Total_Sample_Area_mm2` = min(as.numeric(Total_Sample_Area_mm2), na.rm = TRUE),
          `max-Total_Sample_Area_m2` = max(as.numeric(Total_Sample_Area_m2), na.rm = TRUE),
          `min-Total_Sample_Area_m2` = min(as.numeric(Total_Sample_Area_m2), na.rm = TRUE),
          # `max-Centred_log_Total_Sample_Area_m2` = max(as.numeric(Centred_log_Total_Sample_Area_m2), na.rm = TRUE),
          # `min-Centred_log_Total_Sample_Area_m2` = min(as.numeric(Centred_log_Total_Sample_Area_m2), na.rm = TRUE),
          `min-Sample_Volume_mm3` = min(as.numeric(Sample_Volume_mm3), na.rm = TRUE),
          `max-Sample_Volume_mm3` = max(as.numeric(Sample_Volume_mm3),na.rm = TRUE),
          # `min-Total_Sample_Volume_mm3` = min(as.numeric(Total_Sample_Volume_mm3), na.rm = TRUE),
          # `max-Total_Sample_Volume_mm3`= max(as.numeric(Total_Sample_Volume_mm3),na.rm = TRUE),
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
  separate(name, into = c("minmax", "name"), sep="-") %>% spread(minmax, value)

sb_deets

write.csv(sb_deets,  "sb_details_summary.csv")
write.csv(sb_deets,  "sb_details_biome_summary.csv")

# sb_mod %>% distinct(Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)
# 
# sb_mod %>% select(Biome_Broad_Hab, Total_Sample_Area_m2, Total_Seeds, Total_Species) %>%
#   filter(Biome_Broad_Hab == "Tundra") %>% distinct() %>% arrange(desc(Total_Species))
#   
# nrow(sb_mod %>% filter(Total_Seeds == 0))

#setwd(paste0(path2wd, 'Data/'))
write.csv(sb_mod,  "sb_prep.csv")



sb_n_hz <- sb_calc %>%  filter(!is.na(Total_Species)) %>%
          group_by(Biome_WWF_Zone, Habitat_Broad ) %>%
  summarise(n_dat = n_distinct(rowID))

sb_n_hz
head(sb_n_hz)


ggplot( data = sb_n_hz,
        aes(x = Habitat_Broad, y = n_dat,
        color = Habitat_Broad)) + 
  facet_wrap(~Biome_WWF_Zone, scales= "free") +
  geom_bar(data = sb_n_hz ,
             aes(x = Habitat_Broad, y = n_dat,
                 ),  fill= "white", stat = "identity", position= 'dodge') +
  scale_color_viridis(discrete = T, option="D")  +
  theme_classic( ) + labs(y ="Number of data Points", x= "Habitat",
                          subtitle = "Total Species") +
  geom_text(aes(label=n_dat), position=position_dodge(width=0.9), vjust=-0.25)



sb_n_h <- sb_calc %>%  filter(!is.na(Total_Species)) %>%
  group_by( Habitat_Broad ) %>%
  summarise(n_dat = n_distinct(rowID))

sb_n_h
head(sb_n_h)


ggplot( data = sb_n_h,
        aes(x = Habitat_Broad, y = n_dat,
            color = Habitat_Broad)) + 
  #facet_wrap(~Biome_WWF_Zone, scales= "free") +
  geom_bar(data = sb_n_h ,
           aes(x = Habitat_Broad, y = n_dat,
           ),  fill= "white", stat = "identity", position= 'dodge') +
  scale_color_viridis(discrete = T, option="D")  +
  theme_classic( ) + labs(y ="Number of data Points", x= "Habitat",
                          subtitle = "Total Species") +
  geom_text(aes(label=n_dat), position=position_dodge(width=0.9), vjust=-0.25)


sb_n_z <- sb_calc %>%  filter(!is.na(Total_Species)) %>%
  group_by( Biome_WWF_Zone ) %>%
  summarise(n_dat = n_distinct(rowID))

sb_n_z
head(sb_n_z)


ggplot( data = sb_n_z,
        aes(x = Biome_WWF_Zone, y = n_dat,
            color = Biome_WWF_Zone)) + 
  #facet_wrap(~Biome_WWF_Zone, scales= "free") +
  geom_bar(data = sb_n_z ,
           aes(x = Biome_WWF_Zone, y = n_dat,
           ),  fill= "white", stat = "identity", position= 'dodge') +
  scale_color_viridis(discrete = T, option="D")  +
  theme_classic( ) + labs(y ="Number of data Points", x= "Habitat",
                          subtitle = "Total Species") +
  geom_text(aes(label=n_dat), position=position_dodge(width=0.9), vjust=-0.25)


# first explore data
# 3 possible metrics for continuous 'sample effort'
colnames(sb)

ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales= "free") +
  geom_point(data = sb_calc ,#%>% filter(Biome_WWF_Zone == "Boreal"),
             aes(x = Total_Sample_Volume_mm3, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_classic( ) 


ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales = "free") +
  geom_point(data = sb_calc , #%>% filter(Biome_WWF_Zone == "Boreal"),
             aes(x = Total_Number_Samples, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  scale_color_viridis(discrete = T, option="D")  +
  #xlim(0,1000)
  coord_cartesian(xlim = c(min(sb_prep_r$Total_Number_Samples), quantile(sb_prep_r$Total_Number_Samples, 0.90)))+
  theme_classic( ) 

ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales = "free") +
  geom_point(data = sb_calc %>%  group_by(Biome_WWF_Zone) %>% filter(Total_Sample_Area_mm2 > quantile(Total_Sample_Area_mm2, probs=0.025, na.rm = TRUE),
                                        Total_Sample_Area_mm2 < quantile(Total_Sample_Area_mm2, probs=0.975, na.rm = TRUE)),
             aes(x = Total_Sample_Area_m2, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1) +
  scale_color_viridis(discrete = T, option="D")  +
  #coord_cartesian(xlim = c(min(sb_calc$Total_Sample_Area_m2, na.rm = TRUE), quantile(sb_calc$Total_Sample_Area_m2, 0.97, na.rm = TRUE)))+
  theme_classic( ) 

