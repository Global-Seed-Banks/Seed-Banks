


#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2),
                                   Number_Sites == 1 
) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)


head(sb_prep)
summary(sb_prep)
# remove NA values 
sb_density_area <- sb_prep %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))  

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')
load( 'density_m2.Rdata')
load( 'ratio.Rdata')

head(sb_rich_area)

# not working? reload r- lapply and sf fuck this up
# richness/m2 for 1 site predicted values
rich_m2_predict <- tidyr::crossing( 
  Number_Sites = c( 1 ),
  sb_rich_area %>% group_by(Biome_Broad_Hab) %>%  
    summarise(Total_Sample_Area_m2 = c( seq( 1, length.out = 1) ) ), 
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
  mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, ndraws = 2000,  
                                                allow_new_levels = TRUE, 
                                                re_formula =  NA))) 

# ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2 + Centred_log_Number_Sites)
# posterior_epred

rich_m2_predict_df <- rich_m2_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted_rich_m2 = .prediction) %>%
  select(-c(.prediction, .chain, .iteration, .row)) %>% ungroup()

head(rich_m2_predict_df)
nrow(rich_m2_predict_df)

# density m2- lazy version here- i should probs copy what i did for richness
head(sb_density_area)

density_predict <- sb_density_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(biome_mean_Seed_density_m2 =  mean(Seed_density_m2)) %>%
  nest(data = c(Biome_Broad_Hab, biome_mean_Seed_density_m2) ) %>%
  mutate(predicted = map(data, ~predicted_draws(density_m2, newdata= .x, ndraws = 2000, re_formula = NA ))) 

head(density_predict)

density_predict_df  <- density_predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  mutate( predicted_density_m2 = .prediction) %>%
  select(-c(.prediction, .row, .chain, .iteration))  %>%
  select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  ungroup() 

nrow(density_predict_df)
head(density_predict_df)
View(density_predict_df)

ratio_m2_df <- rich_m2_predict_df %>% left_join(density_predict_df) %>%
  mutate( ratio_m2 = (predicted_density_m2/predicted_rich_m2))

nrow(ratio_m2_df)
head(ratio_m2_df)
View(ratio_m2_df)

ratio_m2 <- ratio_m2_df %>%
  select(c(Biome_Broad_Hab, ratio_m2)) %>%
  group_by(Biome_Broad_Hab, ) %>%
  mutate( ratio_Estimate = mean(ratio_m2, na.rm =TRUE ),
          `ratio_Upper CI` = quantile(ratio_m2, probs=0.975, na.rm =TRUE ),
          `ratio_Lower CI` = quantile(ratio_m2, probs=0.025, na.rm =TRUE ),
  ) %>% 
  select(-c(ratio_m2 )) %>% distinct() %>% ungroup()

nrow(ratio_m2)
head(ratio_m2)

ratio_m2_total_mean <- ratio_m2_df %>%
  select(c(ratio_m2)) %>%
  mutate( P_Estimate = mean(ratio_m2, na.rm =TRUE ),
          `P_Estimate_Upper` = quantile(ratio_m2, probs=0.975, na.rm =TRUE ),
          `P_Estimate_Lower` = quantile(ratio_m2, probs=0.025, na.rm =TRUE ),
  ) %>% 
  select(-c(ratio_m2 )) %>% distinct() %>% ungroup()

head(ratio_m2_total_mean)

setwd(paste0(path2wd, 'Data/'))
write.csv(ratio_m2,  "ratio_m2_estimates.csv")

ratio_m2_est <- read.csv(paste0(path2wd, 'Data/ratio_m2_estimates.csv'))

ratio_m2 <- ratio_m2 %>% mutate(`Biome_Broad_Hab` = fct_relevel(`Biome_Broad_Hab`,
                                                                                  "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                                  "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                                  "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                                  "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                                  "Aquatic", "Arable"
))


ratio_biome_broad_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = ratio_m2_total_mean,
             aes(yintercept = P_Estimate), size = 0.45) +
  geom_rect(data = ratio_m2_total_mean,
            aes(xmin = -Inf, xmax = Inf,
                ymin = P_Estimate_Lower, ymax =  P_Estimate_Upper ),
            alpha = 0.05) +
  # geom_point(data = sb_ratio,
  #            aes(x = Biome_Broad_Hab, y = ratio_seeds_species, #colour = 	"#C0C0C0"
  #                colour = Biome_Broad_Hab
  #            ),
  #            size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = ratio_m2,
             aes(x =  Biome_Broad_Hab, y = ratio_Estimate, colour =  Biome_Broad_Hab), size = 3) +
  geom_errorbar(data = ratio_m2,
                aes(x =  Biome_Broad_Hab, ymin = `ratio_Lower CI`, ymax = `ratio_Upper CI`, colour =  Biome_Broad_Hab),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Predicted ratio (Seeds/Species)')) ,
       subtitle=  expression(paste('Predicted ratio (seeds/species)'))  ) + 
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  #ylim(0,100000)+
  coord_cartesian( ylim = c(0,1000)) +
  scale_y_continuous(breaks=c(0,50,100,200, 250, 300,500, 800, 1000))+
  theme_bw(base_size=20)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


ratio_biome_broad_Fig
#8.50X18