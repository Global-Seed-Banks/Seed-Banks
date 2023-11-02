
#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox//GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)
head(sb_prep)

# remove NA values 
sb_seed_area <- sb_prep %>% filter(!is.na(Total_Seeds),
                                   # !Total_Seeds == 0,
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_WWF_Broad = as.factor(Biome_WWF_Broad),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_seed_area)



sb_seed_area %>% distinct(Biome_Broad_Hab, Habitat_Degraded, Method, studyID) %>% arrange(Biome_Broad_Hab, Habitat_Degraded, Method, studyID)


head(sb_seed_area %>% distinct(Total_Seeds) %>% arrange(Total_Seeds))
head(sb_seed_area %>% distinct(Total_Species) %>% arrange(Total_Species))
head(sb_seed_area %>% distinct(Seed_density_m2) %>% arrange(Seed_density_m2))

head(sb_seed_area %>% distinct(Total_Seeds, Total_Species, Seed_density_m2) %>% arrange(Seed_density_m2))

nrow(sb_seed_area %>% filter(Total_Seeds == 0) )
nrow(sb_seed_area %>% filter(Seed_density_m2 == 0) )

nrow(sb_seed_area)

4/2569

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'seed_m2.Rdata') # jan folder 

summary(seeds_m2)

sb_seed_area %>% filter( Total_Sample_Area_m2 == 1.000000)

seed_biome_predict <- sb_seed_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Total_Sample_Area_m2 = seq(1.000000, length.out = 10 ),
            Centred_log_Total_Sample_Area_m2 =  seq(1.073539, length.out = 10) ) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, #Total_Sample_Area_mm2
  )) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(seeds_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 

View(seed_biome_predict)

seed_biome_predict_df <- seed_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup()

View(head(seed_biome_predict_df))
head(seed_biome_predict_df, n = 30)

setwd(paste0(path2wd, 'Data/'))
write.csv(seed_biome_predict_df,  "seed_biome_predict_df.csv")

seed_biome_predict_df <- read.csv(paste0(path2wd, 'Data/seed_biome_predict_df.csv'))


seed_predicted_density <- seed_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter( Total_Sample_Area_m2 == 1.000000  ) %>% 
  group_by(Biome_Broad_Hab) %>%
  filter(
    predicted > quantile(predicted, probs=0.025),
    predicted < quantile(predicted, probs=0.975),
  ) %>% sample_n(1000)  %>%
  mutate(m2_scale = Total_Sample_Area_m2,
         predicted_seeds = predicted) %>%
  select(-c(Total_Sample_Area_m2, predicted, X ))

nrow(seed_predicted_density)
View(seed_predicted_density)

head(seed_biome_predict_df)

p_density.total.mean <- seed_biome_predict_df %>%
  filter(Total_Sample_Area_m2 == 1) %>%
  select(-.draw) %>%
  select(-c(Biome_Broad_Hab_group, X, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab,
            Total_Sample_Area_m2, .row, .chain, .iteration)) %>%
  ungroup() %>%
  #group_by(Seed_density_m2) %>%
  mutate( P_Estimate = mean(predicted),
          P_Estimate_lower = quantile(predicted, probs = 0.025),
          P_Estimate_upper = quantile(predicted, probs = 0.975) ) %>% 
  select(-predicted) %>% distinct()

head(p_density.total.mean)


predicted_density <- seed_predicted_density %>%
  group_by(Biome_Broad_Hab) %>%
  mutate( d_Estimate = mean(predicted_seeds, na.rm =TRUE ),
          `d_Upper CI` = quantile(predicted_seeds, probs=0.975, na.rm =TRUE ),
          `d_Lower CI` = quantile(predicted_seeds, probs=0.025, na.rm =TRUE ),
  ) %>% 
  select(-c(predicted_seeds)) %>% distinct() %>% ungroup()

setwd(paste0(path2wd, 'Data/'))
write.csv(predicted_density,  "predicted_density.csv")

predicted_density<- read.csv(paste0(path2wd, 'Data/predicted_density.csv'))

head(predicted_density)

predicted_density <- predicted_density %>% mutate(`Biome_Broad_Hab` = fct_relevel(`Biome_Broad_Hab`,
                                                                                                "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                                                "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                                                "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                                                "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                                                "Aquatic", "Arable"
))

figure_s7 <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = p_density.total.mean,
             aes(yintercept = P_Estimate), size = 0.45) +
  geom_rect(data = p_density.total.mean,
            aes(xmin = -Inf, xmax = Inf,
                ymin = P_Estimate_lower, ymax =  P_Estimate_upper ),
            alpha = 0.05) +
  geom_point(data = predicted_density,
             aes(x =  `Biome_Broad_Hab`, y = d_Estimate, colour =  `Biome_Broad_Hab`), size = 3) +
  geom_errorbar(data = predicted_density,
                aes(x = `Biome_Broad_Hab`, ymin = `d_Lower.CI`, ymax = `d_Upper.CI`, colour =  `Biome_Broad_Hab`),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Predicted seed density (',m^2,')')),
       subtitle= '') +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  #ylim(0,100000)+
  coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


figure_s7
# Landscape 8.50 X 16
