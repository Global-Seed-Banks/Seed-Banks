


rm(list = ls())


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
                                   #Number_sites == 1 
) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome = as.factor(Biome),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)
sb_rich_area %>% select(Method) %>% distinct()



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'rich_aq.Rdata')
load( 'rich_ar.Rdata')
load( 'rich_forest.Rdata')
load( 'rich_grass.Rdata')
load( 'rich_med_de.Rdata')
load( 'rich_po_alp.Rdata')
load( 'rich_wetland.Rdata')

sb_forest_r <- sb %>% 
  filter(!is.na(Total_species),
         # !Total_species == 0,
         !is.na(Centred_log_total_sample_area_m2) #,
         #Number_sites == 1 
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome = as.factor(Biome),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome) %>%
  filter(Realm == "Forest") 

forest_predict <- tidyr::crossing( 
  Number_sites = c(1, 20, 100),
  sb_forest_r %>% group_by(Biome, Habitat_degraded) %>%  
    dplyr::summarise(Total_sample_area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
)  %>%
  mutate( log_number_sites = log(Number_sites),
          log_total_sample_area_m2 = log(Total_sample_area_m2),
          Centred_log_number_sites = log_number_sites - mean(log_number_sites, na.rm = TRUE),
          Centred_log_total_sample_area_m2 = log_total_sample_area_m2 - mean(log_total_sample_area_m2, na.rm = TRUE) ) %>%
  select(-c( log_number_sites, log_total_sample_area_m2 ) ) %>%
  arrange( Total_sample_area_m2, Number_sites ) %>%
  mutate(Biome_group = Biome) %>%
  group_by(Biome_group, Biome ) %>%
  nest(data = c(Biome, Habitat_degraded, Centred_log_total_sample_area_m2, Total_sample_area_m2, Centred_log_number_sites, Number_sites)) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(mod_forest_r, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 

head(forest_predict)

forest_predict_df <- forest_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
  mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup() %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

head(forest_predict_df)
View(forest_predict_df)
forest_predict_df %>% select(Total_sample_area_m2) %>% distinct()


ggplot() +
  geom_hline(yintercept = 0,linetype="longdash") +
stat_halfeye(data = forest_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 0.01  ) ,
             aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded, shape=Habitat_degraded),
  point_interval = mean_qi,  .width = c(0.50, 0.9), 
  alpha=0.4, position = position_dodge(width = 1)) +
  stat_halfeye(data = forest_predict_df %>% filter(Number_sites == 1, Total_sample_area_m2 == 15.00  ) ,
               aes(x = Biome , y = predicted,  fill= Biome, group = Habitat_degraded,
                   shape= Habitat_degraded,
                   ),
               point_interval = mean_qi,  .width = c(0.50, 0.9), point_size=5, 
               alpha=0.4, position = position_dodge(width = 1)) +
  scale_fill_manual( values= c(  "#1e3d14",   "#788f33", "#228B22"
  )) +   coord_cartesian( ylim = c(0,60)) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") 



setwd(paste0(path2wd, 'Data/'))
write.csv(forest_predict_df,  "forest_predict_df.csv")

forest_predict_df <- read.csv(paste0(path2wd, 'Data/forest_predict_df.csv'))

head(forest_predict_df)
colnames(forest_predict_df)
View(forest_predict_df %>% distinct(Total_sample_area_m2))

# 0.010000
# 15.000000

nrow(forest_predict_df)
head(forest_predict_df)

forest_a <- forest_predict_df %>%
  select(-c( .row, .chain, .iteration, .draw, Centred_log_total_sample_area_m2, Biome_group)) %>%
  filter( Total_sample_area_m2 == 0.010000   ) %>% 
  mutate(samp_scale = Total_sample_area_m2,
         a_predicted = predicted) %>%
  select(-c(Total_sample_area_m2, predicted, X )) %>%
  dplyr::group_by(Biome, Number_sites, Habitat_degraded) %>%
  mutate( Estimate = round( mean(a_predicted, na.rm =TRUE ) ,0),
          `Upper_CI` = quantile(a_predicted, probs=0.975, na.rm =TRUE ),
          `Lower_CI` = quantile(a_predicted, probs=0.025, na.rm =TRUE )
  ) %>%  select(-c(a_predicted)) %>% distinct() %>% ungroup() %>%
  mutate(scale= "alpha")

nrow(forest_a)
print(forest_a)

forest_g <- forest_predict_df %>%
  select(-c(.row, .chain, .iteration, .draw ,Centred_log_total_sample_area_m2, Biome_group)) %>%
  filter(  Total_sample_area_m2 ==  15.000000   ) %>% 
  mutate(samp_scale = Total_sample_area_m2,
         g_predicted = predicted) %>%
  select(-c(Total_sample_area_m2, predicted, X)) %>%
  dplyr::group_by(Biome, Number_sites, Habitat_degraded) %>%
  mutate( 
          Estimate = round( mean(g_predicted, na.rm =TRUE ) , 0 ),
          `Upper_CI` = quantile(g_predicted, probs=0.975, na.rm =TRUE ),
          `Lower_CI` = round( quantile(g_predicted, probs=0.025, na.rm =TRUE ), 0),
  ) %>% 
  select(-c( g_predicted)) %>% distinct() %>% ungroup() %>%
  mutate(scale= "gamma")


head(forest_g)
nrow(forest_g)

forest_scales <- forest_a %>% bind_rows(forest_g) 

nrow(forest_scales)
head(forest_scales)

setwd(paste0(path2wd, 'Data/'))
write.csv(forest_scales,  "forest_scales.csv")


forest_scales <- read.csv(paste0(path2wd, 'Data/forest_scales.csv'))


forest_div <- forest_scales %>% 
  filter( Number_sites == "1")  %>%
  mutate( Habitat_degraded = as.factor(Habitat_degraded))


figure_4 <- ggplot(data = forest_div,
                   aes(x = Biome , y = Estimate, colour = Biome, 
                       shape=scale,
                       group = Habitat_degraded,  
                   linetype= Habitat_degraded),) + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = forest_div,
             aes(x = Biome , y = Estimate, colour = Biome, 
                 shape=scale, fill=Biome,
                 group = Habitat_degraded,  
             ), alpha=0.7,
             position = position_dodge(width = 0.75), size = 5) +
  geom_errorbar(data = forest_div,
                aes(x = Biome , ymin = `Lower_CI`, ymax =  `Upper_CI`, 
                    colour =  Biome,
                    group = Habitat_degraded
                ),alpha=0.7,
                position = position_dodge(width = 0.75),
                linewidth = 2, width = 0) +
  scale_color_manual( values= c(  "#1e3d14",   "#788f33",   "#228B22" ))+
  scale_fill_manual( values= c(  "#1e3d14",   "#788f33",   "#228B22" ))+
  scale_linetype_manual(values = c( "solid", "dotted")) +
  scale_shape_manual(values = c(  23, 5) ) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") + 
  coord_cartesian( ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle( (expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))) )+
  ylab( (expression(paste('Average ', italic(alpha), '-richness ',sep = ''))) ) +
  labs(subtitle= "a)" )+
  guides(col = guide_legend(ncol = 3))


figure_4


figure_4_b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = forest_div,
             aes(x = Biome , y = g_Estimate, colour = Biome, 
                 shape=Habitat_degraded,
                 group = Habitat_degraded,   
             ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = forest_div,
                aes(x = Biome, g_Estimate  , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = Biome,  #Biome,  
                    #   aes(x = reorder(Biome, g_Estimate ) , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = g_Estimate,  #Biome, 
                    group = Habitat_degraded
                ),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual( values= c(  "#1e3d14",   "#788f33",   "#228B22" ))+
  #scale_color_viridis(discrete = F, option="plasma", limits = c(10, 40) )  +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") + 
  coord_cartesian( ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
  labs(subtitle= "b)" )+
  guides(col = guide_legend(ncol = 3))


figure_4_b

# map objects created in abg_map.R
# LANDSCAPES 16 X 32
figure_4 <- (figure_4_a + figure_4_b) 

figure_4

