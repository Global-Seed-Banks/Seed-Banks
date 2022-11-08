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
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

# remove NA values 
sb_biome_area <- sb_prep %>% # filter(!is.na(Total_Species),
  #    !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))



setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')
load( 'seed_m2.Rdata')



summary(sb_biome_area)

sb_biome_area_prep <- sb_biome_area %>% distinct(Total_Sample_Area_mm2, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
  mutate(Total_Sample_Area_mm2 = round(Total_Sample_Area_mm2, 2),
         Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 6),
         Centred_log_Total_Sample_Area_m2 = round(Centred_log_Total_Sample_Area_m2, 6),
         ) %>% arrange(Total_Sample_Area_m2) #

summary(sb_biome_area_prep)
View(sb_biome_area_prep %>% distinct(Total_Sample_Area_m2) %>%
       filter( Total_Sample_Area_m2 >=8.210905 &  Total_Sample_Area_m2 <16) )

sb_biome_area_prep %>%
  summarise(Total_Sample_Area_m2 = seq(0.010000, 12.311357, length.out = 10 ) ) 
              
sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 0.010000)
sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 15.000000)


rich_biome_predict <- sb_biome_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Total_Sample_Area_m2 = seq(0.010000, 15.000000, length.out = 10 ),
            #Total_Sample_Area_mm2 = seq(1500, 15.000000, length.out = 10),
            Centred_log_Total_Sample_Area_m2 =  seq(-3.554035, 3.759185, length.out = 10) ) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, #Total_Sample_Area_mm2
                )) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 


head(rich_biome_predict)

rich_biome_predict_df <- rich_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
 mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup()

View(head(rich_biome_predict_df))

head(rich_biome_predict_df)
colnames(rich_biome_predict_df)
View(rich_biome_predict_df %>% distinct(Total_Sample_Area_m2))

# 0.010000
# 15.000000

rich_biome_a <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter( Total_Sample_Area_m2 == 0.010000   ) %>% # Yang et al
  mutate(a_samp_scale = Total_Sample_Area_m2,
         a_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted ))
  

rich_biome_g <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter(  Total_Sample_Area_m2 ==  15.000000   ) %>% # arbitrary gamma scale
  mutate(g_samp_scale = Total_Sample_Area_m2,
         g_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted ))


rich_biome_scales <- rich_biome_a %>% left_join(rich_biome_g) %>%
  mutate(b_predicted = (g_predicted/a_predicted))
  
head(rich_biome_scales)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_scales,  "sb_av_div_scales.csv")



rich_biome_div <- rich_biome_scales %>%
  group_by(Biome_Broad_Hab_group, Centred_log_Total_Sample_Area_m2, #Total_Sample_Area_mm2,
           Total_Sample_Area_m2) %>%
  mutate( Estimate = mean(predicted, na.rm =TRUE ),
          `Upper CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `Lower CI` = quantile(predicted, probs=0.025, na.rm =TRUE )
  ) %>% 
  select(-predicted) %>% distinct() %>% ungroup() %>%
  mutate(scale = "alpha")

rich_biome_fit_g <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration)) %>%
  filter( Total_Sample_Area_m2 == 15.000000) %>%
  group_by(Biome_Broad_Hab_group, Centred_log_Total_Sample_Area_m2, #Total_Sample_Area_mm2,
           Total_Sample_Area_m2) %>%
  mutate( Estimate = mean(predicted, na.rm =TRUE ),
          `Upper CI` = quantile(predicted, probs=0.975, na.rm =TRUE ),
          `Lower CI` = quantile(predicted, probs=0.025, na.rm =TRUE )
  ) %>% 
  select(-predicted) %>% distinct() %>% ungroup() %>%
  mutate(scale = "gamma")

rich_biome_fit <- rich_biome_fit_a %>% bind_rows(rich_biome_fit_g)


View(rich_biome_fit)
setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')

setwd(paste0(path2wd, 'Tables/'))
write.csv(rich_biome_fit.mm, "table_4.csv")

rich_biome_i <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  facet_wrap(.~scale) +
  geom_point(data = rich_biome_fit,
             aes(x = Biome_Broad_Hab , y = Estimate, colour = Biome_Broad_Hab), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_fit,
                aes(x = Biome_Broad_Hab , ymin = `Lower CI`, ymax =  `Upper CI`, colour = Biome_Broad_Hab),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  # scale_shape_manual(name = "Average total species",
  #                    values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
 # coord_cartesian( ylim = c(0,150)) +
  labs(x='',
       y = 'Average species richness in the soil seed bank',
       ) 


rich_biome_i


fit_a <- rich_biome_fit_a %>% select(Biome_Broad_Hab, Estimate,  `Lower CI` , `Upper CI`) %>%
  mutate(Estimate_a = Estimate,
         `Lower CI a` = `Lower CI`,
         `Upper CI a` = `Upper CI`
  ) %>% select(-c(Estimate,  `Lower CI` , `Upper CI`)) 

fit_g <- rich_biome_fit_g %>% select(Estimate,  `Lower CI` , `Upper CI`) %>%
  mutate(Estimate_g = Estimate,
         `Lower CI g` = `Lower CI`,
         `Upper CI g` = `Upper CI`
  )  %>% select(-c(Estimate,  `Lower CI` , `Upper CI`)) 


fit_b <- fit_a %>% bind_cols(fit_g) %>%
  mutate( Estimate_b = Estimate_g/Estimate_a,
          `Lower CI b` = `Lower CI g`/ `Lower CI a`,
          `Upper CI b` = `Upper CI g`/`Upper CI a`,
          )


rich_biome_b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  #facet_wrap(.~scale) +
  geom_point(data = fit_b,
             aes(x = Biome_Broad_Hab , y = Estimate_b, colour = Biome_Broad_Hab), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = fit_b,
                aes(x = Biome_Broad_Hab , ymin = `Lower CI b`, ymax =  `Upper CI b`, colour = Biome_Broad_Hab),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  # scale_shape_manual(name = "Average total species",
  #                    values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  #coord_cartesian( ylim = c(0,150)) +
  labs(x='',
       y = 'Average species richness in the soil seed bank',
       title = 'beta') 


rich_biome_b

rich_biome_i / rich_biome_b
