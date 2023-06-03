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
                                   #Number_Sites == 1 
) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)
sb_rich_area %>% select(Method) %>% distinct()

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')

summary(rich_m2)

head(sb_rich_area)


rich_biome_predict <- tidyr::crossing( 
  Number_Sites = c(1, 20, 100),
  sb_rich_area %>% group_by(Biome_Broad_Hab) %>%  
    dplyr::summarise(Total_Sample_Area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
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
  mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = NA  ))) 


# re_formula = NULL,
# allow_new_levels = TRUE, sample_new_levels = "uncertainty" 

head(rich_biome_predict)

rich_biome_predict_df <- rich_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
 mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup()

head(rich_biome_predict_df)
View(rich_biome_predict_df)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_predict_df,  "rich_biome_predict_df.csv")

rich_biome_predict_df <- read.csv(paste0(path2wd, 'Data/rich_biome_predict_df.csv'))

head(rich_biome_predict_df)
colnames(rich_biome_predict_df)
View(rich_biome_predict_df %>% distinct(Total_Sample_Area_m2))

# 0.010000
# 15.000000

nrow(rich_biome_predict_df)
head(rich_biome_predict_df)

rich_biome_a <- rich_biome_predict_df %>%
  select(-c( .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter( Total_Sample_Area_m2 == 0.010000   ) %>% # Yang et al
  # group_by(Biome_Broad_Hab, Number_Sites) %>%
  #  filter(
  #    predicted > quantile(predicted, probs=0.025),
  #    predicted < quantile(predicted, probs=0.975),
  # ) %>% sample_n(1000)  %>%
  mutate(a_samp_scale = Total_Sample_Area_m2,
         a_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted, X ))

nrow(rich_biome_a)
head(rich_biome_a)

rich_biome_g <- rich_biome_predict_df %>%
  select(-c(.row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter(  Total_Sample_Area_m2 ==  15.000000   ) %>% # arbitrary gamma scale
  #group_by(Biome_Broad_Hab, Number_Sites) %>%
  # filter(
  #   predicted > quantile(predicted, probs=0.025),
  #   predicted < quantile(predicted, probs=0.975),
  # ) %>% sample_n(1000)  %>%
  mutate(g_samp_scale = Total_Sample_Area_m2,
         g_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted, X))

head(rich_biome_g)
nrow(rich_biome_g)

rich_biome_scales <- rich_biome_a %>% left_join(rich_biome_g) %>%
  mutate(b_predicted = (g_predicted/a_predicted))

nrow(rich_biome_scales)
head(rich_biome_scales)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_scales,  "sb_av_div_scales.csv")


rich_biome_scales <- read.csv(paste0(path2wd, 'Data/sb_av_div_scales.csv'))

head(rich_biome_scales)
summary(rich_biome_scales)

rich_biome_div <- rich_biome_scales %>%
  ungroup() %>%
  select(-c(.draw)) %>%
  filter(!is.infinite(b_predicted)) %>%
  dplyr::group_by(Biome_Broad_Hab, Number_Sites) %>%
  mutate( a_Estimate = round( mean(a_predicted, na.rm =TRUE ) ,0),
          `a_Upper CI` = quantile(a_predicted, probs=0.975, na.rm =TRUE ),
          `a_Lower CI` = quantile(a_predicted, probs=0.025, na.rm =TRUE ),
           g_Estimate = round( mean(g_predicted, na.rm =TRUE ) , 0 ),
          `g_Upper CI` = quantile(g_predicted, probs=0.975, na.rm =TRUE ),
          `g_Lower CI` = round( quantile(g_predicted, probs=0.025, na.rm =TRUE ), 0),
           b_Estimate = round( mean(b_predicted, na.rm =TRUE ) , 0 ),
          `b_Upper CI` = round( quantile(b_predicted, probs=0.975, na.rm =TRUE ) ,0),
          `b_Lower CI` = round( quantile(b_predicted, probs=0.025, na.rm =TRUE ), 0),
  ) %>% 
  select(-c(X, a_predicted, g_predicted, b_predicted)) %>% distinct() %>% ungroup()


print(rich_biome_div)
nrow(rich_biome_div)
View(rich_biome_div)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_div,  "sb_av_div_estimates.csv")



rich_biome_div <- read.csv(paste0(path2wd, 'Data/sb_av_div_estimates.csv'))

head(rich_biome_div)

table_prep <- rich_biome_div %>% #mutate(g_Lower.CI = round(g_Lower.CI, 0)) %>%
  unite("a_CI", a_Lower.CI:a_Upper.CI, sep=",") %>%
  unite("g_CI", g_Lower.CI:g_Upper.CI, sep=",") %>%
  mutate(a_CI = paste0("(", a_CI, ")"),
         g_CI = paste0("(", g_CI, ")"), ) %>%
  unite("a_scale", a_Estimate:a_CI, sep=" ") %>%
  unite("g_scale", g_Estimate:g_CI, sep=" ")

a_table <- table_prep %>% select(-g_scale) %>%
  spread(Number_Sites, a_scale)  %>%
  mutate( "a_1" = `1`, "a_20" = `20`, "a_100" = `100`) %>% select(-c("1","20","100"))

a_table

g_table <- table_prep %>% select(-a_scale) %>%
  spread(Number_Sites, g_scale) %>%
  mutate( "g_1" = `1`, "g_20" = `20`, "g_100" = `100`)%>% select(-c("1","20","100"))

g_table


table_6 <- a_table %>% left_join( g_table) %>%
  separate(a_1, c("a_estimate", "a_CI"), sep=" ", remove=F) %>% 
  separate(g_1, c("g_estimate", "g_CI"), sep=" ", remove=F) %>% 
  mutate(a_estimate = as.numeric(a_estimate),
         g_estimate = as.numeric(g_estimate)  ) %>%
  arrange(a_estimate, g_estimate, Biome_Broad_Hab) %>%
  select(-c(a_estimate, g_estimate, a_CI, g_CI))

head(table_6)

setwd(paste0(path2wd, 'Tables/'))
write.csv(table_6, "table_6.csv")

rich_biome_div <- read.csv(paste0(path2wd, 'Data/sb_av_div_estimates.csv'))

 rich_biome_div <- rich_biome_div %>% #left_join(cols) %>% 
   filter( Number_Sites == "1")  %>% 
   mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                                        "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                        "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                        "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                        "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                        "Aquatic", "Arable"
   ))

 head(rich_biome_div)
 
 
 
 # try out a faceted combo color gradient on a log scale
# d_data <- bind_rows( 
#   # a scale
#   rich_biome_div %>% select(Biome_Broad_Hab, a_Estimate, a_Upper.CI, a_Lower.CI) %>%
#    mutate(Estimate = a_Estimate, Upper.CI = a_Upper.CI, Lower.CI= a_Lower.CI) %>% 
#    select(Biome_Broad_Hab, Estimate, Upper.CI, Lower.CI) %>% mutate(scale = "alpha"),
#  # g scale
#   rich_biome_div %>% select(Biome_Broad_Hab,  g_Estimate, g_Upper.CI, g_Lower.CI) %>%
#    mutate(Estimate = g_Estimate, Upper.CI = g_Upper.CI, Lower.CI= g_Lower.CI) %>% 
#    select(Biome_Broad_Hab, Estimate, Upper.CI, Lower.CI)
#    %>% mutate(scale = "gamma") ) %>%
#   # for label parsed
#   mutate( scale = factor(scale,
#                          levels= c("alpha", "gamma"),
#                          labels = c( (expression(paste('a)   ', italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))), 
#                                      (expression(paste('b)   ', italic(gamma), '-scale (15' ,m^2,')', sep = ''))) 
#                          )) )
#  
# View(d_data)

map_breaks <- c(5, 10, 15, 20, 30, 40)

# 
# rich_biome_d <- ggplot() + facet_wrap(~scale, labeller = label_parsed) +
#   geom_hline(yintercept = 0,linetype="longdash") +
#   geom_point(data = d_data,
#              aes(x = Biome_Broad_Hab , y = Estimate, colour = Estimate, #Biome_Broad_Hab,
#                  #aes(x = reorder(Biome_Broad_Hab, a_Estimate ) , y = a_Estimate, colour = a_Estimate, #Biome_Broad_Hab,
#                  #group = Number_Sites,  #shape = Number_Sites
#              ), 
#              position = position_dodge(width = 0.75), size = 3) +
#   geom_errorbar(data = d_data,
#                 aes(x = Biome_Broad_Hab , ymin = `Lower.CI`, ymax =  `Upper.CI`, 
#                     # aes(x = reorder(Biome_Broad_Hab, a_Estimate ) , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, 
#                     colour =  Estimate, #turn off for figure s6
#                     #Biome_Broad_Hab, # turn on for figure s6
#                     #group = Number_Sites
#                 ),
#                 position = position_dodge(width = 0.75),
#                 linewidth = 0.75, width = 0) +
#   #scale_color_viridis(discrete = T, option="D")  +
#   scale_color_viridis(discrete = F, option=  "plasma", #"D",
#                      #  limits = c(0, 40)
#                      trans="log10", breaks= map_breaks, labels= map_breaks
#   )  +
#   theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                                #axis.text.x=element_blank(), 
#                                axis.title.x = element_blank(),
#                                plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
#                                plot.title=element_text(size=18, hjust=0.5),
#                                strip.background = element_blank(),legend.position="none") + 
#   # coord_cartesian( ylim = c(0,30)) +
#    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
#   # ggtitle((expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))))+
#   ylab("Average species richness")
#   # ylab((expression(paste('Average ', italic(alpha), '-richness ',sep = '')))) +
#   # labs(subtitle= "a)" )+
#   # guides(col = guide_legend(ncol = 3))
# 
# rich_biome_d
# 
# (rich_biome_d/d_map)


# seperate plots and color scales old version

rich_biome_a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = a_Estimate, colour = a_Estimate, #Biome_Broad_Hab,
             #aes(x = reorder(Biome_Broad_Hab, a_Estimate ) , y = a_Estimate, colour = a_Estimate, #Biome_Broad_Hab,
                group = Number_Sites,  #shape = Number_Sites
                 ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, 
               # aes(x = reorder(Biome_Broad_Hab, a_Estimate ) , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, 
                    colour =  a_Estimate, #turn off for figure s6
                      #Biome_Broad_Hab, # turn on for figure s6
                    group = Number_Sites
                    ),
                position = position_dodge(width = 0.75),
                linewidth = 0.75, width = 0) +
 #scale_color_viridis(discrete = T, option="D")  +
  scale_color_viridis(discrete = F, option="D", 
                      limits = c(0, 20) 
                      )  +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
   coord_cartesian( ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle( (expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))) )+
  ylab( (expression(paste('Average ', italic(alpha), '-richness ',sep = ''))) ) +
  labs(subtitle= "a)" )+
   guides(col = guide_legend(ncol = 3))


rich_biome_a


rich_biome_g <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = g_Estimate, 
            # aes(x = reorder(Biome_Broad_Hab, g_Estimate ) , y = g_Estimate, 
                 colour = #Biome_Broad_Hab,  # turn on for figure s6
                   g_Estimate, # turn off for figure s6
                 group = Number_Sites,   #shape = Number_Sites
                 ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab, g_Estimate  , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = g_Estimate,  #Biome_Broad_Hab,  
             #   aes(x = reorder(Biome_Broad_Hab, g_Estimate ) , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = g_Estimate,  #Biome_Broad_Hab, 
                    group = Number_Sites
                    ),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  #scale_color_viridis(discrete = T, option="D")  +
  scale_color_viridis(discrete = F, option="plasma", limits = c(10, 40) )  +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  coord_cartesian( ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
  labs(subtitle= "b)" )+
   guides(col = guide_legend(ncol = 3))


rich_biome_g

# map objects created in abg_map.R
# LANDSCAPES 16 X 32
(rich_biome_a + rich_biome_g) / (a_map + g_map) + plot_layout(heights = c(10, 10))


# for supplementary figure version

rich_biome_div <- read.csv(paste0(path2wd, 'Data/sb_av_div_estimates.csv'))

rich_biome_div <- rich_biome_div %>% #left_join(cols) %>% 
mutate(Number_Sites = factor(Number_Sites)) %>% # turn this line and next on for Figure S6
mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100"))) %>%
  mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                                                                          "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                          "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                          "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                          "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                          "Aquatic", "Arable"
))
rich_biome_a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
            # aes(x = Biome_Broad_Hab , y = a_Estimate, colour = a_Estimate, #Biome_Broad_Hab,
                 aes(x = Biome_Broad_Hab , y = a_Estimate, colour = Biome_Broad_Hab, #Biome_Broad_Hab,
                 group = Number_Sites,  shape = Number_Sites
             ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
               # aes(x = Biome_Broad_Hab , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, 
                     aes(x = Biome_Broad_Hab , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, 
                    colour =  Biome_Broad_Hab, #turn off for figure s6
                    #Biome_Broad_Hab, # turn on for figure s6
                    group = Number_Sites
                ),
                position = position_dodge(width = 0.75),
                linewidth = 0.75, width = 0) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  coord_cartesian( ylim = c(0,60)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle( (expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))) )+
  ylab( (expression(paste('Average ', italic(alpha), '-richness ',sep = ''))) ) +
  labs(subtitle= "a)" )+
  guides(col = guide_legend(ncol = 3))


rich_biome_a


rich_biome_g <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = g_Estimate, 
                 # aes(x = reorder(Biome_Broad_Hab, g_Estimate ) , y = g_Estimate, 
                 colour = #Biome_Broad_Hab,  # turn on for figure s6
                   Biome_Broad_Hab, # turn off for figure s6
                 group = Number_Sites,   shape = Number_Sites
             ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab, g_Estimate  , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = Biome_Broad_Hab,  #Biome_Broad_Hab,  
                    #   aes(x = reorder(Biome_Broad_Hab, g_Estimate ) , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = g_Estimate,  #Biome_Broad_Hab, 
                    group = Number_Sites
                ),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  coord_cartesian( ylim = c(0,110)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
  labs(subtitle= "b)" )+
  guides(col = guide_legend(ncol = 3))


rich_biome_g
rich_legend <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = a_Estimate, group= as.character(Number_Sites), shape= as.character(Number_Sites)), 
             position = position_dodge(width = 0.75), alpha=0.5 , size = 3, color="black") +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") + 
  scale_shape(solid = FALSE)+
  guides(shape=guide_legend(title="Number of sites"))

rich_legend

# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect for controls
rich_legend_o <- g_legend(rich_legend)


# landscape 10 x 16
#(rich_biome_a )/ ( rich_biome_g) / (rich_biome_b)  / (rich_legend_o) + plot_layout(heights = c(10, 10, 10, 0.5))

(rich_biome_a )/ ( rich_biome_g)  / (rich_legend_o) + plot_layout(heights = c(10, 10,  0.5))



rich_biome_b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = reorder(Biome_Broad_Hab, b_Estimate ) , y = b_Estimate, 
                 colour = Biome_Broad_Hab,  # turn on for figure s6
                 # b_Estimate, # turn off for figure s6
                 group = Number_Sites,   #shape = Number_Sites
             ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = reorder(Biome_Broad_Hab, b_Estimate ) , ymin = `b_Lower.CI`, ymax =  `b_Upper.CI`,  colour = Biome_Broad_Hab, #g_Estimate,
                    group = Number_Sites
                ),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  #scale_color_viridis(discrete = F, option="plasma", limits = c(10, 40) )  +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  coord_cartesian( ylim = c(0,15)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  #ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(beta), '-diversity ',sep = '')))) +
  labs(subtitle= "b)" )+
  guides(col = guide_legend(ncol = 3))


rich_biome_b

rich_biome_div <- rich_biome_div %>% mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                                                                   "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                   "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                   "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                   "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                   "Aquatic", "Arable"
))

rich_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
   # overall effects
  geom_point(data = rich_biome_div,
             aes(x = a_Estimate, y = g_Estimate, colour = Biome_Broad_Hab, shape= Biome_Broad_Hab
             ), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = a_Estimate , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`,  colour = Biome_Broad_Hab )) +
  geom_errorbarh(data = rich_biome_div,
                aes(y = g_Estimate , xmin = `a_Lower.CI`, xmax =  `a_Upper.CI`,  colour = Biome_Broad_Hab )) +
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  scale_shape_manual(values = c(  16, 18, 16,
                                  18, 18, 16, 
                                  18, 16,  18, 16, 
                                  17, 15) ) +
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
  xlab((expression(paste('Average ', italic(alpha), '-richness ',sep = '')))) +
  theme_classic(base_size=18) +
  labs(subtitle= "" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title="", ncol = 3),
         shape=guide_legend(title="", ncol = 3),)

# 8.50 X 14
rich_joint

# 12 X 14
(rich_joint / rich_biome_b) + plot_layout(heights = c(10, 10))

# what about joint richness ratio

rich_ratio<- rich_biome_div %>% mutate(`WWF Biome` = Biome_Broad_Hab) %>%
  left_join(ratio_conditional_effects) 

head(rich_ratio)

# a diversity first
ratio_rich_a_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = rich_ratio,
             aes(x = a_Estimate, y = Estimate, colour = Biome_Broad_Hab
             ), size = 3) +
  geom_errorbar(data = rich_ratio,
                aes(x = a_Estimate , ymin = `Lower CI`, ymax =  `Upper CI`,  colour = Biome_Broad_Hab )) +
  geom_errorbarh(data = rich_ratio,
                 aes(y = Estimate , xmin = `a_Lower.CI`, xmax =  `a_Upper.CI`,  colour = Biome_Broad_Hab )) +
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
 # scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_viridis(discrete = T, option="D")  +
  ylab("Average Ratio") +
  xlab((expression(paste('Average ', italic(alpha), '-richness ',sep = '')))) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title="Biome", ncol = 3))

# 8.50 X 14
ratio_rich_a_joint


ratio_rich_g_joint <- ggplot()+
  geom_vline(xintercept = 0,linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + 
  # overall effects
  geom_point(data = rich_ratio,
             aes(x = g_Estimate, y = Estimate, colour = Biome_Broad_Hab
             ), size = 3) +
  geom_errorbar(data = rich_ratio,
                aes(x = g_Estimate , ymin = `Lower CI`, ymax =  `Upper CI`,  colour = Biome_Broad_Hab )) +
  geom_errorbarh(data = rich_ratio,
                 aes(y = Estimate , xmin = `g_Lower.CI`, xmax =  `g_Upper.CI`,  colour = Biome_Broad_Hab )) +
  #scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25)) +
   scale_x_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_viridis(discrete = T, option="D")  +
  ylab("Average Ratio") +
  xlab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
  theme_classic(base_size=16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_rect(colour="black", fill="white"),legend.position="bottom") +
  guides(color=guide_legend(title="Biome", ncol = 3))

# 8.50 X 14
ratio_rich_g_joint

(ratio_rich_a_joint + ratio_rich_g_joint)


