
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

head(sb_prep)
summary(sb_prep)

sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)

sb_seed_area <- sb_prep %>% filter(!is.na(Total_Seeds),
                                   !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_seed_area)


# remove NA values 
sb_density_area <- sb_prep %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))  
nrow(sb_density_area)

levels(sb_density_area$Habitat_Broad)



setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_biome_broad.Rdata')
load( 'seed_biome_broad.Rdata')
load( 'density_biome_broad.Rdata')

head(sb_rich_area)
summary(rich_biome_broad)

rich.fitted <- sb_rich_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Centred_log_Total_Sample_Area_m2 =  mean(Centred_log_Total_Sample_Area_m2),
            Total_Sample_Area_m2 =  mean(Total_Sample_Area_m2)) %>%
  nest(data = c(Biome_Broad_Hab, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) ) %>%
  mutate(fitted = map(data, ~epred_draws(rich_biome_broad, newdata= .x, re_formula = ~(Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab) ))) 


head(rich.fitted)


rich.fitted.df  <- rich.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(rich.fitted.df)


rich.fitted <- rich.fitted.df %>%
  select(-.draw) %>%
  group_by(Biome_Broad_Hab, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(rich.fitted)



rich.study <- rich.study.df %>%
  select(-.draw) %>%
  group_by(studyID, Biome_Broad_Hab, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

View(rich.study)


fig_4a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # overall hab means
  geom_point(data = rich.fitted,
             aes(x = Biome_Broad_Hab, y = P_Estimate, colour = Biome_Broad_Hab), size = 3) +
  geom_errorbar(data = rich.fitted,
                aes(x = Biome_Broad_Hab, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = Biome_Broad_Hab),
                size = 1, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               axis.text.x=element_blank(),
                               strip.background = element_blank(),legend.position="none") +
   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(x='',
       y = 'Average total number of species',
       title= 'a) Average species richness') 


fig_4a


# seeds
head(sb_seed_area)
summary(seeds_biome_broad)

seed.fitted <- sb_seed_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Centred_log_Total_Sample_Area_m2 =  mean(Centred_log_Total_Sample_Area_m2),
            Total_Sample_Area_m2 =  mean(Total_Sample_Area_m2)) %>%
  nest(data = c(Biome_Broad_Hab, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) ) %>%
  mutate(fitted = map(data, ~epred_draws(seeds_biome_broad, newdata= .x, re_formula = ~(Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab) ))) 


head(seed.fitted)


seed.fitted.df  <- seed.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(seed.fitted.df)


seed.fitted <- seed.fitted.df %>%
  select(-.draw) %>%
  group_by(Biome_Broad_Hab, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(seed.fitted)


fig_4b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # geom_point(data = p.all,
  #            aes(y = s.loss.n , x = trt.y, colour = 	"#C0C0C0"),
  #            size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = seed.fitted,
             aes(x = Biome_Broad_Hab, y = P_Estimate, colour = Biome_Broad_Hab), size = 3) +
  geom_errorbar(data = seed.fitted,
                aes(x = Biome_Broad_Hab, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = Biome_Broad_Hab),
                size = 1, width = 0) +
  scale_color_viridis(discrete = T, option="D")  +
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               axis.text.x=element_blank(),
                               strip.background = element_blank(),legend.position="bottom") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  labs(x='',
       y = 'Average total number of species',
       title= 'a) Average species seedness') 


fig_4b

fig <- (fig_4a + fig_4b)
fig

# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# overall legend
av_leg <- g_legend(fig_4b)


(fig_4a  | fig_4b +  theme(legend.position="none"))/ (av_leg) +  plot_layout(ncol=1, nrow=2, heights = c(10,1))



