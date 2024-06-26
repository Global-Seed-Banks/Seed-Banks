
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(viridis)
library(tidybayes)
library(ggplot2)
library(purrr)


user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

head(sb_prep)
summary(sb_prep)
# remove NA values 
sb_density_area <- sb_prep %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_broad_hab = as.factor(Biome_broad_hab),
         Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
           RowID = as.factor(RowID))  
nrow(sb_density_area)

levels(sb_density_area$Biome_broad_hab)

sb_density_area %>% distinct(Biome_broad_hab, Seed_density_m2) %>% arrange(Biome_broad_Hab, Seed_density_m2)

View(sb_density_area %>% select(Biome_Broad_Hab, Seed_density_m2) %>% 
       filter(Biome_Broad_Hab == "Boreal Forests/Taiga") %>%
       distinct() %>% arrange(Seed_density_m2))


setwd(paste0(path2wd, 'Model_Fits/New/'))
# models run on cluster, load in model objects here
load( 'seed_density.Rdata')



# density_biome_broad
# density_habs
# density_deg

# model summary
summary(density_m2)

# posterior predictive check
color_scheme_set("darkgray")
figure_s1_b <- pp_check(density_m2)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

figure_s1_b 


# caterpillars/chains
plot(density_m2)

# # check model residuals
# biome_broad
mr.biome_broad <- residuals(density_biome_broad)
mr.biome_broad <- as.data.frame(mr.biome_broad)
nrow(mr.biome_broad)

nrow(sb_density_area_biome_broad)
biome_broad.plot <- cbind(sb_density_area_biome_broad, mr.biome_broad$Estimate)
head(biome_broad.plot)
#mr.biome_broad make sure they are factors
biome_broad.plot$biome_broad_WWF_biome_broad <- as.factor(biome_broad.plot$biome_broad_WWF_biome_broad )
biome_broad.plot$Method <- as.factor(biome_broad.plot$Method )
biome_broad.plot$studyID <- as.factor(biome_broad.plot$studyID )
biome_broad.plot$rowID <- as.factor(biome_broad.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome_broad.plot, plot(biome_broad_WWF_biome_broad, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(Method, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(studyID, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(rowID, mr.biome_broad$Estimate))




density_biome_broad_c <- conditional_effects(density_m2, effects = 'Biome_Broad_Hab', re_formula = NA, method = 'fitted')  # conditional effects

density_biome_broad_c

density_biome_broad_df <-
  as.data.frame(density_biome_broad_c$`Biome_Broad_Hab`)

density_conditional_effects <- density_biome_broad_df %>%
  select(Biome_Broad_Hab, estimate__, lower__, upper__) %>%
  mutate( Model = "Density",
          `WWF Biome`= Biome_Broad_Hab,
          Estimate = round(estimate__ , 2),
          `Lower CI` = round(lower__ , 2),
         `Upper CI` = round(upper__ , 2),
         ) %>% select(Model, `WWF Biome`, Estimate, `Upper CI`, `Lower CI`) %>% arrange(Estimate)

head(density_conditional_effects)


d_e <- density_conditional_effects %>% select(`WWF Biome`, Estimate) %>% mutate( Biome_Broad_Hab = `WWF Biome`)

sb_density_area <- sb_density_area %>% left_join(d_e)

setwd(paste0(path2wd, 'Tables/'))
write.csv(density_conditional_effects, "table_3.csv")

# predicted average density across all
density.predicted <- sb_density_area %>% 
  select(Biome_Broad_Hab, Seed_density_m2) %>%
   mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
   group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
   nest(data = c(  Biome_Broad_Hab, Seed_density_m2) ) %>%
  mutate(predicted = purrr::map(data, ~epred_draws(density_m2, newdata= .x, re_formula = NA))) 

head(density.predicted)

density.predicted.df  <- density.predicted %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))  %>%
select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  ungroup() 

head(density.predicted.df)

density.total.mean <- density.predicted.df %>%
  select(-.draw) %>%
  select(-c(Biome_Broad_Hab_group, Seed_density_m2)) %>%
  ungroup() %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs = 0.025),
          P_Estimate_upper = quantile(.epred, probs = 0.975) ) %>% 
  select(-.epred) %>% distinct()

head(density.total.mean)
View(head(density.total.mean))

sb_density_area %>% select(Biome_Broad_Hab) %>% distinct(Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)

# reorder? tundra _. tropical....aquatic, arable?
sb_density_area <- sb_density_area %>% mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                          "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                          "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                          "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                          "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                          "Aquatic", "Arable"
                          ))
  
density_conditional_effects <- density_conditional_effects %>% mutate(`WWF Biome` = fct_relevel(`WWF Biome`,
                                                                            "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                            "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                            "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                            "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                            "Aquatic", "Arable"
))
  
figure_5_a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = density.total.mean,
             aes(yintercept = P_Estimate), size = 0.5) +
  geom_rect(data = density.total.mean,
            aes(xmin = -Inf, xmax = Inf,
                ymin = P_Estimate_lower, ymax =  P_Estimate_upper ),
            alpha = 0.05) +
  geom_point(data = sb_density_area,
             aes(x = Biome_Broad_Hab, y = Seed_density_m2, 
                 colour = Biome_Broad_Hab
                 ), 
             size = 1.5, alpha = 0.2, position = position_jitter(width = 0.25, height=0.45)) +
  geom_point(data = density_conditional_effects,
             aes(x =  `WWF Biome`, y = Estimate, colour =  `WWF Biome`), size = 3) +
  geom_errorbar(data = density_conditional_effects,
                aes(x =   `WWF Biome`, ymin = `Lower CI`, ymax = `Upper CI`, colour =  `WWF Biome`),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle=  "a)"
       ) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
   coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title = element_text(size=18, hjust=0.5),
                               strip.background = element_blank(), legend.position="none",
                               #axis.text.x=element_blank()
                               ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


figure_5_a
# Landscape 8.50 X 18

(figure_5_a/figure_5_b)
