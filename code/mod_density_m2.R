
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(viridis)
library(tidybayes)
library(ggplot2)


user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

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
nrow(sb_density_area)

levels(sb_density_area$Biome_Broad_Hab)

sb_density_area %>% distinct(Biome_Broad_Hab, Seed_density_m2) %>% arrange(Biome_Broad_Hab, Seed_density_m2)

View(sb_density_area %>% select(Biome_Broad_Hab, Seed_density_m2) %>% 
       filter(Biome_Broad_Hab == "Boreal Forests/Taiga") %>%
       distinct() %>% arrange(Seed_density_m2))


setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'density_m2.Rdata')



# density_biome_broad
# density_habs
# density_deg

# model summary
summary(density_m2)

# posterior predictive check
color_scheme_set("darkgray")
pp_den.biome_broad <- pp_check(density_m2)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_den.biome_broad 


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
         `Upper CI` = round(upper__ , 2),
         `Lower CI` = round(lower__ , 2),
         ) %>% select(Model, `WWF Biome`, Estimate, `Upper CI`, `Lower CI`)

head(density_conditional_effects)

setwd(paste0(path2wd, 'Tables/'))
write.csv(density_conditional_effects, "table_3.csv")


# predicted average density across all
density.fitted <- sb_density_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Seed_density_m2 =  mean(Seed_density_m2)) %>%
  nest(data = c(Biome_Broad_Hab, Seed_density_m2) ) %>%
  mutate(fitted = map(data, ~epred_draws(density_m2, newdata= .x, re_formula = ~(Seed_density_m2 * Biome_Broad_Hab) ))) 


head(density.fitted)


density.fitted.df  <- density.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))  %>%
select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  ungroup() 

head(density.fitted.df)


density.total.mean <- density.fitted.df %>%
  select(-.draw) %>%
  select(-c(Biome_Broad_Hab_group, Seed_density_m2)) %>%
  ungroup() %>%
  #group_by(Seed_density_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs = 0.025),
          P_Estimate_upper = quantile(.epred, probs = 0.975) ) %>% 
  select(-.epred) %>% distinct()

head(density.total.mean)



Density_biome_broad_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = density.total.mean,
             aes(yintercept = P_Estimate), size = 0.45) +
  geom_rect(data = density.total.mean,
            aes(xmin = -Inf, xmax = Inf,
                ymin = P_Estimate_lower, ymax =  P_Estimate_upper ),
            alpha = 0.05) +
  geom_point(data = sb_density_area,
             aes(x = Biome_Broad_Hab, y = Seed_density_m2, #colour = 	"#C0C0C0"
                 colour = Biome_Broad_Hab
                 ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = density_conditional_effects,
             aes(x =  `WWF Biome`, y = Estimate, colour =  `WWF Biome`), size = 3) +
  geom_errorbar(data = density_conditional_effects,
                aes(x =  `WWF Biome`, ymin = `Lower CI`, ymax = `Upper CI`, colour =  `WWF Biome`),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle= '') +
 # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
   coord_cartesian( ylim = c(0,25000)) +
  scale_y_continuous(breaks=c(0,1000, 3000,5000,10000,15000,20000,25000))+
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


Density_biome_broad_Fig
# Landscape 8.50 X 16

