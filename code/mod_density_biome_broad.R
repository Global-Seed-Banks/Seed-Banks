
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
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
# remove NA values 
sb_density_area <- sb_prep %>% filter(!is.na(Seed_density_m2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          #Biome_WWF_Broad = as.factor(Biome_WWF_Broad),
         # Habitat_Broad = as.factor(Habitat_Broad),
         # Biome_Hab = as.factor(Biome_Hab),
          studyID = as.factor(studyID),
           rowID = as.factor(rowID))  %>% 
   mutate(Biome_Broad_Hab = case_when(Habitat_Broad %in% c("Arable", "Aquatic") ~ Habitat_Broad ,
                                     TRUE ~ Biome_WWF_Broad))

nrow(sb_density_area)

levels(sb_density_area$Habitat_Broad)



setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'density_biome_broad.Rdata')



# density_biome
# density_habs
# density_deg

# model summary
summary(density_biome_broad)

# posterior predictive check
color_scheme_set("darkgray")
pp_den.biome <- pp_check(density_biome)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") +# xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_den.biome 


# caterpillars/chains
plot(density_biome)


# # check model residuals
# biome
mr.biome <- residuals(density_biome)
mr.biome <- as.data.frame(mr.biome)
nrow(mr.biome)

nrow(sb_density_area_biome)
biome.plot <- cbind(sb_density_area_biome, mr.biome$Estimate)
head(biome.plot)
#mr.biome make sure they are factors
biome.plot$Biome_WWF_biome <- as.factor(biome.plot$Biome_WWF_biome )
biome.plot$Method <- as.factor(biome.plot$Method )
biome.plot$studyID <- as.factor(biome.plot$studyID )
biome.plot$rowID <- as.factor(biome.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome.plot, plot(Biome_WWF_biome, mr.biome$Estimate))
with(biome.plot, plot(Method, mr.biome$Estimate))
with(biome.plot, plot(studyID, mr.biome$Estimate))
with(biome.plot, plot(rowID, mr.biome$Estimate))




density_biome_c <- conditional_effects(density_biome, effects = 'Biome_Hab', re_formula = NA, method = 'fitted')  # conditional effects

head(density_biome_c)


Density_biome_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_density_area,
             aes(x = Biome_Hab, y = Seed_density_m2, #colour = 	"#C0C0C0"
                 colour = Biome_Hab
                 ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = density_biome_c$Biome_Hab,
             aes(x = Biome_Hab, y = estimate__, colour = Biome_Hab), size = 3) +
  geom_errorbar(data = density_biome_c$Biome_Hab,
                aes(x = Biome_Hab, ymin = lower__, ymax = upper__, colour = Biome_Hab),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),
       subtitle= 'WWF Biome') +
 # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
   coord_cartesian( ylim = c(0,10000)) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


Density_biome_Fig

