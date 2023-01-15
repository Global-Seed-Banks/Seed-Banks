
rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(viridis)
library(tidybayes)

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
sb_ratio <- sb_prep %>% filter(!is.na(ratio_seeds_species)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
         Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
           rowID = as.factor(rowID))  

nrow(sb_ratio)

levels(sb_ratio$Habitat_Broad)

View(sb_ratio)

sb_ratio %>% 
  mutate(Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 2)) %>%
  distinct(Biome_Broad_Hab, Method, Total_Sample_Area_m2, Total_Number_Samples, Total_Species) %>% 
  arrange(Biome_Broad_Hab, Method, Total_Sample_Area_m2, Total_Number_Samples, Total_Species)

sb_ratio %>% distinct(Centred_log_Total_Sample_Area_m2) %>% arrange(Centred_log_Total_Sample_Area_m2)



ratio_1 <- sb_ratio %>% select(rowID, studyID, Total_Sample_Area_m2, Total_Seeds, Total_Species, ratio_seeds_species, Biome_Broad_Hab, Method, studyID, rowID ) %>%
  filter(!ratio_seeds_species == 1.000000)

nrow(ratio_1)

View(ratio_1)

setwd(paste0(path2wd, 'Data/'))
write.csv(ratio_1,  "ratio_1.csv")

setwd(paste0(path2wd, 'Model_Fits/jan/'))
# models run on cluster, load in model objects here
load( 'ratio.Rdata')



# density_biome_broad
# density_habs
# density_deg

# model summary
summary(ratio)

# posterior predictive check
color_scheme_set("darkgray")
pp_ratio <- pp_check(ratio)+ xlab( "Ratio") + ylab("Density") +
  labs(title= "") + xlim(0,400)+ #ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_ratio 


# caterpillars/chains
plot(ratio)


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




ratio_c <- conditional_effects(ratio, effects = 'Biome_Broad_Hab', re_formula = NA, method = 'fitted')  # conditional effects


ratio_df <-
  as.data.frame(ratio_c$`Biome_Broad_Hab`)

ratio_conditional_effects <- ratio_df %>%
  select(Biome_Broad_Hab, estimate__, lower__, upper__) %>%
  mutate( Model = "Ratio",
          `WWF Biome`= Biome_Broad_Hab,
          Estimate = round(estimate__ , 2),
         `Upper CI` = round(upper__ , 2),
         `Lower CI` = round(lower__ , 2),
         ) %>% select(Model, `WWF Biome`, Estimate, `Upper CI`, `Lower CI`)

head(ratio_conditional_effects)

setwd(paste0(path2wd, 'Tables/'))
write.csv(ratio_conditional_effects, "table_3.csv")


# predicted average density across all
ratio.fitted <- sb_ratio %>%
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>%
  summarise(ratio_seeds_species =  mean(ratio_seeds_species)) %>%
  nest(data = c(Biome_Broad_Hab, ratio_seeds_species) ) %>%
  mutate(fitted = map(data, ~epred_draws(ratio, newdata= .x, re_formula = ~(Seed_ratio_m2 * Biome_Broad_Hab) )))


head(ratio.fitted)


ratio.fitted.df  <- ratio.fitted %>%
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))  %>%
select(-c(Biome_Broad_Hab_group, Biome_Broad_Hab)) %>%
  ungroup()

head(ratio.fitted.df)


ratio.total.mean <- ratio.fitted.df %>%
  select(-.draw) %>%
  select(-c(Biome_Broad_Hab_group, ratio_seeds_species)) %>%
  ungroup() %>%
  #group_by(Seed_ratio_m2) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>%
  select(-.epred) %>% distinct()

head(ratio.total.mean)



ratio_biome_broad_Fig <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = ratio.total.mean,
             aes(yintercept = P_Estimate), size = 0.45) +
  geom_rect(data = ratio.total.mean,
            aes(xmin = -Inf, xmax = Inf,
                ymin = P_Estimate_lower, ymax =  P_Estimate_upper ),
            alpha = 0.05) +
  geom_point(data = sb_ratio,
             aes(x = Biome_Broad_Hab, y = ratio_seeds_species, #colour = 	"#C0C0C0"
                 colour = Biome_Broad_Hab
                 ), 
             size = 0.25, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = ratio_conditional_effects,
             aes(x =  `WWF Biome`, y = Estimate, colour =  `WWF Biome`), size = 3) +
  geom_errorbar(data = ratio_conditional_effects,
                aes(x =  `WWF Biome`, ymin = `Lower CI`, ymax = `Upper CI`, colour =  `WWF Biome`),
                size = 1, width = 0) +
  labs(x = '',
       y = expression(paste('Ratio')),
       subtitle= '') +
 # scale_color_manual(values =  c(	"#C0C0C0","#228B22", 	"#6B8E23"))  + 
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  #ylim(0,100000)+
   coord_cartesian( ylim = c(0,350)) +
  theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


ratio_biome_broad_Fig

