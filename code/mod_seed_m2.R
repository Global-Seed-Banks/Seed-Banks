

rm(list = ls())


#packages
library(plyr)
library(tidyverse)
library(brms)
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

conditional_effects(seeds_m2)

# posterior predictive check
color_scheme_set("darkgray")
pp_seed.biome_broad <- pp_check(seeds_m2) + xlab( "Total Seeds") + ylab("Density") +
  labs(title= "d) Seeds ~ area") + xlim(0,8500)+ ylim(0,0.0006)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_seed.biome_broad 


# caterpillars/chains
plot(seeds_m2)


# # check model residuals
# zones
mr.biome_broad <- residuals(seeds_biome_broad)
mr.biome_broad <- as.data.frame(mr.biome_broad)
nrow(mr.biome_broad)

nrow(sb_prep_area_zone)
biome_broad.plot <- cbind(sb_seed_area, mr.biome_broad$Estimate)
head(biome_broad.plot)
#mr.biome_broad make sure they are factors
biome_broad.plot$Biome_WWF_Broad <- as.factor(biome_broad.plot$Biome_WWF_Broad )
biome_broad.plot$Method <- as.factor(biome_broad.plot$Method )
biome_broad.plot$studyID <- as.factor(biome_broad.plot$studyID )
biome_broad.plot$rowID <- as.factor(biome_broad.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome_broad.plot, plot(Biome_WWF_Broad, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(Method, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(studyID, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(rowID, mr.biome_broad$Estimate))



head(sb_seed_area)
# WWF biome_broadS model
# # for plotting fixed effects
seed_biome_broad_fitted <- cbind(seeds_m2$data,
                          fitted(seeds_m2, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_seed_area %>% select(Total_Seeds, log_Total_Seeds,
                                                          Total_Number_Samples, 
                                                          Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                          Centred_log_Total_Sample_Area_m2, Number_Sites,
                                                     Biome_WWF_Broad, Habitat_Broad, Biome_Broad_Hab),
                             #  by= c("Total_Species2","Biome_WWF_Broad","Habitat_Broad","studyID", "rowID")
  )


View(seed_biome_broad_fitted)
nrow(seed_biome_broad_fitted)


seed.fitted <- tidyr::crossing( sb_seed_area %>% dplyr::group_by(Biome_Broad_Hab) %>%
                                  dplyr::summarise(Total_Sample_Area_m2 = c( seq( min(Total_Sample_Area_m2), max(Total_Sample_Area_m2),
                                                                                  length.out = n()
                                                                                  #length.out = 100
                                                                                  ) ) ), 
                                Number_Sites = c(1, 20, 100),
)  %>% mutate( log_Number_Sites = log(Number_Sites),
               log_Total_Sample_Area_m2 = log(Total_Sample_Area_m2),
               Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
               Centred_log_Total_Sample_Area_m2 = log_Total_Sample_Area_m2 - mean(log_Total_Sample_Area_m2, na.rm = TRUE) ) %>%
  select(-c( log_Number_Sites, log_Total_Sample_Area_m2 ) ) %>%
  arrange( Total_Sample_Area_m2, Number_Sites ) %>%
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Centred_log_Number_Sites, Number_Sites)) %>%
  mutate(fitted = map(data, ~fitted(seeds_m2, newdata= .x,  re_formula =  NA  ))) 


seed.fitted.df  <- seed.fitted %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_Broad_Hab_group) %>%
  arrange(Biome_Broad_Hab, Total_Sample_Area_m2, Number_Sites)

head(seed.fitted.df)
nrow(seed.fitted.df)

# fixed effect coefficients
seed_biome_broad_fixef <- fixef(seeds_m2)
head(seed_biome_broad_fixef)

# Random effect coefficients
seed_biome_broad_coef <- coef(seeds_m2)
seed_biome_broad_coef # dont really need this


setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(seed_biome_broad_fitted, seed_biome_broad_fixef, seed.fitted.df, seed_biome_broad_coef, file = 'seed_biome_broad.mod_dat.Rdata')

# plots
setwd(paste0(path2wd, 'Data/'))
load('seed_biome_broad.mod_dat.Rdata')
load('global.seed_biome_broad.posteriors.Rdata')

library(plyr)
# wrap text
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

sb_seed_area$wrapped_text <- llply(sb_seed_area$Biome_Broad_Hab, wrapit)
sb_seed_area$wrapped_text <- unlist(sb_seed_area$wrapped_text)

seed.fitted.df$wrapped_text <- llply(seed.fitted.df$Biome_Broad_Hab, wrapit)
seed.fitted.df$wrapped_text <- unlist(seed.fitted.df$wrapped_text)

seed.fitted.df <- seed.fitted.df %>%   mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100"))) 

head(seed.fitted.df)
head(global.seed_biome_broad.p)

p_e <- global.seed_biome_broad.p %>% select(`WWF Biome`, Estimate) %>% mutate( Biome_Broad_Hab = `WWF Biome`)

# relevel number of sites as a factor and reorder
seed.fitted.df <- seed.fitted.df %>% mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100"))) %>%
  left_join(p_e)

sb_seed_area <- sb_seed_area %>% left_join(p_e)


fig_seed.biome_broad <- ggplot() + 
  facet_wrap(~reorder(wrapped_text, Estimate), scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_seed_area ,
             aes( x = Total_Sample_Area_m2,
               y = Total_Seeds, colour = Biome_Broad_Hab,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # geom_line(data = obs_nest.seeds.df,
  #           aes(x = Total_Sample_Area_m2, y= exp(predicted[,1]), 
  #               group = Habitat_Broad, line_type = Habitat_Broad
  #               ),
  #           size = 0.25) +
  # fixed effect
  geom_line(data = seed.fitted.df,
            aes( x = Total_Sample_Area_m2,
                 y = fitted[,1] , colour = Biome_Broad_Hab, group = Number_Sites , linetype = Number_Sites ),
            size = 1 ) +
  geom_ribbon(data = seed.fitted.df ,
              aes(
                x = Total_Sample_Area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Number_Sites , fill = Biome_Broad_Hab),
              alpha = 0.2) +
  #coord_cartesian(xlim = c(min(sb_seed_area_biome_broad$Total_Sample_Area_m2), quantile(sb_seed_area_biome_broad$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,5000), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Number of seeds in the soil seed bank",  x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") + guides(col = guide_legend(nrow = 4)) #+


fig_seed.biome_broad

legend.data <- seed.fitted.df %>%   mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100")))

fixed.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = legend.data %>% select(Number_Sites) %>% distinct(Number_Sites),
               aes(x = 0,
                   xend = 15,
                   y = 0,
                   yend = 15,  linetype = Number_Sites ), 
               size = 1.5, alpha= 0.5 )  +
  scale_color_viridis(discrete = T, option="D")  +
  theme(legend.key.width = unit(2,"cm")) +  guides(linetype=guide_legend(title="Number of sites"))

fixed.leg

# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect legend
rich_legend_l <- g_legend(fixed.leg)
# slopes
seed_biome_broad.fixed.p <- as_draws_df(seeds_m2, subset = floor(runif(n = 1000, 1, max = 2000)))

seed_biome_broad.fixed.p
nrow(seed_biome_broad.fixed.p)
head(seed_biome_broad.fixed.p)
colnames(seed_biome_broad.fixed.p)

# select columns of interest and give meaningful names
seed_biome_broad_global_posterior <-  seed_biome_broad.fixed.p %>% 
  as_tibble() %>%
  mutate(seed.aq.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         seed.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabArable` ),
         seed.bor.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabBorealForestsDTaiga` ),
         seed.des.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabDesertsandXericShrublands`),
         seed.medfs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMediterraneanForestsWoodlandsandScrub`),
         seed.mongs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMontaneGrasslandsandShrublands`),
         seed.tempbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateBroadleafandMixedForests`),
         seed.tempcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateConiferForests`),
         seed.tempgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateGrasslandsSavannasandShrublands`),
         seed.tropf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalForests`),
         seed.tropgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         seed.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTundra`),
  ) %>%
  dplyr::select(c(seed.aq.global, seed.arable.global, 
    seed.bor.global, seed.des.global, 
                  seed.medfs.global,
                  seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, 
                  seed.tropf.global, seed.tropgs.global, seed.tund.global
                  ))

View(seed_biome_broad_global_posterior)
head(seed_biome_broad_global_posterior)


sb_seed_area_zone$biome_broad_WWF<- as.factor(sb_seed_area_zone$biome_broad_WWF)
levels(sb_seed_area_zone$biome_broad_WWF)

seed.aq.p <-  seed_biome_broad_global_posterior %>%
  mutate( response = "Aquatic", eff = mean(seed.aq.global),
          eff_lower = quantile(seed.aq.global, probs=0.025),
          eff_upper = quantile(seed.aq.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()

seed.arable.p <-  seed_biome_broad_global_posterior %>%
  mutate( response = "Arable", eff = mean(seed.arable.global),
          eff_lower = quantile(seed.arable.global, probs=0.025),
          eff_upper = quantile(seed.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct()

seed.bor.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(seed.bor.global),
          eff_lower = quantile(seed.bor.global, probs=0.025),
          eff_upper = quantile(seed.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.des.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(seed.des.global),
          eff_lower = quantile(seed.des.global, probs=0.025),
          eff_upper = quantile(seed.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.medfs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(seed.medfs.global),
          eff_lower = quantile(seed.medfs.global, probs=0.025),
          eff_upper = quantile(seed.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.mongs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(seed.mongs.global),
          eff_lower = quantile(seed.mongs.global, probs=0.025),
          eff_upper = quantile(seed.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempbf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(seed.tempbf.global),
          eff_lower = quantile(seed.tempbf.global, probs=0.025),
          eff_upper = quantile(seed.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempcf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(seed.tempcf.global),
          eff_lower = quantile(seed.tempcf.global, probs=0.025),
          eff_upper = quantile(seed.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempgs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(seed.tempgs.global),
          eff_lower = quantile(seed.tempgs.global, probs=0.025),
          eff_upper = quantile(seed.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# Tropical and Subtropical Forests
#seed.tropf.global
seed.tropf.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Forests", eff = mean(seed.tropf.global),
          eff_lower = quantile(seed.tropf.global, probs=0.025),
          eff_upper = quantile(seed.tropf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropgs.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(seed.tropgs.global),
          eff_lower = quantile(seed.tropgs.global, probs=0.025),
          eff_upper = quantile(seed.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tund.p <-  seed_biome_broad_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(seed.tund.global),
          eff_lower = quantile(seed.tund.global, probs=0.025),
          eff_upper = quantile(seed.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.seed_biome_broad.p <- bind_rows(seed.aq.p, seed.arable.p,
                                seed.bor.p, seed.des.p,
                                # seed.fgs.p, seed.mang.p,
                                 seed.medfs.p, seed.mongs.p, seed.tempbf.p, seed.tempcf.p, seed.tempgs.p, 
                                #seed.tropcf.p, seed.tropdbf.p, seed.tropmbf.p,
                                seed.tropf.p, seed.tropgs.p, seed.tund.p
                                 ) %>%
  mutate(  Model = "Number of Seeds",
         `WWF Biome` = response ,
           Estimate = round(eff, 4),
         `Lower CI` = round(eff_lower, 2),
         `Upper CI` = round(eff_upper, 2),
  ) %>% select(-c(eff, eff_lower, eff_upper, response)) %>% arrange(Estimate)

head(global.seed_biome_broad.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.seed_biome_broad.p, file = 'global.seed_biome_broad.posteriors.Rdata')


setwd(paste0(path2wd, 'Tables/'))
write.csv(global.seed_biome_broad.p, "table_2.csv")

# plots
setwd(paste0(path2wd, 'Data/'))
#load('rich.area.poisson.mod_dat.Rdata')
load('global.seed_biome_broad.posteriors.Rdata')

global.seed_biome_broad.p

fig_seed_biome_broad_global <- ggplot() + 
  geom_point(data = global.seed_biome_broad.p, aes(x = reorder(`WWF Biome`, Estimate ), y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.seed_biome_broad.p, aes(x = reorder(`WWF Biome`, Estimate ),ymin = `Lower CI`,
                                               ymax = `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_viridis(discrete = T, option="D")  +
  labs(y = "Slope", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                              # axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_seed_biome_broad_global


# extract legend
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend <- function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# # overall legend
# seed_biome_broad_leg <- g_legend(fig_seed.biome_broad)

(fig_seed.biome_broad ) / (rich_legend_l) / ( fig_seed_biome_broad_global) +  plot_layout(ncol=1, nrow=3, heights = c(12,2,7))


#with legend
# lnadscape 16 X 20
#(fig_seed.biome_broad + theme(legend.position="none") ) / ( fig_seed_biome_broad_global)/ (seed_biome_broad_leg) +  plot_layout(ncol=1, nrow=3, heights = c(12,7,2))

