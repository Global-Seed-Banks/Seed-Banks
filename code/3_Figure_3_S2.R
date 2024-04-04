
rm(list = ls())

# packages
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)

setwd(path2wd)
sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)
head(sb_prep)
# data
sb_rich_area <- sb_prep %>% filter(!is.na(Total_species),
                                   !is.na(Total_sample_area_mm2),
                                   Number_sites == 1 
                                   ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome_droad_hab = as.factor(Biome_broad_hab),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID))

head(sb_rich_area)

# load model
setwd(paste0(path2wd, 'Model_Fits/New/'))
# models run on cluster, load in model objects here
load( 'rich_area.Rdata')

summary(rich_m2)

# posterior predictive check
color_scheme_set("darkgray")
figure_s1_a <- pp_check(rich_m2,)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "a) Species ~ area") + #xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

figure_s1_a 

# chains
plot(rich_m2)

# fixed effects
rich_biome_broad_fitted <- cbind(rich_m2$data,
                          fitted(rich_m2, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_rich_area %>% select(Total_Species, 
                                                          Total_Number_Samples, 
                                                          Total_Sample_Area_m2, log_Total_Sample_Area_m2,
                                                          Centred_log_Total_Sample_Area_m2,
                                                     Centred_log_Number_Sites, Number_Sites,
                                                     Biome_Broad_Hab),
                             #  by= c("Total_Species2","Biome_Broad_Hab","Habitat_Broad","studyID", "rowID")
  )


print(rich_biome_broad_fitted, n=20)
nrow(rich_biome_broad_fitted)
View(rich_biome_broad_fitted)


# make sure plyr isnt loaded
rich.fitted <- tidyr::crossing( sb_rich_area %>% dplyr::group_by(Biome_Broad_Hab) %>%
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
  mutate(fitted = map(data, ~fitted(rich_m2, newdata= .x,  re_formula =  NA  ))) 


rich.fitted.df  <- rich.fitted %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_Broad_Hab_group) %>%
  arrange(Biome_Broad_Hab, Total_Sample_Area_m2, Number_Sites)

head(rich.fitted.df)
nrow(rich.fitted.df)
View(rich.fitted.df)

# fixed effect coefficients
rich_biome_broad_fixef <- fixef(rich_m2)
head(rich_biome_broad_fixef)

# Random effect coefficients
rich_biome_broad_coef <- coef(rich_m2)
rich_biome_broad_coef # dont really need this

setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich_biome_broad_fitted, rich.fitted.df, rich_biome_broad_fixef, rich_biome_broad_coef, file = 'rich_biome_broad.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('rich.area.poisson.mod_dat.Rdata')
load('rich_biome_broad.mod_dat.Rdata')
load('global.rich_biome_broad.posteriors.Rdata')


#View(global.rich_biome_broad.p)
#p_e <- global.rich_biome_broad.p %>% select(`WWF Biome`, Estimate) %>% mutate( Biome_Broad_Hab = `WWF Biome`)

# relevel number of sites as a factor and reorder
rich.fitted.df <- rich.fitted.df %>% mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100"))) %>%
  #left_join(p_e) %>% 
  distinct()

#sb_rich_area <- sb_rich_area %>% left_join(p_e)

head(rich.fitted.df)
head(sb_rich_area)

rich.fitted.df %>% select(Biome_Broad_Hab) %>% distinct()


sb_rich_area <- sb_rich_area %>% mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                          "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                          "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                          "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                          "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                          "Aquatic", "Arable"
                          ))

rich.fitted.df <- rich.fitted.df %>% mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                          "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                          "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                          "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                          "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                          "Aquatic", "Arable"
                          ))


# wrap text
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

library(plyr)

sb_rich_area$wrapped_text <- llply(sb_rich_area$Biome_Broad_Hab, wrapit)
sb_rich_area$wrapped_text <- unlist(sb_rich_area$wrapped_text)

# rich_biome_broad_fitted$wrapped_text <- llply(rich_biome_broad_fitted$Biome_Broad_Hab, wrapit)
# rich_biome_broad_fitted$wrapped_text <- unlist(rich_biome_broad_fitted$wrapped_text)

rich.fitted.df$wrapped_text <- llply(rich.fitted.df$Biome_Broad_Hab, wrapit)
rich.fitted.df$wrapped_text <- unlist(rich.fitted.df$wrapped_text)

View(rich.fitted.df)
rich.fitted.df %>% select(wrapped_text) %>% distinct()

sb_rich_area <- sb_rich_area %>% mutate(wrapped_text = fct_relevel(wrapped_text,
                                                                      "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                      "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and \n Shrublands",
                                                                      "Mediterranean Forests, Woodlands and \n Scrub", "Deserts and Xeric Shrublands",
                                                                      "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, \n Savannas and Shrublands",
                                                                      "Aquatic", "Arable"
))

rich.fitted.df <- rich.fitted.df %>% mutate(wrapped_text = fct_relevel(wrapped_text,
                                                                          "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                          "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and \n Shrublands",
                                                                          "Mediterranean Forests, Woodlands and \n Scrub", "Deserts and Xeric Shrublands",
                                                                          "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, \n Savannas and Shrublands",
                                                                          "Aquatic", "Arable"
))

fig_rich.biome_broad <- ggplot() + 
  facet_wrap(~reorder(wrapped_text, Biome_Broad_Hab), scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area,
             aes(x = Total_Sample_Area_m2,
               y = Total_Species, colour = wrapped_text,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
geom_line(data = rich.fitted.df, 
          aes( x = Total_Sample_Area_m2,
               y = fitted[,1] , colour = wrapped_text, group = Number_Sites , linetype = Number_Sites  ),
          size = 1 
          ) +
  geom_ribbon(data = rich.fitted.df ,
              aes(
                x = Total_Sample_Area_m2,
                ymin = fitted[,3], ymax = fitted[,4], group = Number_Sites , fill = wrapped_text),
              alpha = 0.2) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  scale_fill_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  # scale_color_viridis(discrete = T, option="D")  +
  # scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sampled Area ' , m^2)),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") + guides(col = guide_legend(nrow = 4)) 

fig_rich.biome_broad



# custom legend
legend.data <- rich.fitted.df %>%   mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100")))

fixed.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=20 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = legend.data %>% select(Number_Sites) %>% distinct(Number_Sites),
               aes(x = 0,
                   xend = 15,
                   y = 0,
                   yend = 15,  linetype = Number_Sites ), 
               size = 1.5, alpha= 0.5  )  +
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

rich_biome_broad.fixed.p <- as_draws_df(rich_m2, subset = floor(runif(n = 1000, 1, max = 2000))) %>%
  select(contains("b_"))

rich_biome_broad.fixed.p
nrow(rich_biome_broad.fixed.p)
head(rich_biome_broad.fixed.p)
colnames(rich_biome_broad.fixed.p)

# select columns of interests and give meaningful names
rich_biome_broad_global_posterior <-  rich_biome_broad.fixed.p %>% 
  as_tibble() %>%
  mutate( 
        rich.aq.global = (`b_Centred_log_Total_Sample_Area_m2` ),
         rich.arable.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabArable` ),
         rich.bor.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabBorealForestsDTaiga` ),
         rich.des.global = (`b_Centred_log_Total_Sample_Area_m2` + `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabDesertsandXericShrublands`),
         rich.medfs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMediterraneanForestsWoodlandsandScrub`),
         rich.mongs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabMontaneGrasslandsandShrublands`),
         rich.tempbf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateBroadleafandMixedForests`),
         rich.tempcf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateConiferForests`),
         rich.tempgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTemperateGrasslandsSavannasandShrublands`),
        rich.tropf.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalForests`),
         rich.tropgs.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         rich.tund.global = (`b_Centred_log_Total_Sample_Area_m2`+ `b_Centred_log_Total_Sample_Area_m2:Biome_Broad_HabTundra`),
  ) %>%
  dplyr::select(c(rich.aq.global, rich.arable.global, rich.bor.global, rich.des.global, 
                  rich.medfs.global,
                  rich.mongs.global, rich.tempbf.global, rich.tempcf.global, rich.tempgs.global, 
                 rich.tropf.global,  rich.tropgs.global, 
                  rich.tund.global
                  ))

#View(rich_biome_broad_global_posterior)
head(rich_biome_broad_global_posterior)

rich.aq.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(rich.aq.global),
          eff_lower = quantile(rich.aq.global, probs=0.025),
          eff_upper = quantile(rich.aq.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.arable.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Arable", eff = mean(rich.arable.global),
          eff_lower = quantile(rich.arable.global, probs=0.025),
          eff_upper = quantile(rich.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.bor.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(rich.bor.global),
          eff_lower = quantile(rich.bor.global, probs=0.025),
          eff_upper = quantile(rich.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.des.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(rich.des.global),
          eff_lower = quantile(rich.des.global, probs=0.025),
          eff_upper = quantile(rich.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.medfs.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(rich.medfs.global),
          eff_lower = quantile(rich.medfs.global, probs=0.025),
          eff_upper = quantile(rich.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.mongs.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(rich.mongs.global),
          eff_lower = quantile(rich.mongs.global, probs=0.025),
          eff_upper = quantile(rich.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempbf.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(rich.tempbf.global),
          eff_lower = quantile(rich.tempbf.global, probs=0.025),
          eff_upper = quantile(rich.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempcf.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(rich.tempcf.global),
          eff_lower = quantile(rich.tempcf.global, probs=0.025),
          eff_upper = quantile(rich.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tempgs.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(rich.tempgs.global),
          eff_lower = quantile(rich.tempgs.global, probs=0.025),
          eff_upper = quantile(rich.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropf.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Forests", eff = mean(rich.tropf.global),
          eff_lower = quantile(rich.tropf.global, probs=0.025),
          eff_upper = quantile(rich.tropf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tropgs.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(rich.tropgs.global),
          eff_lower = quantile(rich.tropgs.global, probs=0.025),
          eff_upper = quantile(rich.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

rich.tund.p <-  rich_biome_broad_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(rich.tund.global),
          eff_lower = quantile(rich.tund.global, probs=0.025),
          eff_upper = quantile(rich.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.rich_biome_broad.p <- bind_rows(rich.aq.p, rich.arable.p, rich.bor.p, rich.des.p,
                                 rich.medfs.p, rich.mongs.p, rich.tempbf.p, rich.tempcf.p, rich.tempgs.p, 
                                rich.tropf.p,  rich.tropgs.p, 
                                 rich.tund.p
                                 ) %>%
  mutate( Model = "Richness",
          `WWF Biome` = response ,
           Estimate = round(eff, 4),
          `Lower CI` = round(eff_lower, 2),
          `Upper CI` = round(eff_upper, 2),
         ) %>% select(-c(eff, eff_lower, eff_upper, response)) %>%
  arrange(Estimate)

head(global.rich_biome_broad.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_biome_broad.p, file = 'global.rich_biome_broad.posteriors.Rdata')

 setwd(paste0(path2wd, 'Tables/'))
 write.csv(global.rich_biome_broad.p, "table_1.csv")

 setwd(paste0(path2wd, 'Data/'))
 load('global.rich_biome_broad.posteriors.Rdata')
 
head(global.rich_biome_broad.p)

global.rich_biome_broad.p <- global.rich_biome_broad.p %>% mutate(`WWF Biome` = fct_relevel(`WWF Biome`,
                                                                      "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                      "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                      "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                      "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                      "Aquatic", "Arable"
))

#slopes
fig_rich_biome_broad_global <- ggplot() + 
  geom_point(data = global.rich_biome_broad.p, aes(x = `WWF Biome` , y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.rich_biome_broad.p, aes(x = `WWF Biome`, ymin = `Lower CI`,
                                               ymax =  `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  #scale_color_viridis(discrete = T, option="D")  +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  labs(y = "Slope", 
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  theme_bw(base_size=20)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               #axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_rich_biome_broad_global

# put together with patchwork
figure_3 <- ( (fig_rich.biome_broad) / (rich_legend_l) / ( fig_rich_biome_broad_global) )  +
  plot_layout(ncol = 1, nrow = 3, heights = c(12, 2, 7) )

# lnadscape 16 X 20
figure_3



# LOG SCALE #####################

head(rich.fitted.df)


figure_s2_a <- ggplot() +
  facet_wrap(~ reorder(wrapped_text, Biome_Broad_Hab),
  scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area,
             aes(x = Total_Sample_Area_m2,
                 y = Total_Species, colour = wrapped_text,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = rich.fitted.df,
            aes( x =  Total_Sample_Area_m2,
                 y = fitted[,1] , colour = wrapped_text, group = Number_Sites , linetype = Number_Sites  ),
            size = 1
  ) +
  geom_ribbon(data = rich.fitted.df ,
              aes(
                x = Total_Sample_Area_m2,
                ymin = fitted[,3] , ymax = fitted[,4] , group = Number_Sites , fill = wrapped_text),
              alpha = 0.2) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  scale_fill_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                "#447fdd","#99610a" # aquatic, arable
  ))+
  coord_cartesian( ylim = c(5,100),  xlim = c(1,15)
                   ) +
  scale_y_continuous(trans = 'log', breaks= c(5,10,20,50, 100)) +  scale_x_continuous(trans = 'log', breaks=c( 1, 2, 5, 10, 15) ) +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  
  labs(y = "log[Species richness in the soil seed bank]",  x = expression(paste('Total Sample Area log[' , m^2,']')),
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") #+ guides(col = guide_legend(nrow = 4))

figure_s2_a


figure_s2_b <- ggplot() + 
  geom_point(data = global.rich_biome_broad.p, aes(x = `WWF Biome` , y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.rich_biome_broad.p, aes(x = `WWF Biome`, ymin = `Lower CI`,
                                                      ymax =  `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  #scale_color_viridis(discrete = T, option="D")  +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  labs(y = "Slope", 
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  scale_y_continuous(trans = 'log', breaks = c(0.01, 0.02, 0.05, 0.15)) + 
  labs( y= "log[Slope]")+
  theme_bw(base_size=20)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               #axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

figure_s2_b

# put together with patchwork
figure_s2 <- ( (figure_s2_a) / (rich_legend_l) / ( figure_s2_b) )  +
  plot_layout(ncol = 1, nrow = 3, heights = c(12, 2, 7) )

# lnadscape 16 X 20
figure_s2





fig_together <- ggplot() + 
  #facet_wrap(~reorder(wrapped_text, Biome_Broad_Hab), scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area %>% filter(Number_Sites == "1"),
             aes(x = Total_Sample_Area_m2,
                 y = Total_Species, colour = Biome_Broad_Hab,
             ),  size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  geom_line(data = rich.fitted.df  %>% filter(Number_Sites == "1"), 
            aes( x = Total_Sample_Area_m2,
                 y = fitted[,1] , colour = Biome_Broad_Hab  ),
            size = 1 
  ) +
  geom_ribbon(data = rich.fitted.df  %>% filter(Number_Sites == "1") ,
              aes(
                x = Total_Sample_Area_m2,
                ymin = fitted[,3], ymax = fitted[,4], fill = Biome_Broad_Hab),
              alpha = 0.2) +
  coord_cartesian( ylim = c(5,100),  xlim = c(1,15)
  ) +
  scale_y_continuous(trans = 'log', breaks= c(5,10,20,50, 100)) +  scale_x_continuous(trans = 'log', breaks=c( 1, 2, 5, 10, 15) ) +
  theme_bw(base_size=20 ) + 
  
  labs(y = "log[Species richness in the soil seed bank]",  x = expression(paste('Total Sample Area log[' , m^2,']')),
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") +

  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  scale_fill_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                "#447fdd","#99610a" # aquatic, arable
  ))+
  # scale_color_viridis(discrete = T, option="D")  +
  # scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=20 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") + guides(col = guide_legend(nrow = 4)) 

fig_together
