

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

nrow(sb_prep)
head(sb_prep)

# remove NA values 
sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2),
                                   Number_Sites == 1 
                                   ) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)


setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')


summary(rich_m2)



# posterior predictive check
color_scheme_set("darkgray")
pp_rich.biome_broad <- pp_check(rich_m2,)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "a) Species ~ area") + #xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values


pp_rich.biome_broad 



# caterpillars/chains
plot(rich_m2)



head(sb_rich_area)
View(rich_m2$data)
# WWF biome_broadS model
# # for plotting fixed effects
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

View(rich_biome_broad_fitted)
head(rich_biome_broad_fitted)
nrow(rich_biome_broad_fitted)


rich.fitted <- tidyr::crossing( sb_rich_area %>% dplyr::group_by(Biome_Broad_Hab) %>%
                                    summarise(Total_Sample_Area_m2 = c( seq( min(Total_Sample_Area_m2), max(Total_Sample_Area_m2), length.out = 2000) ) ), 
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
# mutate(fitted = map(data, ~epred_draws(rich_m2, newdata= .x, ndraws = 5000, re_formula =  NA  )))


#View(rich.fitted)

rich.fitted.df  <- rich.fitted %>% 
  unnest(cols = c(fitted, data)) %>% 
  ungroup() %>% select(-Biome_Broad_Hab_group) %>%
  #select(-c(.row, .chain, .iteration)) 
  arrange(Biome_Broad_Hab, Total_Sample_Area_m2, Number_Sites)

head(rich.fitted.df)

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

# plot richness area biome_broad relationship

head(sb_rich_area_biome_broad)

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


fig_rich.biome_broad <- ggplot() + 
  facet_wrap(~wrapped_text, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_area ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
               # x = Total_Sample_Area_mm2,
               x = Total_Sample_Area_m2,
               y = Total_Species, colour = Biome_Broad_Hab,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  # geom_line(data = rich_biome_broad_fitted,
  #           aes(
  #             # x = Total_Sample_Area_mm2,
  #             x = Total_Sample_Area_m2,
  #             y = Estimate, colour = Biome_Broad_Hab),
  #           size = 1) +
  # # uncertainy in fixed effect
  # geom_ribbon(data = rich_biome_broad_fitted,
  #             aes(
  #               #x =  Total_Sample_Area_mm2,
  #               x = Total_Sample_Area_m2,
  #               ymin = Q2.5, ymax = Q97.5, fill = Biome_Broad_Hab),
  #             alpha = 0.1 ) +
geom_line(data = rich.fitted.df,
          aes( x = Total_Sample_Area_m2,
               y = fitted[,1] , colour = Biome_Broad_Hab, group = as.character(Number_Sites) , linetype= as.character(Number_Sites) ),
          size = 1) +
  geom_ribbon(data = rich.fitted.df ,
              aes(
                x = Total_Sample_Area_m2,
                ymin = fitted[,3], ymax = fitted[,4], fill = Biome_Broad_Hab),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_rich_area_biome_broad$Total_Sample_Area_m2), quantile(sb_rich_area_biome_broad$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,100), xlim = c(0,15)) +
  scale_color_viridis(discrete = T, option="D")  +
  scale_fill_viridis(discrete = T, option="D")  +
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Species richness in the soil seed bank",  x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") + guides(col = guide_legend(nrow = 4)) #+
#xlim(0,800)+ ylim(0,200)+
#scale_x_log10() + scale_y_log10() 

fig_rich.biome_broad

# custom legend

legend.data <- tidyr::crossing( sb_rich_area %>% dplyr::group_by(Biome_Broad_Hab) %>%
                                  summarise(Total_Sample_Area_m2 = c( seq( min(Total_Sample_Area_m2), max(Total_Sample_Area_m2), length.out = 2000) ) ), 
                                Number_Sites = c(1, 20, 100),
) %>%   mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100")))

legend.data

fixed.leg <- ggplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_classic(base_size=14 )+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  geom_segment(data = legend.data %>% distinct(Number_Sites),
               aes(x = 0,
                   xend = 15,
                   y = 0,
                   yend = 15,  linetype= Number_Sites ), 
               size = 1.5, #linetype=2,
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  scale_color_viridis(discrete = T, option="D")  +
  # scale_color_manual(name='Number of Sites',
  #                    breaks=c("Losses","Gains","Persistent Sp."),
  #                    values=c("Losses"="#B40F20","Gains"="#3B9AB2","Persistent Sp."="#F98400")
  #                    )+
  theme(legend.key.width = unit(2,"cm")) +  guides(linetype=guide_legend(title="Number of sites"))

fixed.leg


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect for controls
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

View(rich_biome_broad_global_posterior)
head(rich_biome_broad_global_posterior)


sb_rich_area_zone$biome_broad_WWF<- as.factor(sb_rich_area_zone$biome_broad_WWF)
levels(sb_rich_area_zone$biome_broad_WWF)


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
          `Upper CI` = round(eff_upper, 2),
          `Lower CI` = round(eff_lower, 2),
         ) %>% select(-c(eff, eff_lower, eff_upper, response))

head(global.rich_biome_broad.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_biome_broad.p, file = 'global.rich_biome_broad.posteriors.Rdata')

 setwd(paste0(path2wd, 'Tables/'))
 write.csv(global.rich_biome_broad.p, "table_1.csv")

 
 # plots
 setwd(paste0(path2wd, 'Data/'))
 #load('rich.area.poisson.mod_dat.Rdata')
 load('global.rich_biome_broad.posteriors.Rdata')
 
head(global.rich_biome_broad.p)

fig_rich_biome_broad_global <- ggplot() + 
  geom_point(data = global.rich_biome_broad.p, aes(x = `WWF Biome`, y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.rich_biome_broad.p, aes(x = `WWF Biome`, ymin = `Lower CI`,
                                               ymax =  `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_viridis(discrete = T, option="D")  +
  labs(y = "Slope", 
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                               #axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

fig_rich_biome_broad_global



# lnadscape 16 X 20
( (fig_rich.biome_broad) / (rich_legend_l) / ( fig_rich_biome_broad_global) )  +
  plot_layout(ncol = 1, nrow = 3, heights = c(12, 2, 7) )






