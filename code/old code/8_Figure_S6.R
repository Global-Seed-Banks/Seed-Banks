

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
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)
head(sb_prep)

# remove NA values 
sb_rich_seed <- sb_prep %>%  filter(!is.na(Total_Species),
                                       !log_Total_Seeds == "-Inf",
                                       !is.na(log_Total_Seeds)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_seed)



sb_seed_area %>% distinct(biome_broad_WWF, biome_broad_WWF_Broad, Biome_Broad_Hab) %>% arrange(Biome_Broad_Hab)




setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_seeds.Rdata')


summary(rich_seeds)


exp(-0.031)

# posterior predictive check
color_scheme_set("darkgray")
figure_s1_e <- pp_check(rich_seeds)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "e) Species ~ seeds") + xlim(0,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

figure_s1_e

figure_s1 <- ( figure_s1_a +  figure_s1_b + figure_s1_c) / ( figure_s1_d +  figure_s1_e)
figure_s1

# caterpillars/chains
plot(seeds_biome_broad)


# # check model residuals
# zones
mr.biome_broad <- residuals(seeds_biome_broad)
mr.biome_broad <- as.data.frame(mr.biome_broad)
nrow(mr.biome_broad)

nrow(sb_prep_area_zone)
biome_broad.plot <- cbind(sb_seed_area, mr.biome_broad$Estimate)
head(biome_broad.plot)
#mr.biome_broad make sure they are factors
biome_broad.plot$biome_broad_hab <- as.factor(biome_broad.plot$biome_broad_Hab )
biome_broad.plot$Method <- as.factor(biome_broad.plot$Method )
biome_broad.plot$studyID <- as.factor(biome_broad.plot$studyID )
biome_broad.plot$rowID <- as.factor(biome_broad.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(biome_broad.plot, plot(biome_broad_Hab, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(Method, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(studyID, mr.biome_broad$Estimate))
with(biome_broad.plot, plot(rowID, mr.biome_broad$Estimate))



head(sb_seed_area)
# WWF biome_broadS model
# # for plotting fixed effects
rich_seeds_biome_broad_fitted <- cbind(rich_seeds$data,
                          fitted(rich_seeds, re_formula = NA
                          )) %>%
  as_tibble() %>% inner_join(sb_rich_seed %>% select(Total_Species, 
                                                      log_Total_Species, log_Total_Seeds,
                                                      Total_Seeds, Number_Sites,
                                                          Biome_Broad_Hab),
  )


head(rich_seeds_biome_broad_fitted)
nrow(rich_seeds_biome_broad_fitted)
# 
head(sb_rich_seed)
nrow(sb_rich_seed)
summary()

seed_rich_fitted <- tidyr::crossing( sb_rich_seed %>% dplyr::group_by(Biome_Broad_Hab) %>%
                                  dplyr::summarise(Total_Seeds = c( seq( min(Total_Seeds), max(Total_Seeds),
                                                                                  length.out = n()
                                  ) ) ),
                                Number_Sites = c(1, 20, 100),
)  %>% mutate( log_Number_Sites = log(Number_Sites),
               log_Total_Seeds = log(Total_Seeds),
               Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
               Centred_log_Total_Seeds = log_Total_Seeds - mean(log_Total_Seeds, na.rm = TRUE) ) %>%
  select(-c( log_Number_Sites, log_Total_Seeds ) ) %>%
  arrange( Total_Seeds, Number_Sites ) %>%
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Seeds, Total_Seeds, Centred_log_Number_Sites, Number_Sites)) %>%
  mutate(fitted = map(data, ~fitted(rich_seeds, newdata= .x,  re_formula =  NA  )))


seed_rich_fitted.df  <- seed_rich_fitted %>%
  unnest(cols = c(fitted, data)) %>%
  ungroup() %>% select(-Biome_Broad_Hab_group) %>%
  arrange(Biome_Broad_Hab, Total_Seeds, Number_Sites)

head(seed_rich_fitted.df)
nrow(seed_rich_fitted.df)

# fixed effect coefficients
rich_seeds_biome_broad_fixef <- fixef(rich_seeds)
head(rich_seeds_biome_broad_fixef)

# Random effect coefficients
rich_seeds_biome_broad_coef <- coef(rich_seeds)
rich_seeds_biome_broad_coef # dont really need this



setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
save(rich_seeds_biome_broad_fitted, seed_rich_fitted.df, rich_seeds_biome_broad_fixef, rich_seeds_biome_broad_coef, file = 'rich_seeds_biome_broad.mod_dat.Rdata')


# plots
setwd(paste0(path2wd, 'Data/'))
#load('seed.area.poisson.mod_dat.Rdata')
load('rich_seeds_biome_broad.mod_dat.Rdata')
load('global.rich_seed_biome_broad.posteriors.Rdata')

# plot seedness area biome_broad relationship

head(sb_seed_area_biome_broad)

# wrap text
wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

sb_rich_seed$wrapped_text <- llply(sb_rich_seed$Biome_Broad_Hab, wrapit)
sb_rich_seed$wrapped_text <- unlist(sb_rich_seed$wrapped_text)

seed_rich_fitted.df$wrapped_text <- llply(seed_rich_fitted.df$Biome_Broad_Hab, wrapit)
seed_rich_fitted.df$wrapped_text <- unlist(seed_rich_fitted.df$wrapped_text)

p_e <- global.rich_seed_biome_broad.p %>% select(`WWF Biome`, Estimate) %>% mutate( Biome_Broad_Hab = `WWF Biome`)

# relevel number of sites as a factor and reorder
seed_rich_fitted.df <- seed_rich_fitted.df %>% mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100"))) %>%
  left_join(p_e)

sb_rich_seed <- sb_rich_seed %>% left_join(p_e)


sb_rich_seed <- sb_rich_seed %>% mutate(wrapped_text = fct_relevel(wrapped_text,
                                                                   "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                   "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and \n Shrublands",
                                                                   "Mediterranean Forests, Woodlands and \n Scrub", "Deserts and Xeric Shrublands",
                                                                   "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, \n Savannas and Shrublands",
                                                                   "Aquatic", "Arable"
))

seed_rich_fitted.df <- seed_rich_fitted.df %>% mutate(wrapped_text = fct_relevel(wrapped_text,
                                                                       "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                       "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and \n Shrublands",
                                                                       "Mediterranean Forests, Woodlands and \n Scrub", "Deserts and Xeric Shrublands",
                                                                       "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, \n Savannas and Shrublands",
                                                                       "Aquatic", "Arable"
))

figure_s6_a <- ggplot() + 
  facet_wrap(~reorder(wrapped_text, Biome_Broad_Hab), scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = sb_rich_seed ,
             aes(#x = (Total_Sample_Area_mm2/1000000),
               # x = Total_Sample_Area_mm2,
               x = Total_Seeds,
               y = Total_Species, colour = wrapped_text,
             ), 
             size = 1.2, alpha = 0.3,   position = position_jitter(width = 0.25, height=2.5)) +
  # fixed effect
  geom_line(data = seed_rich_fitted.df,
            aes(#x = (Total_Sample_Area_mm2/1000000), 
              # x = Total_Sample_Area_mm2,
              x = Total_Seeds,
              y = fitted[,1], colour = wrapped_text,  group = Number_Sites , linetype = Number_Sites),
            size = 0.75) +
  # uncertainy in fixed effect
  geom_ribbon(data = seed_rich_fitted.df,
              aes( #x =  (Total_Sample_Area_mm2/1000000), 
                #x =  Total_Sample_Area_mm2, 
                x = Total_Seeds,
                ymin = fitted[,3], ymax = fitted[,4], group = Number_Sites , fill = wrapped_text),
              alpha = 0.1 ) +
  #coord_cartesian(xlim = c(min(sb_seed_area_biome_broad$Total_Sample_Area_m2), quantile(sb_seed_area_biome_broad$Total_Sample_Area_m2, 0.97))) +
  coord_cartesian( ylim = c(0,200), xlim = c(0,50000)) +
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
  theme_bw(base_size=14 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="none") +
  labs(y = "Number of species in the soil seed bank",  x = expression(paste('Number of seeds in the soil seed bank')),
       x="",
       color = "WWF Biome", fill = "WWF Biome", subtitle= "a)") + guides(col = guide_legend(nrow = 4)) #+
#xlim(0,800)+ ylim(0,200)+
#scale_x_log10() + scale_y_log10() 

figure_s6_a



rich_seed_biome_broad.fixed.p <- as_draws_df(rich_seeds, subset = floor(runif(n = 1000, 1, max = 2000)))

rich_seed_biome_broad.fixed.p
nrow(rich_seed_biome_broad.fixed.p)
head(rich_seed_biome_broad.fixed.p)
colnames(rich_seed_biome_broad.fixed.p)

# select columns of interests and give meaningful names
rich_seed_biome_broad_global_posterior <-  rich_seed_biome_broad.fixed.p %>% 
  as_tibble() %>%
  mutate(seed.aq.global = (`b_log_Total_Seeds` ),
         seed.arable.global = (`b_log_Total_Seeds` + `b_log_Total_Seeds:Biome_Broad_HabArable` ),
         seed.bor.global = (`b_log_Total_Seeds` + `b_log_Total_Seeds:Biome_Broad_HabBorealForestsDTaiga` ),
         seed.des.global = (`b_log_Total_Seeds` + `b_log_Total_Seeds:Biome_Broad_HabDesertsandXericShrublands`),
         seed.medfs.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabMediterraneanForestsWoodlandsandScrub`),
         seed.mongs.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabMontaneGrasslandsandShrublands`),
         seed.tempbf.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTemperateBroadleafandMixedForests`),
         seed.tempcf.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTemperateConiferForests`),
         seed.tempgs.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTemperateGrasslandsSavannasandShrublands`),
         seed.tropf.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTropicalandSubtropicalForests`),
         seed.tropgs.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTropicalandSubtropicalGrasslandsSavannasandShrublands`),
         seed.tund.global = (`b_log_Total_Seeds`+ `b_log_Total_Seeds:Biome_Broad_HabTundra`),
  ) %>%
  dplyr::select(c(seed.aq.global, seed.arable.global, seed.bor.global, seed.des.global, 
                  seed.medfs.global,
                  seed.mongs.global, seed.tempbf.global, seed.tempcf.global, seed.tempgs.global, 
                  seed.tropf.global, seed.tropgs.global, seed.tund.global
                  ))

View(rich_seed_biome_broad_global_posterior)
head(rich_seed_biome_broad_global_posterior)


sb_seed_area_zone$biome_broad_WWF<- as.factor(sb_seed_area_zone$biome_broad_WWF)
levels(sb_seed_area_zone$biome_broad_WWF)

seed.aq.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Aquatic", eff = mean(seed.aq.global),
          eff_lower = quantile(seed.aq.global, probs=0.025),
          eff_upper = quantile(seed.aq.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.arable.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Arable", eff = mean(seed.arable.global),
          eff_lower = quantile(seed.arable.global, probs=0.025),
          eff_upper = quantile(seed.arable.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.bor.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Boreal Forests/Taiga", eff = mean(seed.bor.global),
          eff_lower = quantile(seed.bor.global, probs=0.025),
          eff_upper = quantile(seed.bor.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.des.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Deserts and Xeric Shrublands", eff = mean(seed.des.global),
          eff_lower = quantile(seed.des.global, probs=0.025),
          eff_upper = quantile(seed.des.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


seed.medfs.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Mediterranean Forests, Woodlands and Scrub", eff = mean(seed.medfs.global),
          eff_lower = quantile(seed.medfs.global, probs=0.025),
          eff_upper = quantile(seed.medfs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.mongs.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Montane Grasslands and Shrublands", eff = mean(seed.mongs.global),
          eff_lower = quantile(seed.mongs.global, probs=0.025),
          eff_upper = quantile(seed.mongs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempbf.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Broadleaf and Mixed Forests", eff = mean(seed.tempbf.global),
          eff_lower = quantile(seed.tempbf.global, probs=0.025),
          eff_upper = quantile(seed.tempbf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempcf.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Conifer Forests", eff = mean(seed.tempcf.global),
          eff_lower = quantile(seed.tempcf.global, probs=0.025),
          eff_upper = quantile(seed.tempcf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tempgs.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Temperate Grasslands, Savannas and Shrublands", eff = mean(seed.tempgs.global),
          eff_lower = quantile(seed.tempgs.global, probs=0.025),
          eff_upper = quantile(seed.tempgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

# Tropical and Subtropical Forests
#seed.tropf.global
seed.tropf.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Forests", eff = mean(seed.tropf.global),
          eff_lower = quantile(seed.tropf.global, probs=0.025),
          eff_upper = quantile(seed.tropf.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tropgs.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Tropical and Subtropical Grasslands, Savannas and Shrublands", eff = mean(seed.tropgs.global),
          eff_lower = quantile(seed.tropgs.global, probs=0.025),
          eff_upper = quantile(seed.tropgs.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 

seed.tund.p <-  rich_seed_biome_broad_global_posterior %>% 
  mutate( response = "Tundra", eff = mean(seed.tund.global),
          eff_lower = quantile(seed.tund.global, probs=0.025),
          eff_upper = quantile(seed.tund.global, probs=0.975)) %>%
  dplyr::select(c(eff,eff_upper,eff_lower,response)) %>% distinct() 


global.rich_seed_biome_broad.p <- bind_rows(seed.aq.p, seed.arable.p,
                                seed.bor.p, seed.des.p,
                                # seed.fgs.p, seed.mang.p,
                                 seed.medfs.p, seed.mongs.p, seed.tempbf.p, seed.tempcf.p, seed.tempgs.p, 
                                #seed.tropcf.p, seed.tropdbf.p, seed.tropmbf.p,
                                seed.tropf.p, seed.tropgs.p, seed.tund.p
                                 ) %>%
  mutate(  Model = "Richness and seeds",
         `WWF Biome` = response ,
           Estimate = round(eff, 2),
         `Lower CI` = round(eff_lower, 2),
         `Upper CI` = round(eff_upper, 2),
  ) %>% select(-c(eff, eff_lower, eff_upper, response)) %>% arrange(Estimate)

head(global.rich_seed_biome_broad.p)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(global.rich_seed_biome_broad.p, file = 'global.rich_seed_biome_broad.posteriors.Rdata')


setwd(paste0(path2wd, 'Tables/'))
write.csv(global.rich_seed_biome_broad.p, "table_5.csv")


setwd(paste0(path2wd, 'Data/'))
#load('seed.area.poisson.mod_dat.Rdata')
load('global.rich_seed_biome_broad.posteriors.Rdata')


global.rich_seed_biome_broad.p

global.rich_seed_biome_broad.p <- global.rich_seed_biome_broad.p %>% mutate(`WWF Biome` = fct_relevel(`WWF Biome`,
                                                                                            "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                                                            "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                                                            "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                                                            "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                                                            "Aquatic", "Arable"
))

figure_s6_b <- ggplot() + 
  geom_point(data = global.rich_seed_biome_broad.p, aes(x = `WWF Biome`, y = Estimate, color=`WWF Biome`),size = 2) +
  geom_errorbar(data = global.rich_seed_biome_broad.p, aes(x = `WWF Biome`, ymin = `Lower CI`,
                                               ymax = `Upper CI`, color = `WWF Biome`),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope', subtitle = "") +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(limits = c(-0.2, 0.4), breaks=c(0, 0.2, 0.4)) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  labs(y = "Slope", # x = expression(paste('Total Sample Area ' , m^2)),
       x="",
       color = "WWF Biome", subtitle= "b)") + 
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #panel.background = element_rect(fill = "transparent"), # bg of the panel
                               plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                              # axis.text.x=element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

figure_s6_b


(figure_s6_a )/( figure_s6_b) +  plot_layout(ncol=1, nrow=2, heights = c(12,7))

# lnadscape 16 X 20

