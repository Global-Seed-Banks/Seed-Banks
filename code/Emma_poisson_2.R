
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

# remove NA values 
sb_prep_area <- sb_prep %>% filter(!is.na(Total_Species2),
                                !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Biome_WWF_Zone = as.factor(Biome_WWF_Zone),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID),
          Method = as.factor(Method))


head(sb_prep_area)
nrow(sb_prep_area)


levels(sb_prep_area$Method)

# area model
rich.area <- brm(Total_Species ~ log_Total_Sample_Area_mm2 * Biome_WWF_Zone + (log_Total_Sample_Area_mm2 * Biome_WWF_Zone | Habitat_Broad/Method ),
                family = poisson(), data = sb_prep, cores = 4, chains = 4)


setwd(paste0(path2wd, 'Model_Fits/'))
# save model object
save(rich.area, file = 'gsb_rich_area-poisson_2.Rdata')
load( 'gsb_rich_area-poisson_2.Rdata')

# model summary
 summary(rich.area)

 # posterior predictive check
color_scheme_set("darkgray")
pp_rich.area <- pp_check(rich.area)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "") +
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values
pp_rich.area


# caterpillars/chains
plot(rich.area)


# check model residuals
ma <- residuals(rich.area)
ma <- as.data.frame(ma)
ar.plot <- cbind(sb_prep_area, ma$Estimate)

#make sure they are factors
ar.plot$Biome_WWF_Zone <- as.factor(ar.plot$Biome_WWF_Zone )
ar.plot$Habitat_Broad <- as.factor(ar.plot$Habitat_Broad )
ar.plot$studyID <- as.factor(ar.plot$studyID )
ar.plot$rowID <- as.factor(ar.plot$rowID )
#plot residuals
par(mfrow=c(2,2))
with(ar.plot, plot(Biome_WWF_Zone, ma$Estimate))
with(ar.plot, plot(Habitat_Broad, ma$Estimate))
with(ar.plot, plot(studyID, ma$Estimate))
with(ar.plot, plot(rowID, ma$Estimate))


 head(sb_prep_area)
#  
# # for plotting fixed effects
# rich.area_fitted <- cbind(rich.area$data,
#                              fitted(rich.area, re_formula = NA
#                              )) %>%
#   as_tibble() %>% inner_join(sb_prep_area %>% select(Total_Species, Total_Species2,
#                                                     Total_Number_Samples, Total_Sample_Area_mm2,
#                                                     log_Total_Sample_Area_mm2,
#                                                   Biome_WWF_Zone, Habitat_Broad, studyID, rowID),
#                            #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
#   )
# 
# 
# head(rich.area_fitted)
# nrow(rich.area_fitted)
# 
# 
# 
# # fixed effect coefficients
# rich.area_fixef <- fixef(rich.area)
# head(rich.area_fixef)
# 
# # Random effect coefficients
# rich.samps_coef <- coef(rich.area)
# rich.samps_coef
# 
# # predict estimates for each habitat within each biome across a sequence of log_total_volumes and total_volumes
# # we could also just extract the coefs, but since we are plotting a linear curve in log space this is more accurate
# # and better practice
# obs_nest.rich.area <- sb_prep_area %>%
#   mutate(Biome_WWF_Zone_group = Biome_WWF_Zone,
#          Habitat_Broad_group = Habitat_Broad) %>%
#   group_by(Biome_WWF_Zone_group, Biome_WWF_Zone, Habitat_Broad_group, Habitat_Broad) %>%
#   summarise(log_Total_Sample_Area_mm2 = seq(min(log_Total_Sample_Area_mm2, na.rm = TRUE), max(log_Total_Sample_Area_mm2, na.rm = TRUE),  length.out = 1000 ),
#             Total_Sample_Area_mm2 = seq(min(Total_Sample_Area_mm2, na.rm = TRUE), max(Total_Sample_Area_mm2, na.rm = TRUE),  length.out = 1000)) %>%
#   nest(data = c( Biome_WWF_Zone, Habitat_Broad, log_Total_Sample_Area_mm2, Total_Sample_Area_mm2)) %>%
#   mutate(predicted = map(data, ~predict(rich.area, newdata= .x, re_formula = ~(log_Total_Sample_Area_mm2  | Biome_WWF_Zone/Habitat_Broad) )))
# 
# 
# View(obs_nest.rich.area)
# 
# setwd(paste0(path2wd, 'Data/'))
# # # save data objects to avoid doing this every time
#  save(rich.area_fitted, rich.area_fixef, obs_nest.rich.area, file = 'rich.area.poisson.mod_dat.Rdata')
load('rich.area.poisson.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_area)


fig_rich.area <- ggplot() + 
   facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = rich.area_fitted ,
  aes(x = (Total_Sample_Area_mm2/1000000), y = Total_Species,
      colour = Biome_WWF_Zone),
  size = 1.2, shape=1, position = position_jitter(width = 0.25, height=2.5)) +
 # random slopes
  geom_line(data = obs_nest.rich.area  %>% unnest(cols = c(data, predicted)) ,
            aes(x = (Total_Sample_Area_mm2/1000000), y= predicted[,1] ,
                                        group = Biome_WWF_Zone,
                                        colour = Biome_WWF_Zone),
            size = 1.2) +
  # fixed effect
  geom_line(data = rich.area_fitted,
            aes(x = (Total_Sample_Area_mm2/1000000), y = Estimate),
            size = 1.5) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich.area_fitted,
              aes(x =  (Total_Sample_Area_mm2/1000000), ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_r$Total_Sample_Area_mm2), quantile(sb_prep_r$Total_Sample_Area_mm2, 0.97))) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(y = "Total Species",  x = expression(paste('Total Sample Area ' , m^2)),
       color = "WWF \n Biogeographic Zone") + guides(col = guide_legend(ncol = 2))

fig_rich.area


# sample posterior etc

setwd(paste0(path2wd, 'Model_Fits/'))
load( 'gsb_rich_area-poisson.Rdata')

# 
# # biome posterior samples
# zone_levels <- rich.area$data %>%
#   as_tibble() %>%
#   distinct(Biome_WWF_Zone) %>%
#   mutate(level =  Biome_WWF_Zone) %>%
#   nest_legacy(level)
# 
# 
# # extract 1000 biome-level posterior samples for each  wwf zone
# zone_rich.area_posterior <- zone_levels %>%
#   mutate( area.I.zone = purrr::map(data, ~posterior_samples(rich.area,
#                                                             pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',Intercept]', sep=''),
#                                                             fixed = TRUE,
#                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
#           area.zone = purrr::map(data, ~posterior_samples(rich.area,
#                                                           pars = paste('r_Biome_WWF_Zone[', as.character(.x$level), ',log_Total_Sample_Area_mm2]', sep=''),
#                                                           fixed = TRUE,
#                                                           subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
# 
#   )
# 
# head(zone_rich.area_posterior)
# 
# # habitat effects
# rich.area_zone_posterior <- zone_rich.area_posterior  %>%
#   dplyr::select(-data) %>%
#   unnest_legacy(area.I.zone, area.zone) %>%
#   mutate( area.trt.zone = (area.I.zone + area.zone))
# 
# # habitat level effects
# head(rich.area_zone_posterior)
# 
# rich.area.zone.p <-  rich.area_zone_posterior %>%
#   group_by(Biome_WWF_Zone) %>%
#   mutate( response = "Zones",
#           eff = mean(area.trt.zone),
#           eff_lower = quantile(area.trt.zone, probs=0.025),
#           eff_upper = quantile(area.trt.zone, probs=0.975)) %>%
#   dplyr::select(c(Biome_WWF_Zone,eff,eff_upper,eff_lower,response)) %>% distinct()
# 
# rich.area.zone.p
# 
# # global effects
# rich.area.fixed.p <- posterior_samples(rich.area, "^b" , subset = floor(runif(n = 1000, 1, max = 2000)))
# 
# head(rich.area.fixed.p)
# 
# 
# # select columns of interests and give meaningful names
# rich.area_global_posterior <-  rich.area.fixed.p %>% dplyr::select(`b_Intercept`,
#                                                                  `b_log_Total_Sample_Area_mm2`) %>%
#   mutate(rich.area.I.global =`b_Intercept`,
#          rich.area.global =`b_log_Total_Sample_Area_mm2`,
#          rich.area.trt.global = (rich.area.I.global + rich.area.global))  %>%
#   dplyr::select(-c(`b_Intercept`,
#                    `b_log_Total_Sample_Area_mm2`))
# 
# head(rich.area_global_posterior)
# 
# rich.area.global.p <-  rich.area_global_posterior %>%
#   mutate( response = "Global", Biome_WWF_Zone = "Global",
#           eff = mean(rich.area.trt.global),
#           eff_lower = quantile(rich.area.trt.global, probs=0.025),
#           eff_upper = quantile(rich.area.trt.global, probs=0.975)) %>%
#   dplyr::select(c(Biome_WWF_Zone,eff,eff_upper,eff_lower,response)) %>% distinct()
# 
# rich.area.global.p
# 
# setwd(paste0(path2wd, 'Data/'))
# # save data objects to avoid time of compiling every time
# save(rich.area.global.p,rich.area.zone.p, file = 'global.rich.area.poisson.zone.posteriors.Rdata')


setwd(paste0(path2wd, 'Data/'))
load('global.rich.area.poisson.zone.posteriors.Rdata')

head(rich.area.global.p)
head(rich.area.zone.p)

fig_rich.area_global <- ggplot() + 
  geom_point(data = rich.area.global.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = rich.area.global.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_global

head(rich.area.zone.p)

fig_rich.area_zones <- ggplot() + 
  #facet_wrap(~response) +
  geom_point(data = rich.area.zone.p, aes(x = Biome_WWF_Zone, y = eff,color=Biome_WWF_Zone),size = 2) +
  geom_errorbar(data = rich.area.zone.p, aes(x = Biome_WWF_Zone,ymin = eff_lower,
                                            ymax = eff_upper, color=Biome_WWF_Zone),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  #scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_zones



(fig_rich.area_global|fig_rich.area_zones)

