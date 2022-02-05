
library(tidyverse)
library(brms)
library(bayesplot)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'gsb_slim.csv'))

head(sb)
colnames(sb)

# do the calc's we need
sb_calc <- sb %>% mutate( Total_Sample_Volume_mm3 = (Total_Number_Samples * Sample_Volume_mm3),
                  Total_Sample_Area_mm2 = (Total_Number_Samples * Sample_Area_mm2),
                  log_Total_Number_Samples = log(Total_Number_Samples),
                  log_Total_Sample_Volume_mm3 = log(Total_Sample_Volume_mm3),
                  log_Total_Sample_Area_mm2 = log(Total_Sample_Area_mm2),
                   Centred_Total_Number_Samples = Total_Number_Samples - mean(Total_Number_Samples, na.rm = TRUE),
                   Centred_Total_Sample_Volume_mm3 = Total_Sample_Volume_mm3 - mean(Total_Sample_Volume_mm3, na.rm = TRUE),
                   Centred_Total_Sample_Area_mm2 = Total_Sample_Area_mm2 - mean(Total_Sample_Area_mm2, na.rm = TRUE)
                  ) 


head(sb_calc)

sb_calc$Biome_WWF_Zone<- as.factor(as.character(sb_calc$Biome_WWF_Zone))
levels(sb_calc$Biome_WWF_Zone)

sb_deets <- sb_calc %>% summarise(`min-Total_Number_Samples` = min(as.numeric(Total_Number_Samples), na.rm = TRUE),
                             `max-Total_Number_Samples` = max(as.numeric(Total_Number_Samples),na.rm = TRUE),
                             `min-Sample_Area_mm2` = min(as.numeric(Sample_Area_mm2),na.rm = TRUE),
                             `max-Sample_Area_mm2` = max(as.numeric(Sample_Area_mm2), na.rm = TRUE),
                             `max-Total_Sample_Area_mm2` = max(as.numeric(Total_Sample_Area_mm2), na.rm = TRUE),
                             `min-Total_Sample_Area_mm2` = min(as.numeric(Total_Sample_Area_mm2), na.rm = TRUE),
                             `min-Sample_Volume_mm3` = min(as.numeric(Sample_Volume_mm3), na.rm = TRUE),
                             `max-Sample_Volume_mm3` = max(as.numeric(Sample_Volume_mm3),na.rm = TRUE),
                             `min-Total_Sample_Volume_mm3` = min(as.numeric(Total_Sample_Volume_mm3), na.rm = TRUE),
                             `max-Total_Sample_Volume_mm3`= max(as.numeric(Total_Sample_Volume_mm3),na.rm = TRUE),
                             `min-Total_Species` = min(as.numeric(Total_Species), na.rm = TRUE),
                             `max-Total_Species` = max(as.numeric(Total_Species),na.rm = TRUE),
                             `min-Seed_density_m2` = min(as.numeric(Seed_density_m2), na.rm = TRUE),
                             `max-Seed_density_m2` = max(as.numeric(Seed_density_m2),na.rm = TRUE),
                             `min-Total_Seeds` = min(as.numeric(Total_Seeds), na.rm = TRUE),
                             `max-Total_Seeds` = max(as.numeric(Total_Seeds),na.rm = TRUE),
                                                       ) %>%
  pivot_longer(`min-Total_Number_Samples` : `max-Total_Seeds`) %>%
  separate(name, into = c("minmax", "name"), sep="-") %>% spread(minmax, value)

sb_deets
# uh-oh we have richness , total seeds and density- equals = 0, and that 0 is real, a problem for poisson and log
#  temporarily convert 0's to 1's and figure out whats best later
# could use other distributions instead...

# for now, change 0's to 1's- to discuss later
sb_prep <- sb_calc %>% mutate( Total_Species2 = case_when(Total_Species == 0 ~ 1, 
                                                 TRUE ~ as.numeric(as.character(Total_Species))),
                          Seed_density_m22 = case_when(Seed_density_m2 == 0 ~ 1, 
                                                     TRUE ~ as.numeric(as.character(Seed_density_m2))),
                          Total_Seeds2 = case_when(Total_Seeds == 0 ~ 1, 
                                                       TRUE ~ as.numeric(as.character(Total_Seeds))),
                          ) %>%
  mutate(log_Total_Species2 = log(Total_Species2),
         log_Seed_density_m22 = log(Seed_density_m22),
         log_Seed_density_m22 = log(Seed_density_m22) )

head(sb_prep)
nrow(sb_prep)

is.numeric(sb_prep$Total_Species2)
min(sb_prep$Total_Species2, na.rm = TRUE)
max(sb_prep$Total_Species2, na.rm = TRUE)

sb00 <- sb_calc %>% filter(Total_Species == 0)
sb00
nrow(sb00)

setwd(paste0(path2wd, 'Data/'))
write.csv(sb_prep,  "sb_prep.csv")

# remove NA values from our predictor and response to get same number of rows to match model 
# use this to match up data for residual effects
sb_prep_area <- sb_prep %>% filter(!is.na(Total_Species2),
                                !is.na(Total_Sample_Area_mm2))
nrow(sb_prep_area)


sb_prep_samps <- sb_prep %>% filter(!is.na(Total_Species2),
                                   !is.na(Total_Number_Samples))
nrow(sb_prep_samps)

# first explore data
# 3 possible metrics for continuous 'sample effort'
colnames(sb)

ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales= "free") +
  geom_point(data = sb_prep_r ,#%>% filter(Biome_WWF_Zone == "Boreal"),
             aes(x = Total_Sample_Volume_mm3, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  coord_cartesian(xlim = c(min(sb_prep_r$Total_Sample_Volume_mm3), quantile(sb_prep_r$Total_Sample_Volume_mm3, 0.90))) +
  theme_classic( ) 


ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales = "free") +
  geom_point(data = sb_prep_r , #%>% filter(Biome_WWF_Zone == "Boreal"),
             aes(x = Total_Number_Samples, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  #xlim(0,1000)
  coord_cartesian(xlim = c(min(sb_prep_r$Total_Number_Samples), quantile(sb_prep_r$Total_Number_Samples, 0.90)))+
  theme_classic( ) 

ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales = "free") +
  geom_point(data = sb_prep_r , #%>% filter(Biome_WWF_Zone == "Boreal"),
             aes(x = Total_Sample_Area_mm2, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  coord_cartesian(xlim = c(min(sb_prep_r$Sample_Area_mm2), quantile(sb_prep_r$Sample_Area_mm2, 0.95)))+
  theme_classic( ) 


# try a first model
# rich.mod <- brm(Total_Species2 ~ log_Total_Sample_Volume_mm3 * Biome_WWF_Zone + (log_Total_Sample_Volume_mm3 * Biome_WWF_Zone  | Habitat_Broad/studyID/samp.loc ), 
#                 family = poisson(), data = sb_prep, cores = 4, chains = 4)

# takes about 3 hours, will set up cluster folder to run some more mods with lessons learned from this one
setwd(paste0(path2wd, 'Model_Fits/'))
setwd('~/Desktop/')
load( 'gsb_rich_samps-12179403.Rdata')
# save model object
# save(rich.mod, file = 'rich.mod.Rdata')
#load( 'rich.mod.Rdata')

# does not converge buts gives us hints for fixing and next steps
summary(rich.samps)

color_scheme_set("darkgray")
pp_rich.samps <- pp_check(rich.samps)+ xlab( "Species richness") + ylab("Density") +
  labs(title= "") +
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values
# posterior predictive check
# grey lines are predicted values, black is observed
pp_rich.samps
# a bit wonky, we can see where predictions dont fit data

# caterpillars/chains
plot(rich.samps) 
# ewww messy


sb_prep_r$Habitat_Broad <- as.factor(as.character(sb_prep_r$Habitat_Broad))
sb_prep_r$studyID <- as.factor(as.character(sb_prep_r$studyID))

# check model residuals
ma <- residuals(rich.samps)
ma <- as.data.frame(ma)
ar.plot <- cbind(sb_prep_samps, ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Habitat_Broad, ma$Estimate))
with(ar.plot, plot(studyID, ma$Estimate))
# surprisingly not bad


View(sb_prep_samps)
# for plotting fixed effects
rich.samps_fitted <- cbind(rich.samps$data,
                             fitted(rich.samps, re_formula = NA
                             )) %>% 
  as_tibble() %>% inner_join(sb_prep %>% select(Total_Species, Total_Species2, 
                                                    Total_Number_Samples,
                                                  Biome_WWF_Zone, Habitat_Broad, studyID, rowID),
                           #  by= c("Total_Species2","Biome_WWF_Zone","Habitat_Broad","studyID", "rowID")
  )


head(rich.samps_fitted)
nrow(rich.samps_fitted)



# fixed effect coefficients
rich.samps_fixef <- fixef(rich.samps)
head(rich.samps_fixef)

# Random effect coefficients
rich.samps_coef <- coef(rich.samps)
rich.samps_coef 

# predict estimates for each habitat within each biome across a sequence of log_total_volumes and total_volumes
# we could also just extract the coefs, but since we are plotting a linear curve in log space this is more accurate
# and better practice
obs_nest.rich.samps <- sb_prep %>% 
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone,
         Habitat_Broad_group = Habitat_Broad) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone, Habitat_Broad_group, Habitat_Broad) %>% 
  summarise(log_Total_Number_Samples = seq(min(log_Total_Number_Samples, na.rm = TRUE), max(log_Total_Number_Samples, na.rm = TRUE),  length.out = 20 ),
            Total_Number_Samples = seq(min(Total_Number_Samples, na.rm = TRUE), max(Total_Number_Samples, na.rm = TRUE),  length.out = 20)) %>%
  nest(data = c( Biome_WWF_Zone, Habitat_Broad, log_Total_Number_Samples, Total_Number_Samples)) %>%
  mutate(predicted = map(data, ~predict(rich.samps, newdata= .x, re_formula = ~(log_Total_Number_Samples * Biome_WWF_Zone | Habitat_Broad) ))) 


View(obs_nest.rich.samps)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(rich.samps_fitted, rich.samps_fixef, obs_nest.rich.samps, file = 'rich.samps.mod_dat.Rdata')
load('rich.samps.mod_dat.Rdata')

 # plot richness number of sample relationship
colnames(sb_prep_r)


fig_rich.samps <- ggplot() + 
   facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = rich.samps_fitted ,
  aes(x = Total_Number_Samples, y = Total_Species,
      colour = Habitat_Broad),
  size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
 # random slopes
  geom_line(data = obs_nest.rich.samps  %>% unnest(cols = c(data, predicted)) ,
            aes(x = Total_Number_Samples, y= predicted[,1] ,
                                        group = Habitat_Broad,
                                        colour = Habitat_Broad),
            size = 1.2) +
  # fixed effect
  geom_line(data = rich.samps_fitted,
            aes(x = Total_Number_Samples, y = Estimate),
            size = 1.5) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich.samps_fitted,
              aes(x = Total_Number_Samples, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_r$Total_Sample_Area_mm2), quantile(sb_prep_r$Total_Sample_Area_mm2, 0.97))) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(subtitle= ''
  ) +
  ylab("Total Species")  + xlab("Total_Number_Samples")

fig_rich.samps


# AREA ################################################################

setwd('~/Desktop/')
load( 'gsb_rich_area-12179402.Rdata')
# save model object
# save(rich.mod, file = 'rich.mod.Rdata')
#load( 'rich.mod.Rdata')

# does not converge buts gives us hints for fixing and next steps
summary(rich.area)

color_scheme_set("darkgray")
pp_rich.area <- pp_check(rich.area)+ xlab( "Species richness") + ylab("Density") +
  labs(title= "") +
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values
# posterior predictive check
# grey lines are predicted values, black is observed
pp_rich.area
# a bit wonky, we can see where predictions dont fit data

# caterpillars/chains
plot(rich.area) 
# ewww messy


sb_prep_r$Habitat_Broad <- as.factor(as.character(sb_prep_r$Habitat_Broad))
sb_prep_r$studyID <- as.factor(as.character(sb_prep_r$studyID))

# check model residuals
ma <- residuals(rich.area)
ma <- as.data.frame(ma)
ar.plot <- cbind(sb_prep_area, ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Habitat_Broad, ma$Estimate))
with(ar.plot, plot(studyID, ma$Estimate))
with(ar.plot, plot(rowID, ma$Estimate))
# surprisingly not bad

# for plotting fixed effects
rich.area_fitted <- cbind(rich.area$data,
                           fitted(rich.area, re_formula = NA
                           )) %>% 
  as_tibble() %>% left_join(sb_prep_r %>% distinct(Total_Species, Total_Species2, 
                                                    Total_Sample_Area_mm2,
                                                    Biome_WWF_Zone, Habitat_Broad, studyID, rowID),
                             #by= c("")
  )


head(rich.area_fitted)
nrow(rich.area_fitted)


# fixed effect coefficients
rich.area_fixef <- fixef(rich.area)
head(rich.area_fixef)

# Random effect coefficients
rich.area_coef <- coef(rich.area)
rich.area_coef 

# predict estimates for each habitat within each biome across a sequence of log_total_volumes and total_volumes
# we could also just extract the coefs, but since we are plotting a linear curve in log space this is more accurate
# and better practice
obs_nest.rich.area <- sb_prep %>% 
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone,
         Habitat_Broad_group = Habitat_Broad) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone, Habitat_Broad_group, Habitat_Broad) %>% 
  summarise(log_Total_Sample_Area_mm2 = seq(min(log_Total_Sample_Area_mm2, na.rm = TRUE), max(log_Total_Sample_Area_mm2, na.rm = TRUE), length.out = 20 ),
            Total_Sample_Area_mm2 = seq(min(Total_Sample_Area_mm2, na.rm = TRUE), max(Total_Sample_Area_mm2, na.rm = TRUE), length.out = 20)) %>%
  nest(data = c( Biome_WWF_Zone, Habitat_Broad, log_Total_Sample_Area_mm2, Total_Sample_Area_mm2)) %>%
  mutate(predicted = map(data, ~predict(rich.area, newdata= .x, re_formula = ~(log_Total_Sample_Area_mm2 * Biome_WWF_Zone | Habitat_Broad) ))) 


View(obs_nest.rich.area)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
save(rich.area_fitted, rich.area_fixef, obs_nest.rich.area, file = 'rich.area.mod_dat.Rdata')
load('rich.area.mod_dat.Rdata')

# plot richness area relationship
colnames(sb_prep_r)


fig_rich.area <- ggplot() + 
  facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = rich.area_fitted ,
             aes(x = Total_Sample_Area_mm2, y = Total_Species,
                 colour = Habitat_Broad),
             size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
  # random slopes
  geom_line(data = obs_nest.rich.area  %>% unnest(cols = c(data, predicted)) ,
            aes(x = Total_Sample_Area_mm2, y= predicted[,1] ,
                                        group = Habitat_Broad,
                                        colour = Habitat_Broad),
            size = 1.2) +
  # fixed effect
  geom_line(data = rich.area_fitted,
            aes(x = Total_Sample_Area_mm2, y = Estimate),
            size = 1.5) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich.area_fitted,
              aes(x = Total_Sample_Area_mm2, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  #coord_cartesian(xlim = c(min(sb_prep_r$Total_Sample_Area_mm2), quantile(sb_prep_r$Total_Sample_Area_mm2, 0.97))) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(subtitle= ''
  ) +
  ylab("Total Species")  + xlab("Total_Sample_Area_mm2")

fig_rich.area

# data produced in 'Emma_Posterior_Samples.R'
setwd(paste0(path2wd, 'Data/'))
load('global.posteriors.Rdata')
load('habitat.posteriors.Rdata')

fig_rich.area_global_zones <- ggplot() + 
  geom_point(data = global.rich.area.p, aes(x = response, y = eff,color=response),size = 2) +
  geom_errorbar(data = global.rich.area.p, aes(x = response,ymin = eff_lower,
                                               ymax = eff_upper, color=response),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_global_zones


fig_rich.area_habitat <- ggplot() + 
  facet_wrap(~response) +
  geom_point(data = hab.rich.area.p, aes(x = Habitat_Broad, y = eff,color=Habitat_Broad),size = 2) +
  geom_errorbar(data = hab.rich.area.p, aes(x = Habitat_Broad,ymin = eff_lower,
                                            ymax = eff_upper, color=Habitat_Broad),
                width = 0, size = 0.7) +
  labs(x = '',
       y='Slope') +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(breaks=c(0,-8)) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=12)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.1, unit = "cm"),
                               strip.background = element_blank(),legend.position="none")

fig_rich.area_habitat



# what priors did the model assume under default settings?

prior_summary(rich.mod)
# no comment for now, will return to this later

summary(rich.mod)
# Effective Sample Sizes (ESS) are very low
# because we have so many groups....and we need to increase sampling iterations


get_prior(Total_Species2 ~ log_Total_Sample_Volume_mm3 * Biome_WWF_Zone + (log_Total_Sample_Volume_mm3 * Biome_WWF_Zone  | Habitat_Broad/studyID/samp.loc ), 
                           family = poisson(), data = sb_prep, cores = 4, chains = 4)


get_prior(Total_Species ~ log_Total_Sample_Volume_mm3 * Biome_WWF_Zone + (log_Total_Sample_Volume_mm3 * Biome_WWF_Zone  | Habitat_Broad/studyID/samp.loc ), 
          family = hurdle_lognormal(), data = sb_prep, cores = 4, chains = 4)


get_prior(Total_Species ~ log_Total_Sample_Volume_mm3 * Biome_WWF_Zone + (log_Total_Sample_Volume_mm3 * Biome_WWF_Zone  | Habitat_Broad/studyID/samp.loc ), 
          family = student(), data = sb_prep, cores = 4, chains = 4)




