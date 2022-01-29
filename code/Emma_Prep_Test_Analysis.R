
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
                  log_Total_Sample_Area_mm2 = log(Total_Sample_Area_mm2) ) %>%
 unite("samp.loc", Lat_Deg, Lon_Deg, sep = "_" , remove = F) 


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
                          )

sb00 <- sb0 %>% filter(Total_Species == 0)
sb00

setwd(paste0(path2wd, 'Data/'))
write.csv(sb_prep,  "sb_prep.csv")

# remove NA values from our predictor and response to get same number of rows to match model 
# use this to match up data later too
sb_prep_r <- sb_prep %>% filter(!is.na(Total_Species2),
                                !is.na(log_Total_Sample_Volume_mm3))
nrow(sb_prep_r)


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
# save model object
# save(rich.mod, file = 'rich.mod.Rdata')
load( 'rich.mod.Rdata')

# does not converge buts gives us hints for fixing and next steps
summary(rich.mod)

color_scheme_set("darkgray")
pp_rich <- pp_check(rich.mod)+ xlab( "Species richness") + ylab("Density") +
  labs(title= "")+
  theme_classic()+  theme(legend.position= "bottom") # predicted vs. observed values
# posterior predictive check
# grey lines are predicted values, black is observed
pp_rich
# a bit wonky, we can see where predictions dont fit data


sb_prep$Habitat_Broad <- as.factor(as.character(sb_prep$Habitat_Broad))
sb_prep$studyID <- as.factor(as.character(sb_prep$studyID))

# check model residuals
ma <- residuals(rich.mod)
ma <- as.data.frame(ma)
ar.plot <- cbind(sb_prep_r, ma$Estimate)

par(mfrow=c(1,2))
with(ar.plot, plot(Habitat_Broad, ma$Estimate))
with(ar.plot, plot(studyID, ma$Estimate))
# surprisingly not bad

# for plotting fixed effects
rich_fitted <- cbind(rich.mod$data,
                             fitted(rich.mod, re_formula = NA
                             )) %>% 
  as_tibble() %>% inner_join(sb_prep_r %>% distinct(Total_Species, Total_Species2, log_Total_Sample_Volume_mm3,
                                                  Total_Sample_Volume_mm3,
                                                  Biome_WWF_Zone, Habitat_Broad, studyID, samp.loc),
                             #by= c("")
  )


head(rich_fitted)

# fixed effect coefficients
rich_fixef <- fixef(rich.mod)
head(rich_fixef)

# Random effect coefficients
rich_coef <- coef(rich.mod)
rich_coef 

# predict estimates for each habitat within each biome across a sequence of log_total_volumes and total_volumes
# we could also just extract the coefs, but since we are plotting a linear curve in log space this is more accurate
# and better practice
obs_nest.rich <- sb_prep_r %>% 
  mutate(Biome_WWF_Zone_group = Biome_WWF_Zone,
         Habitat_Broad_group = Habitat_Broad) %>%
  group_by(Biome_WWF_Zone_group, Biome_WWF_Zone, Habitat_Broad_group, Habitat_Broad) %>% 
  summarise(log_Total_Sample_Volume_mm3 = seq(min(log_Total_Sample_Volume_mm3), max(log_Total_Sample_Volume_mm3), length.out = 20 ),
            Total_Sample_Volume_mm3 = seq(min(Total_Sample_Volume_mm3), max(Total_Sample_Volume_mm3), length.out = 20)) %>%
  nest(data = c( Biome_WWF_Zone, Habitat_Broad, log_Total_Sample_Volume_mm3, Total_Sample_Volume_mm3)) %>%
  mutate(predicted = map(data, ~predict(rich.mod, newdata= .x, re_formula = ~(log_Total_Sample_Volume_mm3 * Biome_WWF_Zone | Habitat_Broad) ))) 


View(obs_nest.rich)

setwd(paste0(path2wd, 'Data/'))
# save data objects to avoid time of compiling every time
#save(rich_fitted, rich_fixef, obs_nest.rich, file = 'rich.mod_dat.Rdata')
load('rich.mod_dat.Rdata')

colnames(sb_prep_r)


rich_fitted %>% distinct(Biome_WWF_Broad)

# LOL....*sigh*

fig_rich <- ggplot() + 
   facet_wrap(~Biome_WWF_Zone, scales="free") +
  # horizontal zero line
  geom_hline(yintercept = 0, lty = 2) +
  # raw data points
  geom_point(data = rich_fitted ,
  aes(x = Total_Sample_Volume_mm3, y = Total_Species,
      colour = Habitat_Broad),
  size = 1.2, shape=1, position = position_jitter(width = 2, height=2.5)) +
 # random slopes
  geom_line(data = obs_nest.rich  %>% unnest(cols = c(data, predicted)) ,
            aes(x = Total_Sample_Volume_mm3, y= predicted[,1] ,
                                        group = Habitat_Broad,
                                        colour = Habitat_Broad),
            size = 1.2) +
  # fixed effect
  geom_line(data = rich_fitted,
            aes(x = Total_Sample_Volume_mm3, y = Estimate),
            size = 1.5) +
  # uncertainy in fixed effect
  geom_ribbon(data = rich_fitted,
              aes(x = Total_Sample_Volume_mm3, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  coord_cartesian(xlim = c(min(sb_prep_r$Total_Sample_Volume_mm3), quantile(sb_prep_r$Total_Sample_Volume_mm3, 0.97))) +
  scale_color_viridis(discrete = T, option="D")  +
  theme_bw(base_size=18 ) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),
                                  legend.position="bottom") +
  labs(subtitle= ''
  ) +
  ylab("Total Species")  + xlab("Total_Sample_Volume_mm3")

fig_rich



# what priors did the model assume under default settings?

prior_summary(rich.mod)
# no comment for now, will return to this later

summary(rich.mod)
# Effective Sample Sizes (ESS) are very low
# because we have so many groups....and we need to increase sampling iterations

# think i'll try total number of samps next
# I'll keep groups complexity for now....
# In obs nest can I fit a line to quantiles of the data instead of min and max?
# would visualise better




