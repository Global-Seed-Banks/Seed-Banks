
#packages
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)
library(viridis)
library(sjPlot)
library(performance)
library(ggplot2)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  "keil" = "C:/Users/keil/Dropbox/GSB/")

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
          rowID = as.factor(rowID))

head(sb_prep_area)
nrow(sb_prep_area)

# the model
rich.area <- glm(Total_Species ~ Biome_WWF_Zone*log(Total_Sample_Area_mm2),
                 family = poisson(), data = sb_prep)

# overall summary
summary(rich.area)

# explained deviance (percentage) - a.k.a. McFadden R2:
r2_mcfadden(rich.area)

# in log-log
plot_model(rich.area, type = "pred", terms = c("Total_Sample_Area_mm2", "Biome_WWF_Zone")) +
           scale_x_log10() + scale_y_log10() 

# on linear scale
plot_model(rich.area, type = "pred", terms = c("Total_Sample_Area_mm2", "Biome_WWF_Zone")) 

# voilá! ;-)
