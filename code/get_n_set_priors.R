


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

# get default priors
get_prior(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + (1 | Method/studyID/rowID ),
               family = poisson(), data = sb_prep)

get_prior(log_Total_Seeds ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + (1 | Method/studyID/rowID ),
          family = student(), data = sb_prep)


get_prior(Seed_density_m2 ~  Biome_Broad_Hab + (1 | Method/studyID/rowID ),
          family = student(), data = sb_prep)


get_prior(Total_Species ~ log_Total_Seeds * Biome_Broad_Hab + (1 | Method/studyID/rowID ),
          family = poisson(), data = sb_prep)


# work out how to set new priors so that slope is always greater than 1

# define some priors
#biome_prior <- c(prior(normal(1,2), class = b, coef = Biome_Broad_Hab) )

# prior = prior(student_t(3, 0.01, 2.5), coef = Biome_Broad_Hab)


load( '~/Desktop/rich_seeds.Rdata')



summary(rich_seeds)
