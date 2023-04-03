
library(tidyverse)
library(brms)
library(MCMCvis)



#packages
library(plyr)
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


# rich_m2 <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + ( 1  | Method/studyID ),
#                     family = poisson(), data = sb_dat, cores = 4, chains = 4, iter = 4000, warmup = 1000,
#                     control = list(adapt_delta = 0.99999,
#                                    max_treedepth = 13)
# )


# ATTEMPT NUMBER 1: NORMAL PRIORS (ON MULTIPLE PARAMETERS) WITH LOWE BOUND OF 0 
form <- formula( Total_Species ~ Centred_log_Total_Sample_Area_m2  * Biome_Broad_Hab + Centred_log_Number_Sites + ( 1  | Method/studyID ))

prior <- get_prior(form,  data = sb_dat, family = poisson(link="log"))

prior$prior[13:36] <- "normal(0, 10)"
prior$lb[13:36] <- 0


rich_m2 <- brm(form,
               family = poisson(link="log"), 
               data = sb_dat, 
               cores = 2, 
               chains = 2, 
               iter = 400, 
               warmup = 100,
               prior = prior, # use the modified priors from above
               control = list(adapt_delta = 0.999,
                              max_treedepth = 13
               ))

MCMCplot(rich_m2)


# ATTEMPT NUMBER 1: LOGNORMAL PRIORS ON ALL PARAMETERS, WITH LOWER BOUND OF 0 
# ON ALL FIXED EFFECT COEFFICIENTS

rich_m2_lp <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2  * Biome_Broad_Hab + Centred_log_Number_Sites + ( 1  | Method/studyID ),
               family = poisson(link="log"), 
               data = sb_rich_area, 
               cores = 1, 
               chains = 1, 
               iter = 2000, 
               warmup = 1000,
               prior = c(prior(lognormal(0, 1), # lognormal
                               class = "b", # apply to fixed effects only
                               lb = 0)), # lower bound
               # control = list(adapt_delta = 0.999,
               #                max_treedepth = 13 )
               )


summary(rich_m2_lp)

pp_check(rich_m2_lp)

conditional_effects(rich_m2_lp)

rich_m2_sp <- brm(Total_Species ~ Centred_log_Total_Sample_Area_m2 * Biome_Broad_Hab + Centred_log_Number_Sites + ( 1  | Method/studyID ),
    family = poisson(), data = sb_rich_area, cores = 1, chains = 1, iter = 2000, warmup = 1000,
    prior = c(prior( student_t(3, 0.5, 1) , class = "b",  lb = 0)),
    # control = list(adapt_delta = 0.99999,
    #                max_treedepth = 13) 
    )

summary(rich_m2_sp)

pp_check(rich_m2_sp)

conditional_effects(rich_m2_sp)

plot(rich_m2_sp)


