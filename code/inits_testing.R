



library(ggmcmc)
library(lisa)

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'density_m2.Rdata')
load( 'seed_m2.Rdata')
load( 'rich_m2.Rdata')



# density_biome_broad
# density_habs
# density_deg

# model summary
summary(density_m2)

# posterior predictive check
color_scheme_set("darkgray")
pp_den.biome_broad <- pp_check(density_m2)+ xlab( "Total density") + ylab("Density") +
  labs(title= "") + xlim(0,10000)+ ylim(0,0.00075)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_den.biome_broad 

color_scheme_set("darkgray")
pp_seed.biome_broad <- pp_check(seeds_m2) + xlab( "Total Seeds") + ylab("Density") +
  labs(title= "") + xlim(0,1000)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

pp_seed.biome_broad 



# caterpillars/chains
plot(density_m2)


density.fixed.p <- as_draws_df(density_m2, subset = floor(runif(n = 1000, 1, max = 2000)))

density.fixed.p
nrow(density.fixed.p)
head(density.fixed.p)
colnames(density.fixed.p)


# define the color palette
fk <- lisa_palette("FridaKahlo", n = 31, type = "continuous")

geom_trace <- function(subtitle = NULL, 
                       xlab = "iteration", 
                       xbreaks = 0:4 * 500) {
  
  list(
    annotate(geom = "rect", 
             xmin = 0, xmax = 1000, ymin = -Inf, ymax = Inf,
             fill = fk[16], alpha = 1/2, size = 0),
    geom_line(size = 1/3),
    scale_color_manual(values = fk[c(3, 8, 27, 31)]),
    scale_x_continuous(xlab, breaks = xbreaks, expand = c(0, 0)),
    labs(subtitle = subtitle),
    theme(panel.grid = element_blank())
  )
  
}

ggs(seeds_m2) %>%
  filter(Parameter == "b_Biome_Broad_HabBorealForestsDTaiga") %>% 
  mutate(chain = factor(Chain),
         intercept = value) %>%
  ggplot(aes(x = Iteration, y = intercept, color = chain)) +
  geom_trace() #+
  #coord_cartesian(ylim = c(-2500, 7500))



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



sb_seed_area %>% 
  ggplot(aes(x = Seed_density_m2)) +
  geom_density(fill = fk[3], color = fk[3]) + xlim(0,25000)+ ylim(0,0.00025)



 tibble(Total_Seeds = seq(from = 0, to = 25000, length.out = 1000000),
       d = dexgaussian(Total_Seeds, mu = 2000, sigma =150, beta = 420)) %>% 
  ggplot( aes(x = Total_Seeds)) +
  geom_density(data = sb_seed_area,
               fill = fk[3], color = fk[3]) +
  geom_line(aes(y = d), 
            color = fk[31], size = 5/4) +
  # zoom in on the bulk of the values
  coord_cartesian(xlim = c(0, 25000))



set_inits <- function(seed = 1) {
  
  set.seed(seed)
  list(
    Intercept = rnorm(n = 1, mean = 3000, sd = 500),
    sigma     = runif(n = 1, min = 100, max = 150),
    beta      = runif(n = 1, min = 350, max = 500)
  )
  
}

# try it out
set_inits(seed = 0)

my_inits <- list(
  # different seed values will return different results
  set_inits(seed = 1),
  set_inits(seed = 2),
  set_inits(seed = 3),
  set_inits(seed = 4)
)

# what have we done?
str(my_inits)
