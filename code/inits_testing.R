



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

ggs(seed_m2) %>%
  filter(Parameter == "b_Biome_Broad_HabBorealForestsDTaiga") %>% 
  mutate(chain = factor(Chain),
         intercept = value) %>%
  ggplot(aes(x = Iteration, y = intercept, color = chain)) +
  geom_trace() +
  coord_cartesian(ylim = c(-2500, 7500))

