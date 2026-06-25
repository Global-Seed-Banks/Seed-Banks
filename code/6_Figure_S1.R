# ==============================================================================
# Seed Bank Database — Figure S1: Posterior Predictive Checks
# ==============================================================================
# Builds posterior predictive check (pp_check) panels for each biome's
# richness model (a) and density model (b), combined into two multi-panel
# figures with a shared legend, matching the Supplementary Information's
# Figure S1 (which has parts a and b only).
#
# ==============================================================================

library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)
library(viridis)

setwd('~/Dropbox/GSB/')

# models run on cluster, load in model objects here
load('Model_Fits/Habs/rich_aq.Rdata')
load('Model_Fits/Habs/rich_ar.Rdata')
load('Model_Fits/Habs/rich_forest.Rdata')
load('Model_Fits/Habs/rich_grass.Rdata')
load('Model_Fits/Habs/rich_med_de.Rdata')
load('Model_Fits/Habs/rich_po_alp.Rdata')
load('Model_Fits/Habs/rich_wetland.Rdata')

color_scheme_set("darkgray")

s1a_i <- pp_check(mod_tund_r) + xlab( "Total Species") + ylab("Density") +
  labs(title= "i) Tundra") + xlim(-200,300)+ ylim(0,0.045)+
  theme_classic()+  theme(legend.position= "none")

s1a_i

s1a_ii <-pp_check(mod_forest_r) + xlab( "Total Species") + ylab("Density") +
  labs(title= "ii) Forests") + xlim(-200,300)+ ylim(0,0.02)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1a_ii

s1a_iii <- pp_check(mod_grass_r) + xlab( "Total Species") + ylab("Density") +
  labs(title= "iii) Grassland") + xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none")

s1a_iii

s1a_iv <- pp_check(mod_med_de_r)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "iv) Mediterranean and Desert") + xlim(-200,300)+ ylim(0,0.02)+
  theme_classic()+  theme(legend.position= "none")

s1a_iv


s1a_v <- pp_check(mod_ar_r) + xlab( "Total Species") + ylab("Density") +
  labs(title= "v) Arable") + xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none")

s1a_v

s1a_vi <- pp_check(mod_wetland_r) + xlab( "Total Species") + ylab("Density") +
  labs(title= "vi) Wetland") + xlim(-200,300)+ ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none")

s1a_vi

s1a_vii <-pp_check(mod_aq_r)+ xlab( "Total Species") + ylab("Density") +
  labs(title= "vii) Aquatic") + xlim(-200,300)+ ylim(0,0.04)+
  theme_classic()+  theme(legend.position= "bottom")

s1a_vii


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

s1_legend <- g_legend(s1a_vii)

fig_s1a <- (s1a_i + s1a_ii + s1a_iii) /
  ( s1a_iv + s1a_v) /
  ( s1a_vi + s1a_vii  +  theme(legend.position= "none")) / (s1_legend) + plot_layout(heights = c(10, 10,  10, 1))

fig_s1a

ggsave("Figures/Fig_S1a.png", plot = fig_s1a, width = 14, height = 16, units = "in", dpi = 300)


load('Model_Fits/Habs/den_aq.Rdata')
load('Model_Fits/Habs/den_ar.Rdata')
load('Model_Fits/Habs/den_forest.Rdata')
load('Model_Fits/Habs/den_grass.Rdata')
load('Model_Fits/Habs/den_med_de.Rdata')
load('Model_Fits/Habs/den_po_alp.Rdata')
load('Model_Fits/Habs/den_wetland.Rdata')


s1b_i <- pp_check(mod_tund_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= "i) Tundra"  ) + xlim(-2000,40000)+ ylim(0,0.00030)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_i

s1b_ii <-pp_check(mod_forest_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title=  "ii) Forest" ) + xlim(-2000,40000)+ ylim(0,0.00030)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_ii


s1b_iii <-pp_check(mod_grass_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= "iii) Grassland"  ) + xlim(-2000,40000)+ ylim(0,0.00015)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_iii

s1b_iv <-pp_check(mod_med_de_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= "iv) Mediterranean and Desert"  ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_iv

s1b_v <-pp_check(mod_ar_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title=  "v) Arable" ) + xlim(-2000,40000)+ ylim(0,0.00010)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_v

s1b_vi <-pp_check(mod_wetland_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= "vi) Wetland"  ) + xlim(-2000,40000)+ ylim(0,0.00010)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_vi

s1b_vii <-pp_check(mod_aq_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= "vii) Aquatic"  ) + xlim(-2000,40000)+ ylim(0,0.00012)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values

s1b_vii


fig_s1b <- (s1b_i + s1b_ii + s1b_iii) /
  ( s1b_iv + s1b_v) /
  ( s1b_vi + s1b_vii ) / (s1_legend) + plot_layout(heights = c(10, 10,  10, 1))

fig_s1b

#ggsave("Figures/Fig_S1b.png", plot = fig_s1b, width = 14, height = 16, units = "in", dpi = 300)

ggsave("Figures/Fig_s1b.pdf", plot = fig_s1b,  width = 355.6, height = 406.4, units = "mm",   device = cairo_pdf)

