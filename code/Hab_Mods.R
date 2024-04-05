



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'rich_aq.Rdata')
load( 'rich_ar.Rdata')
load( 'rich_forest.Rdata')
load( 'rich_grass.Rdata')
load( 'rich_med_de.Rdata')
load( 'rich_tund.Rdata')
load( 'rich_wetland.Rdata')

summary(mod_aq_r)
conditional_effects(mod_aq_r)

summary(mod_ar_r)
conditional_effects(mod_ar_r)

summary(mod_forest_r)
pp_check(mod_forest_r)
conditional_effects(mod_forest_r)

summary(mod_grass_r)
pp_check(mod_grass_r)
conditional_effects(mod_grass_r)


summary(mod_med_de_r)
conditional_effects(mod_med_de_r)

summary(mod_tund_r)
conditional_effects(mod_tund_r)

summary(mod_wetland_r)
conditional_effects(mod_wetland_r)

setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'den_aq.Rdata')
load( 'den_ar.Rdata')
load( 'den_forest.Rdata')
load( 'den_grass.Rdata')
load( 'den_med_de.Rdata')
load( 'den_po_alp.Rdata')
load( 'den_wetland.Rdata')



summary(mod_aq_d)
pp_check(mod_aq_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_aq_d)

summary(mod_ar_d)
pp_check(mod_ar_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_ar_d)

summary(mod_forest_d)
pp_check(mod_forest_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_forest_d)

summary(mod_grass_d)
pp_check(mod_forest_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_grass_d)

summary(mod_med_de_d)
pp_check(mod_grass_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_med_de_d)

summary(mod_tund_d)
pp_check(mod_tund_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_tund_d)

summary(mod_wetland_d)
pp_check(mod_wetland_d)+ xlab( expression(paste('Seed density (',m^2,')')) ) + ylab("Density") +
  labs(title= expression(paste('b) Density (',m^2,')')) ) + xlim(-2000,40000)+ ylim(0,0.00020)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_wetland_d)



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# models run on cluster, load in model objects here
load( 'ratio_aq.Rdata')
load( 'ratio_ar.Rdata')
load( 'ratio_forest.Rdata')
load( 'ratio_grass.Rdata')
load( 'ratio_med_de.Rdata')
load( 'ratio_po_alp.Rdata')
load( 'ratio_wetland.Rdata')

summary(mod_aq_ra)
pp_check(mod_aq_ra)+ xlab( "Ratio (seeds/species)") + ylab("Density") +
  labs(title= "c) Ratio") + xlim(0,400)+ #ylim(0,0.025)+
  theme_classic()+  theme(legend.position= "none") # predicted vs. observed values
conditional_effects(mod_aq_ra)

summary(mod_ar_ra)
conditional_effects(mod_ar_ra)

summary(mod_forest_ra)
pp_check(mod_forest_ra)
conditional_effects(mod_forest_ra)

summary(mod_grass_ra)
pp_check(mod_grass_ra)
conditional_effects(mod_grass_ra)


summary(mod_med_de_ra)
conditional_effects(mod_med_de_ra)

summary(mod_tund_ra)
conditional_effects(mod_tund_ra)

summary(mod_wetland_ra)
conditional_effects(mod_wetland_ra)




