

# Note to self: Download World Clim data, and join with google sheet lat longs
# can extract whittaker biomes and make a whittaker plot

library(plotbiomes)
library(sp)

clim_dat <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat.csv", stringsAsFactors = FALSE)

# In order to intersect the study points with the Whittaker biomes polygons, we
# need to transform the climate data to spatial point object, forcing
# temperature and precipitation (cm) data as coordinates without a CRS.
points_sp <- sp::SpatialPoints(coords = clim_dat[, c("MAT_v2", "MAP")])


# Extract biomes for each study location. # Whittaker biomes as polygons (comes
# with the plotbiomes package)
Whittaker_biomes_df <- sp::over(x = points_sp,
                                y = plotbiomes::Whittaker_biomes_poly)

clim_dat <- cbind(clim_dat, Whittaker_biomes_df)

write.csv(clim_dat, file = "~/Dropbox/Projects/NutNet/Data/clim_dat_with_Whittaker_biomes.csv", row.names = FALSE)


# make a whittaker biom plot
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


meta$MAP_mm<-meta$MAP_v2 / 10


whittaker_base_plot() +
  geom_point(data = meta, 
             aes(x = MAT_v2, 
                 y = MAP_mm), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()




# also label biogeographic Realms by latitude



meta3$Realm <- ifelse(meta3$latitude.p > 23.5 & meta3$latitude.p < 60, 'Temperate',
                      ifelse(meta3$latitude.p >23.5 , 'Tropical',
                             ifelse(meta3$latitude.p  < 60, 'Polar', 'other')))



