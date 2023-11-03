### Make map figure with alpha, gamma and beta diversity
rm(list = ls())
# Libraries
library(rgdal)
library(rgeos)
library(viridis)
library(tidyverse)
library(ggplot2)
library(sp)
library(sf)
library(patchwork)

# Add biomes, rename biomes according to broad cats used.
biomes<-readOGR("./GIS","tnc_terr_ecoregions", stringsAsFactors = FALSE)

biomes$broad<-biomes$WWF_MHTNAM
biomes$broad[biomes$broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
biomes$broad[biomes$broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"


# Emma
div.vals<-read.csv("~/Dropbox/GSB/Data/sb_av_div_estimates.csv")
div.vals<-div.vals[div.vals$Number_Sites==1,]
head(div.vals)

biomes$alpha<-div.vals$a_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$gamma<-div.vals$g_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]


# Emma
sb<-read.csv("~/Dropbox/GSB/Data/gsb_slim.csv")

sb<-sb[!sb$Habitat_Current %in% c("Arable", "Aquatic"),]
sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS(proj4string(biomes)))
biomes.sb<-biomes[sb.shp,]


head(biomes.sb)
head(world)

tidy_world <- map_data("world")

# convert biomes.sb to sf object
tidy_biomes.sb <- st_as_sf(biomes.sb) 
class(tidy_biomes.sb)
head(tidy_biomes.sb)
colnames(tidy_biomes.sb)


tidy_sb_biomes<- tidy_biomes.sb %>% gather( key= "scale", value="richness", alpha, gamma) %>%
  mutate( scale = factor(scale,
                         levels= c("alpha", "gamma"),
                         labels = c( (expression(paste('c)'))), 
                                     (expression(paste('d)'))) 
                         )) )

map_breaks <- c(5, 10, 15, 20, 30, 40)


#other versions
figure_4_c <- ggplot() +
  geom_polygon(data = tidy_world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_sf(data = tidy_biomes.sb, aes(fill = alpha) , colour=NA, lwd=0) +
  scale_fill_viridis(discrete = F, option="D",  limits = c(0, 20) 
                     )  +
  theme_void(base_size=18) + scale_x_continuous(expand = c(0.006, 0.006)) +
  theme(legend.position = 'bottom',
    legend.direction="horizontal",
    legend.key.width = unit(1.5,"cm") ,
    plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 1, unit = "cm")
  ) + labs(fill = (expression(paste('Average ', italic(alpha), '-species richness ',sep = ''))) ,
           subtitle = "c)")

figure_4_c

figure_4_d <- ggplot() +
  geom_polygon(data = tidy_world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_sf(data = tidy_biomes.sb, aes(fill = gamma), colour=NA,  lwd=0) +
  scale_fill_viridis(discrete = F, option="plasma", limits = c(10, 40)) +
  theme_void(base_size=18) + scale_x_continuous(expand = c(0.006, 0.006)) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.key.width = unit(1.5,"cm") ,
        plot.margin= margin(t = 0.2, r = 1, b = -0.2, l = 0.2, unit = "cm")
  )  + labs(fill= (expression(paste('Average ', italic(gamma), '-species richness ',sep = ''))) ,
              #'Soil seedbank average \n species richness' ,
            subtitle = "d)")
figure_4_d



# After plotting look at how biomes swap places depending on alpha or gamma

head(div.vals)
div.grid<-expand.grid(div.vals$Biome_Broad_Hab,div.vals$Biome_Broad_Hab)
div.grid<-div.grid[!div.grid$Var1==div.grid$Var2,]
div.grid$a1<-div.vals$a_Estimate[match(div.grid$Var1,div.vals$Biome_Broad_Hab)]
div.grid$a2<-div.vals$a_Estimate[match(div.grid$Var2,div.vals$Biome_Broad_Hab)]
div.grid$g1<-div.vals$g_Estimate[match(div.grid$Var1,div.vals$Biome_Broad_Hab)]
div.grid$g2<-div.vals$g_Estimate[match(div.grid$Var2,div.vals$Biome_Broad_Hab)]
div.grid$adiff<-div.grid$a1-div.grid$a2
div.grid$gdiff<-div.grid$g1-div.grid$g2
sum(!sign(div.grid$adiff) == sign(div.grid$gdiff)) / nrow(div.grid) # 30.3% combinations swapped places

div.grid.naterra<-div.grid[!div.grid$Var1 %in% c("Arable", "Aquatic") & !div.grid$Var2 %in% c("Arable", "Aquatic"),]
sum(!sign(div.grid.naterra$adiff) == sign(div.grid.naterra$gdiff)) / nrow(div.grid.naterra) # 24.4% combinations swapped places




