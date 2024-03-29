

library("googlesheets4")
library(tidyverse)
library("ggplot2")
library("sf")                 
library("rnaturalearth")
library("rnaturalearthdata")
library(viridis)
library(MetBrewer)
library(hrbrthemes)
library(mapdata)
library(ggrepel)
library(sp) # For converting to decimal degrees
library(maps)
library(mapproj)
library(ggmap)
library(rworldmap)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))


nrow(sb)

head(sb)
# theme
theme_set(theme_bw())
# map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)                 
ggplot(data = world) +
  geom_sf()
# Get the world polygon
world <- map_data("world")



sb <- sb %>% mutate(Biome_Broad_Hab = fct_relevel(Biome_Broad_Hab,
                                                  "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                                  "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                                  "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                                  "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                                  "Aquatic", "Arable"
                                                                       
))

head(sb)
nrow(sb)

# coord_equal version
gsbm <- sb %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                  shape= Biome_Broad_Hab,
                 color=`Biome_Broad_Hab`
                 ), size=3, alpha=0.5
             ) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  # trop "#007e2f" tundra
  scale_shape_manual(values = c( 16, 18, 16,
                                 18, 18, 16, 
                                 18, 16,  18, 16, 
                                 17, 15) ) +
  coord_equal() +
  theme_void(base_size=18) +
  theme(
    legend.position = 'none',
  ) +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 4, label = paste("Study Locations"), color = "black", alpha = 0.5) +
  labs(color= "Biome_Broad_Hab")+
  scale_x_continuous(expand = c(0.006, 0.006)) #+ guides(col = guide_legend(ncol = 3))

gsbm



gsbm_legend <- sb %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                 shape= Biome_Broad_Hab,
                 color=`Biome_Broad_Hab`), size=3) +
  scale_color_manual( values= c( "#94b594", "#1e3d14",   "#20B2AA", #tundra, boreal fs, montane grasslands
                                 "#788f33", "#3b7c70",  "#d8b847", #temp broad, temp con, temp grass
                                 "#da7901", "#fab255", "#228B22","#b38711", # med forests, deserts, trop forests, trop grass
                                 "#447fdd","#99610a" # aquatic, arable
  ))+
  scale_shape_manual(values = c(  16, 18, 16,
                                18, 18, 16, 
                                18, 16,  18, 16, 
                                17, 15) ) +
  coord_equal() +
  theme_void(base_size=18) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(color= "Biome_Broad_Hab")+
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_legend

# extract legend
#Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(gsbm_legend)

figure_2 <- (gsbm)/(legend) +  plot_layout(ncol=1, nrow=2, heights = c(12,2))
# LANDSCAPE 8.50 X 16




