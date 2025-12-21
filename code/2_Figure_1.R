

library("googlesheets4")
library(tidyverse)
library("ggplot2")
library(patchwork)
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
                  "emmaladouceur" = "~/Dropbox/GSB/",
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

sb %>% select(Realm, Biome) %>% distinct() %>% arrange(Realm, Biome)
sb %>% select(Realm) %>% distinct()
sb %>% select(Biome) %>% distinct()

sb <- sb %>%   mutate(Realm_Biome = case_when(
  Realm == "Aquatic" ~ Realm,
  Realm == "Arable" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Arable", 
  Realm == "Arable" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Arable", 
  Realm == "Arable" & Biome ==  "Tropical" ~  "Tropical Arable", 
  Realm == "Forest" & Biome ==  "Temperate" ~  "Temperate Forests",
  Realm == "Forest" & Biome ==  "Tropical" ~  "Tropical & Subtropical Forests",
  Realm == "Forest" & Biome ==  "Boreal" ~  "Boreal Forests/Taiga",
  Realm == "Grassland" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Grasslands, Savannas & Shrublands",
  Realm == "Grassland" & Biome ==  "Tropical" ~  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  Realm == "Mediterranean and Desert" & Biome ==  "Deserts and Xeric Shrublands" ~  "Deserts & Xeric Shrublands",
  Realm == "Mediterranean and Desert" & Biome ==  "Mediterranean Forests, Woodlands and Scrub" ~  "Mediterranean Forests, Woodlands & Scrub",
  Realm ==  "Tundra" ~  Realm,
  Realm ==  "Wetland" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Wetlands",
  Realm ==  "Wetland" & Biome ==  "Temperate and Boreal"~  "Temperate & Boreal Wetlands",
  Realm ==  "Wetland"  & Biome ==  "Tropical" ~  "Tropical Wetlands",
  )) %>% 
  mutate(Realm_Biome = fct_relevel(Realm_Biome, "Tundra", "Boreal Forests/Taiga","Temperate Forests","Tropical & Subtropical Forests", 
                                   "Temperate & Boreal Grasslands, Savannas & Shrublands",    "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                             "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands", 
                             "Temperate & Boreal Arable", "Mediterranean & Desert Arable", "Tropical Arable",
                             "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
                             "Aquatic", 
                             
                             )) %>% mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  

head(sb)
nrow(sb)
sb %>% select(Realm, Biome, Realm_Biome) %>% distinct()
sb %>% select(Realm_Biome) %>% distinct()
# old colors
# "#3b7c70", "#fab255", "#b38711", "#d8b847", "#228B22","#20B2AA", "#94b594", "#1e3d14",   #tundra, "#788f33", "#da7901", 
# "#d8b847", "#228B22",  "#da7901",

# coord_equal version
# gsbm <- sb %>% #filter( Realm %in% c("Tundra","Forest", "Grassland")) %>%
#   ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
#   geom_point(aes(x=Lon_deg, y=Lat_deg, 
#                   shape= Realm_Biome,
#                  color=`Realm_Biome`
#                  ), size=3, #alpha=0.5
#              position = position_jitter(width = 1.5, height=1.5)
#              ) +
#   scale_color_manual( values= c( "#94b594", "#1e3d14",   "#788f33", "#228B22", 
#                                  "#d8b847", "#b38711", #temp broad, temp con, temp grass
#                                  "#da7901",  "#fab255",  # med forests, deserts, trop forests, trop grass
#                                  "#99610a" , "#E2C59F", "#AA3929" ,#aquatic, arable
#                                  "#20B2AA", "#4E84C4", "#293352", "#447fdd"
#   ))+
#   scale_shape_manual(values = c(  16, 18, 18, 18,
#                                   16, 16, 16, 16,
#                                   15, 15, 15,
#                                   1,1,1,
#                                   17
#                                   ) ) +
#   coord_equal() +
#   theme_void(base_size=18) +
#   theme(
#     legend.position = 'none',
#   ) +
# #  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 4, label = paste("Study Locations"), color = "black", alpha = 0.5) +
#   labs(color= "Realm_Biome")+
#   scale_x_continuous(expand = c(0.006, 0.006)) #+ guides(col = guide_legend(ncol = 3))
# 
# gsbm

head(sb)
summary(sb)

gsbm_tundra <- sb %>% filter( Realm == "Tundra") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 color=`Realm_Biome`, shape= Habitat_degraded), size=3, alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5)
             ) +
  scale_color_manual( values= c( "#94b594")
                      
  )+
  scale_shape_manual( values= c( 16,17),
                      labels = c("Undisturbed", "Degraded"), guide = "none")+
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(subtitle = "a)",color= "", shape= "")+
  ggplot2::annotate("text", x = -150, y = -44, hjust = 0, size = 6, label = paste("Tundra"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_tundra


gsbm_forest <- sb %>% filter( Realm == "Forest") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3, alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5) 
             ) +
  scale_color_manual( values= c(  "#1e3d14",   "#788f33", "#228B22" ), 
                      labels = c("Boreal", "Temperate", "Tropical & Subtropical"),
  
  )+
  scale_shape_manual(values = c(  16,17) ,
                     labels = c("Boreal", "Temperate", "Tropical & Subtropical"),
                      guide = "none"
                     ) +
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(subtitle = "b)",color= "Forests", shape= "Forests")+
  ggplot2::annotate("text", x = -150, y = -44, hjust = 0, size = 6, label = paste("Forests"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_forest

gsbm_grass <- sb %>% filter( Realm == "Grassland") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3, alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5)
             ) +
  scale_color_manual( values= c( "#d8b847", "#b38711"), 
                      labels = c("Temperate & Boreal", "Tropical & Subtropical")
                      
  )+
  scale_shape_manual(values = c( 16, 17) ,
                     labels = c("Temperate & Boreal", "Tropical & Subtropical"), guide = "none"
  ) +
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +  labs(subtitle = "c)", color= "Grasslands & \nSavannas", shape= "Grasslands & \nSavannas")+
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = paste("Grasslands & \nSavannas"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_grass

gsbm_med <- sb %>% filter( Realm == "Mediterranean and Desert") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3, alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5)
             ) +
  scale_color_manual( values= c( "#da7901",  "#fab255" ), 
                      labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"),
    
    
  )+
  scale_shape_manual(values = c( 16, 17) ,
                     labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"), guide = "none"
  ) +
  coord_equal() +
  theme_void(base_size=18) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 6, label = paste("Mediterranean & \nDesert"), color = "black", alpha = 0.7) +
  labs(subtitle = "d)", color= "Mediterranean \nand Desert", shape= "Mediterranean \nand Desert")+
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_med

gsbm_ar <- sb %>% filter( Realm == "Arable") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 #shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3, alpha=0.9, shape=15,
             position = position_jitter(width = 2.5, height=2.5) 
             ) +
  scale_color_manual( values= c(   "#99610a" , "#E2C59F", "#AA3929"  ), 
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"),
                     
                      
  )+
  # scale_shape_manual(values = c(  15, 15, 15) ,
  #                    labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"),
  #                     
  # ) +
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(subtitle ="e)",color= "Arable", shape= "Arable")+
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = paste("Arable"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_ar

gsbm_wet <- sb %>% filter( Realm == "Wetland") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3,alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5)
             ) +
  scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352" ), 
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"),
                      
                      
  )+
  scale_shape_manual(values = c( 16, 17) ,
                     labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"),
                      guide = "none"
  ) +
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(subtitle = "f)",color= "Wetlands", shape= "Wetlands")+
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 6, label = paste("Wetlands & \nFlooded Grasslands"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_wet


gsbm_aq <- sb %>% filter( Realm == "Aquatic") %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Habitat_degraded,
                 color=`Realm_Biome`), size=3, alpha=0.9, 
             position = position_jitter(width = 2.5, height=2.5) 
             ) +
  scale_color_manual( values= c( "#447fdd" )
                      
                      
  )+
  scale_shape_manual(values = c( 16, 17) ,  guide = "none"
  ) +
  coord_equal() +
  theme_void(base_size=20) +
  theme(legend.position = 'bottom',
        legend.direction="horizontal",
        legend.title = element_blank()
  ) +
  labs(subtitle = "g)",color= "Aquatic", shape= "Aquatic")+
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = paste("Aquatic"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_aq
# 
# # extract legend
# #Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend <- function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# legend_t <- g_legend(gsbm_tundra)
# legend_f <- g_legend(gsbm_forest)
# legend_g <- g_legend(gsbm_grass)
# legend_m <- g_legend(gsbm_med)
# legend_ar <- g_legend(gsbm_ar)
# legend_wet <- g_legend(gsbm_wet)
# legend_aq <- g_legend(gsbm_aq)
# 
# 
# 
# 
# legend_top <- gridExtra::grid.arrange( grobs = list( legend_t, legend_f , legend_g ),
#                                      layout_matrix = rbind( c( 1,2,3) ) )
# 
# legend_mid <- gridExtra::grid.arrange( grobs = list( legend_m, legend_ar),
#                                    layout_matrix = rbind( c( 1,2) )
# )
# 
# legend_bot <- gridExtra::grid.arrange( grobs = list( legend_wet, legend_aq),
#                                      layout_matrix = rbind( c( 1,2) )
# )
# 
# legend <- gridExtra::grid.arrange(grobs = list( legend_top, legend_mid , legend_bot ))
# 
# gsbm
# 
# figure_2 <- (gsbm)/(legend) +  plot_layout(ncol=1, nrow=2, heights = c(12,3), widths=c(10,10))
# # LANDSCAPE 8.50 X 16
# 
# figure_2
 sb %>% distinct(Realm)
gsbm_legend_fig <- sb %>% filter( Realm %in% c( "Aquatic" , "Wetland", "Tundra")) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_deg, y=Lat_deg, 
                 shape= Realm,
                 color=Realm), size=5, color="grey"#, alpha=0.9, 
             #position = position_jitter(width = 2.5, height=2.5) 
  ) +
  scale_color_manual( values= c( "#447fdd" , guide = "none")


  )+
  scale_shape_manual( name= "State", values= c( 16,17,15),
                      labels = c("Undisturbed", "Degraded", "Arable"))+
  coord_equal() +
  theme_void(base_size=18) +
  theme(#legend.position = 'bottom',
       # legend.direction="horizontal",
        #legend.title = element_blank()
  ) +
  labs(subtitle = "g)",color= "Aquatic", shape= "State")+
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 5, label = paste("Aquatic"), color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) + guides(col = guide_legend(ncol = 3))

gsbm_legend_fig



# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

gsbm_legend <- g_legend(gsbm_legend_fig)


top_row <- (gsbm_tundra) + (gsbm_forest) + (gsbm_grass) 
middle_row <- (gsbm_med) + (gsbm_ar) 
bottom_row <-  (gsbm_wet) + (gsbm_aq) 

#(top_row)/(middle_row)/(bottom_row)/(gsbm_legend) +  plot_layout(heights = c(10,10,10,1))
# LANDSCAPE 12 X 21


p_maps <- (top_row)/(middle_row)/(bottom_row)#+ plot_layout(heights = c(10,10,10))
p_maps 

library(cowplot)

p_final <- ggdraw(p_maps) +
  draw_grob(
    gsbm_legend,
    x = 0.82,   # push right of arable
    y = 0.40,  # center of middle row
    width  = 0.16,
    height = 0.20
  )

p_final



