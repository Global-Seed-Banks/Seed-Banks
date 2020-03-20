
install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library("ggplot2")
library("sf")                 
library("rnaturalearth")
library("rnaturalearthdata")
library(viridis)
library(hrbrthemes)
library(mapdata)
library(ggrepel)


# Read oue google sheet!
sbm<-read_sheet("https://https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0")
colnames(sbm)


theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)                 


ggplot(data = world) +
  geom_sf()


# Get the world polygon
world <- map_data("world")

colnames(sbm)
# Reformat data: I count the occurence of each unique position
gsbm <- sbm %>%
  #group_by(latitude, longitude, site_code, `Length of study`, continent) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, color=`Habitat`), alpha=0.5) +
  # geom_label_repel(
  #   aes(x=longitude, y=latitude, label = site_code),family = 'Times',
  #   segment.size = 0.5, segment.alpha = 0.5,
  #   size = 3, 
  #   box.padding = 0.1, point.padding = 0.3, fill = NA,
  #   segment.color = 'grey50') +
  scale_color_viridis(discrete=TRUE,name="Habitat") +
  #scale_size_continuous(range=c(2,8), name="Length of Study") +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(1,1,1,1), "cm"),
    legend.position=c(0.17,0.001),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -185, y = -34, hjust = 0, size = 7, label = paste("Soil Seedbanks of the World"), color = "Black") +
  ggplot2::annotate("text", x = -181, y = -44, hjust = 0, size = 4, label = paste("Experimental Locations"), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

gsbm

# Save as PNG
ggsave('~/Dropbox/Projects/SeedbankMap/Plots/gsbm_map.png', width = 36, height = 15.22, units = "in", dpi = 90)
