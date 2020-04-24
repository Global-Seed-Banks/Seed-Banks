

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
library(sp) # For converting to decimal degrees

# Read our google sheet!
sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0", col_types = "ccccccccnnncnnncccccccccccccccccnnnnnncc")


# Ali's Lat Long wrangling code
sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] # remove rows that don't have both lat and long at degree resolution

# First have to split the dataset into those with decimals and those without
sb.dec<-sb[grep("\\.", sb$Lat_Deg),]
sb<-sb[!grepl("\\.", sb$Lat_Deg),]


summary(sb$Lon_Deg)
summary(sb$Lon_Min)
summary(sb$Lon_Sec)


# Then account for mistakes in coordinates - 
# run if google sheet has not been cleaned
#sb<-sb[!sb$Lat_Deg>90,] # remove rows with impossible latitudes
#sb$Lon_Deg[sb$Lon_Deg>180]<-180 # remove rows with impossible longitudes
#sb$Lat_Min[sb$Lat_Min>59]<-59; sb$Lon_Min[sb$Lon_Min>179]<-179 # Some places have minutes (and some seconds) above 60, which I think is impossible. Need to sort these out better eventually but the conversion seems to work anyway.

# add zeroes for minutes/seconds where they are blank
sb$Lon_Min[is.na(sb$Lon_Min)]<-0; sb$Lat_Min[is.na(sb$Lat_Min)]<-0  
sb$Lat_Sec[is.na(sb$Lat_Sec)]<-0; sb$Lon_Sec[is.na(sb$Lon_Sec)]<-0 

# for conversion - first paste together the coordinates with d, m, s as separators (needed later)
sb$Lat<-paste0(sb$Lat_Deg,"d",sb$Lat_Min,"m",round(as.numeric(sb$Lat_Sec)),"s",sb$Lat_NS) 
sb$Lon<-paste0(sb$Lon_Deg,"d",sb$Lon_Min,"m",round(as.numeric(sb$Lon_Sec)),"s",sb$Lon_EW)

# check values
# summary(sb$Lon_Deg)
# summary(sb$Lon_Min)
# summary(sb$Lon_Sec)

# then char2dms converts the coordinates to decimals, using the separators we just added. Overwrite original column
sb$Lat_Deg<-as.numeric(char2dms(sb$Lat,"d","m","s"))
sb$Lon_Deg<-as.numeric(char2dms(sb$Lon,"d","m","s"))

sb<-sb[,1:(ncol(sb)-2)] # removing the new pre-conversion columns so that data frames line up again 

sb<-rbind(sb,sb.dec) # bind back together

nrow(sb)


# This code copied from 'Data to Vis'  here:
# https://www.data-to-viz.com/graph/bubblemap.html

# theme
theme_set(theme_bw())
# map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)                 
ggplot(data = world) +
  geom_sf()
# Get the world polygon
world <- map_data("world")


gsbm <- sb %>%
  #group_by(latitude, longitude, site_code, `Length of study`, continent) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                 color=`Habitat`
                 #color=`Total_Species`
                 ), alpha=0.5) +
  # labels from a former map
  # geom_label_repel(
  #   aes(x=Lon_Deg, y=Lat_Deg, label = Lon_Deg),family = 'Times',
  #   segment.size = 0.5, segment.alpha = 0.5,
  #   size = 3,
  #   box.padding = 0.1, point.padding = 0.3, fill = NA,
  #   segment.color = 'grey50') +
  scale_color_viridis(discrete=TRUE,name="Habitat") +
 # scale_color_viridis(discrete=FALSE,name="Total_Species",option="A") +
  #scale_size_continuous(range=c(2,8), name="Length of Study") +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(1,1,1,1), "cm"),
    legend.position=c(0.20,0.001),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -190, y = -34, hjust = 0, size = 7, label = paste("Soil Seedbanks of the World"), color = "Black") +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 4, label = paste("Study Locations n=891"), color = "black", alpha = 0.5) +
  xlim(-180,180) +
  ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

gsbm

# Save as PNG
# Emma's path
ggsave('~/Dropbox/Projects/SeedbankMap/Plots/gsbm_map.png', width = 36, height = 15.22, units = "in", dpi = 90)
# Ali's path




