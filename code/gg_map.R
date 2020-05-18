

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
library(maps)
library(mapproj)

# Read our google sheet!
sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0", col_types = "ccccccccnnncnnncccccccccccccccccnnnnnncc")


nrow(sb[!is.na(sb$Total_Species),]) # count rows with species data, just for info

# Ali's Lat Long wrangling code
sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] # remove rows that don't have both lat and long at degree resolution

# First have to split the dataset into those with decimals and those without
sb.dec<-sb[grepl("\\.", sb$Lat_Deg) | grepl("\\.", sb$Lon_Deg),]
sb<-sb[!sb$URL %in% sb.dec$URL,]

# Then change sign for those dec degrees with compass directions where needed
# doesnt work for me, but does for ali
# sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1] <-sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1]*-1
# sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1] <-sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1]*-1


# Then account for mistakes in coordinates -
# run if google sheet has not been cleaned
nrow(sb[sb$Lat_Deg>90,]) # check for impossibles
# sb<-sb[!sb$Lat_Deg>90,] # remove rows with impossible latitudes
# sb$Lon_Deg[sb$Lon_Deg>180]<-180 # remove rows with impossible longitudes
# sb$Lat_Min[sb$Lat_Min>59]<-59; sb$Lon_Min[sb$Lon_Min>59]<-59 # Some places have minutes (and some seconds) above 60, which I think is impossible. Need to sort these out better eventually but the conversion seems to work anyway.
# sb$Lat_Sec[sb$Lat_Sec>59]<-59; sb$Lon_Sec[sb$Lon_Sec>59]<-59

# add zeroes for minutes/seconds where they are blank
sb$Lon_Min[is.na(sb$Lon_Min)]<-0; sb$Lat_Min[is.na(sb$Lat_Min)]<-0
sb$Lat_Sec[is.na(sb$Lat_Sec)]<-0; sb$Lon_Sec[is.na(sb$Lon_Sec)]<-0

# for conversion - first paste together the coordinates with d, m, s as separators (needed later)
sb$Lat<-paste0(sb$Lat_Deg,"d",sb$Lat_Min,"m",round(as.numeric(sb$Lat_Sec)),"s",sb$Lat_NS)
sb$Lon<-paste0(sb$Lon_Deg,"d",sb$Lon_Min,"m",round(as.numeric(sb$Lon_Sec)),"s",sb$Lon_EW)

# check values
 summary(sb$Lon_Deg)
 summary(sb$Lon_Min)
 summary(sb$Lon_Sec)

# then char2dms converts the coordinates to decimals, using the separators we just added. Overwrite original column
sb$Lat_Deg<-as.numeric(char2dms(sb$Lat,"d","m","s"))
sb$Lon_Deg<-as.numeric(char2dms(sb$Lon,"d","m","s"))

sb<-sb[,1:(ncol(sb)-2)] # removing the new pre-conversion columns so that data frames line up again

sb<-rbind(sb,sb.dec) # bind back together

nrow(sb)
colnames(sb)


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


# coord_equal version
gsbm <- sb %>%
  #group_by(latitude, longitude, site_code, `Length of study`, continent) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                 color=`Habitat`
                 #color=`Total_Species`
                 ), alpha=0.5) +
  scale_color_viridis(discrete=TRUE,name="Habitat") +
 # scale_color_viridis(discrete=FALSE,name="Total_Species",option="A") +
  #scale_size_continuous(range=c(2,8), name="") +
  #coord_map(projection="mollweide")+
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(1,1,1,1), "cm"),
    legend.position=c(0.20,0.001),
    legend.direction="horizontal"
  ) +
  ggplot2::annotate("text", x = -190, y = -34, hjust = 0, size = 5, label = paste("Soil Seedbanks of the World"), color = "Black") +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 4, label = paste("Study Locations"), color = "black", alpha = 0.5) +
  geom_text(data= sb %>% mutate(n_study = nrow(sb)) %>%
            distinct( n_study, .keep_all = T),
            aes(x=-147, y=-44,
                label=paste('n[location] == ', n_study)),
            hjust = 0, size=4, color="black", alpha=0.5, parse=T) +
  # xlim(-190,190) +
  # ylim(-60,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) 

gsbm

# Save as PNG in plots folder
ggsave('./plots/map_update.png', width = 13, height = 8.27, units = "in", dpi = 90)



