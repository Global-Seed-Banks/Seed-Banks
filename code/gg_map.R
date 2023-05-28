

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

# Read our google sheet!
#sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0", col_types = "ccccccccnnncnnncccccccccccccccccnnnnnncc")

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Ali puts his computer username and file path here
)


setwd(path2wd)

sb <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))



colnames(sb)
nrow(sb[!is.na(sb$Total_Species),]) # count rows with species data, just for info
# how many individual studies?
sb %>% distinct(Title)
# 1,434 papers 

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

# sbd <- sb %>% filter(is.na(Habitat))
# View(sbd)


# This code copied from 'Data to Vis'  here:
# https://www.data-to-viz.com/graph/bubblemap.html
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

sb <- sb %>%   mutate(Biome_Cats = case_when(Biome_Broad_Hab %in% c("Arable", "Aquatic") ~ Biome_Broad_Hab ,
                                                  TRUE ~ "Natural")) 
sb %>% select(Biome_Broad_Hab , Biome_Cats) %>% distinct()


# coord_equal version
gsbm <- sb %>%
  #group_by(latitude, longitude, site_code, `Length of study`, continent) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                  shape= Biome_Broad_Hab,
                 color=`Biome_Broad_Hab`
                 ), size=3, alpha=0.5
             ) +
  scale_color_manual( values= c( "#447fdd","#99610a", "#1e3d14", "#fab255", # aquatic, arable, boreal, deserts
                                 "#da7901",  "#20B2AA" , "#788f33","#3b7c70",#"#165d43", # med forests, montane grasslands, temp forests, temp confier forests
                                 "#d8b847", "#228B22","#b38711", "#94b594" # temp grasslands, trop forests, trop grasslands, tundra
  ))+
  # trop "#007e2f" tundra
  scale_shape_manual(values = c(17, 15, 18, 16, 
                                18, 16, 18, 18,
                                16, 18, 16, 16) ) +
 # scale_shape_manual(values=c(15, 17, 16))+
 # scale_color_viridis(discrete = T, option="D")  +
  #scale_color_manual(values=met.brewer("Signac", 12))+
  #scale_size_continuous(range=c(2,8), name="") +
  #coord_map(projection="mollweide")+
  coord_equal() +
  theme_void(base_size=18) +
  theme(
    # panel.spacing=unit(c(0,0,0,0), "null"),
    # plot.margin=grid::unit(c(1,1,1,1), "cm"),
    # legend.position=c(0.20,0.001),
    legend.position = 'none',
 #   legend.direction="horizontal",
  #  legend.title = element_blank()
  ) +
 # ggplot2::annotate("text", x = -190, y = -34, hjust = 0, size = 5, label = paste("The Global Soil Seed Bank"), color = "Black") +
  #ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 4, label = paste("Study Locations"), color = "black", alpha = 0.5) +
  # geom_text(data= sb %>% mutate(n_study = nrow(sb)) %>%
  #           distinct( n_study, .keep_all = T),
  #           aes(x=-147, y=-44,
  #               label=paste('n[location] == ', n_study)),
  #           hjust = 0, size=4, color="black", alpha=0.5, parse=T) +
  # xlim(-190,190) +
  # ylim(-60,80) +
  labs(color= "Biome_Broad_Hab")+
  scale_x_continuous(expand = c(0.006, 0.006)) #+ guides(col = guide_legend(ncol = 3))

gsbm



gsbm_legend <- sb %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                 shape= Biome_Broad_Hab,
                 color=`Biome_Broad_Hab`), size=3) +
  scale_color_manual( values= c( "#447fdd","#99610a", "#1e3d14", "#fab255", # aquatic, arable, boreal, deserts
                                 "#da7901",   "#20B2AA" , "#788f33", "#3b7c70", #"#165d43", # med forests, montane grasslands, temp forests, temp confier forests
                                 "#d8b847","#228B22","#b38711", "#94b594" # temp grasslands, trop forests, trop grasslands, tundra
  ))+
  scale_shape_manual(values = c(17, 15, 18, 16, 
                                18, 16, 18, 18,
                                16, 18, 16, 16) ) +
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

# overall legend
legend <- g_legend(gsbm_legend)

# with non-alpha legend to see colors better
(gsbm)/(legend) +  plot_layout(ncol=1, nrow=2, heights = c(12,2))



# Save as PNG in plots folder
ggsave('./plots/map_update.png', width = 13, height = 8.27, units = "in", dpi = 90)


#------------------------------------------------------------------------
 
colnames(sb)

gsbm.S <- sb %>%
  filter(Total_Species < 22602.00) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point(aes(x=Lon_Deg, y=Lat_Deg, 
                 #color=`Habitat`
                 color=`Total_Species`
  ), size=0.75,alpha=0.5,shape=1) +
  #scale_color_viridis(discrete=TRUE,name="Habitat") +
  scale_color_viridis(discrete=FALSE,name="Total_Seeds",option="A",trans="log10"
                      ) +
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

gsbm.S



#------------------------------------------------------------------------




