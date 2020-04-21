
rm(list=ls())
# Note to self: Download World Clim data, and join with google sheet lat longs
# can extract whittaker biomes and make a whittaker plot

library(tidyverse)
library(plotbiomes)
library(sp)
library(raster)

# get worldclim data
r <- getData("worldclim",var="bio",res=10)

r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")


# seedbank data
sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0", col_types = "ccccccccnnncnnncccccccccccccccccnnnnnncc")

# Ali's Lat Long wrangling code
sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] # remove rows that don't have both lat and long at degree resolution

# First have to split the dataset into those with decimals and those without
sb.dec<-sb[grep("\\.", sb$Lat_Deg),]
sb<-sb[!grepl("\\.", sb$Lat_Deg),]

# add zeroes for minutes/seconds where they are blank
sb$Lon_Min[is.na(sb$Lon_Min)]<-0; sb$Lat_Min[is.na(sb$Lat_Min)]<-0  
sb$Lat_Sec[is.na(sb$Lat_Sec)]<-0; sb$Lon_Sec[is.na(sb$Lon_Sec)]<-0 

# for conversion - first paste together the coordinates with d, m, s as separators (needed later)
sb$Lat<-paste0(sb$Lat_Deg,"d",sb$Lat_Min,"m",round(as.numeric(sb$Lat_Sec)),"s",sb$Lat_NS) 
sb$Lon<-paste0(sb$Lon_Deg,"d",sb$Lon_Min,"m",round(as.numeric(sb$Lon_Sec)),"s",sb$Lon_EW)

# then char2dms converts the coordinates to decimals, using the separators we just added. Overwrite original column
sb$Lat_Deg<-as.numeric(char2dms(sb$Lat,"d","m","s"))
sb$Lon_Deg<-as.numeric(char2dms(sb$Lon,"d","m","s"))

sb<-sb[,1:(ncol(sb)-2)] # removing the new pre-conversion columns so that data frames line up again 

sb<-rbind(sb,sb.dec) # bind back together


coords <- sb %>% dplyr::select(Lat_Deg,Lon_Deg) %>%
  mutate(x=Lon_Deg) %>%
  mutate(y=Lat_Deg) 

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

sb_clim_dat <- cbind.data.frame(coordinates(points),values)
#lots of NA's and doesnt match up well, need to get higher resolution/work on matching nearest neighbour

# but for now- remove NA's
sb_clim_dat<-sb_clim_dat %>% drop_na()

# adjust the world clim data to cm and celcius 
sb_clim_dat$Prec_cm<-sb_clim_dat$Prec / 10
sb_clim_dat$Temp_a<-sb_clim_dat$Temp / 10

# In order to intersect the study points with the Whittaker biomes polygons, we
# need to transform the climate data to spatial point object, forcing
# temperature and precipitation (cm) data as coordinates without a CRS.
points_sp <- sp::SpatialPoints(coords = sb_clim_dat[, c("Temp_a", "Prec_cm")])

# Extract biomes for each study location. # Whittaker biomes as polygons (comes
# with the plotbiomes package)
Whittaker_biomes_df <- sp::over(x = points_sp,
                                y = plotbiomes::Whittaker_biomes_poly)

clim_dat <- cbind(sb_clim_dat, Whittaker_biomes_df)

head(clim_dat)

# also label biogeographic Realms by latitude
sb_clim_dat$Realm <- ifelse(sb_clim_dat$Lat_Deg > 23.5 & sb_clim_dat$y < 60, 'Temperate',
                      ifelse(sb_clim_dat$Lat_Deg >23.5 , 'Tropical',
                             ifelse(sb_clim_dat$Lat_Deg  < 60, 'Polar', 'other')))


#write.csv(clim_dat, file = "./data/try/Whittaker_biomes.csv", row.names = FALSE)



whittaker_base_plot() +
  geom_point(data = sb_clim_dat, 
             aes(x = Temp_a, 
                 y = Prec_cm), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()





