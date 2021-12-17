#################################
### THE MAP 

### googlesheets package - couldn't get to work  ###
# 
# #install.packages("googlesheets4")
# library(googlesheets4)
# #sheets_auth("ali.auffret@gmail.com") # 
#sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/edit#gid=0", col_types = "ccccccccnnncnnncccccccccccccccccnnnnnncc")

### So this is just copying the spreadsheet and saving as csv ###

#library(maps) # For plotting map
library(sp) # For converting to decimal degrees
library(scales)
library(rgdal)
library(rgeos)

world<-readOGR("GIS","CNTR_RG_60M_2014", stringsAsFactors = FALSE) # import world map
#cc<-read.csv("GIS/countrycodes.csv",stringsAsFactors = FALSE)
#world$country<-cc$Name[match(world$CNTR_ID,cc$Code)]

system("curl -o tmpfiles/sbtemp.csv https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/gviz/tq?tqx=out:csv&sheet=Data")
sb<-read.csv("tmpfiles/sbtemp.csv",stringsAsFactors = FALSE)

nrow(sb[!is.na(sb$Total_Species) | !is.na(sb$Total_Seeds) | !is.na(sb$Seed_density_m2),]) # count rows with species data, just for info

# Finding errors
sb.noloc<-sb[(is.na(sb$Lat_Deg) | is.na(sb$Lon_Deg)) & nchar(sb$Habitat)>0, 1:which(names(sb)=="Target_Habitat")]
#write.csv(sb.noloc, "sb.findloc.csv", row.names=FALSE)

# remove rows that don't have both lat and long at degree resolution
sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] 

# First have to split the dataset into those with decimals and those without
sb.dec<-sb[grepl("\\.", sb$Lat_Deg) | grepl("\\.", sb$Lon_Deg),]
sb<-sb[!sb$Title %in% sb.dec$Title,]

# Then change sign for those dec degrees with compass directions where needed
sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1] <-sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1]*-1
sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1] <-sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1]*-1

# Ident
sb[sb$Lat_Deg>90 | sb$Lon_Deg>180,] 
sb<-sb[!sb$Lat_Deg>90,] # remove rows with impossible latitudes
#sb$Lat_Min[sb$Lat_Min>59]<-59; sb$Lon_Min[sb$Lon_Min>179]<-179 # Some places have minutes (and some seconds) above 60, which I think is impossible. Need to sort these out better eventually but the conversion seems to work anyway.

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


##### # Very rough summary info # #####

nrow(sb[!is.na(sb$Total_Species) | !is.na(sb$Total_Seeds) | !is.na(sb$Seed_density_m2) | !is.na(sb$Seed_density_litre),]) # 2971 data points

length(unique(paste(sb$Authors,sb$Title))) # 1458 papers

sb$Number_Sites[is.na(sb$Number_Sites)]<-1
sum(sb$Number_Sites, na.rm=TRUE) # 18439 sites!

sum(sb$Total_Number_Samples, na.rm=TRUE) + sum(sb$Number_Sites[is.na(sb$Total_Number_Samples)]*sb$Samples_Per_Site[is.na(sb$Total_Number_Samples)],na.rm=TRUE) # 1 059 579 samples

sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))
sb.world.df<-over(sb.shp,world)
sb.shp$countryID<-sb.world.df$CNTR_ID
sb$countryID<-sb.world.df$CNTR_ID
#sb$country<-sb.world.df$country

sb.sea<-sb.shp[is.na(sb.shp$countryID),]
sb.sea.dist<-gDistance(sb.sea,world, byid=TRUE)
sb$countryID[is.na(sb$countryID)]<-sapply(1:nrow(sb.sea),function(x) world$CNTR_ID[which.min(sb.sea.dist[,x])])


length(unique(sb$countryID)) # 96 countries

sb$Target_Habitat[sb$Habitat=="Arable"]<-""

hab.res.list<-list()
for(i in unique(sb$Habitat)){
  hab.res.list[[i]]<-nrow(sb[sb$Habitat==i & nchar(sb$Target_Habitat)==0,])
  hab.res.list[[paste0(i,".deg")]]<-nrow(sb[sb$Habitat==i & nchar(sb$Target_Habitat)>0,])
}
hab.tot<-do.call(c,hab.res.list)

barplot(as.matrix(hab.tot), horiz=TRUE,col=c(
  alpha("darkseagreen1",0.8),alpha("darkseagreen1",0.8),
  alpha("skyblue3",0.8),alpha("skyblue3",0.8),
  alpha("forestgreen",0.8),alpha("forestgreen",0.8),
  alpha("gold",0.8),alpha("gold",0.8),
  alpha("navyblue",0.8),alpha("navyblue",0.8)), border=NA
  , axes=FALSE)

barplot(as.matrix(hab.tot), horiz=TRUE,density= rep(c(0,5),length(unique(sb$Habitat))), angle=45, add=TRUE, border=TRUE, axes=FALSE, col="black")

sb$Habitat[nchar(sb$Target_Habitat)>0]<-sb$Target_Habitat[nchar(sb$Target_Habitat)>0]
coun.hab.list<-list()
#pc<-"FI"
for(pc in c("BE", "FI", "DE", "NL", "ZA", "SE")){
  sb.pc<-sb[sb$countryID==pc,]
  sapply(unique(sb$Habitat), function(x) sum(sb.pc$Number_Sites[sb.pc$Habitat==x],na.rm=TRUE))
  coun.hab.list[[pc]]<-sapply(unique(sb$Habitat), function(x) sum(sb.pc$Number_Sites[sb.pc$Habitat==x],na.rm=TRUE))
}
do.call(rbind,coun.hab.list)

# plot
pdf("plots/seedbankworldtour.pdf", height = 3, width = 5)
par(mar=c(1,1,1,1))
plot(world, lwd=0.5, col="lightgrey", border="grey")
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], col=alpha("gold",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], col=alpha("forestgreen",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], col=alpha("darkseagreen1",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], col=alpha("skyblue3",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],col=alpha("navyblue",0.5), pch=16, cex=0.3,lwd=0.3) 

# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], bg="gold", pch=21, cex=0.35,lwd=0.3) # add the points!
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], bg="forestgreen", pch=21, cex=0.35,lwd=0.3)
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], bg="darkseagreen1", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], bg="blue", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],bg="navyblue", pch=21, cex=0.35,lwd=0.3) 
text(-120,-15,"Seed banks of the world", cex=0.5)
legend(-150,-20,c("Arable","Forest","Grassland","Wetland", "Aquatic"),pch=16,cex=0.35,col=c("gold", "forestgreen","darkseagreen1", "skyblue3","navyblue"),bty="n", pt.lwd=0.3)
dev.off()

### Plots for succseed
#world map
pdf("plots/succseedmap.pdf", height = 3, width = 5)
par(mar=c(1,1,1,1))
plot(world, lwd=0.5, col="lightgrey", border="grey")
points(Lat_Deg~Lon_Deg, data=sb, col=alpha("black",0.3), pch=16, cex=0.3,lwd=0.3)
dev.off()

pdf("plots/succseedbar.pdf", height = 3, width = 9)
barplot(as.matrix(hab.tot), horiz=TRUE,col=c(
  alpha("darkseagreen1",0.8),alpha("darkseagreen1",0.8),
  alpha("skyblue3",0.8),alpha("skyblue3",0.8),
  alpha("forestgreen",0.8),alpha("forestgreen",0.8),
  alpha("gold",0.8),alpha("gold",0.8),
  alpha("navyblue",0.8),alpha("navyblue",0.8)), border=NA
  , axes=FALSE)
barplot(as.matrix(hab.tot), horiz=TRUE,density= rep(c(0,5),length(unique(sb$Habitat))), angle=45, add=TRUE, border=TRUE, axes=FALSE, col="black")
dev.off()


## Geographical error checking
 
# make shapefile of points
#sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS("+init=epsg:4326"))
sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS("+proj=longlat +ellps=GRS80 +no_defs"))


# subset by rows which do not intersect with world map
sb.out<-sb.shp[which(!rownames(sb.shp@data) %in% rownames(sb.shp[world,]@data)),]

# plot on world map
plot(sb.out, pch=19, col="red") # first plot points (as some may be outside the known world)
plot(world, lwd=0.5, col="lightgrey", border="grey", add=TRUE) # add world map
plot(sb.out, pch=19, col="red", add=TRUE) # add points back on top

# Show in table
sb.out<-sb.out[,which(names(sb.out)=="Human"):which(names(sb.out)=="Habitat")]
sb.out[sign(sb.out$Lon_Deg)==-1,]

# More error checking - Mixed up decimal and nondecimal degrees.
sb.dec[(sign(sb.dec$Lat_Deg)==1 & sb.dec$Lat_NS=="S") | (sign(sb.dec$Lon_Deg)==1 & sb.dec$Lon_EW=="W") ,]

# coverage info. Which countries not covered.
world.cov<-world[sb.shp,]
world$country[!world$country %in% world.cov$country]



##########
# MAP - cleaned database
#library(maps) # For plotting map
library(sp) # For converting to decimal degrees
library(scales)
library(rgdal)
library(rgeos)

world<-readOGR("GIS","CNTR_RG_20M_2020_4326", stringsAsFactors = FALSE) # import world map
#cc<-read.csv("GIS/countrycodes.csv",stringsAsFactors = FALSE)
#world$country<-cc$Name[match(world$CNTR_ID,cc$Code)]

sb<-read.csv("gsb_slim.csv",stringsAsFactors = FALSE)
pdf("plots/seedbankworldtour.pdf", height = 3, width = 5)
par(mar=c(1,1,1,1))
plot(world, lwd=0.5, col="lightgrey", border="grey")
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat_Current=="Arable",], col=alpha("gold",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat_Current=="Forest",], col=alpha("forestgreen",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat_Current=="Grassland",], col=alpha("darkseagreen1",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat_Current=="Wetland",], col=alpha("skyblue3",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat_Current=="Aquatic",],col=alpha("navyblue",0.5), pch=16, cex=0.3,lwd=0.3) 

# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], bg="gold", pch=21, cex=0.35,lwd=0.3) # add the points!
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], bg="forestgreen", pch=21, cex=0.35,lwd=0.3)
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], bg="darkseagreen1", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], bg="blue", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],bg="navyblue", pch=21, cex=0.35,lwd=0.3) 
text(-120,-15,"Seed banks of the world", cex=0.5)
legend(-150,-20,c("Arable","Forest","Grassland","Wetland", "Aquatic"),pch=16,cex=0.35,col=c("gold", "forestgreen","darkseagreen1", "skyblue3","navyblue"),bty="n", pt.lwd=0.3)
dev.off()
