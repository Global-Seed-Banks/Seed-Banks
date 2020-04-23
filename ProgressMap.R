#################################
### THE MAP 

### googlesheets package - couldn't get to work  ###
# 
# #install.packages("googlesheets4")
# library(googlesheets4)
# #sheets_auth("ali.auffret@gmail.com") # 
# sb<-read_sheet("https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4")

### So this is just copying the spreadsheet and saving as csv ###

#library(maps) # For plotting map
library(sp) # For converting to decimal degrees
library(scales)
library(rgdal)
library(rgeos)

world<-readOGR("GIS","CNTR_RG_60M_2014", stringsAsFactors = FALSE) # import world map
cc<-read.csv("GIS/countrycodes.csv",stringsAsFactors = FALSE)
world$country<-cc$Name[match(world$CNTR_ID,cc$Code)]

system("curl -o sbtemp.csv https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/gviz/tq?tqx=out:csv&sheet=Data")
sb<-read.csv("sbtemp.csv",stringsAsFactors = FALSE)

nrow(sb[!is.na(sb$Total_Species),]) # count rows with species data, just for info

sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] # remove rows that don't have both lat and long at degree resolution



# First have to split the dataset into those with decimals and those without
sb.dec<-sb[grep("\\.", sb$Lat_Deg),]
sb<-sb[!grepl("\\.", sb$Lat_Deg),]

# Then account for mistakes in coordinates - need to work out properly later, but do this for now
sb<-sb[!sb$Lat_Deg>90,] # remove rows with impossible latitudes
sb$Lat_Min[sb$Lat_Min>59]<-59; sb$Lon_Min[sb$Lon_Min>179]<-179 # Some places have minutes (and some seconds) above 60, which I think is impossible. Need to sort these out better eventually but the conversion seems to work anyway.

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


# plot
#pdf("seedbankworldtour.pdf", height = 3, width = 5)
par(mar=c(1,1,1,1))
plot(world, lwd=0.5, col="lightgrey", border="grey")
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], col=alpha("gold",0.4), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], col=alpha("forestgreen",0.4), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], col=alpha("darkseagreen1",0.4), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], col=alpha("blue",0.4), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],col=alpha("navyblue",0.4), pch=16, cex=0.3,lwd=0.3) 

# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], bg="gold", pch=21, cex=0.35,lwd=0.3) # add the points!
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], bg="forestgreen", pch=21, cex=0.35,lwd=0.3)
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], bg="darkseagreen1", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], bg="blue", pch=21, cex=0.35,lwd=0.3) 
# points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],bg="navyblue", pch=21, cex=0.35,lwd=0.3) 
text(-120,-15,"Seed banks of the world", cex=0.5)
legend(-150,-20,c("Arable","Forest","Grassland","Wetland", "Aquatic"),pch=16,cex=0.35,col=c("gold", "forestgreen","darkseagreen1", "blue","navyblue"),bty="n", pt.lwd=0.3)
#dev.off()


## Geographical error checking
 
# make shapefile of points
sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS("+init=epsg:4326"))

# subset by rows which do not intersect with world map
sb.out<-sb.shp[which(!rownames(sb.shp@data) %in% rownames(sb.shp[world,]@data)),]

# plot on world map
plot(sb.out, pch=19, col="red") # first plot points (as some may be outside the known world)
plot(world, lwd=0.5, col="lightgrey", border="grey", add=TRUE) # add world map
plot(sb.out, pch=19, col="red", add=TRUE) # add points back on top

# Show in table
sb.out[,which(names(sb.out)=="Human"):which(names(sb.out)=="Location")]


# More error checking - Mixed up decimal and nondecimal degrees.
sb.dec[(sign(sb.dec$Lat_Deg)==1 & sb.dec$Lat_NS=="S") | (sign(sb.dec$Lon_Deg)==1 & sb.dec$Lon_EW=="W") ,]

# coverage info. Which countries not covered.
world.cov<-world[sb.shp,]
world$country[!world$country %in% world.cov$country]
