sb<-read.csv("sb_pub.csv")

############
# Abstract

nrow(sb) # 3097 data points

length(unique(paste(sb$Lat_Deg), sb$Lon_Deg)) # 1778 locations

length(unique(sb$StudyID)) # 1442 studies
 
range(sb$Year) # 1940 - 2020.


############
# Intro
nrow(sb) # 3096 data points
length(unique(sb$StudyID)) # 1442 studies
sum(sb$Total_number_samples, na.rm=TRUE) # 1 081 363
sum(sb$Number_sites, na.rm=TRUE) # 18 477 sites 
length(unique(sb$Country)) # 94 countries

######
ncol(sb)

#############
# Biome table
table(sb$biome_wwf)
table(sb$biome_wwf_broad)
table(sb$biome_wwf_zone)


##############
# Habitat table
table(paste(sb$Habitat, sb$Target_Habitat))


###############
# Ref list
sb$studylong<-paste(sb$Authors, sb$Year, sb$Title, sb$Journal, paste("doi:",sb$Doi),sep=", ")

cat(sort(unique(sb$studylong)),sep="\n\n", file="tmpfiles/component_refs.txt")



#############
# Make a map
#library(maps) # For plotting map
library(rgdal)
library(rgeos)
transp<-function(col,alpha){
  trans.col<-adjustcolor(col,alpha.f = alpha)
  return(trans.col)}


world<-readOGR("GIS","CNTR_RG_20M_2020_4326") # import world map
pdf("plots/datapaper_fig1.pdf", height = 3, width = 5)
par(mar=c(1,1,1,1))
plot(world, lwd=0.5, col="lightgrey", border="grey")
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Arable",], col=transp("gold",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Forest",], col=transp("forestgreen",0.5), pch=16, cex=0.3,lwd=0.3)
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Grassland",], col=transp("darkseagreen1",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Wetland",], col=transp("skyblue3",0.5), pch=16, cex=0.3,lwd=0.3) 
points(Lat_Deg~Lon_Deg, data=sb[sb$Habitat=="Aquatic",],col=transp("navyblue",0.5), pch=16, cex=0.3,lwd=0.3) 


legend(-150,-20,c("Arable","Forest","Grassland","Wetland", "Aquatic"),pch=16,cex=0.5,col=c("gold", "forestgreen","darkseagreen1", "skyblue3","navyblue"),bty="n", pt.lwd=0.6)
dev.off()



#################
## Final crap

# Change order of columns, get rid of excess. 

# see if there are any 
