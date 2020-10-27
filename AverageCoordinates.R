#### Coordinate averaging
# First, copy from table or text, then save as a csv.

# Then read in the csv
coords<-read.csv("tmpfiles/coortemp.csv", header=TRUE, stringsAsFactors = FALSE)
coords<-data.frame(Latitude=c('44°33’00"N', '44°58’00"N','43°59’00"N', '43°43’00"N' ), Longitude=c('87°43’00"W','88°2’0"W','83°31’00"W','83°56’0"W'), stringsAsFactors = FALSE)

# Load the required packaged
library(sp)

# The package requires you to give separators for your coordinates, i.e. what comes after the degree number, the minute number or the second number. The defaults are "d", "'", and "\". But your data might be different, in which case assign them as objects, e.g.
degsign<-"°"
minsign<-"′" 
secsign<-'"'

# In my original case, the coordinates looked like this "47°44.227N". The function didn't accept "N" as the minute separator, so I added one as below
coords$Latitude<-gsub("N","'N", coords$Latitude)
coords$Longitude<-gsub("E","'E", coords$Longitude)

# Then I should convert. 'as.numeric' converts to decimal degrees, otherwise it just returns standardised degrees. Add chs=secsign if your data has seconds rather than decimalised minutes. 
coords$Lat_Deg<-as.numeric(char2dms(coords$Latitude,chd=degsign,chm=minsign))  
coords$Lon_Deg<-as.numeric(char2dms(coords$Longitude,chd=degsign,chm=minsign))

# So then I can get my averaged coordinates
mean(coords$Lat_Deg)
mean(coords$Lon_Deg)

# or according to habitat or something else.
mean(coords$Lat_Deg[coords$Cat=="Deveg"])
mean(coords$Lon_Deg[coords$Cat=="Deveg"])


### Coordinate  conversion
library(rgdal)

## UTM
# This first study gave e.g.
# UTM 34 05029 E, 86 701 N.
# UTM 34 05048 E, 86 694 N
# The first number is the zone (you need the specific latitudinal zone for UTM) so apply that below. Also the numbers (E without the 34)  needed 00 on the end for it to make sense.

zone<-34

coords<-SpatialPoints(cbind(
  c(418300, 421600, 422300), # X (EW) COORDINATES
  c(7733100, 7725100,7726600)),
    proj4string=CRS(paste0("+proj=utm +zone=",zone," ellps=WGS84")))

(coords.trans<-spTransform(coords, CRS("+proj=longlat +datum=WGS84") ))

# means
mean(coords.trans@coords[,2]) # LATITUDE
mean(coords.trans@coords[,1]) # LONGITUDE


## British national grid coordinates (e.g. SY 865923)
# First edit the tmpfiles/bng_in.txt file (i.e. not excel)that has the grid squares that need coordinates (no header)
library(rgdal)

system(paste0(paste0("cd '",getwd(),"' | "), "perl ngconv.pl tmpfiles/bng_in.txt -o=tmpfiles/bng_out.csv -point=mid -includengr"))

bng<-read.csv("tmpfiles/bng_out.csv", stringsAsFactors = FALSE, header=FALSE)
coords<-SpatialPoints(bng[,2:3], proj4string=CRS("+init=epsg:27700"))
(coords.trans<-spTransform(coords, CRS("+proj=longlat +datum=WGS84") ))

mean(coords.trans@coords[,2]) # LATITUDE
mean(coords.trans@coords[,1]) # LONGITUDE

