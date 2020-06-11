#### Coordinate averaging
# First, copy from table or text, then save as a csv.

# Then read in the csv
coords<-read.csv("coortemp.csv", header=TRUE, stringsAsFactors = FALSE)
coords<-data.frame(Latitude=c('11°50’36"N', '12°11’20"N'), Longitude=c('39°37’26"E','39°45’15"E'), stringsAsFactors = FALSE)

# Load the required packaged
library(sp)

# The package requires you to give separators for your coordinates, i.e. what comes after the degree number, the minute number or the second number. The defaults are "d", "'", and "\". But your data might be different, in which case assign them as objects, e.g.
degsign<-"°"
minsign<-"’" 
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
mean(coords$Lat_Deg[!coords$Type=="Natural wetland"])
mean(coords$Lon_Deg[!coords$Type=="Natural wetland"])
