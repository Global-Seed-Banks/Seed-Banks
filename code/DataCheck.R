#################################
## THE BIG DATA-CHECKING SCRIPT
#################################

## Getting the latest version
system("curl -o tmpfiles/sbtemp.csv https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/gviz/tq?tqx=out:csv&sheet=Data") # download from google

# while loop to make sure that the new version has downloaded before bringing it in
while(difftime(Sys.time(),file.info("tmpfiles/sbtemp.csv")$mtime, units="secs")>30){cat(".") }
sb<-read.csv("tmpfiles/sbtemp.csv",stringsAsFactors = FALSE)


### GEOGRAPHICAL CHECKS AND CONVERSION ###

# Any rows with data that do not have lat and lon? Check according to seeds, species and density, because all data papers should have at least one of those.
nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lat_Deg)) & !is.na(sb$Seed_density_m2),])
nrow(sb[(is.na(sb$Lon_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Seed_density_m2),])

nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lat_Deg)) & !is.na(sb$Total_Species),])
nrow(sb[(is.na(sb$Lon_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Total_Species),])

nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lat_Deg)) & !is.na(sb$Total_Seeds),])
nrow(sb[(is.na(sb$Lon_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Total_Seeds),])

# remove rows that don't have both lat and long at degree resolution (i.e. no data)
sb<-sb[!is.na(sb$Lat_Deg) & !is.na(sb$Lon_Deg),] 

# Now to split the dataset into those with decimals and those without
sb.dec<-sb[grepl("\\.", sb$Lat_Deg) | grepl("\\.", sb$Lon_Deg),]
sb<-sb[!sb$URL %in% sb.dec$URL,]

# Identify rows with impossible coordinates
nrow(sb[sb$Lat_Deg>90 | sb$Lon_Deg>180,] )
sum(sb$Lat_Min>=60 & !is.na(sb$Lat_Min)) # NAs mean that different approach needed for mins and secs
sum(sb$Lon_Min>=60 & !is.na(sb$Lon_Min))
sum(sb$Lat_Sec>=60 & !is.na(sb$Lat_Sec))
sum(sb$Lon_Sec>=60 & !is.na(sb$Lon_Sec))

sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1]
sb.dec$Lat_Deg[sb.dec$Lat_NS=="N" & sign(sb.dec$Lat_Deg)==-1]
sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1]
sb.dec$Lon_Deg[sb.dec$Lon_EW=="E" & sign(sb.dec$Lon_Deg)==-1]

# Change sign for those dec degrees with compass directions where needed
sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1] <-sb.dec$Lat_Deg[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1]*-1 # make positive south decimals negative
sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1] <-sb.dec$Lon_Deg[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1]*-1 # make positive west decimals negative


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
