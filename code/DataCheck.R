#################################
## THE BIG DATA-CHECKING SCRIPT
#################################

## Getting the latest version
system("curl -o tmpfiles/sbtemp.csv https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/gviz/tq?tqx=out:csv&sheet=Data") # download from google

# while loop to make sure that the new version has downloaded before bringing it in
while(difftime(Sys.time(),file.info("tmpfiles/sbtemp.csv")$mtime, units="secs")>30){
  Sys.sleep(1)}
cat("wait a sec... ") 
Sys.sleep(3)

sb<-read.csv("tmpfiles/sbtemp.csv",stringsAsFactors = FALSE, strip.white = TRUE)

# Emma, feel free to add something for your download from sheets...


### Packages
library(sp) # for converting to decimal degrees

### GEOGRAPHICAL CHECKS AND CONVERSION ###

# Any rows with data that do not have lat and lon? Check according to seeds, species and density, because all data papers should have at least one of those.
nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Seed_density_m2),])

nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Total_Species),])

nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Total_Seeds),])

nrow(sb[(is.na(sb$Lat_Deg) | is.na(sb$Lon_Deg)) & !is.na(sb$Seed_density_litre),])

# Also check where there is lat or lon but not the other
nrow(sb[!is.na(sb$Lon_Deg) & is.na(sb$Lat_Deg),] )
nrow(sb[!is.na(sb$Lat_Deg) & is.na(sb$Lon_Deg),] )

# remove rows that don't have both lat and long at degree resolution (i.e. no data)
sb<-sb[!is.na(sb$Lon_Deg) & !is.na(sb$Lat_Deg),] # 3015 rows with coordinates

# Now to split the dataset into those with decimals and those without
sb.dec<-sb[grepl("\\.", sb$Lat_Deg) | grepl("\\.", sb$Lon_Deg),]
sb<-sb[!sb$Title %in% sb.dec$Title,]

sb$URL[!sb$Lat_NS %in% c("N","S")]
sb$URL[!sb$Lon_EW %in% c("E","W")]

# Identify rows with impossible coordinates
nrow(sb[sb$Lat_Deg>90 | sb$Lon_Deg>180,] )
sum(sb$Lat_Min>=60 & !is.na(sb$Lat_Min)) # NAs mean that different approach needed for mins and secs
sum(sb$Lon_Min>=60 & !is.na(sb$Lon_Min))
sum(sb$Lat_Sec>=60 & !is.na(sb$Lat_Sec))
sum(sb$Lon_Sec>=60 & !is.na(sb$Lon_Sec))

# Check out decimal coordinates with compass directions and non-matching signs.
sb.dec$URL[sb.dec$Lat_NS=="S" & sign(sb.dec$Lat_Deg)==1]
sb.dec$URL[sb.dec$Lat_NS=="N" & sign(sb.dec$Lat_Deg)==-1 & !is.na(sb.dec$Lat_Deg)]
sb.dec$URL[sb.dec$Lon_EW=="W" & sign(sb.dec$Lon_Deg)==1]
sb.dec$URL[sb.dec$Lon_EW=="E" & sign(sb.dec$Lon_Deg)==-1]

# add zeroes for minutes/seconds where they are blank
sb$Lon_Min[is.na(sb$Lon_Min)]<-0; sb$Lat_Min[is.na(sb$Lat_Min)]<-0  
sb$Lat_Sec[is.na(sb$Lat_Sec)]<-0; sb$Lon_Sec[is.na(sb$Lon_Sec)]<-0 

# for conversion - first paste together the coordinates with d, m, s as separators (needed later)
sb_LatConv<-paste0(sb$Lat_Deg,"d",sb$Lat_Min,"m", sb$Lat_Sec,"s",sb$Lat_NS) 
sb_LonConv<-paste0(sb$Lon_Deg,"d",sb$Lon_Min,"m", sb$Lon_Sec,"s",sb$Lon_EW)

# then char2dms converts the coordinates to decimals, using the separators we just added. Overwrite original column
sb$Lat_Deg<-as.numeric(char2dms(sb_LatConv,"d","m","s"))
sb$Lon_Deg<-as.numeric(char2dms(sb_LonConv,"d","m","s"))

sb<-rbind(sb,sb.dec) # bind back together

sb<-sb[,!names(sb) %in% c("Lat_Min","Lat_Sec", "Lat_NS", "Lon_Min", "Lon_Sec", "Lon_EW" )]

# not sure if necessary...
#write.csv(sb,"tmpfiles/sbtemp_geocleaned.csv", row.names=FALSE) # write new version, so that cleaned dataset can be read in again with appropriate column classes

### Error checks - i.e. incorrect categories or impossible values ###
# Here, trying to go though the columns from left to right...

#sb<-read.csv("tmpfiles/sbtemp_geocleaned.csv", stringsAsFactors = FALSE, strip.white = TRUE)

## But first, check the class of the columns, are they reasonable (i.e. numeric/integer or character as appropriate)
sapply(sb,class)
#sb$URL[grep(",", sb$Sample_Area_mm2)]
#sort(sb$Method_Volume_Fraction[nchar(sb$Method_Volume_Fraction)>1])


## Info - White section ##
# studies that have different dois (prob drag and autofill)
sum(lapply(unique(sb$Title), function(x) length(unique(sb$Doi[sb$Title==x])))>1)
# unique(sb$Title)[lapply(unique(sb$Title), function(x) length(unique(sb$Doi[sb$Title==x])))>1]


## Habitats - Green section ##

# Habitat check
table(sb$Habitat)

# Target habitats
table(sb$Target_Habitat)

# Arable plus has target habitat - need checking as should only be rare occasion that it is at that point in a transition (i.e. a restored grassland on an arable field would be a sort of grassland in the first year).
sb[sb$Habitat=="Arable" & nchar(sb$Target_Habitat)>0,]
#araplustarget<-sb$Title[sb$Habitat=="Arable" & nchar(sb$Target_Habitat)>0]
#write.csv(sb[sb$Title %in% araplustarget,], "tmpfiles/ara_plus_target_check.csv", row.names = FALSE)

# Checked 4-5/1 2021 - all now changed so that arable is always arable. Even after 1 year of succession towards target forest, it is now forest forest, as I think that we should see it as a degraded forest that is managed/treated as a forest. Next step is then to check other Grassland Forest, to make sure that they are all managed/treated as grasslands rather than just something in between arable and forest

# Checking Grassland Forest then.
nrow(sb[sb$Habitat=="Grassland" & sb$Target_Habitat=="Forest",]) # post check this is 19
#grasslandforest<-sb$Title[sb$Habitat=="Grassland" & sb$Target_Habitat=="Forest"]
#write.csv(sb[sb$Title %in% grasslandforest,], "tmpfiles/grassland_forest_check.csv", row.names=FALSE)

# Checked 11-12/1 2021 - Grassland Forest retained where grassland is naturally or anthropogenically stable. In many cases the study biome and author language determine whether it is Forest Forest (rainforest degradation, abandonment of slash and burn agriculture) or Grassland Grassland - deterioration of traditional, long term native and species rich grasslands, or Grassland Forest where 'bad' pasture activities or other stability means that it is Grassland but with Forest target.


# Checking Wetland Forest then.
sb[sb$Habitat=="Wetland" & sb$Target_Habitat=="Forest",] # there are none


# Arable as target habitat - need checking, but probably just editing to remove the target. Sites should be arable fields, or if not we should look at them as degraded/restored habitats rather than degraded arable fields
nrow(sb[sb$Target_Habitat=="Arable",])


# Rows with target habitats but no habitat 
nrow(sb[is.na(sb$Habitat) & !is.na(sb$Target_Habitat),]) # none

# What is left:
unique(cbind(sb$Habitat, sb$Target_Habitat))

# [1,] "Grassland" ""          # Managed or natural grassland (including shrublands like dehesa, fynbos)
# [2,] "Grassland" "Grassland" # Degraded grassland, can also be early abandonment
# [3,] "Wetland"   ""          # Managed or natural wetland
# [4,] "Forest"    ""          # Mature forest communities
# [5,] "Arable"    ""          # Arable fields, orchards
# [6,] "Wetland"   "Wetland"   # Degraded wetlands
# [7,] "Forest"    "Forest"    # Degraded forest, includes range of successional stages and plantations
# [8,] "Forest"    "Grassland" # Secondary forest or plantation where managed grassland is ideal community
# [9,] "Grassland" "Forest"    # Managed or natural grassland where forest is ideal community (usually tropical)
# [10,] "Aquatic"   ""         # Natural rivers, lakes, ponds
# [11,] "Aquatic"   "Aquatic"  # Degraded rivers, lakes, ponds
# [12,] "Grassland" "Wetland"  # Degraded fens and riparian habitats (2 papers)
# [13,] "Forest"    "Wetland"  # Secondary forest or plantation where wetland is ideal community
# [14,] "Aquatic"   "Wetland"  # aquatic undergoing conversion to wetland (1 paper)


# Experiment should be 1 or empty
table(sb$Experiment) # okay


## Sampling - Yellow section ##

# do published areas and calculated areas match up where both diameter and area are given?
sb_multiarea<-sb[!is.na(sb$Sample_Diameter_mm) & !is.na(sb$Sample_Area_mm2),]
sb_multiarea_check<-cbind(pi*(0.5*sb_multiarea$Sample_Diameter_mm)^2,sb_multiarea$Sample_Area_mm2, sb_multiarea)
nrow(sb_multiarea_check)
#write.csv(sb_multiarea_check, "tmpfiles/multi_sampling_area_check.csv", row.names = FALSE)
# After check, 7 rows, all close enough (to be overwritten)

# same but for volumes
sb_multivol<-sb[(!is.na(sb$Sample_Diameter_mm) | !is.na(sb$Sample_Area_mm2)) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3),]
sb_multivol_check<-cbind(sb_multivol$Sample_Volume_mm3,((pi*(0.5*sb_multivol$Sample_Diameter_mm)^2)*sb_multivol$Sample_Depth_mm),sb_multivol$Sample_Area_mm2*sb_multivol$Sample_Depth_mm, sb_multivol)
nrow(sb_multivol_check)
#write.csv(sb_multivol_check, "tmpfiles/multi_sampling_vol_check.csv", row.names = FALSE)
# After check, 56 rows, all close enough (to be overwritten)

# So now overwrite!
sb$Sample_Area_mm2[is.na(sb$Sample_Area_mm2)]<-pi*(sb$Sample_Diameter_mm[is.na(sb$Sample_Area_mm2)]/2)^2
sb$Sample_Volume_mm3[is.na(sb$Sample_Volume_mm3)]<-sb$Sample_Area_mm2[is.na(sb$Sample_Volume_mm3)] * sb$Sample_Depth_mm[is.na(sb$Sample_Volume_mm3)] 


## Sites and Plots - Purple Section ##

# Check that total plots equals sites * plots when all are given.
sb_multiplot<-sb[!is.na(sb$Number_Sites) & !is.na(sb$Samples_Per_Site) & !is.na(sb$Total_Number_Samples) ,]
nrow(sb_multiplot[!sb_multiplot$Total_Number_Samples == sb_multiplot$Number_Sites*sb_multiplot$Samples_Per_Site,])

# Empty number of sites - where samples per site given, it is important
sb_sps<-sb[!is.na(sb$Samples_Per_Site),]
nrow(sb_sps[is.na(sb_sps$Number_Sites),])

# Empty number of sites - but total sample number there, so less important but still good to look at
sb_tns<-sb[!is.na(sb$Total_Number_Samples),]
nrow(sb_tns[is.na(sb_tns$Number_Sites),])

# Now can actually calculate total plots
sb$Total_Number_Samples[is.na(sb$Total_Number_Samples)]<-sb$Number_Sites[is.na(sb$Total_Number_Samples)]*sb$Samples_Per_Site[is.na(sb$Total_Number_Samples)]


## Method - Beige section ##

# Actual method
table(sb$Method)
MethCheck<-sb[sb$Method %in% c("","Unknown"),] # looks good after checks
#write.csv(MethCheck, "tmpfiles/method_check.csv", row.names = FALSE)
### Outlier checks - i.e. outliers that should be checked in case of errors ###

# Fraction bigger than 1?
nrow(sb[sb$Method_Volume_Fraction>1 & !is.na(sb$Method_Volume_Fraction),])

# Method volume mm3 bigger than sampled volume mm3
MethVolCheck<-sb[!is.na(sb$Method_Volume_mm3) & !is.na(sb$Sample_Volume_mm3),]
nrow(MethVolCheck[(MethVolCheck$Sample_Volume_mm3>MethVolCheck$Sample_Volume_mm3*MethVolCheck$Total_Number_Samples),])


## Results - Blue section ##

# More species than seeds?
SeedSpeCheck<-sb[!is.na(sb$Total_Seeds) & !is.na(sb$Total_Species),]
nrow(SeedSpeCheck[SeedSpeCheck$Total_Species>SeedSpeCheck$Total_Seeds,])

# Pos species
PosSpeCheck<-sb[!is.na(sb$Pos_Species) & !is.na(sb$Total_Species),]
nrow(PosSpeCheck[PosSpeCheck$Pos_Species>PosSpeCheck$Total_Species,])

# Neg species
NegSpeCheck<-sb[!is.na(sb$Neg_Species) & !is.na(sb$Total_Species),]
nrow(NegSpeCheck[NegSpeCheck$Neg_Species>NegSpeCheck$Total_Species,])

# Pos + Neg species
PosNegSpeCheck<-sb[!is.na(sb$Pos_Species) & !is.na(sb$Neg_Species) & !is.na(sb$Total_Species),]
nrow(PosNegSpeCheck[PosNegSpeCheck$Neg_Species+PosNegSpeCheck$Pos_Species>PosNegSpeCheck$Total_Species,])

# Seed number ever a fraction?
SeedFracCheck<-sb[!is.na(sb$Total_Seeds),]
TotChar<-as.character(SeedFracCheck$Total_Seeds)
sum(grepl("\\.",TotChar))
nrow(SeedFracCheck[grepl("\\.",TotChar),])

# Species number ever a fraction?
SpeFracCheck<-sb[!is.na(sb$Total_Species),]
TotChar<-as.character(SpeFracCheck$Total_Species)
sum(grepl("\\.",TotChar))
nrow(SpeFracCheck[grepl("\\.",TotChar),])