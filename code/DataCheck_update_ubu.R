#################################
## THE BIG DATA-CHECKING SCRIPT
#################################

# ## Getting the latest version -- temporarily allow access on Google Sheets
system("curl -o sbtemp.csv https://docs.google.com/spreadsheets/d/10H1CWb5cc2FNEzTjxROdZuT2F6DwXCa-Ng3_DAsZ2K4/gviz/tq?tqx=out:csv&sheet=Data") # download from google

# # while loop to make sure that the new version has downloaded before bringing it in
 while(difftime(Sys.time(),file.info("tmpfiles/sbtemp.csv")$mtime, units="secs")>30){
 Sys.sleep(1)}
 cat("wait a sec... ") 
 Sys.sleep(3) 

sb<-read.csv("sbtemp.csv",stringsAsFactors = FALSE, strip.white = TRUE)
nrow(sb)
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

# And if there is a direction in one column but not the other
nrow(sb[nchar(sb$Lat_NS)==0 & !nchar(sb$Lon_EW)==0,])
nrow(sb[!nchar(sb$Lat_NS)==0 & nchar(sb$Lon_EW)==0,])

# remove rows that don't have both lat and long at degree resolution (i.e. no data)
sb<-sb[!is.na(sb$Lon_Deg) & !is.na(sb$Lat_Deg),] 
nrow(sb) # 3108 rows with coordinates

# Now to split the dataset into those with decimals and those without
sb.dec<-sb[grepl("\\.", sb$Lat_Deg) | grepl("\\.", sb$Lon_Deg),] 
nrow(sb.dec) #1314 with decimals

sb<-sb[!rownames(sb) %in% rownames(sb.dec),]
nrow(sb) # 1794 without that need converting

# Any strange directions?
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
nrow(sb) # 3108

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



############################################
########################################
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


############################################
########################################
## Sampling - Yellow section ##

# quick sanity check - diameters and depths
sort(unique(sb$Sample_Diameter_mm)) # diameters checked and okay
#sb$URL[sb$Sample_Diameter_mm==330 & !is.na(sb$Sample_Diameter_mm)]
sort(unique(sb$Sample_Depth_mm)) # depths checked and okay
#sb$URL[sb$Sample_Depth_mm==2 & !is.na(sb$Sample_Depth_mm)]


# do published areas and calculated areas match up where both diameter and area are given?
sb_multiarea<-sb[!is.na(sb$Sample_Diameter_mm) & !is.na(sb$Sample_Area_mm2),]
sb_multiarea_check<-cbind(pi*(0.5*sb_multiarea$Sample_Diameter_mm)^2,sb_multiarea$Sample_Area_mm2, sb_multiarea)
nrow(sb_multiarea_check) # After check, 7 rows, all close enough (to be overwritten)
#write.csv(sb_multiarea_check, "tmpfiles/multi_sampling_area_check.csv", row.names = FALSE)


# same but for volumes
sb_multivol<-sb[(!is.na(sb$Sample_Diameter_mm) | !is.na(sb$Sample_Area_mm2)) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3),]
sb_multivol_check<-cbind(sb_multivol$Sample_Volume_mm3,((pi*(0.5*sb_multivol$Sample_Diameter_mm)^2)*sb_multivol$Sample_Depth_mm),sb_multivol$Sample_Area_mm2*sb_multivol$Sample_Depth_mm, sb_multivol)
nrow(sb_multivol_check) # After check, 49 rows, all close enough (to be overwritten)

#write.csv(sb_multivol_check, "tmpfiles/multi_sampling_vol_check.csv", row.names = FALSE)

# So now overwrite!
sb$Sample_Area_mm2[is.na(sb$Sample_Area_mm2)]<-pi*(sb$Sample_Diameter_mm[is.na(sb$Sample_Area_mm2)]/2)^2
sb$Sample_Volume_mm3[is.na(sb$Sample_Volume_mm3)]<-sb$Sample_Area_mm2[is.na(sb$Sample_Volume_mm3)] * sb$Sample_Depth_mm[is.na(sb$Sample_Volume_mm3)] 

############################################
########################################
## Sites and Plots - Purple Section ##

# Check that total plots equals sites * plots when all are given.
sb_multiplot<-sb[!is.na(sb$Number_Sites) & !is.na(sb$Samples_Per_Site) & !is.na(sb$Total_Number_Samples) ,]
nrow(sb_multiplot[!sb_multiplot$Total_Number_Samples == sb_multiplot$Number_Sites*sb_multiplot$Samples_Per_Site,])

# Empty number of sites - where samples per site given, it is important
sb_sps<-sb[!is.na(sb$Samples_Per_Site),]
nrow(sb_sps[is.na(sb_sps$Number_Sites),])

# Empty number of samples per site when number of site is given.. also important.
sb_nositeonly<-sb[!is.na(sb$Number_Sites) & is.na(sb$Total_Number_Samples) & is.na(sb$Samples_Per_Site),] # cam 
nrow(sb_nositeonly) # 8 ... all with unclear sampling
#write.csv(sb_nositeonly, "tmpfiles/sb_nositeonly_check.csv", row.names = FALSE)

# Empty number of sites - but total sample number there, so less important but still good to look at
sb_nonosites<-sb[is.na(sb$Number_Sites),]
table(sb_nonosites$Human) # just TE and VO now.
#write.csv(sb_nonosites, "tmpfiles/sb_nonosites_check.csv", row.names = FALSE)

# Now can actually calculate total plots where it was empty
sb$Total_Number_Samples[is.na(sb$Total_Number_Samples)]<-sb$Number_Sites[is.na(sb$Total_Number_Samples)]*sb$Samples_Per_Site[is.na(sb$Total_Number_Samples)]


############################################
########################################
## Method - Beige section ##

# Actual method
table(sb$Method)
MethCheck<-sb[sb$Method %in% c("","Unknown"),] # looks good after checks --- 20
nrow(MethCheck)
#write.csv(MethCheck, "tmpfiles/method_check.csv", row.names = FALSE)


# Fraction bigger than 1?
nrow(sb[sb$Method_Volume_Fraction>1 & !is.na(sb$Method_Volume_Fraction),])

# Method volume mm3 bigger than sampled volume mm3
MethVolCheck<-sb[!is.na(sb$Method_Volume_mm3) & !is.na(sb$Sample_Volume_mm3),]
nrow(MethVolCheck[(MethVolCheck$Sample_Volume_mm3>MethVolCheck$Sample_Volume_mm3*MethVolCheck$Total_Number_Samples),])


############################################
########################################
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

# How does calculated vs. given seed density match up?
DensTotCheck<-sb[!is.na(sb$Total_Seeds) & !is.na(sb$Seed_density_m2),]
DensTotCheck$DensityCalc<-DensTotCheck$Total_Seeds/(DensTotCheck$Total_Number_Samples*(DensTotCheck$Sample_Area_mm2/1000000))
DensTotCheck$DensityCalcMultiple<-DensTotCheck$Seed_density_m2/DensTotCheck$DensityCalc
DensTotCheck<-DensTotCheck[!is.na(DensTotCheck$DensityCalc) & (DensTotCheck$DensityCalcMultiple>1.05 | DensTotCheck$DensityCalcMultiple<0.95),]
nrow(DensTotCheck) # 32 post check
#write.csv(DensTotCheck,"Calc_Dens_check_part3.csv", row.names=FALSE)


#############################################
########################################
### Infilling and back-calculation ###

# if unknown number of sites -> 1
sb$Number_Sites[is.na(sb$Number_Sites)]<-1

# First calculate density if possible (overwriting the weird ones)
sb.denscalc1<-sb[is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds),]
sb.denscalc1.rows<-which(is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds))
sb$Seed_density_m2[sb.denscalc1.rows]<-sb.denscalc1$Total_Seeds/((sb.denscalc1$Sample_Area_mm2*sb.denscalc1$Total_Number_Samples)/1000000)

# Then, calculate area from volume and depth
sb.areacalc1<-sb[is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3),]
sb.areacalc1.rows<-which(is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3))
sb$Sample_Area_mm2[sb.areacalc1.rows]<-sb.areacalc1$Sample_Volume_mm3/sb.areacalc1$Sample_Depth_mm

# Then area from seeds, samples and density
sb.areacalc2<-sb[is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds),]
sb.areacalc2.rows<-which(is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds))
sb$Sample_Area_mm2[sb.areacalc2.rows]<-round(((sb.areacalc2$Total_Seeds/sb.areacalc2$Seed_density_m2)*1000000)/sb.areacalc2$Total_Number_Samples)

# Then area from volume and depth where there is no density.
sb.areacalc3<-sb[is.na(sb$Sample_Area_mm2) & is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3),]
sb.areacalc3.rows<-which(is.na(sb$Sample_Area_mm2) & is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3))
sb$Sample_Area_mm2[sb.areacalc3.rows]<-sb.areacalc3$Sample_Volume_mm3/sb.areacalc3$Sample_Depth_mm

# Then area from volume and assumed 10 mm depth
sb.areacalc4<-sb[is.na(sb$Sample_Area_mm2) & is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3),]
sb.areacalc4.rows<-which(is.na(sb$Sample_Area_mm2) & is.na(sb$Sample_Depth_mm) & !is.na(sb$Sample_Volume_mm3))
sb$Sample_Area_mm2[sb.areacalc4.rows]<-sb.areacalc4$Sample_Volume_mm3/100

# Then try again to calculate density
sb.denscalc2<-sb[is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds),]
sb.denscalc2.rows<-which(is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds))
sb$Seed_density_m2[sb.denscalc2.rows]<-sb.denscalc2$Total_Seeds/((sb.denscalc2$Sample_Area_mm2*sb.denscalc2$Total_Number_Samples)/1000000)

# what about with Litres?
sb.denscalc3<-sb[is.na(sb$Seed_density_m2) & !is.na(sb$Seed_density_litre) & !is.na(sb$Sample_Area_mm2) &!is.na(sb$Total_Number_Samples),]
sb.denscalc3.rows<-which(is.na(sb$Seed_density_m2) & !is.na(sb$Seed_density_litre) & !is.na(sb$Sample_Area_mm2) &!is.na(sb$Total_Number_Samples))
sb.denscalc3.totseeds<-sb.denscalc3$Seed_density_litre*((sb.denscalc3$Sample_Volume_mm3*sb.denscalc3$Total_Number_Samples)/1000000)
sb$Seed_density_m2[sb.denscalc3.rows]<-sb.denscalc3.totseeds/((sb.denscalc3$Total_Number_Samples*sb.denscalc3$Sample_Area_mm2)/1000000)

# seeds and sample size but no number of samples? -- none
sb[!is.na(sb$Sample_Area_mm2) & is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds) &!is.na(sb$Seed_density_m2),]

# Calculate for simplicity, total sampled area
sb$Total_sampled_area_m2<-((sb$Sample_Area_mm2/1000000)*sb$Total_Number_Samples)

# And now species density
sb$Species_Density_m2<-sb$Total_Species/sb$Total_sampled_area_m2

# Back-calculate species number from density and sampled area
sb$Total_Seeds[is.na(sb$Total_Seeds) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples)]<-round(((sb$Sample_Area_mm2[is.na(sb$Total_Seeds) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples)]/1000000)*sb$Total_Number_Samples[is.na(sb$Total_Seeds) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples)])* (sb$Seed_density_m2[is.na(sb$Total_Seeds) & !is.na(sb$Seed_density_m2) & !is.na(sb$Sample_Area_mm2) & !is.na(sb$Total_Number_Samples)]))

# More to check where number species is higher than calculated number of seeds.
nrow(sb[sb$Total_Seeds < sb$Total_Species & !is.na(sb$Total_Seeds) & !is.na(sb$Total_Species),])
sp.gt.sds<-sb[sb$Total_Seeds < sb$Total_Species & !is.na(sb$Total_Seeds) & !is.na(sb$Total_Species),]
#write.csv(sp.gt.sds, "tmpfiles/morespeciesthanseedscheck.csv", row.names=FALSE)

## Also, for info:

# Yes: Density, Samples; No: Area, Seeds - nothing to be done? - 41 rows
nrow(sb[is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & !is.na(sb$Total_Number_Samples) & is.na(sb$Total_Seeds),])

# 3. Yes: Density, Seeds; No: Area, Samples - none
nrow(sb[is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds),])

# 4. Yes: Density, Seeds; No: Area, Samples - none
nrow(sb[is.na(sb$Sample_Area_mm2) & !is.na(sb$Seed_density_m2) & is.na(sb$Total_Number_Samples) & !is.na(sb$Total_Seeds),])




### OUTLIER CHECKS ###

# area sampled
# boxplot(sb$Total_sampled_area_m2)
# sb.area.out<-sb[sb$Total_sampled_area_m2 %in% boxplot.stats(sb$Total_sampled_area_m2)$out,]
# sb.area.out<-sb.area.out[order(sb.area.out$Total_sampled_area_m2),]
# sb.area.out<-sb.area.out[!duplicated(sb.area.out$Title),]

area.mean<-mean(sb$Total_sampled_area_m2, na.rm=TRUE)
area.3sd<-sd(sb$Total_sampled_area_m2, na.rm=TRUE)*3

out.area.up<-sb[sb$Total_sampled_area_m2 > (area.mean+area.3sd) & !is.na(sb$Total_sampled_area_m2),]
out.area.lo<-sb[sb$Total_sampled_area_m2 < (area.mean-area.3sd) & !is.na(sb$Total_sampled_area_m2),]
nrow(out.area.up) # 14, checked and ok
nrow(out.area.lo) # 0

#write.csv(out.area.up,"tmpfiles/outliers_area_up_2.csv", row.names=FALSE)
#write.csv(out.area.lo,"tmpfiles/outliers_area_up_1.csv", row.names=FALSE)


# seeds m2
# boxplot(sb$Seed_density_m2)
# boxplot.stats(sb$Seed_density_m2)

seeddens.mean<-mean(sb$Seed_density_m2, na.rm=TRUE)
seeddens.3sd<-sd(sb$Seed_density_m2, na.rm=TRUE)*3

out.seed.up<-sb[sb$sb$Seed_density_m2 > (seeddens.mean+seeddens.3sd) & !is.na(sb$Seed_density_m2),]
out.seed.lo<-sb[sb$sb$Seed_density_m2 < (seeddens.mean-seeddens.3sd) & !is.na(sb$Seed_density_m2),]
nrow(out.seed.up) # 0
nrow(out.seed.lo) # 0


#write.csv(out.seed.up,"tmpfiles/outliers_seed_up_1.csv", row.names=FALSE)
#write.csv(out.seed.lo,"tmpfiles/outliers_seed_up_1.csv", row.names=FALSE)

# species m2
# boxplot(sb$Species_Density_m2)
# boxplot.stats(sb$Species_Density_m2)

spedens.mean<-mean(sb$Species_Density_m2, na.rm=TRUE)
spedens.3sd<-sd(sb$Species_Density_m2, na.rm=TRUE)*3

out.spe.up<-sb[sb$Species_Density_m2 > (spedens.mean+spedens.3sd) & !is.na(sb$Species_Density_m2),] # $Human
out.spe.lo<-sb[sb$Species_Density_m2 < (spedens.mean-spedens.3sd) & !is.na(sb$Species_Density_m2),]

nrow(out.spe.up) # 91 post check
nrow(out.spe.lo) # 0


#write.csv(out.spe.up,"tmpfiles/outliers_spe_up_5.csv", row.names=FALSE)
#write.csv(out.spe.lo,"tmpfiles/outliers_spe_up_1.csv", row.names=FALSE)



#########################################################

## ALL CHECKS COMPLETED 10 JANUARY 2023 ##
 
########################################################


### NOW SOME FINAL SORTING OF THE 


#### Add country

library(rgdal)
library(rgeos)
world<-readOGR("GIS","CNTR_RG_20M_2020_4326", stringsAsFactors = FALSE) # import world map

sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS(proj4string(world)))
sb.world.df<-over(sb.shp,world)
sb.shp$country<-sb.world.df$NAME_ENGL
sb$country<-sb.world.df$NAME_ENGL
#sb$country<-sb.world.df$country

sb.sea<-sb.shp[is.na(sb.shp$country),]
sb.sea.dist<-gDistance(sb.sea,world, byid=TRUE)
sb$country[is.na(sb$country)]<-sapply(1:nrow(sb.sea),function(x) world$NAME_ENGL[which.min(sb.sea.dist[,x])])

#sb.countrycheck<-sb[,c(1:11,which(names(sb)=="country"))]
#write.csv(sb.countrycheck,"tmpfiles/country_check.csv", row.names=FALSE)

# All countries checked manually, changed where necessary 21 October 2021, two final changes below
sb$country[sb$Doi=="10.1111/j.1654-109X.2005.tb00643.x"]<-"Switzerland" # Too close to border for low res world map.
sb$country[sb$Doi=="10.1017/S0266467400010774"]<-"French Guiana" # Strange colonial thing

sort(unique(sb$country))



########################################################
#### Add climate

library(raster)
library(gdalUtils)

# climate data only available 1979-2013. H
1-sum(sb$Year<1979 | sb$Year>2019)/nrow(sb) # 96% covered by good quality 79-2019.

# First bring in any chelsa layer - they have the same extent, resolution etc.
bio<-raster("/media/auff/AuffLacie2/CHELSA/chelsa_V1/bioclim/integer/CHELSA_bio10_01.tif")
             
# then create spatial layer of the sb data 
sb.shp<-SpatialPoints(cbind(sb$Lon_Deg,sb$Lat_Deg),proj4string=CRS(proj4string(bio)))

# extract the data from each point
sb$bio1<-(extract(bio,sb.shp)/10)-273.15

# create no climate layer (where they don't intersect, i.e. points are in the sea)
sb.nc<-sb.shp[is.na(sb$bio1),]

# buffer around these, then write the shapefile
sb.nc.buff<-gBuffer(sb.nc,byid=FALSE, width=0.5)
sb.nc.buff<-SpatialPolygonsDataFrame(sb.nc.buff, data=data.frame(ID=1), match.ID=FALSE)
writeOGR(sb.nc.buff,"GIS","sb_nc_buff","ESRI Shapefile",overwrite_layer=TRUE)

# rasterize this shapefile according to the chelsa parameters- Bring it in, then re-save smaller using raster.
gdal_rasterize("GIS/sb_nc_buff.shp","GIS/sb_nc_buff_tmpras.tif", tr=res(bio),burn=1,verbose=TRUE, a_nodata=NA, a_srs=proj4string(bio), te=extent(bio)[c(1,3,2,4)], ot="Byte")

nc.buffras.tmp<-raster("GIS/sb_nc_buff_tmpras.tif")
writeRaster(nc.buffras.tmp,"GIS/sb_nc_buff_ras.tif", dataType="LOG1S", overwrite=TRUE)
rm(nc.buffras.tmp); file.remove("GIS/sb_nc_buff_tmpras.tif") # remove the big ones

# bring in smaller version, convert to points, then overlay with bio to get the points that actually have data
nc.buffras<-raster("GIS/sb_nc_buff_ras.tif")
nc.buffras.pts<-rasterToPoints(nc.buffras,spatial = TRUE)
nc.buffras.pts$bio<-extract(bio,nc.buffras.pts)
nc.buffras.pts<-nc.buffras.pts[!is.na(nc.buffras.pts$bio),]

# run difference, get coordinates of closest pixel centroids of those that have data
sb.nc.dist<-gDistance(sb.nc,nc.buffras.pts, byid=TRUE)
sapply(1:length(sb.nc),function(x) min(sb.nc.dist[,x])) # sanity check, they are all close (i.e. buffer was big enough)
nc.nearpoints<-coordinates(nc.buffras.pts[sapply(1:length(sb.nc),function(x) which.min(sb.nc.dist[,x])),])

# make new columns for use in the climate data extraction, make spatial
sb$Lat_Deg_Clim<-sb$Lat_Deg
sb$Lon_Deg_Clim<-sb$Lon_Deg
sb$Lon_Deg_Clim[is.na(sb$bio1)]<-nc.nearpoints[,1]
sb$Lat_Deg_Clim[is.na(sb$bio1)]<-nc.nearpoints[,2]
sb.shp.clim<-SpatialPoints(cbind(sb$Lon_Deg_Clim,sb$Lat_Deg_Clim),proj4string=CRS(proj4string(bio)))

rm(bio) # remove the bioclim one that is just hanging around taking up space


## Now to get the real climate data sorted

# First copy January 1979 data over from V1 - I figure it is better than nothing
file.copy("/media/auff/AuffLacie2/CHELSA/chelsa_V1/timeseries/tmax/CHELSA_tmax_1979_01_V1.2.1.tif","/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmax/CHELSA_tasmax_01_1979_V.2.1.tif" )

file.copy("/media/auff/AuffLacie2/CHELSA/chelsa_V1/timeseries/tmin/CHELSA_tmin_1979_01_V1.2.1.tif","/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmin/CHELSA_tasmin_01_1979_V.2.1.tif" )

file.copy("/media/auff/AuffLacie2/CHELSA/chelsa_V1/timeseries/tmean/CHELSA_tmean_1979_01_V1.2.1.tif","/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tas/CHELSA_tas_01_1979_V.2.1.tif" )

# Sort out parameters for the climate data we want
tp<-10 # assign time period in years
mo<-sprintf("%02d",1:12) # months with leading 0

# Bring in lists of lists - all years and months of all climate data
# Average T
ras.list.tmean<-list()
for(ye in 1979:2019){ # all years
  ras.list.tmean[[paste0("Y",ye)]]<-lapply(paste0("/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tas/CHELSA_tas_",mo,"_",ye,"_V.2.1.tif"),raster)}

# Max T
ras.list.tmax<-list()
for(ye in 1979:2019){ # all years
  ras.list.tmax[[paste0("Y",ye)]]<-lapply(paste0("/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmax/CHELSA_tasmax_",mo,"_",ye,"_V.2.1.tif"),raster)}

# Min T
ras.list.tmin<-list()
for(ye in 1979:2019){ # all years
  ras.list.tmin[[paste0("Y",ye)]]<-lapply(paste0("/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmin/CHELSA_tasmin_",mo,"_",ye,"_V.2.1.tif"),raster)}

# Prec
ras.list.prec<-list()
for(ye in 1979:2018){ # all years
  ras.list.prec[[paste0("Y",ye)]]<-lapply(paste0("/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/pr/CHELSA_pr_",mo,"_",ye,"_V.2.1.tif"),raster)}


# Then run the loop, a year at a time, taking all plot points that were taken (published) that year
for(i in sort(unique(sb$Year))){

# Assign the years we want to extract for, depending on what year the data are from
temp.years<- if(i<1988) 1979:(1979+(tp-1)) else if(i>2019) (2019-(tp-1)):2019 else (i-(tp-1)):unique(i)
prec.years<- if(i<1988) 1979:(1979+(tp-1)) else if(i>2018) (2018-(tp-1)):2018 else (i-(tp-1)):unique(i)

sb.ye.shp<-sb.shp.clim[sb$Year==i] # subset the shapefile according to the plots for that year

  # Do overlay - for every year, take each month's temperature for each coordinate. The CHELSA time series data is in Kelvins*10, so need   to convert to Celsius also.
  plot.mo.tmean<-lapply(temp.years,function(x) lapply(ras.list.tmean[[paste0("Y",x)]], function(y) (extract(y,sb.ye.shp)/10)-273.15))

  # Average annual temperature for the ten years (average of each month's average)
  plot.ye.tmean<-lapply(plot.mo.tmean, function(x) Reduce('+',x)/12)
  sb$t_mean[sb$Year==i]<-plot.tp.tmean<-Reduce('+',plot.ye.tmean)/tp


  if(length(sb.ye.shp)>1){  # Identify the each year's warmest month
    # Identify the each year's warmest month
  plot.ye.warm<-lapply(plot.mo.tmean, function(x) sapply(1:length(sb.ye.shp), function(y) which.max(mapply(c,x)[y,])))
  
  # # the above broken down for future reference and understanding:
  # plot.ye1.mean<-plot.mo.tmean[[1]] # take first year (lapply above applies to all years)
  # plot.ye1.mean.mat<-mapply(c,plot.ye1.mean) # make that into a table (rows= each plot, cols = each month)
  # sapply(1:nrow(sb), function(x) which.max(plot.ye1.mean.mat[x,])) # from that table, identify the column with the highest value for     each row (plot).
  
  # Identify the each year's coolest month in the same way...
  plot.ye.cold<-lapply(plot.mo.tmean, function(x) sapply(1:length(sb.ye.shp), function(y) which.min(mapply(c,x)[y,])))
  
  # Do overlay to get monthly max temps for each month in each year
  plot.mo.tmax<-lapply(temp.years,function(x) lapply(ras.list.tmax[[paste0("Y",x)]], function(y) (extract(y,sb.ye.shp)/10)-273.15))
  
  # Get each year's max temperature from the warmest month.
  plot.ye.tmax<-lapply(1:tp, function(x) mapply(c,plot.mo.tmax[[x]])[cbind(1:length(sb.ye.shp),plot.ye.warm[[x]])])
  
  # # Break it down
  # plot.y1.tmax<-plot.mo.tmax[[1]] # take first year only
  # plot.y1.tmax.mat<-mapply(c,plot.y1.tmax) # convert to matrix (rows=plots, cols=monthly max temp)
  # plot.y1.tmax.mat[cbind(1:nrow(sb),plot.ye.warm[[1]])] # take the cell from each row that is the month with warmest average temp, as   calculated above (plot.ye.warm)
  
  # Do overlay to get monthly min temps for each month in each year
  plot.mo.tmin<-lapply(temp.years,function(x) lapply(ras.list.tmin[[paste0("Y",x)]], function(y) (extract(y,sb.ye.shp)/10)-273.15))
  
  # Get each year's min temperature from the coolest month.
  plot.ye.tmin<-lapply(1:tp, function(x) mapply(c,plot.mo.tmin[[x]])[cbind(1:length(sb.ye.shp),plot.ye.cold[[x]])])
  }
  
  if(length(sb.ye.shp)==1){  # single plots need different (mapply-free) approach
    plot.ye.warm<-lapply(plot.mo.tmean, which.max) # Identify the each year's warmest month
    plot.ye.cold<-lapply(plot.mo.tmean,which.min)# Identify the each year's coolest month
    plot.mo.tmax<-lapply(temp.years,function(x) lapply(ras.list.tmax[[paste0("Y",x)]], function(y) (extract(y,sb.ye.shp)/10)-273.15)) # Do overlay to get monthly max temps for each month in each year
    plot.ye.tmax<-sapply(1:tp, function(x) plot.mo.tmax[[x]][plot.ye.warm[[x]]]) # Each year's max temperature from the warmest month.
    plot.mo.tmin<-lapply(temp.years,function(x) lapply(ras.list.tmin[[paste0("Y",x)]], function(y) (extract(y,sb.ye.shp)/10)-273.15)) # Do overlay to get monthly min temps for each month in each year
    plot.ye.tmin<-sapply(1:tp, function(x) plot.mo.tmin[[x]][plot.ye.cold[[x]]]) # Get each year's min temperature from the coolest month. 
  }
  # Then take each year's temperature range 
  plot.ye.atr<-lapply(1:tp, function(x) plot.ye.tmax[[x]] - plot.ye.tmin[[x]]) 
  
  # And finally get the mean over the time period
    sb$t_range[sb$Year==i]<-plot.tp.atr<-Reduce('+',plot.ye.atr)/tp
  
  
  # Do overlay to get monthly precipitation for each month in each year (kg per m2 = mm)
  plot.mo.prec<-lapply(prec.years,function(x) lapply(ras.list.prec[[paste0("Y",x)]], function(y) extract(y,sb.ye.shp)/100))
  
  # Total precipitation for the time period
  plot.ye.prec<-lapply(plot.mo.prec, function(x) Reduce('+',x))
  
  # Take the average per year across the time period
  sb$p_tot[sb$Year==i]<-Reduce('+',plot.ye.prec)/tp
  
  cat(i,"...done! //  ")
}

# Remove the cheeky january 1979 copies
file.remove("/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmax/CHELSA_tasmax_01_1979_V.2.1.tif","/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tasmin/CHELSA_tasmin_01_1979_V.2.1.tif" ,"/media/auff/AuffLacie2/CHELSA/chelsa_V2/monthly/tas/CHELSA_tas_01_1979_V.2.1.tif")

# check NAs, plus some countries, climates

# ## Add PCNM eigenvectors now, just in case.
# 
# library(vegan)
# plot.dist<-as.matrix(dist(cbind(sb$Lon_Deg,sb$Lat_Deg)))
# plot.pcnm<-pcnm(plot.dist)
# sb$pcnm1<-plot.pcnm$vectors[,1]
# sb$pcnm2<-plot.pcnm$vectors[,2]

## Add habitat (degraded)
sb$Target_Habitat[sb$Target_Habitat==""]<-NA
sb$Habitat2_Broad<-sb$Target_Habitat
sb$Habitat2_Broad[is.na(sb$Habitat2_Broad)]<-sb$Habitat[is.na(sb$Habitat2_Broad)]
sb$Habitat2_Degraded<-ifelse(is.na(sb$Target_Habitat),0, 1)
    
# Finally?? Make an ID for each row that can be used to link the slim and full versions. Use Human, just because it's a column that has short, unique values and all rows have a value. First check and change.
table(sb$Human)
sb$Human[sb$Human=="JP\n"]<-"JP"
sb$Human[sb$Human=="DR/JP"]<-"DR"
sb$Human[sb$Human=="NSH"]<-"NH"

# Then loop through
for(i in unique(sb$Human)){ sb$rowID[sb$Human==i]<-paste0(i,sprintf("%03d",1:sum(sb$Human==i)))}

# Hmm. Also need a unique code for each study (potentially as random effect)
sb$studylong<-paste0(sb$Authors,sb$Year,sb$Title,sb$Journal)
length(unique(sb$studylong)) # 1454 studies
table(toupper(substr(sb$Authors,1,1))) # just the first letter is less than 1000, so just use that to avoid a messier loop.

for(let in LETTERS){ # for each letter
  sbx<-sb[toupper(substr(sb$Authors,1,1))==let,] # subset to that letter
  for(let.un in unique(sbx$studylong)){ # then for each unique long name (author, year, title, journal)
   sb$studyID[sb$studylong==let.un]<-paste0(let,sprintf("%03d",which(unique(sbx$studylong)==let.un))) # assign the letter and which study
    }}

length(unique(sb$studyID)) # Matches up.

# Oh, and add a location for each row too.
sb$Location[is.na(sb$Location)]<-sb$country[is.na(sb$Location)]
 
biomes<-readOGR("GIS", "tnc_terr_ecoregions", stringsAsFactors = FALSE)
sb.shp<-SpatialPointsDataFrame(cbind(sb$Lon_Deg,sb$Lat_Deg),data=sb, proj4string=CRS(proj4string(biomes)))
sb.biomes.df<-over(sb.shp,biomes)

# First, originals
sb.shp$biome_wwf<-sb.biomes.df$WWF_MHTNAM
sb$biome_wwf<-sb.biomes.df$WWF_MHTNAM
sb.sea<-sb.shp[is.na(sb.shp$biome_wwf),]
sb.sea.dist<-gDistance(sb.sea,biomes, byid=TRUE)
sb$biome_wwf[is.na(sb$biome_wwf)]<-sapply(1:nrow(sb.sea),function(x) biomes$WWF_MHTNAM[which.min(sb.sea.dist[,x])])

# Second, broader (due to data)
# First just redo without the mangrove (couldn't think of significantly better way to do it)
biomes.nomang<-biomes[!biomes$WWF_MHTNAM=="Mangroves",]
sb.biomes.df.nomang<-over(sb.shp,biomes.nomang)
sb.shp$biome_wwf_broad<-sb.biomes.df.nomang$WWF_MHTNAM
sb$biome_wwf_broad<-sb.biomes.df.nomang$WWF_MHTNAM
sb.sea.nomang<-sb.shp[is.na(sb.shp$biome_wwf_broad),]
sb.sea.dist.nomang<-gDistance(sb.sea.nomang,biomes.nomang, byid=TRUE)
sb$biome_wwf_broad[is.na(sb$biome_wwf_broad)]<-sapply(1:nrow(sb.sea.nomang),function(x) biomes.nomang$WWF_MHTNAM[which.min(sb.sea.dist.nomang[,x])])

# Then reassign.
sb$biome_wwf_broad[sb$biome_wwf_broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
sb$biome_wwf_broad[sb$biome_wwf_broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"

# Third, much broader (due to simplification and that we already have habitats)
# First just redo without the mangrove and montane (couldn't think of significantly better way to do it)
biomes.nomangmont<-biomes[!biomes$WWF_MHTNAM %in% c("Mangroves","Montane Grasslands and Shrublands", "Rock and Ice","Flooded Grasslands and Savannas"),]
sb.biomes.df.nomangmont<-over(sb.shp,biomes.nomangmont)
sb.shp$biome_wwf_zone<-sb.biomes.df.nomangmont$WWF_MHTNAM
sb$biome_wwf_zone<-sb.biomes.df.nomangmont$WWF_MHTNAM
sb.sea.nomangmont<-sb.shp[is.na(sb.shp$biome_wwf_zone),]
sb.sea.dist.nomangmont<-gDistance(sb.sea.nomangmont,biomes.nomangmont, byid=TRUE)
sb$biome_wwf_zone[is.na(sb$biome_wwf_zone)]<-sapply(1:nrow(sb.sea.nomangmont),function(x) biomes.nomangmont$WWF_MHTNAM[which.min(sb.sea.dist.nomangmont[,x])])

# Then reassign.
sb$biome_wwf_zone[sb$biome_wwf_zone %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests","Tropical and Subtropical Grasslands, Savannas and Shrublands")] <- "Tropical"
sb$biome_wwf_zone[sb$biome_wwf_zone %in% c("Mediterranean Forests, Woodlands and Scrub",  "Deserts and Xeric Shrublands")] <- "Mediterranean and Desert"
sb$biome_wwf_zone[sb$biome_wwf_zone %in% c("Temperate Broadleaf and Mixed Forests",  "Temperate Grasslands, Savannas and Shrublands", "Temperate Conifer Forests" )] <- "Temperate"
sb$biome_wwf_zone[sb$biome_wwf_zone == "Boreal Forests/Taiga"] <- "Boreal"


write.csv(sb,"gsb_cleaned.csv", row.names=FALSE)

slim.cols<-c("rowID","studyID", "Year", "Lat_Deg","Lon_Deg","country", "t_mean", "t_range", "p_tot", "Habitat","Target_Habitat","Habitat2_Broad","Habitat2_Degraded", "biome_wwf", "biome_wwf_broad", "biome_wwf_zone","Experiment", "Sample_Diameter_mm","Sample_Area_mm2","Sample_Depth_mm","Sample_Volume_mm3","Sample_Weight_g","Number_Sites","Samples_Per_Site", "Total_Number_Samples", "Method","Method_Volume_mm3","Method_Volume_Fraction","Method_Weight_g",  "Total_Seeds","Seed_density_m2","Seed_density_litre","Total_Species", "Pos_Species","Neg_Species")
  
#,"Authors","Year","Title","Journal","Doi", "Human"
sb.slim<-sb[,slim.cols]

names(sb.slim)<-c("rowID","studyID", "Year", "Lat_Deg","Lon_Deg","Country", "Temp_mean", "Temp_range", "Prec_tot", "Habitat_Current","Habitat_Target","Habitat_Broad","Habitat_Degraded", "Biome_WWF", "Biome_WWF_Broad", "Biome_WWF_Zone", "Experiment", "Sample_Diameter_mm","Sample_Area_mm2","Sample_Depth_mm","Sample_Volume_mm3","Sample_Weight_g","Number_Sites","Samples_Per_Site", "Total_Number_Samples", "Method","Method_Volume_mm3","Method_Volume_Fraction","Method_Weight_g",  "Total_Seeds","Seed_density_m2","Seed_density_litre","Total_Species", "Pos_Species","Neg_Species")

write.csv(sb.slim,"gsb_slim.csv", row.names=FALSE)
sb<-read.csv("gsb_slim.csv", stringsAsFactors = FALSE)







