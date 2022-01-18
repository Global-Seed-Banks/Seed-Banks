#####################################
## GLOBAL SEED BANKS #############
## Analysis... I guess ##

library(lme4)
sb<-read.csv("gsb_slim.csv", stringsAsFactors = FALSE)

length(unique(sb$studyID))
nrow(sb)
sum(sb$Total_Number_Samples, na.rm=TRUE)
sum(sb$Total_Seeds, na.rm=TRUE)
length(unique(sb$Country))

sb[is.na(sb$Total_Number_Samples),]
sum(is.na(sb$Sample_Area_mm2))/nrow(sb)
sum(sb$Habitat_Broad=="Aquatic")
sum(sb$Habitat_Broad=="Wetland")
sum(sb$Habitat_Broad=="Arable")


sb.temp<-sb[sb$Biome_WWF_Zone=="Temperate",]
m1<-glmer(Total_Species ~ log(Sample_Area_mm2) + Habitat_Broad + Habitat_Degraded + Temp_mean + Prec_tot + pcnm1 + pcnm2 + Year +  (1|studyID), data=sb.temp, family=poisson)
summary(m1)

summary(glm(Seed_density_m2 ~ log(Sample_Area_mm2) + Sample_Depth_mm, data=sb.temp))

hist(sb.temp$Total_Species)

head(sb)

plot(Total_Species~log(Sample_Area_mm2), data=sb)
plot(Total_Seeds~Sample_Area_mm2, data=sb)

