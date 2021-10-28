#####################################
## GLOBAL SEED BANKS #############
## Analysis... I guess ##

sb<-read.csv("gsb_slim.csv", stringsAsFactors = FALSE)

length(unique(sb$studyID))
nrow(sb)
sum(sb$Total_Number_Samples, na.rm=TRUE)
sum(sb$Total_Seeds, na.rm=TRUE)
length(unique(sb$Country))

sb[is.na(sb$Total_Number_Samples),]


head(sb)

plot(Total_Species~log(Sample_Area_mm2), data=sb)
