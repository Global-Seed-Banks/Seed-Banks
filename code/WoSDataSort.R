### GLOBAL SEED BANK RICHNESS ###
#
############ SPREADSHEET CREATION ###############################

## DATA FROM WoS:
# Search: 29 January 2020
# TOPIC: “seed bank*” 
# YEARS: 1945-2019

# 6536 hits - exported from WoS in Firefox (Tab-delimited UTF8), 500 hits at a time. Opened in libreoffice and put together to form one csv because of perculiarities with quotations in some rows that was not smoothly handled in R.

sb.all<-read.csv("WoS outputs/seed bank/savedrecs_ALL.csv", stringsAsFactors=FALSE)
sb.all<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)>0,] # reduce to Journal Articles and those with DOIs - 5537
sb.min<-data.frame(Author=sb.all$AU, Year=sb.all$PY, Title=sb.all$TI, Journal=sb.all$SO, Doi=sb.all$DI, URL=paste0("http://doi.org/",sb.all$DI), stringsAsFactors = FALSE) 

write.csv(sb.min,"WoS outputs/seed bank/savedrecs_slim.csv", row.names=FALSE)

head(sb.min)


# Search: 10 January 2020
# TOPIC: “seedbank*” 
# YEARS: 1945-2019

# 1000 hits
sb.all<-read.csv("WoS outputs/seedbank/savedrecs_ALL.csv", stringsAsFactors=FALSE)
sb.all<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)>0,] # reduce to Journal Articles and those with DOIs - 5537
sb.min<-data.frame(Author=sb.all$AU, Year=sb.all$PY, Title=sb.all$TI, Journal=sb.all$SO, Doi=sb.all$DI, URL=paste0("http://doi.org/",sb.all$DI), stringsAsFactors = FALSE) 

write.csv(sb.min,"WoS outputs/seedbank/savedrecs_slim_2.csv", row.names=FALSE)

## TOTAL 6406 papers


##### CHecking doi-free papers.
sb.all<-read.csv("WoS outputs/seedbank/savedrecs_ALL.csv", stringsAsFactors=FALSE)
s.b.all<-read.csv("WoS outputs/seed bank/savedrecs_ALL.csv", stringsAsFactors=FALSE)

sb.all<-rbind(sb.all[,1:63],s.b.all)

sb.nodoi<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)==0,] # take journal articles those without DOIs
sb.min.nodoi<-data.frame(Author=sb.nodoi$AU, Year=sb.nodoi$PY, Title=sb.nodoi$TI, Journal=sb.nodoi$SO,Doi="None", URL=paste0('https://scholar.google.com/scholar?q=',gsub(" ","+",sb.nodoi$TI)), stringsAsFactors = FALSE) 

write.csv(sb.min.nodoi,"/home/auff/Dropbox/Global seed banks/savedrecs_nodoi.csv", row.names=FALSE)

unique(sb.min.nodoi$Journal)


### Potential 2020 update:
# Search: 24 June 2021
# TOPIC: "seed bank*"  OR seedbank*
# YEARS: 2020-2020

sb.all<-read.csv("/home/auff/Dropbox/Global seed banks/WoS outputs/2020/savedrecs.csv", stringsAsFactors=FALSE)
sb.doi<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)>0,] # reduce to Journal Articles and those with DOIs
sb.min.doi<-data.frame(Author=sb.doi$AU, Year=sb.doi$PY, Title=sb.doi$TI, Journal=sb.doi$SO, Doi=sb.doi$DI, URL=paste0("http://doi.org/",sb.doi$DI), stringsAsFactors = FALSE) 

sb.nodoi<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)==0,] # take journal articles those without DOIs
sb.min.nodoi<-data.frame(Author=sb.nodoi$AU, Year=sb.nodoi$PY, Title=sb.nodoi$TI, Journal=sb.nodoi$SO,Doi="None", URL=paste0('https://scholar.google.com/scholar?q=',gsub(" ","+",sb.nodoi$TI)), stringsAsFactors = FALSE) 

sb.min<-rbind(sb.min.doi,sb.min.nodoi)

#checking not to include duplicates
sb<-read.csv("tmpfiles/sbtemp.csv",stringsAsFactors = FALSE, strip.white = TRUE)
nrow(sb.min) # 426 rows
nrow(sb.min[sb.min$URL %in% sb$URL,])
nrow(sb.min[sb.min$Title %in% sb$Title,])

write.csv(sb.min,"/home/auff/Dropbox/Global seed banks/WoS outputs/2020/savedrecs_slim.csv", row.names=FALSE)

