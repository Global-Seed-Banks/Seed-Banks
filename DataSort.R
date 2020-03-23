### GLOBAL SEED BANK RICHNESS ###
##### SPREADSHEET CREATION ######

# Search: 29 January 2020
# TOPIC: “seed bank*” 
# YEARS: 1945-2019

# 6536 hits - exported from WoS in Firefox (Tab-delimited UTF8), 500 hits at a time. Opened in libreoffice and put together manually to form one csv because of perculiarities with quotations in some rows that was not smoothly handled in R (i.e. with lapply and do.call).

sb.all<-read.csv("savedrecs_ALL.csv", stringsAsFactors=FALSE)
sb.all<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)>0,] # reduce to Journal Articles and those with DOIs - 5537
sb.min<-data.frame(Author=sb.all$AU, Year=sb.all$PY, Title=sb.all$TI, Journal=sb.all$SO, Doi=sb.all$DI, URL=paste0("http://doi.org/",sb.all$DI), stringsAsFactors = FALSE) # create slimmed-down df.

write.csv(sb.min,"savedrecs_slim.csv", row.names=FALSE)

# Search: 10 February 2020
# TOPIC: “seedbank*” 
# YEARS: 1945-2019

# 1000 hits - treated as above.

sb.all<-read.csv("savedrecs_ALL2.csv", stringsAsFactors=FALSE)
sb.all<-sb.all[sb.all$PT=="J" & nchar(sb.all$DI)>0,] # reduce to Journal Articles and those with DOIs
sb.min<-data.frame(Author=sb.all$AU, Year=sb.all$PY, Title=sb.all$TI, Journal=sb.all$SO, Doi=sb.all$DI, URL=paste0("http://doi.org/",sb.all$DI), stringsAsFactors = FALSE) 

write.csv(sb.min,"savedrecs_slim_2.csv", row.names=FALSE)

## TOTAL 6406 papers - pasted into google sheets