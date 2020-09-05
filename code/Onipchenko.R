


# Data source: Vladimir Onipchenko vonipchenko@mail.ru, shared on September 4, 2020
# All Russian and USSR seed bank studies up until the year 2000, collected for Ken Thompson's book under NATO (?) funding (feels relevant)

library(tidyverse)
library(openxlsx)



density <- read.xlsx("~/Dropbox/Projects/SeedbankMap/data/Onipchenko/RDB.xlsx", sheet = 1)
species <- read.xlsx("~/Dropbox/Projects/SeedbankMap/data/Onipchenko/RDB.xlsx", sheet = 2)
sites <- read.xlsx("~/Dropbox/Projects/SeedbankMap/data/Onipchenko/RDB.xlsx", sheet = 3)
refs <- read.xlsx("~/Dropbox/Projects/SeedbankMap/data/Onipchenko/RDB.xlsx", sheet = 4)


colnames(sites)
colnames(refs)

# join references with the site information
siteref <- refs %>% left_join(sites)


head(siteref)

# species and density sheet are the same
# use the density sheet
head(density)
View(density)

species_summary <- density %>% filter(Seedbank.present == 1) %>% # only seedbank records, not vegetation records
  group_by(Reference) %>% # ignore trial and layer , and only summarise species by study (reference)
  summarise(
    Species_Richness = n_distinct(Species), # count the species
    Density = sum(Density) ) %>% # sum the densities
  rename(`Ref.No` = Reference ) # rename reference column to ref.no to match 'siteref' sheet


View(species_summary)


trial <- density %>% select(Reference, Trial) %>% #select columns of trial number and study
  distinct() %>% # get distinct values
  group_by(Reference) %>% # group by reference
  filter(Trial == max(Trial)) # take the max value 

trial$Reference <- as.factor(as.character(trial$Reference))

 sum_dat <- siteref %>% left_join(species_summary) %>% # join site and reference info with species richness info
   select(-Trial) %>%
   left_join(trial) %>%
   mutate(  Volume.sampled.cm = (Area.sampled * Total.depth),
            Volume.sampled.mm = (Volume.sampled.cm * 1000 ) ,
            Total.Volume.mm = (Volume.sampled.mm * Trial)) # assuming the volume is cm3 X 1000 for mm3
 

 
head(sum_dat)
summary(sum_dat) # only 1 NA where species richness and density was not reported 

View(sum_dat)




