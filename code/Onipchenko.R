


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

# species and density sheet seem to be the same
# use the species sheet


head(species)

species_summary <- species %>% filter(Seedbank.present == 1) %>% # only seedbank records, not vegetation records
  group_by(Reference) %>% # ignore trial and layer , and only summarise species by study (reference)
  summarise(
    Species_Richness = n_distinct(Species) ) %>%
  rename(`Ref.No` = Reference ) # rename reference column to ref,no to match siteref sheet



View(species_summary)


 sum_dat <- siteref %>% left_join(species_summary) # join site and reference info with species richness info

 
 View(sum_dat)


