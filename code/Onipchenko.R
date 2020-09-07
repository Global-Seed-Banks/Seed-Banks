


# Data source: Vladimir Onipchenko vonipchenko@mail.ru, shared on September 4, 2020
# All Russian and USSR seed bank studies up until the year 2000, collected for Ken Thompson's book under NATO (?) funding (feels relevant)

library(tidyverse)
library(openxlsx)


# Emma's paths
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

species_density <- density %>% filter(Seedbank.present == 1) %>% # only seedbank records, not vegetation records
  group_by(Reference, Trial) %>% # group by study and by trial
  summarise( 
    Density_per_trial = sum(Density) ) %>% # sum the density of each species per trial
  ungroup() %>% # ungroup data
  group_by(Reference) %>% # regroup only by reference/study
  summarise( 
    Avg_density_across_trials = mean(Density_per_trial) ) %>% # average the density across trials
  rename(`Ref.No` = Reference ) # rename reference column to ref.no to match 'siteref' sheet


View(species_density)

species_summary <- density %>% filter(Seedbank.present == 1) %>% # only seedbank records, not vegetation records
  group_by(Reference) %>% # ignore trial and layer , and only summarise species by study (reference)
  summarise(
    Species_Richness = n_distinct(Species) ) %>% # count the total species per study
  rename(`Ref.No` = Reference ) %>% # rename reference column to ref.no to match 'siteref' sheet 
left_join(species_density)


View(species_summary)



trial_dat <- density %>% select(Reference, Trial) %>% #select columns of trial number and study
  distinct() %>% # get distinct values
  group_by(Reference) %>% # group by reference
  filter(Trial == max(Trial)) %>% # take the max value 
  rename(`Ref.No` = Reference ) # rename reference column to ref.no to match 'siteref' sheet 

trial_dat$`Ref.No` <- as.factor(as.character(trial_dat$`Ref.No`))
siteref$`Ref.No` <- as.factor(as.character(siteref$`Ref.No`))
species_summary$`Ref.No` <- as.factor(as.character(species_summary$`Ref.No`))


View(trial_dat)
 
sum_dat <- siteref %>% left_join(species_summary) %>% # join site and reference info with species richness info
   select(-Trial) %>%
   left_join(trial_dat) %>%
   mutate(  Volume.sampled.cm = (Area.sampled * Total.depth),
            Volume.sampled.mm = (Volume.sampled.cm * 1000 ) , # assuming the volume is cm3 X 1000 for mm3
            Total.Volume.mm = (Volume.sampled.mm * Trial)) # i think the size of sample is per trial... or per sample
 

 
head(sum_dat)
summary(sum_dat) # only 1 NA where species richness and density was not reported 
colnames(sum_dat)

# Emma's path
write.csv(sum_dat, "~/Dropbox/Projects/SeedbankMap/data/Onipchenko/processed_data.csv")




