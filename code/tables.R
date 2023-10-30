


user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)

setwd(paste0(path2wd, 'Tables/'))

table_1 <- read.csv('table_1.csv')
table_2 <- read.csv('table_2.csv')
table_3 <- read.csv('table_3.csv')
table_4 <- read.csv('table_4.csv')
table_5 <- read.csv('table_5.csv')
table_6 <- read.csv('table_6.csv')

head(table_1)
head(table_2)
head(table_3)
head(table_4)
head(table_5)
head(table_6)



table_1_dat <- table_1 %>%   unite("Richness-area slope", Lower.CI:Upper.CI, sep="-") %>%
  mutate(`Richness-area slope` = paste0("(", `Richness-area slope`, ")") ) %>%
  select(-c(X, Model))  %>%
  unite("Richness-area slope", Estimate:`Richness-area slope`, sep=" ")

table_1_dat

table_3_dat <- table_3 %>%   unite("Seed density", Lower.CI:Upper.CI, sep="-") %>%
  mutate(`Seed density` = paste0("(", `Seed density`, ")") ) %>%
  select(-c(X, Model)) %>%
  unite("Seed density", Estimate:`Seed density`, sep=" ")

table_3_dat

table_4_dat <- table_4 %>%   unite("Seed-species ratio", Lower.CI:Upper.CI, sep="-") %>%
  mutate(`Seed-species ratio` = paste0("(", `Seed-species ratio`, ")") ) %>%
  select(-c(X, Model)) %>%
  unite("Seed-species ratio", Estimate:`Seed-species ratio`, sep=" ")

table_4_dat


table_5_dat <- table_5 %>%   unite("Richness seeds slope", Lower.CI:Upper.CI, sep="-") %>%
  mutate(`Richness seeds slope` = paste0("(", `Richness seeds slope`, ")") ) %>%
  select(-c(X, Model)) %>%
  unite("Richness seeds slope", Estimate:`Richness seeds slope`, sep=" ")

table_5_dat

combined_dat <- table_1_dat %>% left_join(table_3_dat) %>% left_join(table_4_dat) %>% left_join(table_5_dat) %>%
  mutate(`WWF.Biome` = fct_relevel(`WWF.Biome`, c(
                                       "Tundra", "Boreal Forests/Taiga", "Montane Grasslands and Shrublands",
                                       "Temperate Broadleaf and Mixed Forests",  "Temperate Conifer Forests", "Temperate Grasslands, Savannas and Shrublands",
                                       "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands",
                                       "Tropical and Subtropical Forests", "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                                       "Aquatic", "Arable")
  )) %>% arrange(`WWF.Biome` )

combined_dat
head(combined_dat)

setwd(paste0(path2wd, 'Tables/'))
write.csv(combined_dat, "table_7.csv")
