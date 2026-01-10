
#rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)
library(viridis)
library(ggridges)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "emmaladouceur" = "~/Dropbox/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

sb_density <- sb_prep %>% 
  filter(!is.na(Seed_density_m2),
         !Seed_density_m2 == 0,
  ) %>%
  # treat all random effects as factors
  mutate( Habitat_degraded = as.factor(Habitat_degraded),
          Biome = as.factor(Biome),
          Habitat_broad = as.factor(Habitat_broad),
          StudyID = as.factor(StudyID),
          RowID = as.factor(RowID),
          Method = as.factor(Method)) %>% arrange(Biome) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Arable", 
    Realm == "Arable" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Arable", 
    Realm == "Arable" & Biome ==  "Tropical" ~  "Tropical Arable", 
    Realm == "Forest" & Biome ==  "Temperate" ~  "Temperate Forests",
    Realm == "Forest" & Biome ==  "Tropical" ~  "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome ==  "Boreal" ~  "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome ==  "Tropical" ~  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Deserts and Xeric Shrublands" ~  "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Mediterranean Forests, Woodlands and Scrub" ~  "Mediterranean Forests, Woodlands & Scrub",
    Realm ==  "Tundra" ~  Realm,
    Realm ==  "Wetland" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Wetlands",
    Realm ==  "Wetland" & Biome ==  "Temperate and Boreal"~  "Temperate & Boreal Wetlands",
    Realm ==  "Wetland"  & Biome ==  "Tropical" ~  "Tropical Wetlands",
  )) %>% 
  mutate(Realm_Biome = fct_relevel(Realm_Biome, 
                                   "Tundra", "Boreal Forests/Taiga",  "Temperate Forests",  "Tropical & Subtropical Forests",
                                   "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                   "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands", 
                                   "Temperate & Boreal Arable", "Mediterranean & Desert Arable",    "Tropical Arable",
                                   "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
                                   "Aquatic",
  )) %>%
  mutate(
    Group = case_when(
      # Aquatic
      Realm_Biome == "Aquatic" ~ "Aquatic",
      
      # Arable
      Realm_Biome %in% c(
        "Temperate & Boreal Arable",
        "Tropical Arable",
        "Mediterranean & Desert Arable"
      ) ~ "Arable",
      
      # Terrestrial
      Realm_Biome %in% c(
        "Boreal Forests/Taiga",
        "Tropical & Subtropical Forests",
        "Temperate Forests",
        "Temperate & Boreal Grasslands, Savannas & Shrublands",
        "Tropical & Subtropical Grasslands, Savannas & Shrublands",
        "Deserts & Xeric Shrublands",
        "Mediterranean Forests, Woodlands & Scrub",
        "Tundra"
      ) ~ "Terrestrial",
      
      # Wetlands
      Realm_Biome %in% c(
        "Temperate & Boreal Wetlands",
        "Mediterranean & Desert Wetlands",
        "Tropical Wetlands"
      ) ~ "Wetlands",
      
      TRUE ~ NA_character_
    )
  )#%>%
  #mutate( Habitat_degraded = recode_factor( Habitat_degraded,
      #                                      `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) 

head(sb_density)



setwd(paste0(path2wd, 'Model_Fits/Habs/'))
# Biomes run on cluster, load in Biome objects here
load( 'den_aq.Rdata')
load( 'den_ar.Rdata')
load( 'den_forest.Rdata')
load( 'den_grass.Rdata')
load( 'den_med_de.Rdata')
load( 'den_po_alp.Rdata')
load( 'den_wetland.Rdata')


sb_aq_d <- sb_density %>% filter(Realm == "Aquatic") 

sb_arable_d <- sb_density %>%
  filter(Realm == "Arable") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))

sb_forest_d <- sb_density %>%
  filter(Realm == "Forest")

sb_grass_d <- sb_density %>%
  filter(Realm == "Grassland") 


sb_med_de_d <- sb_density %>%
  filter(Realm == "Mediterranean and Desert") %>%
  #mutate(Biome = fct_relevel(Biome, "Mediterranean", "Deserts"))
  mutate(Biome = fct_relevel(Biome, "Mediterranean Forests, Woodlands and Scrub", "Deserts and Xeric Shrublands"))



sb_tund_d <- sb_density %>%
  filter(Realm == "Tundra") %>%  mutate( Biome = "Tundra and Boreal")

sb_wetland_d <- sb_density %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))  %>%
  filter(Realm == "Wetland") %>%
  mutate(Biome = fct_relevel(Biome,  "Temperate and Boreal", "Mediterranean and Desert","Tropical"))


# Global Mean

# List of models :  mod_aq_d, mod_ar_d,  mod_forest_d, mod_grass_d, mod_med_de_d, mod_tund_d, mod_wetland_dn 
# List of data: sb_aq_d, sb_arable_d, sb_forest_d, sb_grass_d, sb_med_de_d, sb_tund_d, sb_wetland_d

head(sb_aq_d)
summary(mod_ar_d)


aq_fitted <- cbind(mod_aq_d$data,
                             fitted(mod_aq_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                             )) %>% 
  as_tibble() %>% inner_join(sb_aq_d %>% distinct(Seed_density_m2, Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Aquatic") %>% mutate(Group = "Aquatic")

head(aq_fitted)
#View(aq_fitted)

ggplot(aq_fitted, aes(x = Estimate, y = Habitat_degraded, fill = Habitat_degraded, color = Habitat_degraded)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +  # Theme for ridgeline plot
  labs(title = "Density of Posterior Samples by Habitat Degradation", x = "Posterior Sample Value", y = "Habitat Degraded") +
  theme( plot.subtitle = element_text(face = "bold"), legend.position = "none") 

ar_fitted <- cbind(mod_ar_d$data,
                   fitted(mod_ar_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                   )) %>% 
  as_tibble() %>% inner_join(sb_arable_d %>% distinct(Seed_density_m2,  Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Arable", Habitat_degraded = "1") %>% mutate(Group = "Arable")

ar_fitted


f_fitted <- cbind(mod_forest_d$data,
                   fitted(mod_forest_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                   )) %>% 
  as_tibble() %>% inner_join(sb_forest_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Forest") %>% mutate(Group = "Terrestrial")


g_fitted <- cbind(mod_grass_d$data,
                  fitted(mod_grass_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_grass_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Grass") %>% mutate(Group = "Terrestrial")


m_fitted <- cbind(mod_med_de_d$data,
                  fitted(mod_med_de_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_med_de_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Med") %>% mutate(Group = "Terrestrial")

t_fitted <- cbind(mod_tund_d$data,
                  fitted(mod_tund_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_tund_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Tund") %>% mutate(Group = "Terrestrial")


w_fitted <- cbind(mod_wetland_d$data,
                  fitted(mod_wetland_d, re_formula = NA, probs = c(0.25, 0.75, 0.05, 0.95, 0.025, 0.975)
                  )) %>% 
  as_tibble() %>% inner_join(sb_wetland_d %>% distinct(Seed_density_m2,  Habitat_degraded, Biome, Realm),
                             #by= c("Field", "Year", "log_YSA", "log_alpha_rich_p")
  ) %>% mutate(Model = "Wet") %>% mutate(Group = "Wetlands")


fitted_values <- aq_fitted %>% bind_rows(ar_fitted, f_fitted, g_fitted, m_fitted, t_fitted, w_fitted) %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Arable", 
    Realm == "Arable" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Arable", 
    Realm == "Arable" & Biome ==  "Tropical" ~  "Tropical Arable", 
    Realm == "Forest" & Biome ==  "Temperate" ~  "Temperate Forests",
    Realm == "Forest" & Biome ==  "Tropical" ~  "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome ==  "Boreal" ~  "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome ==  "Temperate and Boreal" ~  "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome ==  "Tropical" ~  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Deserts and Xeric Shrublands" ~  "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome ==  "Mediterranean Forests, Woodlands and Scrub" ~  "Mediterranean Forests, Woodlands & Scrub",
    Realm ==  "Tundra" ~  Realm,
    Realm ==  "Wetland" & Biome ==  "Mediterranean and Desert" ~  "Mediterranean & Desert Wetlands",
    Realm ==  "Wetland" & Biome ==  "Temperate and Boreal"~  "Temperate & Boreal Wetlands",
    Realm ==  "Wetland"  & Biome ==  "Tropical" ~  "Tropical Wetlands",
  )) %>% 
  mutate(Realm_Biome = fct_relevel(Realm_Biome, 
                                   "Tundra", "Boreal Forests/Taiga",  "Temperate Forests",  "Tropical & Subtropical Forests",
                                   "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                   "Mediterranean Forests, Woodlands & Scrub",  "Deserts & Xeric Shrublands", 
                                    "Temperate & Boreal Arable", "Mediterranean & Desert Arable",    "Tropical Arable",
                                   "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands","Tropical Wetlands",
                                   "Aquatic",
  )) %>%   mutate(Realm = fct_relevel(Realm, 
                                            "Tundra", "Forest", "Grassland", "Mediterranean and Desert", "Arable", "Wetland", "Aquatic"
  ))  %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) 
  

head(fitted_values)
fitted_values %>% filter(Group == "Arable") 


aq_s <-fitted_values %>% filter(Group == "Aquatic") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
    ) %>% distinct()

aq_s


head(fitted_values)

t_s <-fitted_values %>% filter(Group == "Terrestrial") %>% 
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
t_s



w_s <-fitted_values %>%  filter(Group == "Wetlands") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
w_s

head(fitted_values %>% filter(Group == "Arable"))


ar_s <-fitted_values %>% filter(Group == "Arable") %>%
  group_by(Group) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct()
ar_s


head(t_s)
group_means <- t_s  %>% bind_rows(ar_s,w_s, aq_s ) %>%
  mutate(mean = round(mean, 0),
         median = round(median, 0),
         lower_90 = round(lower_90, 0),
         upper_90 = round(upper_90, 0),
         lower_50 = round(lower_50, 0),
         upper_50 = round(upper_50, 0),
  ) %>%
  unite("50% Credible Interval", lower_50:upper_50, sep="-") %>%
  unite("90% Credible Interval", lower_90:upper_90, sep="-") 


head(group_means)
write.csv(group_means, "~/Dropbox/GSB/Data/Table_Fig_3_Realm_Means.csv")

head(fitted_values)
fitted_values %>% select(Group) %>% distinct()


fit_groups_s <- fitted_values %>% #filter(Group == "Terrestrial") %>% 
  group_by(Group, Realm_Biome, Habitat_degraded) %>%
  summarise(
    median = median(Estimate),
    mean = mean(Estimate),
    lower_50 = mean(Q25),
    upper_50 = mean(Q75),
    lower_90 = mean(Q5),
    upper_90 = mean(Q95),
  ) %>% distinct() %>%
  mutate( Habitat_degraded = recode_factor( Habitat_degraded,
                                            `0` = "Undisturbed habitat",  `1` = "Degraded habitat"  )) 
fit_groups_s


biome_levels <- c(
  "Tundra",
  "Boreal Forests/Taiga",
  "Temperate Forests",
  "Tropical & Subtropical Forests",
  "Temperate & Boreal Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Mediterranean Forests, Woodlands & Scrub",
  "Deserts & Xeric Shrublands",
  "Temperate & Boreal Arable",
  "Mediterranean & Desert Arable",
  "Tropical Arable",
  "Temperate & Boreal Wetlands",
  "Mediterranean & Desert Wetlands",
  "Tropical Wetlands",
  "Aquatic"
)

add_y <- function(df) {
  df %>%
    mutate(
      # --- fix Realm_Biome -> y_base ---
      Realm_Biome = stringr::str_squish(as.character(Realm_Biome)),
      Realm_Biome = factor(Realm_Biome, levels = rev(biome_levels)),
      y_base      = as.numeric(Realm_Biome),
      
      # --- make your existing Habitat_degraded filters work (0/1 -> labels) ---
      Habitat_degraded = dplyr::case_when(
        as.character(Habitat_degraded) == "1" ~ "Degraded habitat",
        as.character(Habitat_degraded) == "0" ~ "Undisturbed habitat",
        TRUE ~ as.character(Habitat_degraded)
      )
    )
}


#


fitted_values <- add_y(fitted_values)
fit_groups_s     <- add_y(fit_groups_s)
sb_density     <- add_y(sb_density)

y_text_offset_w <- 0.18
y_text_offset <- 0.10

y_offset_1 <- 0.36
y_offset_2 <- 0.18
y_offset_3 <- 0.54

summary(sb_density)

# Aquatic

hab_levels <- c("Degraded habitat", "Undisturbed habitat")

add_y_h <- function(df) {
  df %>%
    mutate(
      # make a clean character version first
      Habitat_degraded_chr = as.character(Habitat_degraded) %>%
        stringr::str_squish() %>%
        stringr::str_replace_all("ha\\s+bitat", "habitat"),
      
      # if it's 0/1, recode; otherwise keep as-is
      Habitat_degraded = dplyr::case_when(
        Habitat_degraded_chr %in% c("1", "Degraded habitat")    ~ "Degraded habitat",
        Habitat_degraded_chr %in% c("0", "Undisturbed habitat") ~ "Undisturbed habitat",
        TRUE ~ NA_character_
      ),
      
      Habitat_degraded = factor(Habitat_degraded, levels = rev(hab_levels)),
      y_base = as.numeric(Habitat_degraded),
      
      # cleanup helper
      Habitat_degraded_chr = NULL
    )
}


fitted_values_a <- fitted_values %>% filter(Group == "Aquatic") %>% add_y_h()
fit_groups_s_a  <- fit_groups_s  %>% filter(Group == "Aquatic") %>% add_y_h()
sb_density_a    <- sb_density    %>% filter(Group == "Aquatic") %>% add_y_h()

# sanity checks
sb_density_a %>% count(Habitat_degraded)
sb_density_a %>% summarise(n=n(), n_y_na=sum(is.na(y_base)), n_x_na=sum(is.na(Seed_density_m2)))


y_offset <- 0.18
y_text_offset_t <- 0.10

# Always-valid label positions (never depends on sb_density_a contents)
label_df <- tibble(
  Habitat_degraded = factor(rev(hab_levels), levels = rev(hab_levels))
) %>%
  mutate(
    y_base = as.numeric(Habitat_degraded),
    label = case_when(
      Habitat_degraded == "Undisturbed habitat" ~ "Undisturbed\nhabitat",
      Habitat_degraded == "Degraded habitat"   ~ "Degraded\nhabitat",
      TRUE ~ as.character(Habitat_degraded)
    ),
    y_lab = y_base + y_text_offset_t
  )


aq_fig <- ggplot() +
  geom_density_ridges(data = fitted_values_a, aes(x = Estimate, y = y_base, fill = Habitat_degraded, color = Habitat_degraded), alpha = 0.7, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  geom_point(data = fitted_values_a %>% filter(Habitat_degraded == "Undisturbed habitat") %>%
               group_by(Habitat_degraded,y_base) %>% summarise(mean = mean(Estimate, na.rm = TRUE), .groups = "drop")
               , aes(x = mean, y = y_base, shape = Habitat_degraded), color = "#447fdd", size=5, alpha=0.9)+
  geom_point(data = fitted_values_a  %>% filter(Habitat_degraded == "Degraded habitat") %>%
               group_by(Habitat_degraded,y_base) %>% summarise(mean = mean(Estimate, na.rm = TRUE), .groups = "drop")
             , aes(x = mean, y = y_base, shape = Habitat_degraded), color = "#C0C0C0", size=5, alpha=0.9)+
  geom_errorbarh( data = fit_groups_s_a %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_90, xmax = upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s_a %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_50, xmax = upper_50 ), height = 0.15, linewidth = 1.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s_a %>%  filter(Habitat_degraded == "Undisturbed habitat") ,
                  aes(y = y_base , xmin = lower_90, xmax = upper_90 ), height = 0.15, linewidth = 0.5,color = "#447fdd" )+
  geom_errorbarh( data = fit_groups_s_a %>% filter(Habitat_degraded == "Undisturbed habitat")  ,
                  aes(y = y_base , xmin = lower_50, xmax = upper_50 ), height = 0.15, linewidth = 1.5,color = "#447fdd" )+
  geom_point(data = sb_density_a %>% filter(Habitat_degraded == "Degraded habitat")  ,
             aes(x = Seed_density_m2, y = y_base
             ), color = "#C0C0C0", size = 1.5, alpha = 0.2, shape= 16, position = position_jitter(width = 0, height=0.025)) +
  geom_point(data = sb_density_a %>% filter(Habitat_degraded == "Undisturbed habitat")  ,
             aes(x = Seed_density_m2, y = y_base
             ), color = "#447fdd", size = 1.5, alpha = 0.2, shape= 16, position = position_jitter(width = 0, height=0.025)) +
  scale_color_manual( values= c("#447fdd" , "#447fdd" ),
                      labels = c("Undisturbed", "Degraded"))+
  scale_fill_manual( values= c( "#447fdd", "#C0C0C0"),
                     labels = c("Undisturbed", "Degraded"))+
  scale_shape_manual( values= c( 16,17),
                     labels = c("Undisturbed", "Degraded"))+
  labs(subtitle = "d) Aquatic", 
    #x = expression(paste('Seed density (',m^2,')')), 
    x="", y = "State"
    ) +
  scale_x_continuous(breaks=c(0,2500, 5000,7500,10000,15000,20000), lim= c(0,20000))+
  #coord_cartesian(xlim = c(0, 20000)) +
  # --- Theme ---
  theme_bw(base_size = 18) +
  geom_text(
    data = label_df,
    aes(x = 15000, y = y_lab, label = label),
    inherit.aes = FALSE,
    colour = "grey60",
    vjust = 0,
    size = 6
  )+
  theme( plot.subtitle = element_text(face = "bold"), 
         axis.title.y = element_text(face = "bold"), 
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

aq_fig



ar_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Arable"), aes(x = Estimate, y = y_base, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth = 600) +
  geom_point(data = fitted_values %>% filter(Group %in% c("Arable")) %>%
               group_by(Biome, Realm_Biome, y_base) %>% summarise(mean = mean(Estimate))
             , aes(x = mean, y = y_base,  color = Realm_Biome), size=5, shape = 15, alpha=0.9)+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Arable")  ,
                  aes(y = y_base , xmin = lower_90, xmax = upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Arable")  ,
                  aes(y = y_base , xmin = lower_50, xmax = upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  geom_point(data = sb_density %>% filter(Group == "Arable")  ,
             aes(x = Seed_density_m2, y = y_base,  colour = Realm_Biome 
             ),  size = 1.5, alpha = 0.2, shape= 15, position = position_jitter(width = 0, height=0.025)) +
   geom_vline(data = ar_s, aes(xintercept = mean), size = 1.2, color ="#472c0b", alpha = 0.7, linetype="longdash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  #scale_y_discrete(limits=rev)+
  xlim(0,15000)+
  scale_color_manual( values= c("#AA3929",  "#E2C59F", "#99610a" ),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical") )+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c( "#AA3929",  "#E2C59F", "#99610a"),
                     labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"))+
  geom_text(data = sb_density %>% filter(Group == "Arable") %>% distinct(Realm_Biome, y_base) %>%
              mutate(label = case_when(
                Realm_Biome == "Temperate & Boreal Arable" ~ "Arable \nTemperate & Boreal",
                Realm_Biome ==  "Mediterranean & Desert Arable" ~ "Arable \nMediterranean & Desert",
                Realm_Biome == "Tropical Arable" ~ "Arable \nTropical",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset, label = label), x = 7500, colour = "grey60", vjust = 0, size=6
  )+
  labs(subtitle = "b) Terrestrial - Arable", 
       x = expression(paste('Seed density (',m^2,')')), y = "Ecosystem") +
  scale_x_continuous(breaks=c(0,2500, 5000,7500,10000,12500,15000), lim= c(0,15000))+
  # --- Theme ---
  theme_bw(base_size = 18) +
  theme( plot.subtitle = element_text(face = "bold"), 
         axis.title.y = element_text(face = "bold"), 
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

ar_fig

aq_fig + t_fig + ar_fig



#,  fill = "#C0C0C0"
t_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") , 
                      aes(x = Estimate, y = y_base,  color = Realm_Biome), alpha = 0.7, bandwidth =600, fill = "#C0C0C0") +
  geom_density_ridges(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
                      aes(x = Estimate, y = y_base + y_offset_1, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth =600) +
  theme_ridges() +  # Theme for ridgeline plot
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_90, xmax = upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_50, xmax = upper_50 ), height = 0.15, linewidth = 1.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
                  aes(y = y_base + y_offset_1, xmin = lower_90, xmax = upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
                  aes(y = y_base + y_offset_1, xmin = lower_50, xmax = upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  geom_point(data = sb_density %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") ,
             aes(x = Seed_density_m2, y = y_base ,
             ), color = "#C0C0C0",  size = 1.5, alpha = 0.2, shape= 17, position = position_jitter(width = 0, height=0.025)) +
  geom_point(data = sb_density %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
             aes(x = Seed_density_m2, y = y_base + y_offset_1,  colour = Realm_Biome 
             ),  size = 1.5, alpha = 0.2, shape= 16, position = position_jitter(width = 0, height=0.025)) +
  geom_vline(data = t_s, aes(xintercept = mean), size = 1.2, color = "#0c7156") +
  geom_point(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Degraded habitat") %>%
               group_by(Biome, Realm_Biome, y_base) %>% summarise(mean = mean(Estimate)), 
                      aes(x = mean, y = y_base), alpha = 0.9, color = "#C0C0C0", shape=17, size=5) +
  geom_point(data = fitted_values %>% filter(Group == "Terrestrial") %>% filter(Habitat_degraded == "Undisturbed habitat") %>%
               group_by(Biome, Realm_Biome, y_base) %>% summarise(mean = mean(Estimate)) ,
                      aes(x = mean, y = y_base + y_offset_1, fill = Realm_Biome, color = Realm_Biome), alpha = 0.9, shape=16, size=5) +
  # geom_vline(data = w_s, aes(xintercept = mean), size = 1.2, color = "#208cc0",linetype="dotted") +
  # geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  # geom_vline(data = ar_s, aes(xintercept = mean), size = 1.2, color ="#472c0b", alpha = 0.7, linetype="longdash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  #scale_y_discrete(limits=rev)+
  scale_color_manual( values= c("#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594"),
                      labels = c("Tundra", "Forest: Boreal", "Forest: Temperate", "Forest: Tropical & \nSubtropical",
                                 "Grasslands & Savannas: \nTemperate & Boreal", "Grasslands & Savannas: \nTropical & Subtropical", "Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"))+
  # # # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
   scale_fill_manual( values= c( "#fab255", "#da7901", "#b38711", "#d8b847", "#228B22", "#788f33", "#1e3d14", "#94b594"),
                      labels = c("Tundra", "Forest: Boreal", "Forest: Temperate", "Forest: Tropical & \nSubtropical",
                      "Grasslands & Savannas: \nTemperate & Boreal", "Grasslands & Savannas: \nTropical & Subtropical", "Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"))+
  labs(subtitle = "a) Terrestrial - Non-Arable", 
   # x = expression(paste('Seed density (',m^2,')')), 
    y = "Ecosystem") +
  # geom_text(data = sb_density %>% filter(Group == "Terrestrial") %>% distinct(Realm_Biome, y_base),
  #           aes( y = y_base + y_text_offset, label = Realm_Biome ), x =  8000, colour = "grey60", vjust = 0)+
  geom_text(data = sb_density %>% filter(Group == "Terrestrial") %>% distinct(Realm_Biome, y_base) %>%
              mutate(label = case_when(
                Realm_Biome == "Tundra" ~ "Tundra",
                Realm_Biome ==  "Boreal Forests/Taiga" ~ "Forests \nBoreal",
                Realm_Biome == "Temperate Forests" ~ "Forests \nTemperate",
                Realm_Biome == "Tropical & Subtropical Forests" ~ "Forests \nTropical & Subtropical",
                Realm_Biome == "Temperate & Boreal Grasslands, Savannas & Shrublands" ~ "Grasslands & Savannas \nTemperate & Boreal",
                Realm_Biome == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Grasslands & Savannas \nTropical & Subtropical",
                Realm_Biome == "Deserts & Xeric Shrublands" ~ "Mediterranean & Desert \nDeserts & Xeric Shrublands",
                Realm_Biome == "Mediterranean Forests, Woodlands & Scrub" ~ "Mediterranean & Desert \nForests, Woodlands & Scrub",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset, label = label), x = 8000, colour = "grey60", vjust = 0, size=6
  )+
  theme_bw(base_size = 18) +
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000, 5000,7000,6000,9000), lim= c(0,9000))+
  theme( plot.subtitle = element_text(face = "bold"), 
         axis.title.y = element_text(face = "bold"), 
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
   # panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

t_fig




w_fig <- ggplot() +
  geom_density_ridges(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "Degraded habitat")  , 
                      aes(x = Estimate, y = y_base,  color = Realm_Biome), alpha = 0.7, bandwidth =600, fill = "#C0C0C0") +
  geom_density_ridges(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "Undisturbed habitat")  , 
                      aes(x = Estimate, y = y_base + y_offset_1, fill = Realm_Biome, color = Realm_Biome), alpha = 0.5, bandwidth =600) +
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_90, xmax = upper_90 ), height = 0.15, linewidth = 0.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") ,
                  aes(y = y_base, xmin = lower_50, xmax = upper_50 ), height = 0.15, linewidth = 1.5, colour = "#C0C0C0" )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
                  aes(y = y_base + y_offset_1, xmin = lower_90, xmax = upper_90, color=Realm_Biome ), height = 0.15, linewidth = 0.5 )+
  geom_errorbarh( data = fit_groups_s %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
                  aes(y = y_base + y_offset_1, xmin = lower_50, xmax = upper_50 , color=Realm_Biome), height = 0.15, linewidth = 1.5 )+
  geom_point(data = sb_density %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Degraded habitat") ,
             aes(x = Seed_density_m2, y = y_base ,
             ), color = "#C0C0C0",  size = 1.5, alpha = 0.2, shape= 17, position = position_jitter(width = 0, height=0.025)) +
  geom_point(data = sb_density %>% filter(Group == "Wetlands") %>% filter(Habitat_degraded == "Undisturbed habitat") ,
             aes(x = Seed_density_m2, y = y_base + y_offset_1,  colour = Realm_Biome 
             ),  size = 1.5, alpha = 0.2, shape= 16, position = position_jitter(width = 0, height=0.025)) +
  
  geom_vline(data = w_s, aes(xintercept = mean), size = 1.2, color = "#208cc0",linetype="dotted") +
  geom_point(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "Degraded habitat") %>%
               group_by(Biome, Realm_Biome, y_base) %>% summarise(mean = mean(Estimate)), 
             aes(x = mean, y = y_base), alpha = 0.9, color = "#C0C0C0", shape=17, size=5) +
  geom_point(data = fitted_values %>% filter(Group == c("Wetlands")) %>% filter(Habitat_degraded == "Undisturbed habitat") %>%
               group_by(Biome, Realm_Biome, y_base) %>% summarise(mean = mean(Estimate)) ,
             aes(x = mean, y = y_base + y_offset_1, fill = Realm_Biome, color = Realm_Biome), alpha = 0.9, shape=16, size=5) +
 # geom_vline(data = aq_s, aes(xintercept = mean), size = 1.2, color = "#003967", alpha = 0.7,linetype="twodash") +
  # Add rectangles using the 'lower' and 'upper' bounds from 'aq_s'
  # geom_rect(data = aq_s, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
  #           alpha = 0.3, fill = "black") +
  #scale_y_discrete(limits=rev)+
  #xlim(0,15000)+
  scale_color_manual( values= c( "#293352", "#4E84C4", "#20B2AA"),
                      labels=c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  # scale_color_manual( values= c( "#20B2AA", "#4E84C4", "#293352", "#94b594",    "#94b594", "#fab255",  "#da7901", "#d8b847", "#b38711", "#1e3d14", "#788f33","#228B22", "#99610a" , "#E2C59F", "#AA3929" ))+
  scale_fill_manual( values= c(  "#293352", "#4E84C4", "#20B2AA"),
                     labels=c("Temperate & \nBoreal", "Mediterranean & \nDesert","Tropical & \nSubtropical"))+
  labs(subtitle = "c) Transitional", 
    #x = expression(paste('Seed density (',m^2,')')),
    x= "Seed density m-2",
       y = "Ecosystem") +
  # geom_text(data = sb_density %>% filter(Group == "Wetlands") %>% distinct(Realm_Biome, y_base),
  #           aes( y = y_base + y_text_offset, label = Realm_Biome ), x =  1000, colour = "grey60", vjust = 0)+
  geom_text(data = sb_density %>% filter(Group == "Wetlands") %>%distinct(Realm_Biome, y_base) %>%
              mutate(label = case_when(
                Realm_Biome == "Temperate & Boreal Wetlands" ~ "Wetlands \nTemperate & Boreal",
                Realm_Biome ==  "Mediterranean & Desert Wetlands" ~ "Wetlands \nMediterranean & Desert",
                Realm_Biome == "Tropical Wetlands" ~ "Wetlands \nTropical",
                TRUE ~ Realm_Biome
              )),
            aes(y = y_base + y_text_offset_w, label = label), x = 4000, colour = "grey60", vjust = 0, size=6
  )+
  scale_x_continuous(breaks=c(0,2500, 5000,7500,10000,12500, 15000), lim= c(0,15000))+
  theme_bw(base_size = 18) +
  theme( plot.subtitle = element_text(face = "bold"), 
         axis.title.y = element_text(face = "bold"), 
         axis.title.x = element_text(face = "bold"), 
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "cm"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_blank(),
    legend.position = "none"
  )

w_fig





legend_d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = sb_density %>% filter(Group == "Aquatic") ,
             aes(x = Biome, y = Seed_density_m2, 
                 group= Group , shape= Group,
             ), 
             size=5, alpha = 0.9,color="grey",
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = sb_density %>% filter(Group == "Terrestrial") ,
             aes(x = Biome, y = Seed_density_m2, 
                 group= Group , shape= Group,
             ), 
             size=5, alpha = 0.9,color="grey",
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  geom_point(data = sb_density %>% filter(Group == "Arable") ,
             aes(x = Biome, y = Seed_density_m2, 
                 group= Group , shape= Group,
             ), 
             size=5, alpha = 0.9,color="grey",
             position = position_jitterdodge(jitter.width = 0.75, jitter.height=0.45, dodge.width = 1)) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')), shape = "State",
       subtitle=  "f) Wetlands" ) +
  scale_shape_manual(labels = c("Undisturbed habitat","Degraded habitat", "Arable"), values = c(  16, 17, 15) ) +
  #scale_color_manual( values= c(  "#20B2AA"))+
  coord_cartesian( ylim = c(0,15000)) +
  scale_y_continuous(breaks=c(0,2500,5000,10000,15000,20000,25000))+
  theme_bw(base_size=18) + theme( plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 #axis.text.x=element_blank()
  ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10) )


legend_d



legend_line <- ggplot() + 
  # geom_hline(yintercept = 0,linetype="longdash") +
  geom_hline(data = group_means, aes(yintercept = mean, color = Group, linetype=Group, group=Group), size = 1.2, alpha = 0.7) +
  labs(x = '',
       y = expression(paste('Seed density (',m^2,')')),  
       subtitle=  "f) Wetlands" ) +
  scale_color_manual( name = "Realm Mean",labels = c("Terrestrial - \nNon-Arable", "Terrestrial - \nArable", "Wetlands & \nFlooded Grasslands", "Aquatic"),
                      values= c( "#0c7156","#472c0b",   "#208cc0","#003967") )+
  scale_linetype_manual(name="Realm Mean",labels = c("Terrestrial - \nNon-Arable", "Terrestrial - \nArable", "Wetlands & \nFlooded Grasslands", "Aquatic"),
                        values= c("solid", "longdash", "dotted", "twodash" ) )+
  #scale_color_manual( values= c(  "#20B2AA"))+
  theme_bw(base_size=18) + theme( plot.subtitle = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                                 plot.title = element_text(size=18, hjust=0.5),
                                 strip.background = element_blank(), legend.position="bottom",
                                 legend.key.width = unit(3,"cm")
                                 #axis.text.x=element_blank()
  ) 


legend_line


# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_d <- g_legend(legend_d)
legend_line <- g_legend(legend_line)



t_fig 
#LANDSCAPE 10X20

( ar_fig + w_fig + aq_fig)/(legend_d)/(legend_line) + plot_layout(heights = c(10, 1.5, 1.5))
#LANDSCAPE 10X20
