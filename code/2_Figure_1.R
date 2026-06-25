# ==============================================================================
# Seed bank database (sb_prep.csv) - study location maps (Figure 1)
#
# What this script does:
#   1. Loads the prepared seed bank dataset and builds a combined
#      Realm_Biome grouping variable used to colour/group points on the map.
#   2. Builds one small map panel per Realm (Tundra, Forest, Grassland,
#      Mediterranean & Desert, Arable, Wetland, Aquatic), each showing study
#      locations coloured by sub-biome and shaped by degradation status.
#   3. Extracts a standalone "State" (Undisturbed/Degraded/Arable) shape
#      legend from a throwaway plot, then assembles all panels plus that
#      legend into one composite figure with patchwork + cowplot.

# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LIBRARIES
# ------------------------------------------------------------------------------

library("googlesheets4")
library(tidyverse)
library("ggplot2")
library(patchwork)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(viridis)
library(MetBrewer)
library(hrbrthemes)
library(mapdata)
library(ggrepel)
library(sp)        # for converting to decimal degrees
library(maps)       # provides map_data("world"), the polygon set actually used below
library(mapproj)
library(ggmap)
library(rworldmap)
library(cowplot)    # used at the end to overlay the extracted legend on the panel grid

# ------------------------------------------------------------------------------
# 2. SETUP - working directory & load prepared data
# ------------------------------------------------------------------------------

setwd('~/Dropbox/GSB/')

sb <- read.csv('Data/sb_prep.csv')
nrow(sb)
head(sb)

theme_set(theme_bw())

# ------------------------------------------------------------------------------
# 3. QUICK MAP SANITY CHECK (not used downstream)
# ------------------------------------------------------------------------------


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf()

# ------------------------------------------------------------------------------
# 4. LOAD COUNTRY POLYGONS FOR PLOTTING
# ------------------------------------------------------------------------------


world <- map_data("world")

# ------------------------------------------------------------------------------
# 5. INSPECT REALM/BIOME CATEGORIES
# ------------------------------------------------------------------------------

sb %>% select(Realm, Biome) %>% distinct() %>% arrange(Realm, Biome)
sb %>% select(Realm) %>% distinct()
sb %>% select(Biome) %>% distinct()

# ------------------------------------------------------------------------------
# 6. DERIVE Realm_Biome GROUPING VARIABLE
# ------------------------------------------------------------------------------


sb <- sb %>%
  mutate(Realm_Biome = case_when(
    Realm == "Aquatic" ~ Realm,
    Realm == "Arable" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Arable",
    Realm == "Arable" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Arable",
    Realm == "Arable" & Biome == "Tropical" ~ "Tropical Arable",
    Realm == "Forest" & Biome == "Temperate" ~ "Temperate Forests",
    Realm == "Forest" & Biome == "Tropical" ~ "Tropical & Subtropical Forests",
    Realm == "Forest" & Biome == "Boreal" ~ "Boreal Forests/Taiga",
    Realm == "Grassland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Grasslands, Savannas & Shrublands",
    Realm == "Grassland" & Biome == "Tropical" ~ "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Deserts and Xeric Shrublands" ~ "Deserts & Xeric Shrublands",
    Realm == "Mediterranean and Desert" & Biome == "Mediterranean Forests, Woodlands and Scrub" ~ "Mediterranean Forests, Woodlands & Scrub",
    Realm == "Tundra" ~ Realm,
    Realm == "Wetland" & Biome == "Mediterranean and Desert" ~ "Mediterranean & Desert Wetlands",
    Realm == "Wetland" & Biome == "Temperate and Boreal" ~ "Temperate & Boreal Wetlands",
    Realm == "Wetland" & Biome == "Tropical" ~ "Tropical Wetlands",
  )) %>%
  mutate(Realm_Biome = fct_relevel(Realm_Biome,
    "Tundra", "Boreal Forests/Taiga", "Temperate Forests", "Tropical & Subtropical Forests",
    "Temperate & Boreal Grasslands, Savannas & Shrublands", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands",
    "Temperate & Boreal Arable", "Mediterranean & Desert Arable", "Tropical Arable",
    "Temperate & Boreal Wetlands", "Mediterranean & Desert Wetlands", "Tropical Wetlands",
    "Aquatic"
  )) %>%
  mutate(Habitat_degraded = as.factor(Habitat_degraded)) %>%
  mutate(Habitat_degraded = fct_relevel(Habitat_degraded, "0", "1"))

head(sb)
nrow(sb)
sb %>% select(Realm, Biome, Realm_Biome) %>% distinct()
sb %>% select(Realm_Biome) %>% distinct()

# ------------------------------------------------------------------------------
# 7. PER-REALM MAP PANELS (a-g)
# ------------------------------------------------------------------------------


head(sb)
summary(sb)

# 7a. Tundra
gsbm_tundra <- sb %>%
  filter(Realm == "Tundra") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, color = Realm_Biome, shape = Habitat_degraded), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#94b594")) +
  scale_shape_manual(values = c(16, 17), labels = c("Undisturbed", "Degraded"), guide = "none") +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "a", color = "", shape = "") +
  ggplot2::annotate("text", x = -150, y = -44, hjust = 0, size = 6, label = "Tundra", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_tundra

# 7b. Forest (Boreal / Temperate / Tropical & Subtropical)
gsbm_forest <- sb %>%
  filter(Realm == "Forest") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, shape = Habitat_degraded, color = Realm_Biome), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#1e3d14", "#788f33", "#228B22"),
                      labels = c("Boreal", "Temperate", "Tropical & Subtropical")) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Boreal", "Temperate", "Tropical & Subtropical"), guide = "none") +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "b", color = "Forests", shape = "Forests") +
  ggplot2::annotate("text", x = -150, y = -44, hjust = 0, size = 6, label = "Forests", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_forest

# 7c. Grassland (Temperate & Boreal / Tropical & Subtropical)
gsbm_grass <- sb %>%
  filter(Realm == "Grassland") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, shape = Habitat_degraded, color = Realm_Biome), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#d8b847", "#b38711"),
                      labels = c("Temperate & Boreal", "Tropical & Subtropical")) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Temperate & Boreal", "Tropical & Subtropical"), guide = "none") +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "c", color = "Grasslands & \nSavannas", shape = "Grasslands & \nSavannas") +
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = "Grasslands & \nSavannas", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_grass

# 7d. Mediterranean & Desert
gsbm_med <- sb %>%
  filter(Realm == "Mediterranean and Desert") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, shape = Habitat_degraded, color = Realm_Biome), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#da7901", "#fab255"),
                      labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands")) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Mediterranean Forests, \nWoodlands & Scrub", "Deserts & Xeric \nShrublands"), guide = "none") +
  coord_equal() +
  theme_void(base_size = 18) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 6, label = "Mediterranean & \nDesert", color = "black", alpha = 0.7) +
  labs(subtitle = "d", color = "Mediterranean \nand Desert", shape = "Mediterranean \nand Desert") +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_med

# 7e. Arable (Temperate & Boreal / Mediterranean & Desert / Tropical & Subtropical)
# Uses a single fixed point shape (15) rather than Habitat_degraded, since
# arable records aren't classified as degraded/undisturbed.
gsbm_ar <- sb %>%
  filter(Realm == "Arable") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, color = Realm_Biome), size = 3, alpha = 0.9, shape = 15) +
  scale_color_manual(values = c("#99610a", "#E2C59F", "#AA3929"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "e", color = "Arable", shape = "Arable") +
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = "Arable", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_ar

# 7f. Wetland (Temperate & Boreal / Mediterranean & Desert / Tropical & Subtropical)
gsbm_wet <- sb %>%
  filter(Realm == "Wetland") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, shape = Habitat_degraded, color = Realm_Biome), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#20B2AA", "#4E84C4", "#293352"),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical")) +
  scale_shape_manual(values = c(16, 17),
                      labels = c("Temperate & \nBoreal", "Mediterranean & \nDesert", "Tropical & \nSubtropical"), guide = "none") +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "f", color = "Wetlands", shape = "Wetlands") +
  ggplot2::annotate("text", x = -190, y = -44, hjust = 0, size = 6, label = "Wetlands & \nFlooded Grasslands", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_wet

# 7g. Aquatic
gsbm_aq <- sb %>%
  filter(Realm == "Aquatic") %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(aes(x = Lon_deg, y = Lat_deg, shape = Habitat_degraded, color = Realm_Biome), size = 3, alpha = 0.9) +
  scale_color_manual(values = c("#447fdd")) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  coord_equal() +
  theme_void(base_size = 20) +
  theme(plot.subtitle = element_text(face = "bold"), legend.position = 'bottom',
        legend.direction = "horizontal", legend.title = element_blank()) +
  labs(subtitle = "g", color = "Aquatic", shape = "Aquatic") +
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 6, label = "Aquatic", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_aq

# ------------------------------------------------------------------------------
# 8. LEGEND-EXTRACTION HACK PLOT
# ------------------------------------------------------------------------------


sb %>% distinct(Realm)

gsbm_legend_dat <- sb %>% filter(Realm %in% c("Aquatic", "Wetland", "Tundra"))


gsbm_legend_fig <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.7) +
  geom_point(data = gsbm_legend_dat %>% filter(Realm == "Aquatic"),
             aes(x = Lon_deg, y = Lat_deg, shape = Realm, color = Realm), size = 5, color = "black") +
  geom_point(data = gsbm_legend_dat %>% filter(Realm == "Tundra"),
             aes(x = Lon_deg, y = Lat_deg, shape = Realm, color = Realm), size = 5, color = "grey") +
  geom_point(data = gsbm_legend_dat %>% filter(Realm == "Wetland"),
             aes(x = Lon_deg, y = Lat_deg, shape = Realm, color = Realm), size = 5, color = "black") +
  scale_color_manual(values = c("#447fdd"), guide = "none") +
  scale_shape_manual(name = "State", values = c(16, 17, 15),
                      labels = c("Undisturbed", "Degraded", "Arable")) +
  coord_equal() +
  theme_void(base_size = 18) +
  theme(plot.subtitle = element_text(face = "bold")) +
  labs(subtitle = "g)", color = "Aquatic", shape = "State") +
  ggplot2::annotate("text", x = -160, y = -44, hjust = 0, size = 5, label = "Aquatic", color = "black", alpha = 0.7) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  guides(col = guide_legend(ncol = 3))
gsbm_legend_fig

# Pull just the legend grob out of a ggplot object.
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
gsbm_legend <- g_legend(gsbm_legend_fig)

# ------------------------------------------------------------------------------
# 9. ASSEMBLE FINAL COMPOSITE FIGURE
# ------------------------------------------------------------------------------

top_row <- (gsbm_tundra) + (gsbm_forest) + (gsbm_grass)
middle_row <- (gsbm_med) + (gsbm_ar)
bottom_row <- (gsbm_wet) + (gsbm_aq)

p_maps <- (top_row) / (middle_row) / (bottom_row)
p_maps

Figure_1 <- ggdraw(p_maps) +
  draw_grob(
    gsbm_legend,
    x = 0.82,    # push right of arable
    y = 0.40,    # center of middle row
    width  = 0.16,
    height = 0.20
  )
Figure_1 


#ggsave("Figures/Fig_1.png", plot = Fig_1, width = 20, height = 18, units = "in", dpi = 300)


ggsave(
  "Figures/Fig_1.pdf",
  plot = Figure_1,
  width = 533,
  height = 305,
  units = "mm",
  device = cairo_pdf
)
