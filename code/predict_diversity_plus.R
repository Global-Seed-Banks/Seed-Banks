rm(list = ls())


#packages
library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(patchwork)
library(gridExtra)
library(grid)
library(viridis)

user <- Sys.info()["user"]

path2wd <- switch(user,
                  "el50nico" = "~/GRP GAZP Dropbox/Emma Ladouceur/GSB/",
                  # " " = " " # Petr puts his computer username and file path here
)


setwd(path2wd)

sb_prep <- read.csv(paste0(path2wd, 'Data/sb_prep.csv'))

nrow(sb_prep)

sb_rich_area <- sb_prep %>% filter(!is.na(Total_Species),
                                   !is.na(Total_Sample_Area_mm2),
                                   Number_Sites == 1 
) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))

head(sb_rich_area)

setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')



summary(sb_biome_area)

# sb_biome_area_prep <- sb_biome_area %>% distinct(Total_Sample_Area_mm2, Total_Sample_Area_m2, Centred_log_Total_Sample_Area_m2) %>%
#   mutate(Total_Sample_Area_mm2 = round(Total_Sample_Area_mm2, 2),
#          Total_Sample_Area_m2 = round(Total_Sample_Area_m2, 6),
#          Centred_log_Total_Sample_Area_m2 = round(Centred_log_Total_Sample_Area_m2, 6),
#          ) %>% arrange(Total_Sample_Area_m2) #
# 
# summary(sb_biome_area_prep)
# View(sb_biome_area_prep %>% distinct(Total_Sample_Area_m2) %>%
#        filter( Total_Sample_Area_m2 >=8.210905 &  Total_Sample_Area_m2 <16) )
# 
# sb_biome_area_prep %>%
#   summarise(Total_Sample_Area_m2 = seq(0.010000, 12.311357, length.out = 10 ) ) 
#               
# sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 0.010000)
# sb_biome_area_prep %>% filter( Total_Sample_Area_m2 == 15.000000)

rich_biome_predict <- tidyr::crossing( 
  Number_Sites = c(1, 20, 100),
  sb_rich_area %>% group_by(Biome_Broad_Hab) %>%  
    summarise(Total_Sample_Area_m2 = c( seq( 0.010000, 15.000000, length.out = 2) ) ), 
  )  %>%
  mutate( log_Number_Sites = log(Number_Sites),
          log_Total_Sample_Area_m2 = log(Total_Sample_Area_m2),
          Centred_log_Number_Sites = log_Number_Sites - mean(log_Number_Sites, na.rm = TRUE),
          Centred_log_Total_Sample_Area_m2 = log_Total_Sample_Area_m2 - mean(log_Total_Sample_Area_m2, na.rm = TRUE) ) %>%
  select(-c( log_Number_Sites, log_Total_Sample_Area_m2 ) ) %>%
  arrange( Total_Sample_Area_m2, Number_Sites ) %>%
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab ) %>%
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, Centred_log_Number_Sites, Number_Sites)) %>%
  mutate(predicted = map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2 + Centred_log_Number_Sites) ))) 


# rich_biome_predict <- sb_biome_area %>% 
#   mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
#   group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
#   summarise(Total_Sample_Area_m2 = seq(0.010000, 15.000000, length.out = 10 ),
#             #Total_Sample_Area_mm2 = seq(1500, 15.000000, length.out = 10),
#             Centred_log_Total_Sample_Area_m2 =  seq(-3.554035, 3.759185, length.out = 10) ) %>%
#   group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
#   nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, #Total_Sample_Area_mm2
#                 )) %>%
#   mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 


head(rich_biome_predict)

rich_biome_predict_df <- rich_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
 mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup()

View(rich_biome_predict_df)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_predict_df,  "rich_biome_predict_df.csv")

rich_biome_predict_df <- read.csv(paste0(path2wd, 'Data/rich_biome_predict_df.csv'))



head(rich_biome_predict_df)
colnames(rich_biome_predict_df)
View(rich_biome_predict_df %>% distinct(Total_Sample_Area_m2))

# 0.010000
# 15.000000

nrow(rich_biome_predict_df)

rich_biome_a <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter( Total_Sample_Area_m2 == 0.010000   ) %>% # Yang et al
  group_by(Biome_Broad_Hab, Number_Sites) %>%
   filter(
     predicted > quantile(predicted, probs=0.025),
     predicted < quantile(predicted, probs=0.975),
  ) %>% sample_n(1000)  %>%
  mutate(a_samp_scale = Total_Sample_Area_m2,
         a_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted, X ))

nrow(rich_biome_a)
head(rich_biome_a)

rich_biome_g <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter(  Total_Sample_Area_m2 ==  15.000000   ) %>% # arbitrary gamma scale
  group_by(Biome_Broad_Hab, Number_Sites) %>%
  filter(
    predicted > quantile(predicted, probs=0.025),
    predicted < quantile(predicted, probs=0.975),
  ) %>% sample_n(1000)  %>%
  mutate(g_samp_scale = Total_Sample_Area_m2,
         g_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted, X))

head(rich_biome_g)

rich_biome_scales <- rich_biome_a %>% left_join(rich_biome_g) %>%
  mutate(b_predicted = (g_predicted/a_predicted))
  
head(rich_biome_scales)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_scales,  "sb_av_div_scales.csv")


rich_biome_scales <- read.csv(paste0(path2wd, 'Data/sb_av_div_scales.csv'))

head(rich_biome_scales)
View(rich_biome_scales %>% filter(Biome_Broad_Hab ==  "Boreal Forests/Taiga"))


rich_biome_div <- rich_biome_scales %>%
  group_by(Biome_Broad_Hab, Number_Sites) %>%
  mutate( a_Estimate = mean(a_predicted, na.rm =TRUE ),
          `a_Upper CI` = quantile(a_predicted, probs=0.975, na.rm =TRUE ),
          `a_Lower CI` = quantile(a_predicted, probs=0.025, na.rm =TRUE ),
           g_Estimate = mean(g_predicted, na.rm =TRUE ),
          `g_Upper CI` = quantile(g_predicted, probs=0.975, na.rm =TRUE ),
          `g_Lower CI` = quantile(g_predicted, probs=0.025, na.rm =TRUE ),
           b_Estimate = mean(b_predicted, na.rm =TRUE ),
          `b_Upper CI` = quantile(b_predicted, probs=0.975, na.rm =TRUE ),
          `b_Lower CI` = quantile(b_predicted, probs=0.025, na.rm =TRUE ),
  ) %>% 
  select(-c(X, a_predicted, g_predicted, b_predicted)) %>% distinct() %>% ungroup()


head(rich_biome_div)
nrow(rich_biome_div)
View(rich_biome_div)

setwd(paste0(path2wd, 'Data/'))
write.csv(rich_biome_div,  "sb_av_div_estimates.csv")

rich_biome_div <- read.csv(paste0(path2wd, 'Data/sb_av_div_estimates.csv'))

vir<-colorRampPalette(plasma(36))

aseq<-seq(1,20,1)
gseq<-seq(24,80,2)
bseq<-seq(2,5.8,0.2)


#rich_biome_div_1 <- rich_biome_div %>% filter(Number_Sites == 1)

cola <- rich_biome_div$cola[!is.na(rich_biome_div$a_Estimate)]<-vir(36)[unlist(sapply(rich_biome_div$a_Estimate, function(x) which.min(abs(aseq-x))))]
colb<-rich_biome_div$colb[!is.na(rich_biome_div$b_Estimate)]<-vir(36)[unlist(sapply(rich_biome_div$b_Estimate, function(x) which.min(abs(bseq-x))))]
colg<- rich_biome_div$colg[!is.na(rich_biome_div$g_Estimate)]<-vir(36)[unlist(sapply(rich_biome_div$g_Estimate, function(x) which.min(abs(gseq-x))))]

# cols <- rich_biome_div_1 %>% select(Biome_Broad_Hab, cola, colb, colg)
# 
# cols
# 
 rich_biome_div <- rich_biome_div %>% #left_join(cols) %>% 
  mutate(Number_Sites = factor(Number_Sites)) %>%
  mutate(Number_Sites = fct_relevel(Number_Sites, c("1","20","100")))
#
View(rich_biome_div)

rich_biome_a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = a_Estimate, colour = cola, group= Number_Sites, shape= Number_Sites ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, colour = cola, group= Number_Sites ),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = cola) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
   coord_cartesian( ylim = c(0,80)) +
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle((expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(alpha), '-richness ',sep = '')))) +
   guides(col = guide_legend(ncol = 3))


rich_biome_a


rich_biome_g <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = g_Estimate, colour = Biome_Broad_Hab, group= Number_Sites, shape= Number_Sites ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`, colour = Biome_Broad_Hab, group = Number_Sites),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = colg) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(), 
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  coord_cartesian( ylim = c(0,80)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) +
   guides(col = guide_legend(ncol = 3))


rich_biome_g



rich_biome_b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div %>% filter(Number_Sites == 1),
             aes(x = Biome_Broad_Hab , y = b_Estimate, colour = Biome_Broad_Hab, group= Number_Sites, shape= Number_Sites ), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div  %>% filter(Number_Sites == 1),
                aes(x = Biome_Broad_Hab , ymin = `b_Lower.CI`, ymax =  `b_Upper.CI`, colour = Biome_Broad_Hab, group= Number_Sites),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = colb) +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(), legend.position="none",
                               legend.title = element_blank() ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
   coord_cartesian( ylim = c(0,15)) +
  ggtitle((expression(paste(italic(beta), '-diversity (', italic(gamma/alpha), ')', sep = '')))) + 
  ylab((expression(paste('Average ', italic(beta), '-Diversity ',sep = '')))) +  labs(x=''
  ) + guides(col = guide_legend(ncol = 3)) + labs( #subtitle= 'c)'
    ) 


rich_biome_b


rich_legend <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = a_Estimate, group= Number_Sites, shape= Number_Sites ), 
             position = position_dodge(width = 0.75), size = 3, color="black") +
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") + 
  scale_shape(solid = FALSE)+
  guides(shape=guide_legend(title="Number of sites"))

rich_legend

# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# fixed effect for controls
rich_legend_o <- g_legend(rich_legend)


# landscape 10 x 16
(rich_biome_a )/ ( rich_biome_g) / (rich_biome_b)  / (rich_legend_o) + plot_layout(heights = c(10, 10, 10, 0.5))

(rich_biome_a )/ ( rich_biome_g)  / (rich_legend_o) + plot_layout(heights = c(10, 10,  0.5))

#(rich_biome_a)/ (rich_biome_g) / (rich_biome_b)
