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

# remove NA values 
sb_biome_area <- sb_prep %>% # filter(!is.na(Total_Species),
  #    !is.na(Total_Sample_Area_mm2)) %>%
  # treat all random effects as factors
  mutate( Habitat_Degraded = as.factor(Habitat_Degraded),
          Biome_Broad_Hab = as.factor(Biome_Broad_Hab),
          Habitat_Broad = as.factor(Habitat_Broad),
          studyID = as.factor(studyID),
          rowID = as.factor(rowID))



setwd(paste0(path2wd, 'Model_Fits/'))
# models run on cluster, load in model objects here
load( 'rich_m2.Rdata')
load( 'seed_m2.Rdata')



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


rich_biome_predict <- sb_biome_area %>% 
  mutate(Biome_Broad_Hab_group = Biome_Broad_Hab) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  summarise(Total_Sample_Area_m2 = seq(0.010000, 15.000000, length.out = 10 ),
            #Total_Sample_Area_mm2 = seq(1500, 15.000000, length.out = 10),
            Centred_log_Total_Sample_Area_m2 =  seq(-3.554035, 3.759185, length.out = 10) ) %>%
  group_by(Biome_Broad_Hab_group, Biome_Broad_Hab) %>% 
  nest(data = c(Biome_Broad_Hab, Centred_log_Total_Sample_Area_m2, Total_Sample_Area_m2, #Total_Sample_Area_mm2
                )) %>%
  mutate(predicted = purrr::map(data, ~predicted_draws(rich_m2, newdata= .x, re_formula = ~(Biome_Broad_Hab * Centred_log_Total_Sample_Area_m2) ))) 


head(rich_biome_predict)

rich_biome_predict_df <- rich_biome_predict  %>% 
  select(-data) %>% unnest(cols= c(predicted)) %>%
 mutate( predicted = .prediction) %>%
  select(-.prediction) %>% ungroup()

View(head(rich_biome_predict_df))

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
  group_by(Biome_Broad_Hab) %>%
   filter(
     predicted > quantile(predicted, probs=0.025),
     predicted < quantile(predicted, probs=0.975),
  ) %>% sample_n(1000)  %>%
  mutate(a_samp_scale = Total_Sample_Area_m2,
         a_predicted = predicted) %>%
  select(-c(Total_Sample_Area_m2,predicted, X ))

nrow(rich_biome_a)

rich_biome_g <- rich_biome_predict_df %>%
  select(-c(.draw, .row, .chain, .iteration, Centred_log_Total_Sample_Area_m2, Biome_Broad_Hab_group)) %>%
  filter(  Total_Sample_Area_m2 ==  15.000000   ) %>% # arbitrary gamma scale
  group_by(Biome_Broad_Hab) %>%
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
  group_by(Biome_Broad_Hab) %>%
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

head(rich_biome_div)

rich_biome_div %>% gather(parameter, value ,a_samp_scale:b_Lower.CI)



predicted_points<- rich_biome_scales %>%  
  group_by(Biome_Broad_Hab) %>%
#   filter(a_predicted > quantile(a_predicted, probs=0.025),
#                                a_predicted < quantile(a_predicted, probs=0.975),
#                                g_predicted > quantile(g_predicted, probs=0.025),
#                                g_predicted < quantile(g_predicted, probs=0.975),
#                                b_predicted > quantile(a_predicted, probs=0.025),
#                                b_predicted < quantile(b_predicted, probs=0.975),
#                                # take 50 samples- this will be uncertainty represented in figure 4
# ) %>% 
  sample_n(200)  %>% select(-X)


View(predicted_points)

setwd(paste0(path2wd, 'Data/'))
write.csv(predicted_points,  "predicted_samps.csv")

predicted_points <- read.csv(paste0(path2wd, 'Data/predicted_samps.csv'))

head(rich_biome_div)

vir<-colorRampPalette(plasma(22))

aseq<-seq(1,20,1)
gseq<-seq(24,63,2)
bseq<-seq(2,5.8,0.2)
denseq<-seq(250,5000,250)
cola <- rich_biome_div$cola[!is.na(rich_biome_div$a_Estimate)]<-vir(22)[unlist(sapply(rich_biome_div$a_Estimate, function(x) which.min(abs(aseq-x))))]
colb<-rich_biome_div$colb[!is.na(rich_biome_div$b_Estimate)]<-vir(22)[unlist(sapply(rich_biome_div$b_Estimate, function(x) which.min(abs(bseq-x))))]
colg<- rich_biome_div$colg[!is.na(rich_biome_div$g_Estimate)]<-vir(22)[unlist(sapply(rich_biome_div$g_Estimate, function(x) which.min(abs(gseq-x))))]
# biomes$coldenspred[!is.na(biomes$dens.pred)]<-vir(20)[unlist(sapply(biomes$dens.pred, function(x) which.min(abs(denseq-x))))]
# biomes$coldenstab3[!is.na(biomes$dens.tab3)]<-vir(20)[unlist(sapply(biomes$dens.tab3, function(x) which.min(abs(denseq-x))))]

rich_biome_a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # geom_point(data = predicted_points,
  #            aes(x = Biome_Broad_Hab , y = a_predicted, colour = Biome_Broad_Hab),
  #            #position = position_dodge(width = 0.75),
  #            position = position_jitter(width = 0.3, height=0.5), size = 1, alpha = 0.2) +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = a_Estimate, colour = Biome_Broad_Hab), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `a_Lower.CI`, ymax =  `a_Upper.CI`, colour = Biome_Broad_Hab),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = cola) +
  #scale_color_viridis(discrete = F, option="D")  +
  # scale_shape_manual(name = "Average total species",
  #                    values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
   coord_cartesian( ylim = c(0,80)) +
  ggtitle((expression(paste(italic(alpha), '-scale (0.01' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(alpha), '-richness ',sep = '')))) 


rich_biome_a


rich_biome_g <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  #facet_wrap(.~scale) +
  # geom_point(data = predicted_points,
  #            aes(x = Biome_Broad_Hab , y = g_predicted, colour = Biome_Broad_Hab),
  #            #position = position_dodge(width = 0.75),
  #            position = position_jitter(width = 0.3, height=0.35), size = 1, alpha = 0.2) +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = g_Estimate, colour = Biome_Broad_Hab), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `g_Lower.CI`, ymax =  `g_Upper.CI`, colour = Biome_Broad_Hab),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = colg) +
  #scale_color_viridis(discrete = T, option="D")  +
  # scale_shape_manual(name = "Average total species",
  #                    values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               axis.text.x=element_blank(), axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") + 
 # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  coord_cartesian( ylim = c(0,80)) +
  ggtitle((expression(paste(italic(gamma), '-scale (15' ,m^2,')', sep = ''))))+
  ylab((expression(paste('Average ', italic(gamma), '-richness ',sep = '')))) 

#(g/' ,m^2, '/year)'

rich_biome_g



rich_biome_b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  #facet_wrap(.~scale) +
  # geom_point(data = predicted_points,
  #            aes(x = Biome_Broad_Hab , y = b_predicted, colour = Biome_Broad_Hab),
  #            #position = position_dodge(width = 0.75),
  #            position = position_jitter(width = 0.3, height=0.5), size = 1, alpha = 0.2) +
  geom_point(data = rich_biome_div,
             aes(x = Biome_Broad_Hab , y = b_Estimate, colour = Biome_Broad_Hab), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = rich_biome_div,
                aes(x = Biome_Broad_Hab , ymin = `b_Lower.CI`, ymax =  `b_Upper.CI`, colour = Biome_Broad_Hab),
                position = position_dodge(width = 0.75),
                size = 0.75, width = 0) +
  scale_color_manual(values = colb) +
 # scale_color_viridis(discrete = T, option="D")  +
  # scale_shape_manual(name = "Average total species",
  #                    values = c(16, 17), labels = c("Min area", "Max area") )+ 
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               #axis.text.x=element_blank(),
                               axis.title.x = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(), legend.position="none",
                               legend.title = element_blank() ) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
   coord_cartesian( ylim = c(0,15)) +
  ggtitle((expression(paste(italic(beta), '-Diversity (', italic(gamma/alpha), ')', sep = '')))) + 
  ylab((expression(paste('Average ', italic(beta), '-Diversity ',sep = '')))) +  labs(x=''
  ) + guides(col = guide_legend(ncol = 3)) + labs( #subtitle= 'c)'
    ) 



rich_biome_b
#landscape 10 x 16
(rich_biome_a + rich_biome_g) / (rich_biome_b)


(rich_biome_a)/ (rich_biome_g) / (rich_biome_b)
