### Make map figure with alpha, gamma and beta diversity

# Libraries
library(rgdal)
library(rgeos)

# Add biomes, rename biomes according to broad cats used.
biomes<-readOGR("GIS", "tnc_terr_ecoregions", stringsAsFactors = FALSE)
biomes$broad<-biomes$WWF_MHTNAM
biomes$broad[biomes$broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
biomes$broad[biomes$broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"

# Bring in model estimates and attach to biome data frame
div.vals<-read.csv("/Users/arau0001/Dropbox/GSB/Data/sb_av_div_estimates.csv")
dens.vals.pred<-read.csv("/Users/arau0001/Dropbox/GSB/Data/predicted_density.csv")
dens.vals.tab3<-read.csv("/Users/arau0001/Dropbox/GSB/Tables/table_3.csv")

biomes$alpha<-div.vals$a_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$beta<-div.vals$b_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$gamma<-div.vals$g_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$dens.pred<-dens.vals.pred$d_Estimate[match(biomes$broad,dens.vals.pred$Biome_Broad_Hab)]
biomes$dens.tab3<-dens.vals.tab3$Estimate[match(biomes$broad,dens.vals.tab3$WWF.Biome)]

# Inspect values to find good sequence of 20 values for colour pallette
fivenum(div.vals$a_Estimate[!div.vals$Biome_Broad_Hab %in% c("Aquatic", "Arable")]) # 6-20
length(seq(1,20,1))
fivenum(div.vals$g_Estimate[!div.vals$Biome_Broad_Hab %in% c("Aquatic", "Arable")]) #25-60
length(seq(24,63,2))
fivenum(div.vals$b_Estimate[!div.vals$Biome_Broad_Hab %in% c("Aquatic", "Arable")]) # 2.7-6
length(seq(2,5.8,0.2))
fivenum(div.vals$a_Estimate[!div.vals$Biome_Broad_Hab %in% c("Aquatic", "Arable")]) # 6-20
length(seq(1,20,1))
fivenum(dens.vals.pred$d_Estimate[!dens.vals.pred$Biome_Broad_Hab %in% c("Aquatic", "Arable")]) #25-60
length(seq(250,5000,250))
fivenum(dens.vals.tab3$Estimate[!dens.vals.tab3$WWF.Biome %in% c("Aquatic", "Arable")]) # 2.7-6
length(seq(250,5000,250))



# Assign those sequences and colours
aseq<-seq(1,20,1)
gseq<-seq(24,63,2)
bseq<-seq(2,5.8,0.2)
denseq<-seq(250,5000,250)
biomes$cola[!is.na(biomes$alpha)]<-vir(20)[unlist(sapply(biomes$alpha, function(x) which.min(abs(aseq-x))))]
biomes$colb[!is.na(biomes$beta)]<-vir(20)[unlist(sapply(biomes$beta, function(x) which.min(abs(bseq-x))))]
biomes$colg[!is.na(biomes$gamma)]<-vir(20)[unlist(sapply(biomes$gamma, function(x) which.min(abs(gseq-x))))]
biomes$coldenspred[!is.na(biomes$dens.pred)]<-vir(20)[unlist(sapply(biomes$dens.pred, function(x) which.min(abs(denseq-x))))]
biomes$coldenstab3[!is.na(biomes$dens.tab3)]<-vir(20)[unlist(sapply(biomes$dens.tab3, function(x) which.min(abs(denseq-x))))]

# Make the plot - diversity
leg.pos<-c(-170,50)
leg.nums<-rep(NA,20)
pdf("/Users/arau0001/Dropbox/GSB/Figs/abg_map.pdf",width=6, height=10,useDingbats=F)
par(mfrow=c(3,1))
plot(biomes, main="Alpha (0.01m²)",col=biomes$cola, border=FALSE)
leg.nums[c(1,5,10,15,20)]<-rev(aseq[c(1,5,10,15,20)])
legend(leg.pos[1],leg.pos[2],leg.nums,fill=rev(vir(20)),border = NA, y.intersp = 0.5, cex = 1, text.font = 2, bty="n")

plot(biomes, main="Gamma (15m²)",col=biomes$colg, border=FALSE)
leg.nums[c(1,5,10,15,20)]<-rev(gseq[c(1,5,10,15,20)])
legend(leg.pos[1],leg.pos[2],leg.nums,fill=rev(vir(20)),border = NA, y.intersp = 0.5, cex = 1, text.font = 2, bty="n")

plot(biomes, main="Beta (Gamma/Alpha)",col=biomes$colb, border=FALSE)
leg.nums[c(1,5,10,15,20)]<-rev(bseq[c(1,5,10,15,20)])
legend(leg.pos[1],leg.pos[2],leg.nums,fill=rev(vir(20)),border = NA, y.intersp = 0.5, cex = 1, text.font = 2, bty="n")

dev.off()


# Make the plot - density
leg.pos<-c(-170,50)
leg.nums<-rep(NA,20)
pdf("/Users/arau0001/Dropbox/GSB/Figs/dens_map.pdf",width=6, height=8,useDingbats=F)
par(mfrow=c(2,1))
plot(biomes, main="Density (predicted_density)",col=biomes$coldenspred, border=FALSE)
leg.nums[c(1,5,10,15,20)]<-rev(denseq[c(1,5,10,15,20)])
legend(leg.pos[1],leg.pos[2],leg.nums,fill=rev(vir(20)),border = NA, y.intersp = 0.5, cex = 1, text.font = 2, bty="n")

plot(biomes, main="Density (Table 3)",col=biomes$coldenstab3, border=FALSE)
leg.nums[c(1,5,10,15,20)]<-rev(denseq[c(1,5,10,15,20)])
legend(leg.pos[1],leg.pos[2],leg.nums,fill=rev(vir(20)),border = NA, y.intersp = 0.5, cex = 1, text.font = 2, bty="n")


dev.off()




# After plotting look at how biomes swap places depending on alpha or gamma

head(div.vals)
div.grid<-expand.grid(div.vals$Biome_Broad_Hab,div.vals$Biome_Broad_Hab)
div.grid<-div.grid[!div.grid$Var1==div.grid$Var2,]
div.grid$a1<-div.vals$a_Estimate[match(div.grid$Var1,div.vals$Biome_Broad_Hab)]
div.grid$a2<-div.vals$a_Estimate[match(div.grid$Var2,div.vals$Biome_Broad_Hab)]
div.grid$g1<-div.vals$g_Estimate[match(div.grid$Var1,div.vals$Biome_Broad_Hab)]
div.grid$g2<-div.vals$g_Estimate[match(div.grid$Var2,div.vals$Biome_Broad_Hab)]
div.grid$adiff<-div.grid$a1-div.grid$a2
div.grid$gdiff<-div.grid$g1-div.grid$g2
sum(!sign(div.grid$adiff) == sign(div.grid$gdiff)) / nrow(div.grid) # 30.3% combinations swapped places

div.grid.naterra<-div.grid[!div.grid$Var1 %in% c("Arable", "Aquatic") & !div.grid$Var2 %in% c("Arable", "Aquatic"),]
sum(!sign(div.grid.naterra$adiff) == sign(div.grid.naterra$gdiff)) / nrow(div.grid.naterra) # 24.4% combinations swapped places





# #Failed sf attempt
# 
# library(sf)
# 
# biomes<-st_read("GIS", "tnc_terr_ecoregions")
# #plot(biomes$geometry)
# biomes$broad<-biomes$WWF_MHTNAM
# biomes$broad[biomes$broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
# biomes$broad[biomes$broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"
# 
# div.vals<-read.csv("/Users/arau0001/Dropbox/GSB/Data/sb_av_div_estimates.csv")
# biomes$alpha<-div.vals$a_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
# biomes$beta<-div.vals$b_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
# biomes$gamma<-div.vals$g_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
# 
# africa<-biomes[biomes$WWF_REALM2=="Afrotropic",]
# #plot(africa$geometry)
# pdf("/Users/arau0001/Dropbox/Global seed banks/map.pdf",width=60, height=10,useDingbats=F)
# par(mfrow=c(1,3))
# plot(biomes["alpha"], breaks="quantile", main="Alpha (0.01m2)", border=0, key.pos=NULL, reset=FALSE)
# plot(biomes["gamma"], breaks="quantile", main="Gamma (15m2)", border=0, key.pos=NULL, reset=FALSE)
# plot(biomes["beta"], breaks="quantile", main="Beta (Gamma/Alpha)", border=0,key.pos=NULL, reset=FALSE)
# dev.off()
# 
# pug <- colorRampPalette(c('purple','green'))
# biomes$cola<-pug(20)[as.numeric(cut(biomes$alpha, 20))]
# biomes$colb<-pug(20)[as.numeric(cut(biomes$beta, 20))]
# biomes$colg<-pug(20)[as.numeric(cut(biomes$gamma, 20))]
# 
