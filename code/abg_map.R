
setwd("/Users/arau0001/Dropbox/Global seed banks/Seed-Bank-Map")
library(rgdal)
library(rgeos)

biomes<-readOGR("GIS", "tnc_terr_ecoregions", stringsAsFactors = FALSE)
biomes$broad<-biomes$WWF_MHTNAM
biomes$broad[biomes$broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
biomes$broad[biomes$broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"

div.vals<-read.csv("/Users/arau0001/Dropbox/GSB/Data/sb_av_div_estimates.csv")
biomes$alpha<-div.vals$a_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$beta<-div.vals$b_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$gamma<-div.vals$g_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]

# alpha 1-20
# gamma 25-65
# beta 2-7 (0.25)

# length(seq(1,20,1))
# length(seq(26,65,2))
# length(seq(3,6.8,0.2))

# vir(20)



# bob<-biomes$alpha
# min(abs(aseq-bob))

# which.min(abs(aseq-bob[1]))


# which.min(abs(x - your.number))

# # 
# library(viridis)
# vir<-colorRampPalette(plasma(20))
# biomes$cola<-vir(20)[as.numeric(cut(biomes$alpha, 20))]
# biomes$colb<-vir(20)[as.numeric(cut(biomes$beta, 20))]
# biomes$colg<-vir(20)[as.numeric(cut(biomes$gamma, 20))]


library(viridis)
vir<-colorRampPalette(plasma(20))

aseq<-seq(1,20,1)
gseq<-seq(26,65,2)
bseq<-seq(3,6.8,0.2)
biomes$cola[!is.na(biomes$alpha)]<-vir(20)[unlist(sapply(biomes$alpha, function(x) which.min(abs(aseq-x))))]
biomes$colb[!is.na(biomes$beta)]<-vir(20)[unlist(sapply(biomes$beta, function(x) which.min(abs(bseq-x))))]
biomes$colg[!is.na(biomes$gamma)]<-vir(20)[unlist(sapply(biomes$gamma, function(x) which.min(abs(gseq-x))))]

# biomes$cola<-vir(20)[as.numeric(cut(log10(biomes$alpha), 20))]
# biomes$colb<-vir(20)[as.numeric(cut(log10(biomes$beta), 20))]
# biomes$colg<-vir(20)[as.numeric(cut(log10(biomes$gamma), 20))]

x11()
leg.pos<-c(-170,50)
leg.nums<-rep(NA,20)
pdf("/Users/arau0001/Dropbox/Global seed banks/map.pdf",width=6, height=10,useDingbats=F)
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

plot.new()
nun<-length(unique(biomes$gamma))
lgd = rep(NA, nun)
lgd[c(1,round(nun/2),nun)] = c(1,round(nun/2),nun)
legend(x = 0.5, y = 0.5,
        legend = lgd,
        fill = vir(20),
        border = NA,
        y.intersp = 0.5,
        cex = 2, text.font = 2)





bob<-rep(NA, 20)
bob[c(1,5,10,15,20)]<-aseq[c(1,5,10,15,20)]
legend(x = 0.5, y = 1,bob,fill=vir(20),border = NA, y.intersp = 0.5, cex = 2, text.font = 2, bty="n")


nun<-length(10)
lgd = sort
lgd[c(1,round(nun/2),nun)] = c(1,round(nun/2),nun)
legend(x = 0.5, y = 0.5,
        legend = sort(unique(biomes$gamma)),
        fill = vir(length(unique(biomes$gamma))),
        border = NA,
        y.intersp = 0.5,
        cex = 2, text.font = 2)


sort(unique(biomes$gamma))
unique(biomes$colg[order(biomes$gamma)])





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







library(sf)

biomes<-st_read("GIS", "tnc_terr_ecoregions")
#plot(biomes$geometry)
biomes$broad<-biomes$WWF_MHTNAM
biomes$broad[biomes$broad == "Flooded Grasslands and Savannas"] <- "Tropical and Subtropical Grasslands, Savannas and Shrublands"
biomes$broad[biomes$broad %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and Subtropical Moist Broadleaf Forests", "Tropical and Subtropical Coniferous Forests")] <- "Tropical and Subtropical Forests"

div.vals<-read.csv("/Users/arau0001/Dropbox/GSB/Data/sb_av_div_estimates.csv")
biomes$alpha<-div.vals$a_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$beta<-div.vals$b_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]
biomes$gamma<-div.vals$g_Estimate[match(biomes$broad,div.vals$Biome_Broad_Hab)]

africa<-biomes[biomes$WWF_REALM2=="Afrotropic",]
#plot(africa$geometry)
pdf("/Users/arau0001/Dropbox/Global seed banks/map.pdf",width=60, height=10,useDingbats=F)
par(mfrow=c(1,3))
plot(biomes["alpha"], breaks="quantile", main="Alpha (0.01m2)", border=0, key.pos=NULL, reset=FALSE)
plot(biomes["gamma"], breaks="quantile", main="Gamma (15m2)", border=0, key.pos=NULL, reset=FALSE)
plot(biomes["beta"], breaks="quantile", main="Beta (Gamma/Alpha)", border=0,key.pos=NULL, reset=FALSE)
dev.off()

pug <- colorRampPalette(c('purple','green'))
biomes$cola<-pug(20)[as.numeric(cut(biomes$alpha, 20))]
biomes$colb<-pug(20)[as.numeric(cut(biomes$beta, 20))]
biomes$colg<-pug(20)[as.numeric(cut(biomes$gamma, 20))]

