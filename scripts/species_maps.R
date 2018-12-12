#species maps
library(maps)
library(raster)
library(mapdata)
library(scales)
library(randomcoloR)
library(viridis)

locations <- read.csv("raw_data/climate_data.csv")
#test map
# windows()
# map('world')
# points(locations$longitude, locations$latitude,pch=1)

#mexico specific-----------------------------------------
#all dioons
dioon <- locations[locations$Genus == "Dioon",]
#mexico map
mexico <- getData("GADM", country='MEX', level=1)
#bri's dioons
bri <- read.csv("raw_data/bri_dioon.csv")
bri2 <- unique(bri[c("Genus", "Species")])
bri3 <- dioon[dioon$Species %in% bri2$Species,]
bri4 <- droplevels(bri3)

leglabs <- paste("D", unique(bri4$Species), sep=".")

cols <- distinctColorPalette(length(unique(bri4$Species)))
cols2 <- alpha(cols, .7)

cols3 <- viridis_pal(option = "D")(length(unique(bri4$Species)))
cols4 <- viridis_pal(alpha=.7, option = "D")(length(unique(bri4$Species)))

cols5 <- brewer_pal(11, "Set3")

# windows(10,6)
jpeg(filename = "output/bri_species_map2.jpeg",
     width = 8.4, height = 8.4, units = "in", res= 300)

plot(mexico, axes=T, las=1, xlim=c(-118, -85))
# points(dioon$longitude, dioon$latitude,pch=21, bg="lightgrey")
points(bri4$longitude, bri4$latitude,pch=21, 
       bg=cols4[bri4$Species], cex=1.5)

legend("topright", legend = leglabs, pch=21, 
       pt.bg = cols3, cex=1, bty='n')

# dev.copy2pdf(file="output/bri_species_map.pdf")
dev.off()



#ardi species---------------------------------------------
ardi <- read.csv("raw_data/ardi_species.csv")
ardi2 <- locations[locations$Genus %in% ardi$Genus & 
                     locations$Species %in% ardi$Species,]

#ox coords
oz <- getData("GADM", country='AUS', level=1)

#ardi's genus
cycas <- ardi2[ardi2$Genus == "Cycas",]
  cycas2 <- droplevels(cycas)
  cyccols <- viridis_pal(option = "D")(length(unique(cycas2$Species)))
  cyccols2 <- alpha(cyccols, .7)
  cycleg <- paste("C", unique(cycas2$Species), sep=".")

bowenia <- ardi2[ardi2$Genus == "Bowenia",]
  bowenia2 <- droplevels(bowenia)
  bowcols <- viridis_pal(option = "C")(length(unique(bowenia2$Species)))
  bowcols2 <- alpha(bowcols, .7)
  bowleg <- paste("B", unique(bowenia2$Species), sep=".")

mac <- ardi2[ardi2$Genus == "Macrozamia",]
  mac2 <- droplevels(mac)
  maccols <- viridis_pal(option = "B")(length(unique(mac2$Species)))
  maccols2 <- alpha(maccols, .7)
  macleg <- paste("M", unique(mac2$Species), sep=".")

lep <- ardi2[ardi2$Genus == "Lepidozamia",]
  lep2 <- droplevels(lep)
  lepcols <- viridis_pal(option = "A")(length(unique(lep2$Species)))
  lepcols2 <- alpha(lepcols, .4)
  lepleg <- paste("L", unique(lep2$Species), sep=".")


#plot oz
windows()

jpeg(filename = "output/ardi_species_map2.jpeg",
     width = 8.4, height = 8.4, units = "in", res= 400)
plot(oz, axes=T, las=1, xlim=c(110, 170), ylim=c(-40,-15))

points(cycas$longitude, cycas$latitude,pch=21,bg=cyccols2[cycas2$Species], cex=1.25)
points(bowenia$longitude, bowenia$latitude,pch=22, bg=bowcols2[bowenia2$Species]
       , cex=1.25)
points(mac$longitude, mac$latitude,pch=23, bg=maccols2[mac2$Species], cex=1.25)
points(lep$longitude, lep$latitude,pch=24, bg=alpha("grey20",.5), cex=1.25)


legend("topright", legend = c(cycleg,bowleg, macleg,lepleg), 
       pt.bg = c(cyccols,bowcols, maccols, "grey20"), bty='n',
       pch=c(rep(21, 7), rep(22, 2), rep(23, 5), 24))

dev.off()
       