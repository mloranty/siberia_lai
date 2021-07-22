####    Siberia Tree & Shrub LAI Calculations
#Project Info -------------------------------------------------------------------------------------------------------------------

## This code is the analysis of LAI data from field sites near Cherskiy, Russia
## Colgate University
## Loranty Lab
## Code by N. Bendavid

##SECTION 1: ALLOMETRY ==========================================================================================================

# 1.1: Calculate LAI for shrubs -------------------------------------------------------------------------------------------------

#read in shrub data
#shrub data can be found @ https://github.com/mloranty/siberia_lai -> data -> allometry data
shrub.data <- read.csv("data/allometry_data/shrubdata.csv")

#change column names
colnames(shrub.data) <- c("Site","Plot","Area.Sampled.m2","Species","BD.cm")

#code basic allometric equation
leaf.mass <- function(a,x,b) {
  lm <- a * x ^ b
  print(lm)
}

#prevent scientific notation
options(scipen=999)

#add leaf mass (g) to data frame in new column
#plug BD into equation for Betula or Salix
shrub.data$Leaf.Mass.g <- ifelse(shrub.data$Species == "Betula", 
                                 leaf.mass(4.57, shrub.data$BD.cm, 2.45),  #Betula
                                 leaf.mass(3.11, shrub.data$BD.cm, 2.18))  #Salix

#import SLA data
shrubs.sla <- read.csv("data/allometry_data/SLA_trees_shrubs_2017.csv")

#clean up extra columns
shrubs.sla <- shrubs.sla[,c(1,3,6,8)]
colnames(shrubs.sla) <- c("Site","Species","Mass","Total.Leaf.Area")

#make column for SLA in cm^2/g
shrubs.sla$SLA.cm2.g <- shrubs.sla$Total.Leaf.Area/shrubs.sla$Mass

#calculate average SLA for high density and low density for betula and salix
#subset for species and densities
betula.h <- subset(shrubs.sla, Site == "High Density" & Species == "betula")  #Betula @ High Density
betula.l <- subset(shrubs.sla, Site == "Low Density" & Species == "betula")   #Betula @ Low Density
salix.h <- subset(shrubs.sla, Site == "High Density" & Species == "salix")    #Salix @ High Density
salix.l <- subset(shrubs.sla, Site == "Low Density" & Species == "salix")     #Salix @ Low Density

#calculate 4 mean sla values for each subset of species and density
bet.h.sla <- mean(betula.h$SLA.cm2.g)
bet.l.sla <- mean(betula.l$SLA.cm2.g)
sal.h.sla <- mean(salix.h$SLA.cm2.g)
sal.l.sla <- mean(salix.l$SLA.cm2.g)

#remove unecessary dataframes
rm(betula.h,betula.l,salix.h,salix.l)

#add column denoting high, medium, or low density
shrub.data$Density <- ifelse(grepl("H",shrub.data$Site),paste("HIGH"),
                            ifelse(grepl("M",shrub.data$Site),paste("MED"),
                                   paste("LOW")))

#add column to calculate leaf area (cm^2)
shrub.data$Leaf.Area.cm2 <- ifelse(shrub.data$Species == "Betula" &  shrub.data$Density == "HIGH", 
                                   shrub.data$Leaf.Mass*bet.h.sla,
                                   ifelse(shrub.data$Species == "Betula" &  shrub.data$Density == "LOW",
                                          shrub.data$Leaf.Mass*bet.l.sla,
                                          ifelse(shrub.data$Species == "Salix" &  shrub.data$Density == "HIGH",
                                                 shrub.data$Leaf.Mass*sal.h.sla, shrub.data$Leaf.Mass*sal.l.sla)))

#convert leaf area from cm^2 to m^2
shrub.data$Leaf.Area.m2 <- shrub.data$Leaf.Area.cm2/10000
shrub.data <- subset(shrub.data, select = -c(Leaf.Area.cm2))

#combine site and plot columns to give each plot a unique site.plot name
shrub.data$Site.Plot <- paste(shrub.data$Site,shrub.data$Plot,sep = ".")
shrub.data <- shrub.data[,c(9,3,4,5,6,7,8)]

#subset by species
bet.data <- subset(shrub.data, Species == "Betula")
sal.data <- subset(shrub.data, Species == "Salix")

## betula
#make new data frame to store total leaf area per each site and calculate LAI
#sum leaf area per plot and plot area
bet.sum <- aggregate(x = bet.data["Leaf.Area.m2"],       #finds total leaf area per plot
                        by = bet.data[c("Site.Plot")],
                        FUN = sum
)

bet.site <- aggregate(x = bet.data["Area.Sampled.m2"],   #takes plot area
                         by = bet.data[c("Site.Plot")],
                         FUN = mean,
                         simplify = TRUE
)
bet.sum$Area.Sampled.m2 <- bet.site$Area.Sampled.m2
rm(bet.site)

#add back column to indicate density
bet.sum$Density <- ifelse(grepl("H",bet.sum$Site.Plot),paste("HIGH"),
                            ifelse(grepl("M",bet.sum$Site.Plot),paste("MED"),
                                   paste("LOW")))
bet.sum <- bet.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
bet.sum$LAI.m2.m2 <- bet.sum$Leaf.Area.m2/bet.sum$Area.Sampled.m2

#add species column
bet.sum$Species <- paste("BETULA")

#remove unecessary dataframe
rm(bet.data)

## salix
sal.sum <- aggregate(x = sal.data["Leaf.Area.m2"],       #finds total leaf area per plot
                        by = sal.data[c("Site.Plot")],
                        FUN = sum
)

sal.site <- aggregate(x = sal.data["Area.Sampled.m2"],   #takes plot area
                         by = sal.data[c("Site.Plot")],
                         FUN = mean,
                         simplify = TRUE
)
sal.sum$Area.Sampled.m2 <- sal.site$Area.Sampled.m2
rm(sal.site)

#add back column to indicate density
sal.sum$Density <- ifelse(grepl("H",sal.sum$Site.Plot),paste("HIGH"),
                             ifelse(grepl("M",sal.sum$Site.Plot),paste("MED"),
                                    paste("LOW")))
sal.sum <- sal.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
sal.sum$LAI.m2.m2 <- sal.sum$Leaf.Area.m2/sal.sum$Area.Sampled.m2

#add species column
sal.sum$Species <- paste("SALIX")

#remove unecessary dataframes
rm(sal.data)

#combine data frames for both shrub spp
shrubs.sum <- rbind(bet.sum,sal.sum)
rm(bet.sum,sal.sum,bet.h.sla,bet.l.sla,sal.h.sla,sal.l.sla,shrubs.sla)



# 1.2: Calculate LAI for trees --------------------------------------------------------------------------------------------------

## SECTION 2
## Calculate LAI for trees

#read in tree data
#tree data can be found @ https://github.com/mloranty/siberia_lai -> data -> allometry data
tree.data <- read.csv("data/allometry_data/treedata.csv")
colnames(tree.data) <- c("Site","Plot","Area.Sampled.m2","BD.cm","DBH.cm","Diameter.cm")

#code basic allometric equation
leaf.mass <- function(a,x,b) {
  lm <- a * x ^ b
  print(lm)
}

#prevent scientific notation
options(scipen=999)

#add column to indicate diameter type (BD or DBH)
tree.data$Diameter.Type <- ifelse(is.na(tree.data$BD.cm),paste("DBH"),paste("BD"))

#remove separate BD and DBH columns
tree.data$BD.cm <- NULL
tree.data$DBH.cm <- NULL

#add column denoting high, medium, or low density
tree.data$Density <- ifelse(grepl("H",tree.data$Site),paste("HIGH"),
                        ifelse(grepl("M",tree.data$Site),paste("MED"),
                               paste("LOW")))

#add leaf mass to data frame in new column
tree.data$Leaf.Mass <- ifelse(tree.data$Diameter.Type == "BD",
                              leaf.mass(22.55,tree.data$Diameter.cm,1.45),
                       ifelse(tree.data$Density == "HIGH"|tree.data$Density == "MED",
                              leaf.mass(7.57, tree.data$Diameter.cm, 1.73),
                              leaf.mass(150.5, tree.data$Diameter.cm, 1)))
      
#assign sla values from Kropp 2016 for high and low density
lar.h.sla <- 112.89
lar.l.sla <- 84.69

#add column to calculate leaf area
tree.data$Leaf.Area.cm2 <- ifelse(tree.data$Density == "HIGH"|tree.data$Density == "MED", 
                              tree.data$Leaf.Mass*lar.h.sla,
                              tree.data$Leaf.Mass*lar.l.sla)

#remove extra rows (1:5196 includes "Davy" site, 1:5011 drops "Davy" site)
tree.data <- tree.data[1:5196,]

#convert leaf area from cm^2 to m^2
tree.data$Leaf.Area.m2 <- tree.data$Leaf.Area.cm2/10000
tree.data$Leaf.Area.cm2 <- NULL

#combine site and plot columns to give each plot a unique site.plot name
tree.data$Site.Plot <- paste(tree.data$Site,tree.data$Plot,sep = ".")
tree.data <- tree.data[,c(9,3,4,5,6,7,8)]

#make new data frame to store total leaf area per each site and calculate LAI
#sum leaf area per plot and plot area
trees.sum <- aggregate(x = tree.data["Leaf.Area.m2"],       #finds total leaf area per plot
                        by = tree.data[c("Site.Plot")],
                        FUN = sum,
                       na.rm = TRUE
)

trees.site <- aggregate(x = tree.data["Area.Sampled.m2"],   #takes plot area
                         by = tree.data[c("Site.Plot")],
                         FUN = mean,
                         simplify = TRUE,
                        na.rm = TRUE
)
trees.sum$Area.Sampled.m2 <- trees.site$Area.Sampled.m2
rm(trees.site)

#add back column to indicate density
trees.sum$Density <- ifelse(grepl("H",trees.sum$Site.Plot),paste("HIGH"),
                      ifelse(grepl("M",trees.sum$Site.Plot),paste("MED"),
                                      paste("LOW")))
trees.sum <- trees.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
trees.sum$LAI.m2.m2 <- trees.sum$Leaf.Area.m2/trees.sum$Area.Sampled.m2

#add species column
trees.sum$Species <- paste("LARIX")
rm(lar.h.sla,lar.l.sla)



# 1.3: Combine tree and shrub data ----------------------------------------------------------------------------------------------

#adds column to trees to denote tree data vs. shrub data
trees.sum$Trees.or.Shrubs <- "TREES"
trees.sum <- trees.sum[,c(1,2,7,6,3,4,5)]

#adds column to shrubs to denote tree data vs. shrub data
shrubs.sum$Trees.or.Shrubs <- "SHRUBS"
shrubs.sum <- shrubs.sum[,c(1,2,7,6,3,4,5)]

#combines shrubs LAI and trees LAI results 
trees.and.shrubs <- rbind(shrubs.sum,trees.sum)

#add column to denote sloped or flat site
trees.and.shrubs$Slope <- ifelse(grepl("S",trees.and.shrubs$Site.Plot),paste("SLOPED"),paste("FLAT"))

#add column for site
trees.and.shrubs$Site <- ifelse(nchar(trees.and.shrubs$Site.Plot) < 6,
                                substr(trees.and.shrubs$Site.Plot,1,3),
                                substr(trees.and.shrubs$Site.Plot,1,4))
trees.and.shrubs <- trees.and.shrubs[,c(9,1,2,8,3,4,5,6,7)]

#change column names for easier work
colnames(trees.and.shrubs) <- c("Site","Plot","Density","Slope","Trees.Shrubs","Species","Leaf.Area","Area.Sampled","LAI")

#write combined results to csv
write.csv(trees.and.shrubs,"lai_allom_byplot.csv")



# 1.4: Statistical analyses -----------------------------------------------------------------------------------------------------

#split results by trees and shrubs for analyses
trees.sum <- trees.and.shrubs[trees.and.shrubs$Trees.Shrubs == "TREES",]
shrubs.sum <- trees.and.shrubs[trees.and.shrubs$Trees.Shrubs == "SHRUBS",]

#make dataframe for total (tree+shrub) LAI by plot
tot.sum <- aggregate(trees.and.shrubs$LAI,by=list(trees.and.shrubs$Plot),FUN=sum)
colnames(tot.sum) <- c("Plot","LAI")
#add column to indicate density
tot.sum$Density <- ifelse(grepl("H",tot.sum$Plot),paste("HIGH"),
                          ifelse(grepl("M",tot.sum$Plot),paste("MED"),
                                 paste("LOW")))

## LAI by Density (one-way ANOVAs) ##
#one-way anova for trees LAI by density
trees.dens.aov <- aov(LAI ~ Density, data = trees.sum)
summary(trees.dens.aov)

#one-way anova for shrubs LAI by density
shrubs.dens.aov <- aov(LAI ~ Density, data = shrubs.sum)
summary(shrubs.dens.aov)

#one-way anova for total (trees+shrubs) LAI by density
tot.dens.aov <- aov(LAI ~ Density, data = tot.sum)
summary(tot.dens.aov)

#post-hoc tukey tests for trees, shrubs, and total
#trees
trees.tuk <- TukeyHSD(trees.dens.aov)
trees.tuk
#shrubs
shrubs.tuk <- TukeyHSD(shrubs.dens.aov)
shrubs.tuk
#total (trees+shrubs)
tot.tuk <- TukeyHSD(tot.dens.aov)
tot.tuk

#visualize anovas with boxplots using ggplot2
library(ggplot2)

#change Density column for each set to character and then back to factor assigning levels
#this makes sure data will be ordered correctly on plots (HIGH, MED, LOW)
#trees
trees.sum$Density <- as.character(trees.sum$Density)
trees.sum$Density <- factor(trees.sum$Density,levels=c("HIGH","MED","LOW"))
#shrubs
shrubs.sum$Density <- as.character(shrubs.sum$Density)
shrubs.sum$Density <- factor(shrubs.sum$Density,levels=c("HIGH","MED","LOW"))
#total (trees+shrubs)
tot.sum$Density <- as.character(tot.sum$Density)
tot.sum$Density <- factor(tot.sum$Density,levels=c("HIGH","MED","LOW"))

#trees LAI by density boxplot (one-way anova)
ggplot(data = trees.sum, aes(Density,LAI,fill=Density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Leaf Area Index (m²/m²)") +
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))

#shrubs LAI by density boxplot
ggplot(data = shrubs.sum, aes(Density,LAI,fill=Density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Leaf Area Index (m²/m²)") +
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))


# 1.5: Summary statistics and data table ----------------------------------------------------------------------------------------

#split into different dfs for trees and shrubs
shrubs.tab <- subset(trees.and.shrubs,Trees.Shrubs == "SHRUBS")
trees.tab <- subset(trees.and.shrubs,Trees.Shrubs == "TREES")

#mean tree lai per site by density (run whole chunk)
lai.td.s <- aggregate(trees.tab$LAI,by=list(trees.tab$Density),FUN=mean)
lai.td.sd <- aggregate(trees.tab$LAI,by=list(trees.tab$Density),FUN=sd)
lai.td <- merge(lai.td.s,lai.td.sd,by="Group.1")
colnames(lai.td) <- c("Density","LAI.s","LAI.sd")
lai.td <- lai.td[c(1,3,2),]
rm(lai.td.s,lai.td.sd)
lai.td

#mean salix shrub lai per site by density (run whole chunk)
sal.tab <- subset(shrubs.tab,Species=="SALIX")
lai.ssd.s <- aggregate(sal.tab$LAI,by=list(sal.tab$Density),FUN=mean)
lai.ssd.sd <- aggregate(sal.tab$LAI,by=list(sal.tab$Density),FUN=sd)
lai.ssd <- merge(lai.ssd.s,lai.ssd.sd,by="Group.1")
colnames(lai.ssd) <- c("Density","LAI.s","LAI.sd")
lai.ssd <- lai.ssd[c(1,3,2),]
rm(lai.ssd.s,lai.ssd.sd,sal.tab)
lai.ssd

#mean betula shrub lai per site by density (run whole chunk)
bet.tab <- subset(shrubs.tab,Species=="BETULA")
lai.sbd.s <- aggregate(bet.tab$LAI,by=list(bet.tab$Density),FUN=mean)
lai.sbd.sd <- aggregate(bet.tab$LAI,by=list(bet.tab$Density),FUN=sd)
lai.sbd <- merge(lai.sbd.s,lai.sbd.sd,by="Group.1")
colnames(lai.sbd) <- c("Density","LAI.s","LAI.sd")
lai.sbd <- lai.sbd[c(1,3,2),]
rm(lai.sbd.s,lai.sbd.sd,bet.tab)
lai.sbd

#mean total shrub lai per site by density (run whole chunk)
shrubs.tot.tab <- aggregate(shrubs.tab$LAI,by=list(shrubs.tab$Plot),FUN=sum)
colnames(shrubs.tot.tab) <- c("Plot","LAI")
shrubs.tot.tab$Density <- ifelse(grepl("H",shrubs.tot.tab$Plot),paste("HIGH"),
                                 ifelse(grepl("M",shrubs.tot.tab$Plot),paste("MED"),
                                        paste("LOW")))
lai.sd.s <- aggregate(shrubs.tot.tab$LAI,by=list(shrubs.tot.tab$Density),FUN=mean)
lai.sd.sd <- aggregate(shrubs.tot.tab$LAI,by=list(shrubs.tot.tab$Density),FUN=sd)
lai.sd <- merge(lai.sd.s,lai.sd.sd,by="Group.1")
colnames(lai.sd) <- c("Density","LAI.s","LAI.sd")
lai.sd <- lai.sd[c(1,3,2),]
rm(lai.sd.s,lai.sd.sd)
lai.sd

#total tree and shrub lai per site by density (run whole chunk)
lai.ttd <- merge(lai.td,lai.sd,by="Density")
colnames(lai.ttd) <- c("Density","Tree.LAI.s","Tree.LAI.sd","Shrub.LAI.s","Shrub.LAI.sd")
lai.ttd$Total.LAI.s <- lai.ttd$Tree.LAI.s + lai.ttd$Shrub.LAI.s
lai.ttd$Total.LAI.sd <- lai.ttd$Tree.LAI.sd + lai.ttd$Shrub.LAI.sd
lai.ttd <- lai.ttd[c(1,3,2),]
lai.ttd

#final merged df for table (run whole chunk)
lai.tab.1 <- merge(lai.td,lai.ssd,by="Density")
lai.tab.2 <- merge(lai.tab.1,lai.sbd,by="Density")
lai.tab.3 <- merge(lai.tab.2,lai.sd,by="Density")
lai.tab <- merge(lai.tab.3,lai.ttd[,c(1,6,7)],by="Density")
colnames(lai.tab) <- c("Density","Tree.LAI.s","Tree.LAI.sd","Sal.LAI.s","Sal.LAI.sd","Bet.LAI.s","Bet.LAI.sd","TotShrub.LAI.s","TotShrub.LAI.sd","Tot.LAI.s","Tot.LAI.sd")
lai.tab <- format.data.frame(lai.tab,digits = 3)
lai.tab$Tree <- paste(lai.tab$Tree.LAI.s,lai.tab$Tree.LAI.sd,sep = " ± ")
lai.tab$Sal <- paste(lai.tab$Sal.LAI.s,lai.tab$Sal.LAI.sd,sep = " ± ")
lai.tab$Bet <- paste(lai.tab$Bet.LAI.s,lai.tab$Bet.LAI.sd,sep = " ± ")
lai.tab$TotShrub <- paste(lai.tab$TotShrub.LAI.s,lai.tab$TotShrub.LAI.sd,sep = " ± ")
lai.tab$Tot <- paste(lai.tab$Tot.LAI.s,lai.tab$Tot.LAI.sd,sep = " ± ")
lai.tab <- lai.tab[,c(1,12:16)]
lai.tab <- lai.tab[c(1,3,2),]
rm(lai.tab.1,lai.tab.2,lai.tab.3)
lai.tab



##SECTION 2: HEMISPHERICAL PHOTOGRAPHS ==========================================================================================

# 2.1: Process Hemisfer output files --------------------------------------------------------------------------------------------
#set working directory to folder containing all hemi-photo outputs
setwd("")

#list the names of the files in the folder for use in for loop
files <- list.files()

#create a blank dataframe with named columns - for loop will dump rows into this dataframe
all.sites <- data.frame(matrix(nrow = 0,ncol = 26))
colnames(all.sites) <- c("File","Date.Time","Mill","LiCor","Lang","G","NC","T","Mill-S","LiCor-S","Lang-S","G-S","NC-S","T-S","Mill-C","LiCor-C","Lang-C","G-C","NC-C","T-C","Mill-SC","LiCor-SC","Lang-SC","G-SC","NC-SC","T-SC")

#runs each file in the folder through
for (i in files) {
  
  #extracts table of lai values
  d <- read.delim(file = i,                    
                  skip = 30, 
                  nrows = 6, 
                  sep = "\t",header = F, 
                  na.strings = "-")
  
  #extracts file name and date & time
  d2 <- read.delim(file = i,                   
                   skip = 0, 
                   nrows = 2, 
                   sep = "\t",header = F, 
                   na.strings = "-")
  
  #assigns columns to vectors to rearrange
  file.datetime <- d2$V2                       
  no.corr <- d$V2
  S.corr <- d$V4
  C.corr <- d$V6
  SC.corr <- d$V8
  
  #combines vectors to make one long row
  row <- c(file.datetime,no.corr,S.corr,C.corr,SC.corr)
  #inserts row into blank dataframe
  all.sites[nrow(all.sites) + 1,] <- row
}

#converts all LAI values from characters back to numeric
all.sites[,3:26] <- as.numeric(unlist(all.sites[,3:26]))

#writes all results to a csv
setwd("/Volumes/data/data_repo/field_data/siberia_lai/hemisfer_outputs/")
write.csv(all.sites,file = "all_sites_LAI_hemi_output.csv")

#all.sites <- read.csv("L:\\data_repo\\field_data\\siberia_lai\\hemisfer_outputs\\all_sites_LAI_hemi_output.csv")
#(MAC) all.sites <- read.csv("/Volumes/data/data_repo/field_data/siberia_lai/hemisfer_outputs/all_sites_LAI_hemi_output.csv")

# 2.2: Statistical Analyses -----------------------------------------------------------------------------------------------------

#add a column to the sites table that indicates the Site name from the filename
all.sites$Site <- ifelse(nchar(as.character(all.sites$File))<=18,
                         substr(all.sites$File,1,3),
                         substr(all.sites$File,1,4))

#rename DAV site to DavH
all.sites$Site <- sub("DAV","DavH",all.sites$Site)

#add column that indicates density
all.sites$Density <- ifelse(grepl("H",all.sites$Site),paste("HIGH"),
                               ifelse(grepl("M",all.sites$Site),paste("MED"),
                                      paste("LOW")))

#remove unecessary columns, keep Site, File, Density LiCor+C, Thim+C
hemi.sum <- all.sites[,c(27,1,28,16,20)]
colnames(hemi.sum) <- c("Site","File","Density","LiCor","Thim")

#one-way ANOVAs for LAI by density, one for each method, LiCor & Thim
#LiCor
li.dens.aov <- aov(LiCor ~ Density, data = hemi.sum)
summary(li.dens.aov)
#Thim
th.dens.aov <- aov(Thim ~ Density, data = hemi.sum)
summary(th.dens.aov)

#post-hoc Tukey's tests for LiCor & Thim
#LiCor
li.tuk <- TukeyHSD(li.dens.aov)
li.tuk
#Thim
th.tuk <- TukeyHSD(th.dens.aov)
th.tuk

#convert to factor and set levels
hemi.sum$Density <- as.character(hemi.sum$Density)
hemi.sum$Density <- factor(hemi.sum$Density,levels=c("HIGH","MED","LOW"))

#create boxplots for each method LiCor & Thim
#LiCor
ggplot(data = hemi.sum, aes(Density,LiCor,fill=Density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Leaf Area Index (m²/m²)") +
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))
#Thim
ggplot(data = hemi.sum, aes(Density,Thim,fill=Density)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Stand Density") +
  ylab("Leaf Area Index (m²/m²)") +
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))

##SECTION 3: VEGETATION INDEXES =================================================================================================

# 3.1: Create buffers, calculate NDVI & EVI -------------------------------------------------------------------------------------
library(raster)
library(rgdal)

#read NDVI & EVI files
n <- raster("L:/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_NDVI.tif")
#(MAC) n <- raster("/Volumes/data/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_NDVI.tif")
e <- raster("L:/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_EVI.tif")
#(MAC) e <- raster("/Volumes/data/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_EVI.tif")

#read stand data
den <- read.csv("L:/projects/siberia_lai/cherskiy_stand_data.csv", header=T)
#(MAC) den <- read.csv("/Volumes/data/projects/siberia_lai/cherskiy_stand_data.csv", header=T)
#remove irrelevant sites
den <- subset(den, Type == "DG")

#create SpatialPointsDataFrame out of coords
d <- SpatialPointsDataFrame(den[,5:4],den,
                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#extract NDVI & EVI with 15m buffers 
nv <- extract(n,d,buffer=15,na.rm=T)
ev <- extract(e,d,buffer=15,na.rm=T)

#get standard deviation and mean
#for NDVI
nvs <- unlist(lapply(nv,FUN="mean"))
nvsd <- unlist(lapply(nv,FUN="sd"))
#for EVI
evs <- unlist(lapply(ev,FUN="mean"))
evsd <- unlist(lapply(ev,FUN="sd"))

#add to data frames with site names
sites.ndvi <- data.frame(den$Site,nvs,nvsd)
sites.evi <- data.frame(den$Site,evs,evsd)

#rename columns
colnames(sites.ndvi) <- c("Site","nvs","nvsd")
colnames(sites.evi) <- c("Site","es","esd")

#rename "DAV" site to "DavH"
sites.ndvi$Site <- sub("Dav","DavH",sites.ndvi$Site)
sites.evi$Site <- sub("Dav","DavH",sites.evi$Site)



# 3.2: Compare with allometry LAI data ------------------------------------------------------------------------------------------

#use allometry data from section 1.4 to compare with vegetation indexes

#find mean shrub LAI per site
shrubsv.sum <- aggregate(shrubs.sum$LAI,by=list(shrubs.sum$Site),FUN=mean)
colnames(shrubsv.sum) <- c("Site","LAI")

#find mean tree LAI per site
treesv.sum <- aggregate(trees.sum$LAI,by=list(trees.sum$Site),FUN=mean)
colnames(treesv.sum) <- c("Site","LAI")

#merge tree and shrub dataframes, find total LAI per site
totv.sum <- merge(treesv.sum,shrubsv.sum,by="Site")
colnames(totv.sum) <- c("Site","Tree.LAI.s","Shrub.LAI.s")
#find total lai (trees+shrubs)
totv.sum$Tot.LAI.s <- totv.sum$Tree.LAI.s + totv.sum$Shrub.LAI.s

#merge with ndvi and evi values
veg <- merge(sites.evi,sites.ndvi,by="Site") 
lai.veg <- merge(veg,totv.sum,by="Site")
colnames(lai.veg) <- c("Site","EVI.s","EVI.sd","NDVI.s","NDVI.sd","Tree.LAI.s","Shrub.LAI.s","Tot.LAI.s")

#next, w/ggplot plot ndvi and evi vs. tree LAI, shrub LAI, and total LAI
library(ggplot2)
library(ggpubr)

#ndvi vs. lai
#plot tree lai vs. ndvi w/correlation
ggplot(data = lai.veg, aes(x=Tree.LAI.s,y=NDVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean Tree LAI (m²/m²)") +
  ylab("Mean NDVI") +
  stat_cor(method = "pearson",label.x = 1.5,label.y = 0.71)

#plot shrub lai vs. ndvi w/correlation
ggplot(data = lai.veg, aes(x=Shrub.LAI.s,y=NDVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean Shrub LAI (m²/m²)") +
  ylab("Mean NDVI") +
  stat_cor(method = "pearson",label.x = 0.25,label.y = 0.71)

#plot total lai vs. ndvi w/correlation
ggplot(data = lai.veg, aes(x=Tot.LAI.s,y=NDVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean LAI (m²/m²)") +
  ylab("Mean NDVI") +
  stat_cor(method = "pearson",label.x = 1.7,label.y = 0.71)

#evi vs. lai
#plot tree lai vs. evi w/correlation
ggplot(data = lai.veg, aes(x=Tree.LAI.s,y=EVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean Tree LAI (m²/m²)") +
  ylab("Mean EVI") +
  stat_cor(method = "pearson",label.x = 1.5,label.y = 0.31)

#plot shrub lai vs. evi w/correlation
ggplot(data = lai.veg, aes(x=Shrub.LAI.s,y=EVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean Shrub LAI (m²/m²)") +
  ylab("Mean EVI") +
  stat_cor(method = "pearson",label.x = 0.25,label.y = 0.3)

#plot total lai vs. evi w/correlation
ggplot(data = lai.veg, aes(x=Tot.LAI.s,y=EVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean LAI (m²/m²)") +
  ylab("Mean EVI") +
  stat_cor(method = "pearson",label.x = 1.7,label.y = 0.31)
