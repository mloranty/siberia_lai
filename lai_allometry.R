####    Siberia Tree & Shrub LAI Calculations
#--------------------------------------------------------------------------------------------------------

## SECTION 1
## Calculate LAI for shrubs

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

#add back column to indicate density
bet.sum$Density <- ifelse(grepl("H",bet.sum$Site.Plot),paste("HIGH"),
                            ifelse(grepl("M",bet.sum$Site.Plot),paste("MED"),
                                   paste("LOW")))
bet.sum <- bet.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
bet.sum$LAI.m2.m2 <- bet.sum$Leaf.Area.m2/bet.sum$Area.Sampled.m2

#add species column
bet.sum$Species <- paste("BETULA")

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

#add back column to indicate density
sal.sum$Density <- ifelse(grepl("H",sal.sum$Site.Plot),paste("HIGH"),
                             ifelse(grepl("M",sal.sum$Site.Plot),paste("MED"),
                                    paste("LOW")))
sal.sum <- sal.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
sal.sum$LAI.m2.m2 <- sal.sum$Leaf.Area.m2/sal.sum$Area.Sampled.m2

#add species column
sal.sum$Species <- paste("SALIX")

#combine data frames for both shrub spp
shrubs.sum <- rbind(bet.sum,sal.sum)
#--------------------------------------------------------------------------------------------------------

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

#add back column to indicate density
trees.sum$Density <- ifelse(grepl("H",trees.sum$Site.Plot),paste("HIGH"),
                      ifelse(grepl("M",trees.sum$Site.Plot),paste("MED"),
                                      paste("LOW")))
trees.sum <- trees.sum[,c(1,4,2,3)]

#divide total leaf area per plot by plot area to get LAI!
trees.sum$LAI.m2.m2 <- trees.sum$Leaf.Area.m2/trees.sum$Area.Sampled.m2

#add species column
trees.sum$Species <- paste("LARIX")
#--------------------------------------------------------------------------------------------------------

## SECTION 3
## Combine tree and shrub LAI results

#adds column to trees to denote tree data vs. shrub data
trees.sum$Trees.or.Shrubs <- "TREES"
trees.sum <- trees.sum[,c(1,2,7,4,3,5,6)]

#adds column to shrubs to denote tree data vs. shrub data
shrubs.sum$Trees.or.Shrubs <- "SHRUBS"
shrubs.sum <- shrubs.sum[,c(1,2,7,4,3,5,6)]

#combines shrubs LAI and trees LAI results 
trees.and.shrubs <- rbind(shrubs.sum,trees.sum)

#add column to denote sloped or flat site
trees.and.shrubs$Slope <- ifelse(grepl("S",trees.and.shrubs$Site.Plot),paste("SLOPED"),paste("FLAT"))

#add column for site
trees.and.shrubs$Site <- ifelse(nchar(trees.and.shrubs$Site.Plot) < 6,
                                substr(trees.and.shrubs$Site.Plot,1,3),
                                substr(trees.and.shrubs$Site.Plot,1,4))
trees.and.shrubs <- trees.and.shrubs[,c(9,1,2,8,7,6,3,4,5)]

#change column names for easier work
colnames(trees.and.shrubs) <- c("Site","Plot","Density","Slope","Trees.Shrubs","Species","Leaf.Area","Area.Sampled","LAI")

#write combined results to csv
write.csv(trees.and.shrubs,"lai_allom_byplot.csv")
#--------------------------------------------------------------------------------------------------------

## SECTION 4
## Statistical Analyses

#to read in summary results and split by trees and shrubs, uncomment and run next 3 lines:
trees.and.shrubs <- read.csv("lai_allom_byplot.csv")
trees.sum <- trees.and.shrubs[trees.and.shrubs$Trees.Shrubs == "TREES",]
shrubs.sum <- trees.and.shrubs[trees.and.shrubs$Trees.Shrubs == "SHRUBS",]


## LAI by Density (one-way ANOVAs) ##
#one-way anova for trees LAI by density
trees.dens.aov <- aov(LAI ~ Density, data = trees.sum)
summary(trees.dens.aov)

#one-way anova for shrubs LAI by density
shrubs.dens.aov <- aov(LAI ~ Density, data = shrubs.sum)
summary(shrubs.dens.aov)

#post-hoc tukey tests for trees and shrubs
trees.tuk <- TukeyHSD(trees.dens.aov)
shrubs.tuk <- TukeyHSD(shrubs.dens.aov)

#visualize anovas with boxplots using ggplot2
library(ggplot2)

#trees LAI by density boxplot (one-way anova)
ggplot(data = trees.sum, aes(Density,LAI,fill=Density)) +
  geom_boxplot()

#shrubs LAI by density boxplot
ggplot(data = shrubs.sum, aes(Density,LAI,fill=Density)) +
  geom_boxplot()

## LAI by Density + Slope (two-way ANOVAs) ##
#two-way anova for trees LAI by density + slope
trees.dslope.aov <- aov(LAI ~ Density + Slope, data = trees.sum)
summary(trees.dslope.aov)

#two-way anova for shrub LAI by density + slope
shrubs.dslope.aov <- aov(LAI ~ Density + Slope, data = shrubs.sum)
summary(shrubs.dslope.aov)

#post-hoc tukey tests for trees and shrubs
TukeyHSD(trees.dslope.aov)
TukeyHSD(shrubs.dslope.aov)

## Tree LAI vs. Shrub LAI vs. Total LAI ##
#reformat data by site (instead of site & plot)
#make new dataframe to compare tree LAI vs. shrub LAI vs. total LAI
lai.comp <- trees.and.shrubs
#add column for site only
lai.comp$Site <- ifelse(nchar(lai.comp$Plot) < 6,
                             substr(lai.comp$Plot,1,3),
                             substr(lai.comp$Plot,1,4))
#aggregate now by site and trees or shrubs
lai.comp <- aggregate(lai.comp, list(lai.comp$Site, lai.comp$Trees.Shrubs), FUN = mean, na.rm = FALSE)

#--------------------------------------------------------------------------------------------------------

## SECTION 5
## Format output for results section data table

#split into different dfs for trees and shrubs
shrubs.tab <- subset(trees.and.shrubs,Trees.Shrubs == "SHRUBS")
trees.tab <- subset(trees.and.shrubs,Trees.Shrubs == "TREES")

#mean tree lai per site by density (run whole chunk)
lai.td <- aggregate(trees.tab$LAI,by=list(trees.tab$Site),FUN=mean)
colnames(lai.td) <- c("Site","LAI")
lai.td$Density <- ifelse(grepl("H",lai.td$Site),paste("HIGH"),
                         ifelse(grepl("M",lai.td$Site),paste("MED"),
                                paste("LOW")))
lai.td.s <- aggregate(lai.td$LAI,by=list(lai.td$Density),FUN=mean)
lai.td.sd <- aggregate(lai.td$LAI,by=list(lai.td$Density),FUN=sd)
lai.td <- merge(lai.td.s,lai.td.sd,by="Group.1")
colnames(lai.td) <- c("Density","LAI.s","LAI.sd")
lai.td <- lai.td[c(1,3,2),]
rm(lai.td.s,lai.td.sd)
lai.td

#mean salix shrub lai per site by density (run whole chunk)
sal.tab <- subset(shrubs.tab,Species=="SALIX")
lai.ssd <- aggregate(sal.tab$LAI,by=list(sal.tab$Site),FUN=mean)
colnames(lai.ssd) <- c("Site","LAI")
lai.ssd$Density <- ifelse(grepl("H",lai.ssd$Site),paste("HIGH"),
                         ifelse(grepl("M",lai.ssd$Site),paste("MED"),
                                paste("LOW")))
lai.ssd.s <- aggregate(lai.ssd$LAI,by=list(lai.ssd$Density),FUN=mean)
lai.ssd.sd <- aggregate(lai.ssd$LAI,by=list(lai.ssd$Density),FUN=sd)
lai.ssd <- merge(lai.ssd.s,lai.ssd.sd,by="Group.1")
colnames(lai.ssd) <- c("Density","LAI.s","LAI.sd")
lai.ssd <- lai.ssd[c(1,3,2),]
rm(lai.ssd.s,lai.ssd.sd,sal.tab)
lai.ssd

#mean betula shrub lai per site by density (run whole chunk)
bet.tab <- subset(shrubs.tab,Species=="BETULA")
lai.sbd <- aggregate(bet.tab$LAI,by=list(bet.tab$Site),FUN=mean)
colnames(lai.sbd) <- c("Site","LAI")
lai.sbd$Density <- ifelse(grepl("H",lai.sbd$Site),paste("HIGH"),
                          ifelse(grepl("M",lai.sbd$Site),paste("MED"),
                                 paste("LOW")))
lai.sbd.s <- aggregate(lai.sbd$LAI,by=list(lai.sbd$Density),FUN=mean)
lai.sbd.sd <- aggregate(lai.sbd$LAI,by=list(lai.sbd$Density),FUN=sd)
lai.sbd <- merge(lai.sbd.s,lai.sbd.sd,by="Group.1")
colnames(lai.sbd) <- c("Density","LAI.s","LAI.sd")
lai.sbd <- lai.sbd[c(1,3,2),]
rm(lai.sbd.s,lai.sbd.sd,bet.tab)
lai.sbd

#mean total shrub lai per site by density (run whole chunk)
lai.sd <- aggregate(shrubs.tab$LAI,by=list(shrubs.tab$Site),FUN=mean)
colnames(lai.sd) <- c("Site","LAI")
lai.sd$Density <- ifelse(grepl("H",lai.sd$Site),paste("HIGH"),
                          ifelse(grepl("M",lai.sd$Site),paste("MED"),
                                 paste("LOW")))
lai.sd.s <- aggregate(lai.sd$LAI,by=list(lai.sd$Density),FUN=mean)
lai.sd.sd <- aggregate(lai.sd$LAI,by=list(lai.sd$Density),FUN=sd)
lai.sd <- merge(lai.sd.s,lai.sd.sd,by="Group.1")
colnames(lai.sd) <- c("Density","LAI.s","LAI.sd")
lai.sd <- lai.sd[c(1,3,2),]
rm(lai.sd.s,lai.sd.sd,shrubs.tab)
lai.sd
