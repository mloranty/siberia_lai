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
shrubs.sla$SLA..cm2.g. <- NULL
shrubs.sla$X <- NULL
shrubs.sla$X.1 <- NULL
shrubs.sla$SLA..m2.g. <- NULL

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

#add column to original dataframe for density
shrub.data$density.tf <- grepl("H+", shrub.data$Site, perl = TRUE)
shrub.data$Density.HL <- ifelse(shrub.data$density.tf == "TRUE", print("HIGH"), print("LOW"))
shrub.data <- subset(shrub.data, select = -c(density.tf))

#add column to calculate leaf area (cm^2)
shrub.data$Leaf.Area.cm2 <- ifelse(shrub.data$Species == "Betula" &  shrub.data$Density.HL == "HIGH", 
                                   shrub.data$Leaf.Mass*bet.h.sla,
                                   ifelse(shrub.data$Species == "Betula" &  shrub.data$Density.HL == "LOW",
                                          shrub.data$Leaf.Mass*bet.l.sla,
                                          ifelse(shrub.data$Species == "Salix" &  shrub.data$Density.HL == "HIGH",
                                                 shrub.data$Leaf.Mass*sal.h.sla, shrub.data$Leaf.Mass*sal.l.sla)))

#convert leaf area from cm^2 to m^2
shrub.data$Leaf.Area.m2 <- shrub.data$Leaf.Area.cm2/10000
shrub.data <- subset(shrub.data, select = -c(Leaf.Area.cm2))

#combine site and plot columns to give each plot a unique site.plot name
shrub.data$Site.Plot <- paste(shrub.data$Site,shrub.data$Plot,sep = ".")
shrub.data <- shrub.data[,c(9,3,4,5,6,7,8)]

#make new data frame to store total leaf area per each site and calculate LAI
#sum leaf area per plot and plot area
shrubs.sum <- aggregate(x = shrub.data["Leaf.Area.m2"],       #finds total leaf area per plot
                        by = shrub.data[c("Site.Plot")],
                        FUN = sum
)

shrubs.site <- aggregate(x = shrub.data["Area.Sampled.m2"],   #takes plot area
                         by = shrub.data[c("Site.Plot")],
                         FUN = mean,
                         simplify = TRUE
)
shrubs.sum$Area.Sampled.m2 <- shrubs.site$Area.Sampled.m2

#divide total leaf area per plot by plot area to get LAI!
shrubs.sum$LAI.m2.m2 <- shrubs.sum$Leaf.Area.m2/shrubs.sum$Area.Sampled.m2
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

#remove separate BD and diameter columns
tree.data$BD.cm <- NULL
tree.data$Diameter.cm <- NULL

#add column denoting high or low density
tree.data$Density.HL <- ifelse(grepl("H",tree.data$Site)|grepl("M",tree.data$Site),paste("HIGH"),paste("LOW"))

#add leaf mass to data frame in new column
tree.data$Leaf.Mass <- ifelse(tree.data$Density.HL == "HIGH",
                              leaf.mass(7.57, tree.data$DBH.cm, 1.73),
                              leaf.mass(150.5, tree.data$DBH.cm, 1))
      
#assign sla values from Kropp 2016 for high and low density
lar.h.sla <- 112.89
lar.l.sla <- 84.69

#add column to calculate leaf area
tree.data$Leaf.Area.cm2 <- ifelse(tree.data$Density.HL == "HIGH", 
                              tree.data$Leaf.Mass*lar.h.sla,
                              tree.data$Leaf.Mass*lar.l.sla)

#remove extra rows (1:5196 includes "Davy" site, 1:5011 drops "Davy" site)
tree.data <- tree.data[1:5011,]

#convert leaf area from cm^2 to m^2
tree.data$Leaf.Area.m2 <- tree.data$Leaf.Area.cm2/10000
tree.data$Leaf.Area.cm2 <- NULL

#combine site and plot columns to give each plot a unique site.plot name
tree.data$Site.Plot <- paste(tree.data$Site,tree.data$Plot,sep = ".")
tree.data <- tree.data[,c(8,3,4,5,6,7)]

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

#divide total leaf area per plot by plot area to get LAI!
trees.sum$LAI.m2.m2 <- trees.sum$Leaf.Area.m2/trees.sum$Area.Sampled.m2
#--------------------------------------------------------------------------------------------------------

## SECTION 3
## Combine tree and shrub LAI results

#adds headers for trees and shrubs for combined dataframe 
shrubs.header <- data.frame("Shrubs:",NA,NA,NA)
colnames(shrubs.header) <- c("Site.Plot","Leaf.Area.m2","Area.Sampled.m2","LAI.m2.m2")
trees.header <- data.frame("Trees:",NA,NA,NA)
colnames(trees.header) <- c("Site.Plot","Leaf.Area.m2","Area.Sampled.m2","LAI.m2.m2")

#combines shrubs LAI and trees LAI results with headers separating
trees.and.shrubs <- rbind(shrubs.header,shrubs.sum,trees.header,trees.sum)

#write combined results to csv
write.csv(trees.and.shrubs,"lai_allom_byplot.csv")
