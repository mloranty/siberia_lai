#read in tree data
tree.data <- read.csv("data/allometry_data/treedata.csv")

#code basic allometric equation
leaf.mass <- function(a,x,b) {
  lm <- a * x ^ b
  print(lm)
}

#prevent scientific notation
options(scipen=999)

#add column for diameter type
tree.data$Diameter.Type <- ifelse(is.na(tree.data$BD_cm),paste("DBH"),paste("BD"))

#remove separate BD and DBH columns
tree.data$BD_cm <- NULL
tree.data$DBH_cm <- NULL

#add leaf mass to data frame in new column
tree.data$Leaf.Mass <- ifelse(tree.data$Diameter.Type == "BD", 
                               leaf.mass(22.55, tree.data$Diam_cm, 1.45), 
                               leaf.mass(40.50, tree.data$Diam_cm, 1.41))
#import SLA data
trees.sla <- read.csv("data/allometry_data/SLA_trees_shrubs_2017.csv")

#clean up extra columns
trees.sla$SLA..cm2.g. <- NULL
trees.sla$X <- NULL
trees.sla$X.1 <- NULL
trees.sla$SLA..m2.g. <- NULL
trees.sla$big.leaves <- NULL
trees.sla$petioles <- NULL

#make column for SLA in cm^2/g
trees.sla$SLA.cm2.g <- trees.sla$Total.Leaf.Area/trees.sla$Mass

#calculate average SLA for high density and low density 
#subset by density
larix.h <- subset(trees.sla, Site == "High Density")
larix.l <- subset(trees.sla, Site == "Low Density")

#calculate 2 mean sla values
h.sla <- mean(larix.h$SLA.cm2.g)
l.sla <- mean(larix.l$SLA.cm2.g)

#add column to original for density
tree.data$density.tf <- grepl("H+", tree.data$Site, perl = TRUE)
tree.data$density <- ifelse(tree.data$density.tf == "TRUE", print("HIGH"), print("LOW"))
tree.data = subset(tree.data, select = -c(density.tf))

#add column to calculate leaf area
tree.data$Leaf.Area <- ifelse(tree.data$density == "HIGH", 
                              tree.data$Leaf.Mass*h.sla,
                              tree.data$Leaf.Mass*l.sla)
library(dplyr)
#make new data frame for sums per each site and LAI
#sum leaf area and site area
tree.sum.area <- aggregate(tree.data$Area.Sampled_m2, by=list(Site=tree.data$Site), FUN=sum)
tree.sum.la <- aggregate(tree.data$Leaf.Area, by=list(Site=tree.data$Site), FUN=sum)
#join together and rename columns
tree.sum <- left_join(tree.sum.area,tree.sum.la,by="Site")
tree.sum <- rename(tree.sum, c("Sampled.Area"="x.x","Leaf.Area"="x.y"))
tree.sum <- tree.sum[-c(1),]

#get LAI!
tree.sum$LAI <- tree.sum$Leaf.Area/tree.sum$Sampled.Area

#write csv LAI summary
write.csv(tree.sum,file = "data/allometry_data/tree_lai_sum.csv")
