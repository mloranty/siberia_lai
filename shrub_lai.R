#read in shrub data
shrub.data <- read.csv("data/allometry_data/shrubdata.csv")

#code basic allometric equation
leaf.mass <- function(a,x,b) {
  lm <- a * x ^ b
  print(lm)
}

#prevent scientific notation
options(scipen=999)

#add leaf mass to data frame in new column
shrub.data$Leaf.Mass <- ifelse(shrub.data$Species == "Betula", 
                               leaf.mass(4.57, shrub.data$BD_cm, 2.45), 
                               leaf.mass(3.11, shrub.data$BD_cm, 2.18))

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
betula.h <- subset(shrubs.sla, Site == "High Density" & Species == "betula")
betula.l <- subset(shrubs.sla, Site == "Low Density" & Species == "betula")
salix.h <- subset(shrubs.sla, Site == "High Density" & Species == "salix")
salix.l <- subset(shrubs.sla, Site == "Low Density" & Species == "salix")

#calculate 4 mean sla values
bet.h.sla <- mean(betula.h$SLA.cm2.g)
bet.l.sla <- mean(betula.l$SLA.cm2.g)
sal.h.sla <- mean(salix.h$SLA.cm2.g)
sal.l.sla <- mean(salix.l$SLA.cm2.g)

#add column to original for density
shrub.data$density.tf <- grepl("H+", shrub.data$Site, perl = TRUE)
shrub.data$density <- ifelse(shrub.data$density.tf == "TRUE", print("HIGH"), print("LOW"))
shrub.data = subset(shrub.data, select = -c(density.tf))

#add column to calculate leaf area
shrub.data$Leaf.Area <- ifelse(shrub.data$Species == "Betula" &  shrub.data$density == "HIGH", 
                               shrub.data$Leaf.Mass*bet.h.sla,
                               ifelse(shrub.data$Species == "Betula" &  shrub.data$density == "LOW",
                                      shrub.data$Leaf.Mass*bet.l.sla,
                                      ifelse(shrub.data$Species == "Salix" &  shrub.data$density == "HIGH",
                                             shrub.data$Leaf.Mass*sal.h.sla, shrub.data$Leaf.Mass*sal.l.sla)))

library(dplyr)
#make new data frame for sums per each site and LAI
#sum leaf area and site area
shrubs.sum.area <- aggregate(shrub.data$Area.Sampled..m2., by=list(Site=shrub.data$Site), FUN=sum)
shrubs.sum.la <- aggregate(shrub.data$Leaf.Area, by=list(Site=shrub.data$Site), FUN=sum)
#join together and rename columns
shrubs.sum <- left_join(shrubs.sum.area,shrubs.sum.la,by="Site")
shrubs.sum <- rename(shrubs.sum, c("Sampled.Area"="x.x","Leaf.Area"="x.y"))

#get LAI!
shrubs.sum$LAI <- shrubs.sum$Leaf.Area/shrubs.sum$Sampled.Area

#write csv LAI summary
write.csv(shrubs.sum,file = "data/allometry_data/shrubs_lai_sum.csv")
