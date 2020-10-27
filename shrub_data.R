#read in shrub data
shrub.data <- read.csv("/Users/nadav/Documents/GitHub/Siberia_LAI/shrubdata.csv")

#code basic allometric equation
leaf.mass <- function(a,x,b) {
  lm <- a * x ^ b
  print(lm)
}

#prevent scientific notation
options(scipen=999)

#add lead mass to data frame in new column
shrub.data$Leaf.Mass <- ifelse(shrub.data$Species == "Betula", 
       leaf.mass(4.57, shrub.data$BD_cm, 2.45), 
       leaf.mass(3.11, shrub.data$BD_cm, 2.18))

