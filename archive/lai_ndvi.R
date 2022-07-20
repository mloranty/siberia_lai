library(raster)
library(rgdal)
library(ggplot2)

# read NDVI & EVI files
n <- raster("L:/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_NDVI.tif")
e <- raster("L:/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_EVI.tif")

# read stand data
den <- read.csv("L:/projects/siberia_lai/cherskiy_stand_data.csv", header=T)

d <- SpatialPointsDataFrame(den[,5:4],den,
                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

# create 15m buffers 
nv <- extract(n,d,buffer=15,na.rm=T)
ev <- extract(e,d,buffer=15,na.rm=T)

# get standard deviation and mean
# for NDVI
nvs <- unlist(lapply(nv,FUN="mean"))
nvsd <- unlist(lapply(nv,FUN="sd"))
# for EVI
evs <- unlist(lapply(ev,FUN="mean"))
evsd <- unlist(lapply(ev,FUN="sd"))

# add to data frames with site names
sites.ndvi <- data.frame(den$Site,den$Type,nvs,nvsd)
sites.evi <- data.frame(den$Site,den$Type,evs,evsd)

# remove irrelevant sites 
sites.ndvi <- subset(sites.ndvi, den.Type == "DG")
sites.evi <- subset(sites.evi, den.Type == "DG")

# rename columns
colnames(sites.ndvi) <- c("Site","Type","nvs","nvsd")
colnames(sites.evi) <- c("Site","Type","es","esd")

# read in allometry LAI data for comparison
lai <- read.csv("C:/Users/nbendavid/Documents/siberia_lai/lai_allom_byplot.csv")

# aggregate by plot to add trees and shrubs
lai <- aggregate(lai$LAI, by = list(lai$Plot), FUN = sum)
colnames(lai) <- c("Plot","LAI")

# add column for site
lai$Site <- ifelse(nchar(as.character(lai$Plot)) < 6,
                   substr(lai$Plot,1,3),
                   substr(lai$Plot,1,4))

# mean & sd lai for each site
lai.s <- aggregate(lai$LAI, by = list(lai$Site), FUN = mean)
colnames(lai.s) <- c("Site","LAI.s")
lai.sd <- aggregate(lai$LAI, by = list(lai$Site), FUN = sd)
colnames(lai.sd) <- c("Site","LAI.sd")

# put together in new data frame
# merge LAI mean & sd
lai.ssd <- merge(lai.s,lai.sd, by = "Site")
# merge with ndvi
lai.ndvi <- merge(lai.ssd,sites.ndvi, by = "Site")
lai.ndvi$Type <- NULL
# merge with evi
lai.e.n <- merge(sites.evi,lai.ndvi, by = "Site")
lai.e.n$Type <- NULL
colnames(lai.e.n) <- c("Site","EVI.s","EVI.sd","NDVI.s","NDVI.sd","LAI.s","LAI.sd")

# plot lai vs. ndvi and look at correlation
ggplot(data = lai.e.n, aes(x=LAI.s,y=NDVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean LAI (m²/m²)") +
  ylab("Mean NDVI")

cor(lai.ndvi$NDVI.s,lai.ndvi$LAI.s)

# plot lai vs. evi and look at correlation
ggplot(data = lai.e.n, aes(x=LAI.s,y=EVI.s)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Mean LAI (m²/m²)") +
  ylab("Mean EVI")

cor(lai.e.n$EVI.s,lai.e.n$LAI.s)
