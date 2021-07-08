# install.packages("raster")
install.packages("rgdal")
library(raster)
library(rgdal)

# read ndvi file
n <- raster("L:/data_repo/gis_data/planet_cherskii/PlanetScope_4band_with_SR/20170726_001152_1004/20170726_001152_1004_3B_AnalyticMS_SR_NDVI.tif")

# read stand data
den <- read.csv("L:/projects/siberia_lai/cherskiy_stand_data.csv", header=T)

d <- SpatialPointsDataFrame(den[,5:4],den,
                            proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

# create 15m buffers
nv <- extract(n,d,buffer=15,na.rm=T)

# get standard deviation and mean
nvs <- unlist(lapply(nv,FUN="mean"))
nvsd <- unlist(lapply(nv,FUN="sd"))

# add to data frame with site names
sites.ndvi <- data.frame(den$Site,den$Type,nvs,nvsd)

# remove irrelevant sites 
sites.ndvi <- subset(sites.ndvi, den.Type == "DG")

# read in allometry LAI data for comparison
lai <- read.csv("C:/Users/nbendavid/Documents/siberia_lai/lai_allom_byplot.csv")

# summarize by site instead of plot
lai$Site <- ifelse(nchar(as.character(lai$Site.Plot)) < 6,
                   substr(lai$Site.Plot,1,3),
                   substr(lai$Site.Plot,1,4))
lai <- lai[,c(8,1:7)]
# mean lai for each site
lai.s <- aggregate(lai$LAI.m2.m2, by = list(lai$Site), FUN = mean)
lai.sd <- aggregate(lai$LAI.m2.m2, by = list(lai$Site), FUN = sd)

# put together in new data frame
lai.ndvi <- data.frame(sites.ndvi$den.Site,sites.ndvi$nvs,sites.ndvi$nvsd,lai.s$x,lai.sd$x)
colnames(lai.ndvi) <- c("Site","NDVI.mean","NDVI.sd","LAI.mean","LAI.sd")

# plot lai vs. ndvi
plot(lai.ndvi$NDVI.mean,lai.ndvi$LAI.mean)
