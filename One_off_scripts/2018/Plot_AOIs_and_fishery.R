# Quick script to look at impact of Eastern Canyons Conservation Area overlap with Scallop fishery.
yr <- 2018
direct = "d:/r/"
#direct = "Y:/Offshore scallop/Assessment/"
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
library(PBSmapping)
require(rgdal)
library(maptools)
# Read SHAPEFILE.shp from the current working directory (".")

# Bring in the eastern canynons 
ecca <- read.csv("Y:/Maps/AOIs/Eastern_Canyons_AOI/ecca_coords.csv")
ecca <- as.PolySet(ecca,projection = "LL")
# Bring in the fundian channel shapefile directly, note these data are in NAD83
fund.chan <- readOGR(dsn = "Y:/Maps/AOIs/Fundian_Channel_AOI", layer = "FundianChannel_BrownsBank_AOI_poly")
# Convert from UTM to lat-long, moving to NAD83 as datum as I think this is probably more similar to what our underlying data is...
fund.chan <- spTransform(fund.chan, CRS("+proj=longlat +datum=NAD83"))
# The eastern shore one
east.shore <- readOGR(dsn = "Y:/Maps/AOIs/Eastern_shore_AOI", layer = "esi_poly")
# Convert from UTM to lat-long, moving to NAD83 as datum as I think this is probably more similar to what our underlying data is...
east.shore <- spTransform(east.shore, CRS("+proj=longlat +datum=NAD83"))


# Convert to PBS mapping just cause I'm used to that...
fund.chan.pbs <- SpatialPolygons2PolySet(fund.chan)
east.shore.pbs <- SpatialPolygons2PolySet(east.shore)

# Grab the fishery data.
logs_and_fish(loc="offshore",year = 1981:2017,un=un.ID,pw=pwd.ID,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

# First the ECCA area
windows(11,11)
ScallopMap(xlim = c(-59,-56),ylim=c(43,45),plot.bathy = T,plot.boundries = T,boundries = "offshore")
points(fish.dat$lon,fish.dat$lat,cex=0.5,pch=19)
addPolys(ecca)

# Next up the Fundian Channel.
windows(11,11)
ScallopMap(xlim=c(-67.6,-65),ylim=c(41.25,43),plot.bathy = T,plot.boundries = T,boundries = "offshore")
addPolys(fund.chan.pbs,lwd=2)
points(fish.dat$lon,fish.dat$lat,cex=0.1,pch=19)

# Here's where the Eastern Shore is...
windows(11,11)
ScallopMap(xlim=c(-66,-60),ylim=c(44,46),plot.bathy = T,plot.boundries = T,boundries = "inshore")
addPolys(east.shore.pbs,lwd=2)


