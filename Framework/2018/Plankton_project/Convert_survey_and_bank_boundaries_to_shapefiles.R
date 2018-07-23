# This is a little script to turn the inshore/offshore extents into shapefiles.

# Load a bunch of packages
require(maps)|| stop("Install the maps package for the spatial plots")
require(maptools)|| stop("Install the maptools package for the spatial plots")
require(mapdata)|| stop("Install the mapdata package for the spatial plots")
require(rgeos)|| stop("Install the rgeos package for the spatial plots")
require(splancs)|| stop("Install the splancs package for the spatial plots")
require(boot)|| stop("Install the boot package for the spatial plots")
require(fields)|| stop("Install the fields package for the spatial plots")
library(rgdal)
require(PBSmapping)|| stop("Install the PBSmapping package for the spatial plots")
source("d:/r/Assessment_fns/Maps/ScallopMap.r")

# Bring in the boundaries we want
bof.bound <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BoFSFA29WExtent_wgs84.csv")
offshore.bound <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Fishing_Area_Borders/offshore.csv")
survey.bound <- read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Survey/survey_boundary_polygons.csv")

# Convert to PBS mapping objects
bof.bound <- as.PolySet(bof.bound,projection = "LL")
survey.bound <- as.PolySet(survey.bound[survey.bound$label != "SPB",],projection = "LL")
offshore.bound <- as.PolySet(offshore.bound[offshore.bound$label != "SPB",],projection = "LL")

# Plot these if you like
ScallopMap("inshore")
addPolys(bof.bound)
addPolys(bof.details)

ScallopMap("offshore")
addPolys(offshore.bound)

ScallopMap("offshore")
addPolys(survey.bound)

# Convert the PBS mapping objects to Spatial polygons
prj4s <- CRS("+proj=longlat +datum=WGS84")
bof.bound.sp <- PolySet2SpatialPolygons(bof.bound)
bof.dt <- data.frame(1:length(bof.bound.sp))
# These need to be made into Spatial Data frames, a spatial data frame is the same as a spatial object but it contains a field with data (we will just make that data the polygon ID)
bof.bound.spdf <- SpatialPolygonsDataFrame(bof.bound.sp,data = bof.dt)
# And repeat twice
survey.bound.sp <- PolySet2SpatialPolygons(survey.bound)
survey.dt <- data.frame(1:length(survey.bound.sp))
survey.bound.spdf <- SpatialPolygonsDataFrame(survey.bound.sp,data = survey.dt)

offshore.bound.sp <- PolySet2SpatialPolygons(offshore.bound)
offshore.dt <- data.frame(1:length(offshore.bound.sp))
offshore.bound.spdf <- SpatialPolygonsDataFrame(offshore.bound.sp,data = offshore.dt)

# Now save as shapefiles...
setwd("d:/R/Data/Framework/2018/Plankton_project")
writeOGR(obj = bof.bound.spdf, dsn = "bof_bound", layer ="bof_bound" , driver="ESRI Shapefile")
writeOGR(obj = survey.bound.spdf, dsn = "survey_bound", layer ="survey_bound" , driver="ESRI Shapefile")
writeOGR(obj = offshore.bound.spdf, dsn = "offshore_bound", layer ="offshore_bound" , driver="ESRI Shapefile")



