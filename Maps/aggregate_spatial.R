########## Aggregating data spatially - an example using Browns Bank survey data and the Fundian channel


##### SABHU Functions
source("Y:/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/gridPlot.r")
source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Maps/Convert_PBSmapping_into_GIS_shapefiles.r")

##### Other packages
require(rgdal)
require(PBSmapping)
require(maptools)
require(sp)
require(plyr)

##### Read in and clean up survey data
direct <- "Y:/Offshore scallop/Assessment/"
load(paste0(direct, "Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata"))

surveydata <- rbind(
  surv.Live["BBn"][[1]][, c("year", "tow", "bank", "lon", "lat", "tot.bm")],
  surv.Live["BBs"][[1]][, c("year", "tow", "bank", "lon", "lat", "tot.bm")])

surveydata$EID <- 1:nrow(surveydata)

names(surveydata) <- c("year", "tow", "bank", "X", "Y", "Z", "EID")


##### Read in spatial data
#fundian coords
fundian <- readOGR("Y:/Maps/AOIs/Fundian_Channel_AOI/FundianChannel_BrownsBank_AOI_poly.shp")
# fundian@proj4string
fundian <- spTransform(x = fundian, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0"))
fundian <- SpatialPolygons2PolySet(fundian)

# Browns bank polygons
BBnshp <- readOGR(paste0(direct, "Data/Maps/approved/GIS_layers/offshore/BBn.shp"), layer="BBn")
BBnshp.4.map <- SpatialPolygons2PolySet(BBnshp)
BBsshp <- readOGR(paste0(direct, "Data/Maps/approved/GIS_layers/offshore/BBs.shp"), layer="BBs")
BBsshp.4.map <- SpatialPolygons2PolySet(BBsshp)

BBnset <- SpatialPolygons2PolySet(BBnshp)
BBsset <- SpatialPolygons2PolySet(BBsshp)

BBsset$PID <- 2

# "polys" object for gridPlot
BBset <- rbind(BBnset, BBsset)


##### Run gridPlot
lvls <- c(0, 0.5, 1, 2.5, 5, 10, 15, 20)
gridPlotobj <- gridPlot(surveydata[surveydata$year==2018, c("EID", "X", "Y", "Z")], BBset, lvls, FUN=mean, grid.size=1/60)

##### Tidy up the gridPlot object
dat <- join(gridPlotobj[[1]], gridPlotobj[[2]], type="left")
dat <- dat[!is.na(dat$Z),]
dat$label <- "Browns"
dat$PID <- as.numeric(paste0(dat$PID, dat$SID))


##### Convert gridPlotobj to sp package object SpatialPolygons (but this only deals with the coordinates, not the calculated Z value)
sp.obj <- pbs.2.gis(dat, proj = "LL", c_sys="WGS84", layer.names = "label", type="polygon", env.object=T, spdf=F)
# The following warning message is OK.
# Warning message:
#   In `proj4string<-`(`*tmp*`, value = new("CRS", projargs = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) :
#   A new CRS was assigned to an object with an existing CRS:
#   +proj=longlat +ellps=WGS84
# without reprojecting.
# For reprojection, use function spTransform


##### Attach the Z values to the SpatialPolygons object to get a SpatialPolygonsDataFrame
ID <- sapply(slot(sp.obj, "polygons"), function(x) slot(x, "ID"))

sp.df <- SpatialPolygonsDataFrame(sp.obj, unique(dat[,c("PID", "Z")]), match.ID = "PID")


##### to plot it with ggplot
sp.plot <- fortify(sp.df, region = "PID")
sp.plot$PID <- sp.plot$id
sp.plot <- join(sp.plot, sp.df@data, by="PID")
ggplot() + geom_point(data=sp.plot, aes(long,lat, colour=Z))

