# This script gets some nice pictures of the spatial variability in effort on Georges Bank by
# looking at 5 regions of the bank that likely have shown how the fishery has changed the targeted areas over time...


yr = as.numeric(format(Sys.time(), "%Y")) -1# 
direct = "d:/r/"
# direct <- "f:/r/"
library(RColorBrewer)
library(PBSmapping)
library(R2jags)
library(VGAMdata)
library(ggplot2)
library(lattice)
require(maps)
require(maptools)
require(mapdata)
library(rgeos)
library(splancs)
require(INLA)
library(boot)
library(fields)
library(viridis)
library(reshape2)

source(paste(direct,"Assessment_fns/Survey_and_OSAC/gridPlot.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/maps/ScallopMap.r",sep=""))

# This is a little function to make colours semi-transparent, it is very nice for spatial plots with a lot of points...
addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# A way to plot our data spatially. This can be used to plot everywhere or used to plot a specific location
# This only works 
Scallop.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50),
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  par(mar=c(4,4,1,1))
  #cols <- terrain.colors(length(lvls)-1)
  ScallopMap(xlim=xlim,ylim=ylim,plot.boundries = T)
  image(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, axes=F,las=1,add=T, breaks=lvls, 
        axis.args=list(at=lvls,labels=round(exp(lvls))),
        col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) addPolys(add_boxes,angle = 45,density=8,col="black",lwd=.5)
  # addPolys()
} # end function

# If you don't have access to ScallopMap use this function, plots are the same, just slightly more limited on what you can do with the image plot...
Generic.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50), trans= "none",
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  #field.proj[is.na(field.proj)] <- 0
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  if(trans== "exp") arg.list <- list(at=lvls,labels=round(exp(lvls)))
  if(trans == "none") arg.list <- list(at=lvls,labels=lvls)
  par(mar=c(4,4,1,1))
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, 
             axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
             col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) addPolys(add_boxes,angle = 45,density=8,col="black",lwd=.5)
  lines(x=c(-67.4675,-65.6992),y=c(42.51889,40.45139),lwd=3,col=magma(1,alpha=0.3,begin=0)) # EEZ b/t Can and US
  # addPolys()
} # end function


# Get the GBa.SCboxes...
seedboxes <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),stringsAsFactors = F)
# Bring in the offshore fleet information
off.fleet <- read.csv(paste(direct,"Data/Offshore_Fleet.csv",sep=""),stringsAsFactors = F)
# GBa divided into quadrents...
GBa.quads <- read.csv(paste(direct,"Data/Framework/2018/GBa_quadrents.csv",sep=""),stringsAsFactors = F)

# The 5 regions of GBa...
GBa.west <- GBa.quads[GBa.quads$ID == "West",]
GBa.central <- GBa.quads[GBa.quads$ID == "Central",]
GBa.east <- GBa.quads[GBa.quads$ID == "East",]
GBa.SC <- GBa.quads[GBa.quads$ID == "SC",]
GBa.DS <- GBa.quads[GBa.quads$ID == "DS",]
GBa.core <- GBa.quads[GBa.quads$ID == "Large_core",]

# GBa west
GBa.west.poly <- Polygons(list( Polygon(cbind(GBa.west$X,GBa.west$Y))),"GBa.west")
GBa.west.poly.sp <-SpatialPolygons(list(GBa.west.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.west.X.range <- range(GBa.west.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.west.Y.range <- range(GBa.west.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# GBa central
GBa.central.poly <- Polygons(list( Polygon(cbind(GBa.central$X,GBa.central$Y))),"GBa.central")
GBa.central.poly.sp <-SpatialPolygons(list(GBa.central.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.central.X.range <- range(GBa.central.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.central.Y.range <- range(GBa.central.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# GBa east
GBa.east.poly <- Polygons(list( Polygon(cbind(GBa.east$X,GBa.east$Y))),"GBa.east")
GBa.east.poly.sp <-SpatialPolygons(list(GBa.east.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.east.X.range <- range(GBa.east.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.east.Y.range <- range(GBa.east.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# GBa South central
GBa.SC.poly <- Polygons(list( Polygon(cbind(GBa.SC$X,GBa.SC$Y))),"GBa.SC")
GBa.SC.poly.sp <-SpatialPolygons(list(GBa.SC.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.SC.X.range <- range(GBa.SC.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.SC.Y.range <- range(GBa.SC.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# GBA Deep south
GBa.DS.poly <- Polygons(list( Polygon(cbind(GBa.DS$X,GBa.DS$Y))),"GBa.DS")
GBa.DS.poly.sp <-SpatialPolygons(list(GBa.DS.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.DS.X.range <- range(GBa.DS.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.DS.Y.range <- range(GBa.DS.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])

# GBa core 
GBa.core.poly <- Polygons(list( Polygon(cbind(GBa.core$X,GBa.core$Y))),"GBa.core")
GBa.core.poly.sp <-SpatialPolygons(list(GBa.core.poly),proj4string = CRS("+proj=longlat +ellps=WGS84" ))
GBa.core.X.range <- range(GBa.core.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.core.Y.range <- range(GBa.core.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])

# Get the survey boundary for GBa
#Read4 Get the survey boundary polygons for all banks.
survey.bound.polys<-read.csv(paste(direct,"data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
bound.poly.surv <- as.PolySet(survey.bound.polys[survey.bound.polys$label=="GBa",],projection="LL")
# Now convert this to an object for sp and set this up and the boundary polygon for the mesh.
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)

#Intersect each "quadrent" with the survey boundary and make a PBS mapping object, I only use these for a plot of the different regions...
GBa.DS.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.DS.poly.sp)
GBa.SC.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.SC.poly.sp)
GBa.east.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.east.poly.sp)
GBa.west.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.west.poly.sp)
GBa.central.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.central.poly.sp)
GBa.core.bound.sp <- gIntersection(bound.poly.surv.sp,GBa.core.poly.sp)

# Get the fishery data
logs_and_fish(loc="offshore",year = 1981:2017,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
GBa.fish.dat<-merge(new.log.dat,old.log.dat,all=T)
GBa.fish.dat <- GBa.fish.dat[GBa.fish.dat$bank == "GBa",]
# Get rid of any crap data...
GBa.fish.dat <- GBa.fish.dat[!is.na(GBa.fish.dat$lon) & !is.na(GBa.fish.dat$lat),]
GBa.fish.dat <- GBa.fish.dat[GBa.fish.dat$lon < 0 & GBa.fish.dat$lat > 0,]
GBa.fish.dat$ID<-1:nrow(GBa.fish.dat)
# Get the coordinates for the data
coordinates(GBa.fish.dat) <- ~lon+lat
proj4string(GBa.fish.dat) <- CRS("+proj=longlat +ellps=WGS84" )
# Now make any vessel with ASM's...ASM's
ASMs <- off.fleet[!is.na(off.fleet$ASM_date),]
for(i in 1:nrow(ASMs)) GBa.fish.dat$fleet[GBa.fish.dat$vrnum == ASMs$ID[i] & GBa.fish.dat$date >= ASMs$ASM_date[i]] <- "ASM"
GBa.fish.dat$fleet_fac <- as.numeric(as.factor(GBa.fish.dat$fleet))


###############  Now pull out both of the seeboxes...
# This picks out the points that are within the GBa.SC polygon
GBa.west.pts <- over(GBa.fish.dat,GBa.west.poly.sp)
GBa.central.pts <- over(GBa.fish.dat,GBa.central.poly.sp)
GBa.east.pts <- over(GBa.fish.dat,GBa.east.poly.sp)
GBa.SC.pts <- over(GBa.fish.dat,GBa.SC.poly.sp)
GBa.DS.pts <- over(GBa.fish.dat,GBa.DS.poly.sp)
GBa.core.pts <- over(GBa.fish.dat,GBa.core.poly.sp)

# Get the overall fishery info
GBa.overall.catch <- aggregate(pro.repwt~year,GBa.fish.dat,sum)
GBa.overall.effort <-aggregate(hm~year,GBa.fish.dat,sum)
GBa.overall.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.fish.dat,FUN=sum)

# This get the fishery data within each piece, this should trigger an error at the last step if there was no fishing inside a box...
GBa.west.fish.dat <- GBa.fish.dat[which(GBa.west.pts==1),]
GBa.west.catch <- aggregate(pro.repwt~year,GBa.west.fish.dat,sum)
GBa.west.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.west.catch$pro.repwt/GBa.overall.catch$pro.repwt)
GBa.west.effort <- aggregate(hm~year,GBa.west.fish.dat,sum)
GBa.west.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.west.fish.dat,FUN=sum)
GBa.west.by.fleet$region <- "West"

GBa.central.fish.dat <- GBa.fish.dat[which(GBa.central.pts==1),]
GBa.central.catch <-  aggregate(pro.repwt~year,GBa.central.fish.dat,sum)
GBa.central.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.central.catch$pro.repwt/GBa.overall.catch$pro.repwt)
GBa.central.effort <- aggregate(hm~year,GBa.central.fish.dat,sum)
GBa.central.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.central.fish.dat,FUN=sum)
GBa.central.by.fleet$region <- "Central"

GBa.east.fish.dat <- GBa.fish.dat[which(GBa.east.pts==1),]
GBa.east.catch <-  aggregate(pro.repwt~year,GBa.east.fish.dat,sum)
GBa.east.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.east.catch$pro.repwt/GBa.overall.catch$pro.repwt)
GBa.east.effort <- aggregate(hm~year,GBa.east.fish.dat,sum)
GBa.east.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.east.fish.dat,FUN=sum)
GBa.east.by.fleet$region <- "East"
# South central
GBa.SC.fish.dat <- GBa.fish.dat[which(GBa.SC.pts==1),]
GBa.SC.catch <-  aggregate(pro.repwt~year,GBa.SC.fish.dat,sum)
GBa.SC.effort <- aggregate(hm~year,GBa.SC.fish.dat,sum)
GBa.SC.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.SC.fish.dat,FUN=sum)
GBa.SC.by.fleet$region <- "SC"
GBa.SC.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.SC.catch$pro.repwt/GBa.overall.catch$pro.repwt)
# The deep south
GBa.DS.fish.dat <- GBa.fish.dat[which(GBa.DS.pts==1),]
GBa.DS.catch <- aggregate(pro.repwt~year,GBa.DS.fish.dat,sum)
GBa.DS.effort <-aggregate(hm~year,GBa.DS.fish.dat,sum)
# Need to fill in data for 2007...
GBa.DS.catch <- rbind(GBa.DS.catch,c(2007,0))
GBa.DS.catch <- GBa.DS.catch[order(GBa.DS.catch$year),]
GBa.DS.effort <- rbind(GBa.DS.effort,c(2007,0))
GBa.DS.effort <- GBa.DS.effort[order(GBa.DS.effort$year),]
GBa.DS.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.DS.fish.dat,FUN=sum)
GBa.DS.by.fleet$region <- "DS"
GBa.DS.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.DS.catch$pro.repwt/GBa.overall.catch$pro.repwt)

# Focus on the core area
GBa.core.fish.dat <- GBa.fish.dat[which(GBa.core.pts==1),]
GBa.core.catch <-  aggregate(pro.repwt~year,GBa.core.fish.dat,sum)
GBa.core.effort <- aggregate(hm~year,GBa.core.fish.dat,sum)
GBa.core.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.core.fish.dat,FUN=sum)
GBa.core.by.fleet$region <- "Core"
GBa.core.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.core.catch$pro.repwt/GBa.overall.catch$pro.repwt)

# Focus on the non-core area
GBa.non.core.fish.dat <- GBa.fish.dat[which(is.na(GBa.core.pts)),]
GBa.non.core.catch <-  aggregate(pro.repwt~year,GBa.non.core.fish.dat,sum)
GBa.non.core.effort <- aggregate(hm~year,GBa.non.core.fish.dat,sum)
GBa.non.core.by.fleet <- aggregate(pro.repwt ~ fleet+year,GBa.non.core.fish.dat,FUN=sum)
GBa.non.core.by.fleet$region <- "Non Core"
GBa.non.core.catch.per <- data.frame(year = GBa.overall.catch$year,prop.catch = GBa.non.core.catch$pro.repwt/GBa.overall.catch$pro.repwt)


# How much catch was coming out of this core area in different time periods...
core.post.2000 <- mean(GBa.core.catch$pro.repwt[GBa.core.catch$year >= 2000])/1000 # almost 3200 tonnes
core.pre.2000 <- mean(GBa.core.catch$pro.repwt[GBa.core.catch$year <2000])/1000 # used to be 1800 tonnes
(core.post.2000-core.pre.2000)/core.pre.2000
mean(GBa.core.catch.per$prop.catch[GBa.core.catch$year >= 2000]) # 69% of catch
mean(GBa.core.catch.per$prop.catch[GBa.core.catch$year < 2000])

# Split into just north and south
GBa.north.catch <- data.frame(year=GBa.west.catch$year,catch = GBa.west.catch$pro.repwt/1000 + GBa.east.catch$pro.repwt/1000 +
                                                               GBa.central.catch$pro.repwt/1000,region = "North")
GBa.south.catch <- data.frame(year=GBa.SC.catch$year,catch = GBa.SC.catch$pro.repwt/1000 + GBa.DS.catch$pro.repwt/1000 ,region = "South")
GBa.ns.catch <- rbind(GBa.north.catch,GBa.south.catch)

# Split into just north and south for the percent caught
GBa.north.catch.per <- data.frame(year=GBa.west.catch.per$year,prop.catch = GBa.west.catch.per$prop.catch + GBa.east.catch.per$prop.catch +
                                GBa.central.catch.per$prop.catch,region = "North")
GBa.south.catch.per <- data.frame(year=GBa.DS.catch.per$year,prop.catch = GBa.DS.catch.per$prop.catch + 
                                                                     GBa.DS.catch.per$prop.catch ,region = "South")
GBa.ns.catch.per <- rbind(GBa.north.catch.per,GBa.south.catch.per)
# Now effort...
GBa.north.effort <- data.frame(year=GBa.west.effort$year,effort = GBa.west.effort$hm + GBa.east.effort$hm +
                                    GBa.central.effort$hm,region = "North")
GBa.south.effort <- data.frame(year=GBa.DS.effort$year,effort = GBa.DS.effort$hm + 
                                    GBa.DS.effort$hm ,region = "South")
GBa.ns.effort <- rbind(GBa.north.effort,GBa.south.effort)
# Now effort...



# Now put the catch and effort into objects so we can easily plot them...
GBa.catch.by.region <- data.frame(year = GBa.west.catch$year, West = GBa.west.catch$pro.repwt/1000,
                                  Central = GBa.central.catch$pro.repwt/1000,East = GBa.east.catch$pro.repwt/1000, 
                                  SC = GBa.SC.catch$pro.repwt/1000, DS = GBa.DS.catch$pro.repwt/1000)
GBa.catch.by.region <- melt(GBa.catch.by.region,id.vars="year",variable.name = "region",value.name = "catch")

GBa.effort.by.region <- data.frame(year = GBa.west.effort$year, West = GBa.west.effort$hm/1000,
                                   Central = GBa.central.effort$hm/1000,East = GBa.east.effort$hm/1000, 
                                  SC = GBa.SC.effort$hm/1000, DS = GBa.DS.effort$hm/1000)
GBa.effort.by.region <- melt(GBa.effort.by.region,id.vars="year",variable.name = "region",value.name = "effort")

GBa.per.catch.by.region <- data.frame(year = GBa.west.catch.per$year, West = GBa.west.catch.per$prop.catch,
                                  Central = GBa.central.catch.per$prop.catch, East = GBa.east.catch.per$prop.catch, 
                                  SC = GBa.SC.catch.per$prop.catch, DS = GBa.DS.catch.per$prop.catch)
GBa.per.catch.by.region <- melt(GBa.per.catch.by.region,id.vars="year",variable.name = "region",value.name = "prop.catch")

GBa.catch.by.region <- data.frame(year = GBa.west.catch$year, West = GBa.west.catch$pro.repwt/1000,
                                  Central = GBa.central.catch$pro.repwt/1000,East = GBa.east.catch$pro.repwt/1000, 
                                  SC = GBa.SC.catch$pro.repwt/1000, DS = GBa.DS.catch$pro.repwt/1000)
GBa.catch.by.region <- melt(GBa.catch.by.region,id.vars="year",variable.name = "region",value.name = "catch")

# Put the Fleets together...
GBa.fleet.catch <- rbind(GBa.west.by.fleet, GBa.central.by.fleet, GBa.east.by.fleet,  GBa.SC.by.fleet,GBa.DS.by.fleet)
# To get this in the order I want them for ggplot..
GBa.fleet.catch$region <- as.factor(GBa.fleet.catch$region)
GBa.fleet.catch$region <- factor(GBa.fleet.catch$region,levels(GBa.fleet.catch$region)[c(5,1,3,4,2)])

# now plot the time series of catch and effort by region
windows(11,11)
ggplot(GBa.catch.by.region,aes(year,catch)) + geom_line(size=1) + facet_wrap(~region) + theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank())  +xlab("") + ylab("Catch (tonnes)") + scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_5_quadrents.png"),width=11,height = 11,units = "in")
# Here's just north vs south comparison
windows(11,11)
ggplot(GBa.ns.catch,aes(year,catch,colour=region)) + geom_line(size=1) + theme_bw(base_size = 18) + theme(panel.grid=element_blank()) +xlab("") + ylab("Catch (tonnes)")
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_north_vs_south.png"),width=11,height = 11,units = "in")

# North south using proortions
windows(11,11)
ggplot(GBa.ns.catch.per,aes(year,prop.catch,colour=region)) + geom_line(size=1) + theme_bw(base_size = 18) + theme(panel.grid=element_blank()) +xlab("") + ylab("Proportion of total catch")
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_proportion_ts_north_vs_south.png"),width=11,height = 11,units = "in")

# Now look at effort
windows(11,11)
ggplot(GBa.ns.effort,aes(year,effort,colour=region)) + geom_line(size=1) + theme_bw(base_size = 18) + theme(panel.grid=element_blank(),axis.title.y = element_text(angle=0,vjust=0.5)) +xlab("") + 
  ylab(expression(paste("Effort:",bgroup("(",frac(kg,hm)   ,")"))))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/effort_ts_north_vs_south.png"),width=11,height = 11,units = "in")


windows(11,11)
ggplot(GBa.effort.by.region,aes(year,effort)) + geom_line(size=1) + facet_wrap(~region)  + theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank(),axis.title.y = element_text(angle=0,vjust=0.5)) +xlab("") + 
  ylab(expression(paste("Effort:",bgroup("(",frac(kg,hm)   ,")")))) + scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Effort_ts_5_quadrents.png"),width=11,height = 11,units = "in")


# How about the percet caught...
windows(11,11)
ggplot(GBa.per.catch.by.region,aes(year,prop.catch)) + facet_wrap(~region) + geom_line(size=1) + theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Proportion of total catch")+ scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_proportion_ts_5_quadrents.png"),width=11,height = 11,units = "in")


# Look at catch by fleet
windows(11,11)
ggplot(GBa.fleet.catch,aes(year,pro.repwt/1000,colour=fleet)) + geom_line(size=1)+ facet_wrap(~region) + theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Catch (tonnes)") + scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_by_fleet_5_quadrents.png"),width=11,height = 11,units = "in")

# Since 2000
windows(11,11)
ggplot(GBa.fleet.catch,aes(year,pro.repwt/1000,colour=fleet)) + geom_line(size=1)+ facet_wrap(~region)  +  theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Catch (tonnes)")+ xlim(c(2000,2017)) 
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_by_fleet_since_2000_5_quadrents.png"),width=11,height = 11,units = "in")

# Some plots of the area I defined as the core area...
windows(11,11)
ggplot(GBa.core.catch,aes(year,pro.repwt/1000)) + geom_line(size=1)+  theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Catch (tonnes)")+ scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_core.png"),width=11,height = 11,units = "in")

windows(11,11)
ggplot(GBa.core.catch.per,aes(year,prop.catch)) + geom_line(size=1)+  theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Proportion of total Catch")+ scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_proportion_ts_core.png"),width=11,height = 11,units = "in")

windows(11,11)
ggplot(GBa.core.by.fleet,aes(year,pro.repwt/1000,colour=fleet)) + geom_line(size=1)+  theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Catch(tonnes)")+ scale_x_continuous(breaks = seq(1985,2015,by=10))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_by_fleet_core.png"),width=11,height = 11,units = "in")


windows(11,11)
ggplot(GBa.core.by.fleet,aes(year,pro.repwt/1000,colour=fleet)) + geom_line(size=1)+  theme_bw(base_size = 18) + 
  theme(panel.grid=element_blank()) +xlab("") + ylab("Catch(tonnes)")+ xlim(c(2000,2018))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Catch_ts_by_fleet_since_2000_core.png"),width=11,height = 11,units = "in")


windows(11,11)
ggplot(GBa.core.effort,aes(year,hm)) + geom_line(size=1)+  geom_line(size=1) + theme_bw(base_size = 18) +  
  theme(panel.grid=element_blank(),axis.title.y = element_text(angle=0,vjust=0.5)) +xlab("") + ylab(expression(paste("Effort:",bgroup("(",frac(kg,hm)   ,")"))))
ggsave(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Effort_ts_core.png"),width=11,height = 11,units = "in")



# A plot of the different regions...
windows(11,11)
png(paste0(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Map_of_quadrents.png"),width=11,height = 11,units = "in",res=920)
ScallopMap("GBa",plot.bathy = T,plot.boundries = T)
plot(GBa.west.bound.sp,add=T,col = addalpha("grey",0.2))
plot(GBa.central.bound.sp,add=T,col = addalpha("red",0.2))
plot(GBa.east.bound.sp,add=T,col = addalpha("blue",0.2))
plot(GBa.SC.bound.sp,add=T,col = addalpha("grey",0.5))
plot(GBa.DS.bound.sp,add=T,col = addalpha("green",0.2))
dev.off()

