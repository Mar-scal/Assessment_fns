# This script gets some nice pictures of the spatial variability in effort on Georges Bank and a couple historic seed boxes


yr = as.numeric(format(Sys.time(), "%Y")) -1# 
direct = "d:/r/"
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


# Get the seedboxes...
seedboxes <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),stringsAsFactors = F)
# Bring in the offshore fleet information
off.fleet <- read.csv(paste(direct,"Data/Offshore_Fleet.csv",sep=""),stringsAsFactors = F)

# Current seedboxes...
sb.current <- seedboxes[seedboxes$Active=="Yes" & seedboxes$Bank == "GBa",]
seed.new <- sb.current[sb.current$ID =="C4-122016",]
gout.new <- sb.current[sb.current$ID =="C5-122016",]
exp.new <- sb.current[sb.current$ID =="C6-122016",]
# New seed
seed.new.poly <- Polygons(list( Polygon(cbind(seed.new$X,seed.new$Y))),"seed.new")
seed.new.poly.sp <-SpatialPolygons(list(seed.new.poly))
seed.new.X.range <- range(seed.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
seed.new.Y.range <- range(seed.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# New growout
gout.new.poly <- Polygons(list( Polygon(cbind(gout.new$X,gout.new$Y))),"gout.new")
gout.new.poly.sp <-SpatialPolygons(list(gout.new.poly))
gout.new.X.range <- range(gout.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
gout.new.Y.range <- range(gout.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# New experimental box
exp.new.poly <- Polygons(list( Polygon(cbind(exp.new$X,exp.new$Y))),"exp.new")
exp.new.poly.sp <-SpatialPolygons(list(exp.new.poly))
exp.new.X.range <- range(exp.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
exp.new.Y.range <- range(exp.new.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# Historic Seedboxes that may be interesting...
seed <- seedboxes[seedboxes$ID =="Seed box (2012 modified)",]
peanut <- seedboxes[seedboxes$ID =="Peanut (2010)",]
# Make the seed into a spatial object
seed.poly <- Polygons(list( Polygon(cbind(seed$X,seed$Y))),"seed")
seed.poly.sp <-SpatialPolygons(list(seed.poly))
seed.X.range <- range(seed.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
seed.Y.range <- range(seed.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
peanut.poly <- Polygons(list( Polygon(cbind(peanut$X,peanut$Y))),"peanut")
peanut.poly.sp <-SpatialPolygons(list(peanut.poly))
peanut.X.range <- range(peanut.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
peanut.Y.range <- range(peanut.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])

# Get the survey boundary for GBa
#Read4 Get the survey boundary polygons for all banks.
survey.bound.polys<-read.csv(paste(direct,"data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
bound.poly.surv <- as.PolySet(survey.bound.polys[survey.bound.polys$label=="GBa",],projection="LL")
# Now convert this to an object for sp and set this up and the boundary polygon for the mesh.
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)

# Get the fishery data
logs_and_fish(loc="offshore",year = 2017:2017,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
GBa.fish.dat <- new.log.dat[new.log.dat$bank == "GBa",]

tmp <- GBa.fish.dat[which(GBa.fish.dat$kg.hm > 134),]
#plot(tmp)
# Get the coordinates for the data
coordinates(GBa.fish.dat) <- ~lon+lat

# Now make any vessel with ASM's...ASM's
ASMs <- off.fleet[!is.na(off.fleet$ASM_date),]
for(i in 1:nrow(ASMs)) GBa.fish.dat$fleet[GBa.fish.dat$vrnum == ASMs$ID[i] & GBa.fish.dat$date >= ASMs$ASM_date[i]] <- "ASM"
GBa.fish.dat$fleet_fac <- as.numeric(as.factor(GBa.fish.dat$fleet))


###############  Now pull out both of the seeboxes...
# This picks out the points that are within the seed polygon
seed.new.pts <- over(GBa.fish.dat,seed.new.poly.sp)
gout.new.pts <- over(GBa.fish.dat,seed.new.poly.sp)
exp.new.pts <- over(GBa.fish.dat,seed.new.poly.sp)
seed.pts <- over(GBa.fish.dat,seed.poly.sp)
peanut.pts <- over(GBa.fish.dat,peanut.poly.sp)

# This get the fishery data within each piece, this should trigger an error at the last step if there was no fishing inside a box...
seed.new.fish.dat <- GBa.fish.dat[which(seed.new.pts==1),]
seed.new.catch <- sum(seed.new.fish.dat$pro.repwt)
seed.new.effort <- sum(seed.new.fish.dat$hm)
seed.new.by.fleet <- aggregate(pro.repwt ~ fleet,seed.new.fish.dat,FUN=sum)

gout.new.fish.dat <- GBa.fish.dat[which(gout.new.pts==1),]
gout.new.catch <- sum(gout.new.fish.dat$pro.repwt)
gout.new.effort <- sum(gout.new.fish.dat$hm)
gout.new.by.fleet <- aggregate(pro.repwt ~ fleet,gout.new.fish.dat,FUN=sum)

exp.new.fish.dat <- GBa.fish.dat[which(exp.new.pts==1),]
exp.new.catch <- sum(exp.new.fish.dat$pro.repwt)
exp.new.effort <- sum(exp.new.fish.dat$hm)
exp.new.by.fleet <- aggregate(pro.repwt ~ fleet,exp.new.fish.dat,FUN=sum)

# The old seedbox and peanut data...
seed.fish.dat <- GBa.fish.dat[which(seed.pts==1),]
seed.catch <- sum(seed.fish.dat$pro.repwt)
seed.effort <- sum(seed.fish.dat$hm)
seed.by.fleet <- aggregate(pro.repwt ~ fleet,seed.fish.dat,FUN=sum)

peanut.fish.dat <- GBa.fish.dat[which(peanut.pts==1),]
peanut.catch <- sum(peanut.fish.dat$pro.repwt)
peanut.effort <- sum(peanut.fish.dat$hm)
peanut.by.fleet <- aggregate(pro.repwt ~ fleet,peanut.fish.dat,FUN=sum)

# Now you need to pick what you want to consider outside, this is considering the old peanut and old seedbox as the "inside" area...
Outside.fish.dat <- GBa.fish.dat[is.na(seed.pts) & is.na(peanut.pts),]
Outside.catch <- sum(Outside.fish.dat$pro.repwt)
Outside.effort <- sum(Outside.fish.dat$hm)
Outside.by.fleet <- aggregate(pro.repwt ~ fleet,Outside.fish.dat,FUN=sum)

# here is where they were fishing within each box
plot.col <- addalpha(c("blue","red","black"),0.25)
png(paste(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Fishery_on_GBa_2017.png",sep=""),units="in",width = 11,
    height = 8.5,res=420,bg = "transparent")
ScallopMap("GBa")
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="WF"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="WF"],pch="W",cex=0.4,col=plot.col[1])
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="FT"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="FT"],pch="F",cex=0.4,col=plot.col[2])
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="ASM"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="ASM"],pch="S",cex=0.4,col=plot.col[3])
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)
dev.off()

# Same figure but going for a 3 panel version of it...
png(paste(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/Fishery_on_GBa_2017_by_fleet.png",sep=""),units="in",width = 15,
    height = 8.5,res=420,bg = "transparent")
par(mfrow=c(1,3))
ScallopMap("GBa")
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="WF"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="WF"],pch="W",cex=0.4,col=plot.col[1])
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)
ScallopMap("GBa")
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="FT"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="FT"],pch="F",cex=0.4,col=plot.col[2])
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)
ScallopMap("GBa")
points(GBa.fish.dat$lon[GBa.fish.dat$fleet =="ASM"],GBa.fish.dat$lat[GBa.fish.dat$fleet =="ASM"],pch="S",cex=0.4,col=plot.col[3])
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)
dev.off()

##################################################  SPATIAL MODELING OF FISHERY  ##################################################  SPATIAL MODELING OF FISHERY
# Let's make an INLA model looking at the catch rates on the bank, should be doable...
loc <- cbind(GBa.fish.dat$lon, GBa.fish.dat$lat)

# Try to build a MESH...
bound <- inla.nonconvex.hull(loc, 0.1, 0.1)
prj4s <- CRS("+proj=longlat +datum=WGS84")
bound.sp <- SpatialPolygons(list(Polygons(list(Polygon(bound$loc, FALSE)), '0')), proj4=prj4s)
# Build the mesh, for our purposes I'm hopeful this should do the trick, the offset makes the area a bit larger so the main predictions 
# should cover our entire fishery area.  Just a mattter of playing around with the options to get a good looking mesh, 
mesh <- inla.mesh.2d(loc, 
                        max.edge=0.05, cutoff=0.01,
                        boundary=inla.sp2segment(bound.sp))
mesh$n

ScallopMap("GBa")
plot(mesh,add=T)
points(GBa.fish.dat$lon,GBa.fish.dat$lat,pch=19,cex=0.5)


# Now make the A matrix, combination of your mesh and locations of our data
#loc <- cbind(df$SLO,df$SLA)
A <- inla.spde.make.A(mesh, loc)
A.fleet <- inla.spde.make.A(mesh,loc,group = GBa.fish.dat$fleet_fac)

dim(A)


## 3. Observation Likelihood, 
family1 = "gamma"
control.family1 <- list(control.link = list(model="log"))
GBa.fish.dat$i <- 1:nrow(GBa.fish.dat)

#######################################SPATIAL MODEL###############################################################
## Now let's try a spatial model.  This model is for yellowtail founder
### SPATIAL model
spde <- inla.spde2.pcmatern(mesh,    
                            prior.sigma=c(1,0.5),
                            prior.range=c(1,0.5))

a0 <- 1 # intercept
s <- 1:spde$n.spde # Size of our SPDE model (I think this is our sparse matrix but I might be dumb)
#Now because I h ave a group with the A.fleet piece I need to account for this with an index, "s" is the name of this spatial field, used later in the model...
# Note that the fleet index is 3 times longer than the second dimension of the A matrix without the group.
fleet.index <- inla.spde.make.index('s', n.spde=spde$n.spde,
                                    n.group=length(unique(GBa.fish.dat$fleet_fac)))

pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))

# A stack for the cpue
stk.cpue = inla.stack(tag="est",
                      data=list(y = GBa.fish.dat$kg.hm, link=1L),
                      effects=list(a0 = rep(1, nrow(GBa.fish.dat)), s = 1:spde$n.spde),
                      A = list(1, A))
# This model is a unique field for each fleet, but allows for the fleet fixed effect as well...
stk.cpue.fleet <- inla.stack(tag="est.cpue.fleet",data=list(y = GBa.fish.dat$kg.hm, link=2L),
                      effects=list(data.frame(a0=rep(1, nrow(GBa.fish.dat)),fleet = GBa.fish.dat$fleet),
                                   fleet.index), #s = 1:spde.ps$n.spde),
                      A = list(1, A.fleet))


# A stack for the effort
stk.effort = inla.stack(tag="est",
                        data=list(y = GBa.fish.dat$hm, link=1L),
                        effects=list(a0 = rep(1, nrow(GBa.fish.dat)), s = 1:spde$n.spde),
                        A = list(1, A))
stk.effort.fleet <- inla.stack(tag="est.effort.fleet",data=list(y = GBa.fish.dat$hm, link=2L),
                             effects=list(data.frame(a0=rep(1, nrow(GBa.fish.dat)),fleet = GBa.fish.dat$fleet),
                                          fleet.index), #s = 1:spde.ps$n.spde),
                             A = list(1, A.fleet))
# A stack for the catch
stk.catch = inla.stack(tag="est",
                       data=list(y = GBa.fish.dat$pro.repwt, link=1L),
                       effects=list(a0 = rep(1, nrow(GBa.fish.dat)), s = 1:spde$n.spde),
                       A = list(1, A))
stk.catch.fleet <- inla.stack(tag="est.catch.fleet",data=list(y = GBa.fish.dat$pro.repwt, link=2L),
                               effects=list(data.frame(a0=rep(1, nrow(GBa.fish.dat)),fleet = GBa.fish.dat$fleet),
                                            fleet.index), #s = 1:spde.ps$n.spde),
                               A = list(1, A.fleet))

# As soon as you make a spatial model make your own intercept.  Here is
formula3 <- y ~ 0 + a0 + f(s, model=spde)
formula4 <- y ~ 0 + fleet +  f(s, model=spde,group =s.group) # Does this model make sense, it gets a mean field value and then the deviations from that field, I think!

# Let's giver, make the spatial model.
cpue.spatial.model <- inla(formula3, family=family1, data = inla.stack.data(stk.cpue),
                           control.predictor=list(A=inla.stack.A(stk.cpue),link=link, compute=TRUE),
                           control.compute=list(cpo=TRUE))

cpue.spatial.fleet<- inla(formula4, family=family1, data = inla.stack.data(stk.cpue.fleet),
                          control.predictor=list(A=inla.stack.A(stk.cpue.fleet),link=1L, compute=T),
                          control.compute=list(cpo=TRUE))


effort.spatial.model<- inla(formula3, family=family1, data = inla.stack.data(stk.effort),
                            control.predictor=list(A=inla.stack.A(stk.effort),link=link, compute=TRUE),
                            control.compute=list(cpo=TRUE))
effort.spatial.fleet<- inla(formula4, family=family1, data = inla.stack.data(stk.effort.fleet),
                          control.predictor=list(A=inla.stack.A(stk.effort.fleet),link=1L, compute=T),
                          control.compute=list(cpo=TRUE))

catch.spatial.model<- inla(formula3, family=family1, data = inla.stack.data(stk.catch),
                           control.predictor=list(A=inla.stack.A(stk.catch),link=link, compute=TRUE),
                           control.compute=list(cpo=TRUE))
catch.spatial.fleet<- inla(formula4, family=family1, data = inla.stack.data(stk.catch.fleet),
                            control.predictor=list(A=inla.stack.A(stk.catch.fleet),link=1L, compute=T),
                            control.compute=list(cpo=TRUE))


# The sptial fields...
# For the models without a fleet effect this is simple...
cpue.spatial <- exp(cpue.spatial.model$summary.random$s$mean+cpue.spatial.model$summary.fixed$mean)
effort.spatial <- exp(effort.spatial.model$summary.random$s$mean+effort.spatial.model$summary.fixed$mean)
catch.spatial <- exp(catch.spatial.model$summary.random$s$mean+catch.spatial.model$summary.fixed$mean)

# For the fields with a fleet effect it is trickier...

cpue.fleet.res <- NULL
catch.fleet.res <- NULL
effort.fleet.res <- NULL
fleets <- sort(unique(GBa.fish.dat$fleet))
for(i in 1:length(fleets))
{
  if(i ==1 ) 
  {
    cpue.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(cpue.spatial.fleet$summary.random$s$mean[1:ncol(A)] + 
                                                           cpue.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
    catch.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(catch.spatial.fleet$summary.random$s$mean[1:ncol(A)] + 
                                                                 catch.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
    effort.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(effort.spatial.fleet$summary.random$s$mean[1:ncol(A)] + 
                                                                  effort.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
  } # End if i =1
  
  if(i >1 ) 
  {
    cpue.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(cpue.spatial.fleet$summary.random$s$mean[((i-1)*ncol(A)+1):(i*(ncol(A)))] + 
                                                           cpue.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
    catch.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(catch.spatial.fleet$summary.random$s$mean[((i-1)*ncol(A)+1):(i*(ncol(A)))] + 
                                                                 catch.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
    effort.fleet.res[[fleets[i]]] <- data.frame(mean.field = exp(effort.spatial.fleet$summary.random$s$mean[((i-1)*ncol(A)+1):(i*(ncol(A)))] + 
                                                                  effort.spatial.fleet$summary.fixed$mean[i]),fleet = fleets[i])
  } # end if i > 1
} # end (for i in 1:length(fleets))
cpue.fleet.res <- do.call("rbind",cpue.fleet.res)
catch.fleet.res <- do.call("rbind",catch.fleet.res)
effort.fleet.res <- do.call("rbind",effort.fleet.res)



# This should get me the mean field for each fleet if I've done this correctly....
proj = inla.mesh.projector(mesh, xlim = range(GBa.fish.dat$lon) ,ylim = range(GBa.fish.dat$lat), dims=c(500,500))
# SO this is the random field for each fleet...

# SO this is the random field for each fleet...
cpue.fleet.mean <- lapply(1:length(fleets), function(j) {
  x <- inla.mesh.project(
    proj, field=cpue.spatial.fleet$summary.random$s$mean[
      (j-1)*spde$n.spde + 1:spde$n.spde])
  x
})
catch.fleet.mean <- lapply(1:length(fleets), function(j) {
  x <- inla.mesh.project(
    proj, field=catch.spatial.fleet$summary.random$s$mean[
      (j-1)*spde$n.spde + 1:spde$n.spde])
  x
})
effort.fleet.mean <- lapply(1:length(fleets), function(j) {
  x <- inla.mesh.project(
    proj, field=effort.spatial.fleet$summary.random$s$mean[
      (j-1)*spde$n.spde + 1:spde$n.spde])
  x
})

# The spatial meat cound is this random field + the intercept (a0)
cpue.spatial.mean  <- lapply(1:length(fleets), function(j) {exp(cpue.fleet.mean[[j]] + cpue.spatial.fleet$summary.fixed$mean[j])})
catch.spatial.mean  <- lapply(1:length(fleets), function(j) {exp(catch.fleet.mean[[j]] + catch.spatial.fleet$summary.fixed$mean[j])})
effort.spatial.mean  <- lapply(1:length(fleets), function(j) {exp(effort.fleet.mean[[j]] + effort.spatial.fleet$summary.fixed$mean[j])})



### function to extract the -sum(log(CPO)).  CPO, computed predictive order (or something)
# a cross validation method similar to leave one out, better than DIC or other measures, probability of predicting you
# data point.  A CPO of 0 means you completely miss your estiamte.  If INLA fails to compute the CPO
# That is a good warning that you don't have a good fit (I believe), underlying assumptoins aren't met.
# Lower is better for the below calculations.
#Conditional Predictive Ordinate (CPO) is the density of the observed value of yi within the out-of-sample (y???i) posterior predictive distribution. 
#A small CPO value associated with an observation suggests that this observation is unlikely (or surprising) in light of the model, priors and 
#other data in the model. In addition, the sum of the CPO values (or alternatively, the negative of the mean natural logarithm of the CPO values) 
# is a measure of fit.

fcpo <- function(m, id)
  -sum(log(m$cpo$cpo[id]), na.rm=TRUE) # Good to log this because of distributional reasons.

cpue.base.id <- inla.stack.index(stk.cpue, 'est')$data
cpue.fleet.id <- inla.stack.index(stk.cpue.fleet, 'est.cpue.fleet')$data
# effort data
effort.base.id <- inla.stack.index(stk.effort, 'est')$data
effort.fleet.id <- inla.stack.index(stk.effort.fleet, 'est.effort.fleet')$data
# catch data
catch.base.id <- inla.stack.index(stk.catch, 'est')$data
catch.fleet.id <- inla.stack.index(stk.catch.fleet, 'est.catch.fleet')$data

# We can see that including the spatial piece is useful, pretty large across the board improvement
c(fcpo(cpue.spatial.model, cpue.base.id),fcpo(cpue.spatial.fleet,cpue.fleet.id)) # The model with the fleet as a factor is slightly worse than the fleet as a field mod.
c(fcpo(effort.spatial.model, effort.base.id),fcpo(effort.spatial.fleet,effort.fleet.id)) # The model with the fleet as a factor is slightly worse than the fleet as a field mod.
c(fcpo(catch.spatial.model, catch.base.id),fcpo(catch.spatial.fleet,catch.fleet.id)) # The model with the fleet as a factor is slightly worse than the fleet as a field mod.

# We can see that including the spatial piece is useful, pretty large (positive values mean the first model is better)
hist(log(cpue.spatial.fleet$cpo$cpo[cpue.fleet.id]) - log(cpue.spatial.model$cpo$cpo[cpue.base.id]),breaks=2000,xlim=c(-0.5,0.5))
hist(log(effort.spatial.fleet$cpo$cpo[effort.fleet.id]) - log(effort.spatial.model$cpo$cpo[effort.base.id]),breaks=2000,xlim=c(-0.5,0.5))
hist(log(catch.spatial.fleet$cpo$cpo[effort.fleet.id]) - log(catch.spatial.model$cpo$cpo[effort.base.id]),breaks=2000,xlim=c(-0.5,0.5))

#save.image(paste0(direct,"Data/Framework/2018/GBa_box_model/Spatial_cpue_catch_effort_results.RData"))
load(paste0(direct,"Data/Framework/2018/GBa_box_model/Spatial_cpue_catch_effort_results.RData"))

#########  Now some figure showing what we are seeing, first 3 figures are plots that use the spatial field alone, no informatin about the fleets in here...

# Here's the 3 models.... First up is CPUE....
mins <- floor(min(range(cpue.spatial,na.rm=T)))
maxs <- ceiling(max(range(cpue.spatial,na.rm=T)))
lvls <- c(5,10,15,20,25,30,40,50,75,250)
col.rmp <- rev(magma(length(lvls-1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
leg.col <- addalpha(colorRampPalette(col.rmp,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8)

windows(8,8)
Scallop.plot.field(cpue.spatial, mesh,xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                   lvls=lvls,colors = col.rmp)
# Add the survey locations
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "FT"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "FT"],pch="F",cex=0.1,col=alpha("red",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "WF"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "WF"],pch="W",cex=0.1,col=alpha("blue",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "ASM"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "ASM"],pch="S",cex=0.1,col=alpha("white",alpha=0.5))
legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)


# Next we'll do the catch model...
mins <- floor(min(range(catch.spatial,na.rm=T)))
maxs <- ceiling(max(range(catch.spatial,na.rm=T)))
lvls <- c(200,500,750,1000,1250,1500,1750)
col.rmp <- rev(magma(length(lvls-1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
leg.col <- addalpha(colorRampPalette(col.rmp,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8)

windows(8,8)
Scallop.plot.field(catch.spatial, mesh,xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                   lvls=lvls,colors = col.rmp)
# Add the survey locations
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "FT"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "FT"],pch="F",cex=0.1,col=alpha("red",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "WF"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "WF"],pch="W",cex=0.1,col=alpha("blue",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "ASM"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "ASM"],pch="S",cex=0.1,col=alpha("white",alpha=0.5))
legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)

# Next we'll do the effort model...
mins <- floor(min(range(effort.spatial,na.rm=T)))
maxs <- ceiling(max(range(effort.spatial,na.rm=T)))
lvls <- c(5,10,20,25,30,35,40,45,50)
col.rmp <- rev(magma(length(lvls-1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
leg.col <- addalpha(colorRampPalette(col.rmp,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8)

windows(8,8)
Scallop.plot.field(effort.spatial, mesh,xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                   lvls=lvls,colors = col.rmp)
# Add the survey locations
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "FT"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "FT"],pch="F",cex=0.1,col=alpha("red",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "WF"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "WF"],pch="W",cex=0.1,col=alpha("blue",alpha=0.5))
points(GBa.fish.dat$lon[GBa.fish.dat$fleet == "ASM"],GBa.fish.dat$lat[GBa.fish.dat$fleet == "ASM"],pch="S",cex=0.1,col=alpha("white",alpha=0.5))
legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
plot(exp.new.poly.sp,add=T)
plot(seed.new.poly.sp,add=T)
plot(gout.new.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)



# The more complex spatial field with fleet included model can be plotted like this...
# First one is CPUE
mins <- floor(min(range(cpue.spatial.mean,na.rm=T)))
maxs <- ceiling(max(range(cpue.spatial.mean,na.rm=T)))
lvls <- c(5,7.5,10,12.5,15,17.5,20,25,30,40,50,100,150,200,500)
col.rmp <- rev(magma(length(lvls)))

for(i in 1:length(fleets))
{
  windows(8,8)
  # This little bit is to constrain the colour ramp to relevant values for each fleet...
  loc.min <- min(cpue.fleet.res$mean.field[cpue.fleet.res$fleet == fleets[i]])
  loc.max <- max(cpue.fleet.res$mean.field[cpue.fleet.res$fleet == fleets[i]])
  #loc.min = 10
  pos.1 <- max(which(lvls <= loc.min))
  pos.2 <- min(which(lvls > loc.max))
  cols <- col.rmp[pos.1:pos.2]
  loc.lvls <- lvls[pos.1:pos.2]
  leg.lvls <- c(paste(loc.lvls[-length(loc.lvls)],'-',loc.lvls[-1],sep='')[-(length(loc.lvls)-1):-length(loc.lvls)],
                paste(loc.lvls[length(loc.lvls)-1],'+',sep=''))
  leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(loc.lvls)-1),alpha=0.8)
  # If we want to add in any seedboxes that might have been opened during this period
  add.boxes <- NULL
  # add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
  # Make the plot
  Scallop.plot.field(cpue.fleet.res$mean.field[cpue.fleet.res$fleet == fleets[i]], mesh,
                     xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                     lvls=loc.lvls,colors =cols,add_boxes = add.boxes)
  points(GBa.fish.dat$lon[GBa.fish.dat$fleet == fleets[i]],GBa.fish.dat$lat[GBa.fish.dat$fleet == fleets[i]],pch=19,cex=0.1,col=alpha("white",alpha=0.1))
  legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(leg.lvls))),title.adj = 0.2,
         pt.bg = c(rep(NA,length(leg.lvls))),bg=NA,bty="n")
}


# The more complex spatial field with fleet included model can be plotted like this...
# Here are the catch plots
mins <- floor(min(range(catch.spatial.mean,na.rm=T)))
maxs <- ceiling(max(range(catch.spatial.mean,na.rm=T)))
lvls <- c(100,200,300,400,500,600,700,800,1000,1200,1400,1600,1800,2000)
col.rmp <- rev(magma(length(lvls)))

for(i in 1:length(fleets))
{
  windows(8,8)
  # This little bit is to constrain the colour ramp to relevant values for each fleet...
  loc.min <- min(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]])
  loc.max <- max(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]])
  #loc.min = 10
  pos.1 <- max(which(lvls <= loc.min))
  pos.2 <- min(which(lvls > loc.max))
  cols <- col.rmp[pos.1:pos.2]
  loc.lvls <- lvls[pos.1:pos.2]
  leg.lvls <- c(paste(loc.lvls[-length(loc.lvls)],'-',loc.lvls[-1],sep='')[-(length(loc.lvls)-1):-length(loc.lvls)],
                paste(loc.lvls[length(loc.lvls)-1],'+',sep=''))
  leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(loc.lvls)-1),alpha=0.8)
  # If we want to add in any seedboxes that might have been opened during this period
  add.boxes <- NULL
  # add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
  # Make the plot
  Scallop.plot.field(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]], mesh,
                     xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                     lvls=loc.lvls,colors =cols,add_boxes = add.boxes)
  points(GBa.fish.dat$lon[GBa.fish.dat$fleet == fleets[i]],GBa.fish.dat$lat[GBa.fish.dat$fleet == fleets[i]],pch=19,cex=0.1,col=alpha("white",alpha=0.1))
  legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(leg.lvls))),title.adj = 0.2,
         pt.bg = c(rep(NA,length(leg.lvls))),bg=NA,bty="n")
}

# Here's the catch but zoomed in on the northern portion of the bank...
for(i in 1:length(fleets))
{
  windows(12,8)
  # This little bit is to constrain the colour ramp to relevant values for each fleet...
  loc.min <- min(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]])
  loc.max <- max(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]])
  #loc.min = 10
  pos.1 <- max(which(lvls <= loc.min))
  pos.2 <- min(which(lvls > loc.max))
  cols <- col.rmp[pos.1:pos.2]
  loc.lvls <- lvls[pos.1:pos.2]
  leg.lvls <- c(paste(loc.lvls[-length(loc.lvls)],'-',loc.lvls[-1],sep='')[-(length(loc.lvls)-1):-length(loc.lvls)],
                paste(loc.lvls[length(loc.lvls)-1],'+',sep=''))
  leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(loc.lvls)-1),alpha=0.8)
  # If we want to add in any seedboxes that might have been opened during this period
  add.boxes <- NULL
  # add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
  # Make the plot
  Scallop.plot.field(catch.fleet.res$mean.field[catch.fleet.res$fleet == fleets[i]], mesh,
                     xlim = c(-67.2,-66.2),ylim = c(41.85,42.2),dims=c(500,500),
                     lvls=loc.lvls,colors =cols,add_boxes = add.boxes)
  points(GBa.fish.dat$lon[GBa.fish.dat$fleet == fleets[i]],GBa.fish.dat$lat[GBa.fish.dat$fleet == fleets[i]],pch=19,cex=0.1,col=alpha("white",alpha=0.1))
  legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(leg.lvls))),title.adj = 0.2,
         pt.bg = c(rep(NA,length(leg.lvls))),bg=NA,bty="n")
}


# The more complex spatial field with fleet included model can be plotted like this...
# Here are the effort plots
mins <- floor(min(range(effort.spatial.mean,na.rm=T)))
maxs <- ceiling(max(range(effort.spatial.mean,na.rm=T)))
lvls <- c(0,15,20,25,30,35,40,45,50,60)
col.rmp <- rev(magma(length(lvls)))

for(i in 1:length(fleets))
{
  windows(8,8)
  # This little bit is to constrain the colour ramp to relevant values for each fleet...
  loc.min <- min(effort.fleet.res$mean.field[effort.fleet.res$fleet == fleets[i]])
  loc.max <- max(effort.fleet.res$mean.field[effort.fleet.res$fleet == fleets[i]])
  #loc.min = 10
  pos.1 <- max(which(lvls <= loc.min))
  pos.2 <- min(which(lvls > loc.max))
  cols <- col.rmp[pos.1:pos.2]
  loc.lvls <- lvls[pos.1:pos.2]
  leg.lvls <- c(paste(loc.lvls[-length(loc.lvls)],'-',loc.lvls[-1],sep='')[-(length(loc.lvls)-1):-length(loc.lvls)],
                paste(loc.lvls[length(loc.lvls)-1],'+',sep=''))
  leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(loc.lvls)-1),alpha=0.8)
  # If we want to add in any seedboxes that might have been opened during this period
  add.boxes <- NULL
  # add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
  # Make the plot
  Scallop.plot.field(effort.fleet.res$mean.field[effort.fleet.res$fleet == fleets[i]], mesh,
                     xlim = range(GBa.fish.dat$lon),ylim = range(GBa.fish.dat$lat),dims=c(500,500),
                     lvls=loc.lvls,colors =cols,add_boxes = add.boxes)
  points(GBa.fish.dat$lon[GBa.fish.dat$fleet == fleets[i]],GBa.fish.dat$lat[GBa.fish.dat$fleet == fleets[i]],pch=19,cex=0.1,col=alpha("white",alpha=0.1))
  legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(leg.lvls))),title.adj = 0.2,
         pt.bg = c(rep(NA,length(leg.lvls))),bg=NA,bty="n")
}












