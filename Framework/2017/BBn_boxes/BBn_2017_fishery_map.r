
yr = as.numeric(format(Sys.time(), "%Y")) # 
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

# Get the seedboxes...
seedboxes <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),stringsAsFactors = F)

C2 <- seedboxes[seedboxes$Bank == "BBn" & seedboxes$Common_name =="C2",]
C3 <- seedboxes[seedboxes$Bank == "BBn" & seedboxes$Common_name =="C3",]
# Make the C2 into a spatial object
C2.poly <- Polygons(list( Polygon(cbind(C2$X,C2$Y))),"C2")
C2.poly.sp <-SpatialPolygons(list(C2.poly))
C2.X.range <- range(C2.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
C2.Y.range <- range(C2.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
C3.poly <- Polygons(list( Polygon(cbind(C3$X,C3$Y))),"C2")
C3.poly.sp <-SpatialPolygons(list(C3.poly))
C3.X.range <- range(C3.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
C3.Y.range <- range(C3.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])

# Get the survey boundary for BBn
#Read4 Get the survey boundary polygons for all banks.
survey.bound.polys<-read.csv(paste(direct,"data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
bound.poly.surv <- as.PolySet(survey.bound.polys[survey.bound.polys$label=="BBn",],projection="LL")
# Now convert this to an object for sp and set this up and the boundary polygon for the mesh.
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)

# Get the fishery data
logs_and_fish(loc="offshore",year = 2017:2017,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
bbn.fish.dat <- new.log.dat[new.log.dat$bank == "BBn",]
cpue.dat <- fishery.dat(bbn.fish.dat,bk="BBn",yr=2017,surv='May',method='jackknife',direct=direct,period = "calyr") 	
# Get the coordinates for the data
coordinates(bbn.fish.dat) <- ~lon+lat


###############  Now pull out both of the seeboxes...
# This picks out the points that are within the C2 polygon
C2.pts <- over(bbn.fish.dat,C2.poly.sp)
C3.pts <- over(bbn.fish.dat,C3.poly.sp)
# This get the fishery data
C2.fish.dat <- bbn.fish.dat[which(C2.pts==1),]
C2.catch <- sum(C2.fish.dat$pro.repwt)
C2.effort <- sum(C2.fish.dat$hm)
C2.by.fleet <- aggregate(pro.repwt ~ fleet,C2.fish.dat,FUN=sum)
C3.fish.dat <- bbn.fish.dat[which(C3.pts==1),]
C3.catch <- sum(C3.fish.dat$pro.repwt)
C3.effort <- sum(C3.fish.dat$hm)
C3.by.fleet <- aggregate(pro.repwt ~ fleet,C3.fish.dat,FUN=sum)

Outside.fish.dat <- bbn.fish.dat[is.na(C2.pts) & is.na(C3.pts),]
Outside.catch <- sum(Outside.fish.dat$pro.repwt)
Outside.effort <- sum(Outside.fish.dat$hm)
Outside.by.fleet <- aggregate(pro.repwt ~ fleet,Outside.fish.dat,FUN=sum)

# here is where they were fishing within each box
png(paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/Fishery_on_BBn_May_2017.png",sep=""),units="in",width = 11,
    height = 8.5,res=420,bg = "transparent")
ScallopMap("BBn")
points(C3.fish.dat$lon[C3.fish.dat$fleet =="WF"],C3.fish.dat$lat[C3.fish.dat$fleet =="WF"],pch="W",cex=0.4,col="blue")
points(C3.fish.dat$lon[C3.fish.dat$fleet =="FT"],C3.fish.dat$lat[C3.fish.dat$fleet =="FT"],pch="F",cex=0.4,col="red")
plot(C3.poly.sp,add=T)
points(C2.fish.dat$lon[C2.fish.dat$fleet =="WF"],C2.fish.dat$lat[C2.fish.dat$fleet =="WF"],pch="W",cex=0.4,col="blue")
points(C2.fish.dat$lon[C2.fish.dat$fleet =="FT"],C2.fish.dat$lat[C2.fish.dat$fleet =="FT"],pch="F",cex=0.4,col="red")
points(Outside.fish.dat,pch=19,cex=0.4)
plot(C2.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)
dev.off()


##################################################  SPATIAL MODELING OF FISHERY  ##################################################  SPATIAL MODELING OF FISHERY
# Let's make an INLA model looking at the catch rates on the bank, should be doable...
loc <- cbind(bbn.fish.dat$lon, bbn.fish.dat$lat)

# Try to build a MESH...
#mesh <- inla.mesh.2d(loc, 
#                     max.edge=2, cutoff=0.005)

mesh <- inla.mesh.2d(loc, max.edge=c(0.1), cutoff=0.001,
                     boundary = inla.sp2segment(bound.poly.surv.sp))
ScallopMap("BBn")
plot(mesh,add=T)
points(bbn.fish.dat$lon,bbn.fish.dat$lat,pch=19,cex=0.5)


# Now make the A matrix, combination of your mesh and locations of our data
#loc <- cbind(df$SLO,df$SLA)
A <- inla.spde.make.A(mesh, loc)
dim(A)


## 3. Observation Likelihood, 
family1 = "gamma"
control.family1 <- list(control.link = list(model="log"))
bbn.fish.dat$i <- 1:nrow(bbn.fish.dat)

#######################################SPATIAL MODEL###############################################################
## Now let's try a spatial model.  This model is for yellowtail founder
### SPATIAL model
spde <- inla.spde2.pcmatern(mesh,    
                            prior.sigma=c(1,0.5),
                            prior.range=c(1,0.5))

a0 <- 1 # intercept
s <- 1:spde$n.spde # Size of our SPDE model (I think this is our sparse matrix but I might be dumb)


pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))

# A stack for the cpue
stk.cpue = inla.stack(tag="est",
                      data=list(y = bbn.fish.dat$kg.hm, link=1L),
                      effects=list(a0 = rep(1, nrow(bbn.fish.dat)), s = 1:spde$n.spde),
                      A = list(1, A))

# A stack for the effort
stk.effort = inla.stack(tag="est",
                        data=list(y = bbn.fish.dat$hm, link=1L),
                        effects=list(a0 = rep(1, nrow(bbn.fish.dat)), s = 1:spde$n.spde),
                        A = list(1, A))

# A stack for the catch
stk.catch = inla.stack(tag="est",
                       data=list(y = bbn.fish.dat$pro.repwt, link=1L),
                       effects=list(a0 = rep(1, nrow(bbn.fish.dat)), s = 1:spde$n.spde),
                       A = list(1, A))


# As soon as you make a spatial model make your own intercept.  Here is
formula3 <- y ~ 0 + a0 + f(s, model=spde)

# Let's giver, make the spatial model.
cpue.spatial.model<- inla(formula3, family=family1, data = inla.stack.data(stk.cpue),
                          control.predictor=list(A=inla.stack.A(stk.cpue),link=link, compute=TRUE))
cpue.spatial <- exp(cpue.spatial.model$summary.random$s$mean+cpue.spatial.model$summary.fixed$mean)

effort.spatial.model<- inla(formula3, family=family1, data = inla.stack.data(stk.effort),
                            control.predictor=list(A=inla.stack.A(stk.effort),link=link, compute=TRUE))
effort.spatial <- exp(effort.spatial.model$summary.random$s$mean+effort.spatial.model$summary.fixed$mean)

catch.spatial.model<- inla(formula3, family=family1, data = inla.stack.data(stk.catch),
                           control.predictor=list(A=inla.stack.A(stk.catch),link=link, compute=TRUE))
catch.spatial <- exp(catch.spatial.model$summary.random$s$mean+catch.spatial.model$summary.fixed$mean)





#local.plot.field(cpue.spatial, mesh,xlim = range(bbn.fish.dat$lon),ylim = range(bbn.fish.dat$lat))

# Get the projection.
proj = inla.mesh.projector(mesh,xlim = c(-66.6,-65.8),ylim = c(42.5,43), dims = c(300, 300))
cpue.proj = inla.mesh.project(proj, cpue.spatial)
effort.proj = inla.mesh.project(proj, effort.spatial)
catch.proj = inla.mesh.project(proj, catch.spatial)



###################################### Instead of just getting the CPUE/catch/effort for the current year we can do this for all years...
# Get the fishery data
years <- 1981:2017
logs_and_fish(loc="offshore",year = years,un=un.ID,pw=pwd.ID,db.con="PTRAN64",direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
bbn.all.fish.dat <- fish.dat[fish.dat$bank %in% c("BBn"),]
survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
bnk.survey.bound.poly <- survey.bound.polys[survey.bound.polys$label == "BBn",]
# Get the coordinates for the data
coordinates(bbn.fish.dat) <- ~lon+lat

pdf(paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/Spatial_CPUE_catch_effort_annual.pdf",sep=""),width = 14, height = 8.5,onefile=T)
par(mfrow=c(1,3),mar = c(0, 2, 0, 1), omi = c(0.2, 0.2, 0.2, 0.2))

lvls.cpue = seq(10,80,by=10)
lvls.effort <- c(0,25,50,100,200,400,600,800)
lvls.catch <- c(0,0.5,1,2,5,10,20,40)
bnk.polys <- NULL
for(i in 1:length(years))
{
  if(i < length(years))  dat <- bbn.all.fish.dat[bbn.all.fish.dat$date >= as.Date(paste(years[i],"-01-01",sep="")) & 
                                                bbn.all.fish.dat$date < as.Date(paste(years[i+1],"-01-01",sep="")),  
                                                  which(names(bbn.all.fish.dat) %in% c('ID','lon','lat','pro.repwt','hm'))]
  if(i == length(years)) dat <- bbn.all.fish.dat[bbn.all.fish.dat$date >= as.Date(paste(years[i],"-01-01",sep="")) ,  
                                                                 which(names(bbn.all.fish.dat) %in% c('ID','lon','lat','pro.repwt',"hm"))]
  dat <- na.omit(dat)
  names(dat)[1:5]<-c("X","Y","kg","hm","EID")
  if(nrow(dat) > 0) 
  {
    grid.size <- 1/60
    catch.tmp <- dat[names(dat) %in% c("X","Y","kg","EID")]
    effort.tmp <- dat[names(dat) %in% c("X","Y","hm","EID")]
    names(catch.tmp) <- c("X","Y","Z","EID")
    catch.tmp$Z <- catch.tmp$Z/1000
    names(effort.tmp) <- c("X","Y","Z","EID")
    gp.catch <- gridPlot(catch.tmp,bnk.survey.bound.poly,lvls.catch,border=NULL,FUN=sum,grid.size=1/60)
    gp.effort <- gridPlot(effort.tmp,bnk.survey.bound.poly,lvls.effort,border=NULL,FUN=sum,grid.size=1/60)
    gp.cpue <- gp.catch
    gp.cpue[[2]]$Z <- gp.catch[[2]]$Z*1000/gp.effort[[2]]$Z
    gp.cpue[[3]] <- plasma(n=(length(lvls.cpue)+1),alpha=1,begin=0.8,end=0.1)
    
    for(j in 1:length(lvls+1))
    {
    if(j == 1) gp.cpue[[2]]$col[gp.cpue[[2]]$Z < lvls[j]] <- gp.cpue[[3]][j]
    if(j > 1 && j < length(lvls)) gp.cpue[[2]]$col[gp.cpue[[2]]$Z >= lvls[j-1] & gp.cpue[[2]]$Z < lvls[j]] <- gp.cpue[[3]][j]
    if(j > length(lvls)) gp.cpue[[2]]$col[gp.cpue[[2]]$Z > lvls[j-1]] <- gp.cpue[[3]][j]
    } # end for(i in 1:length(lvls+1))
    
    bnk.polys[[years[i]]] <- gp.cpue
    titl <- paste("CPUE (BBn-",years[i],")",sep="")
    
    # First get CPUE
    ScallopMap("BBn",poly.lst=bnk.polys[[years[i]]][1:2],poly.border=bnk.polys[[2]]$border,xlab="",ylab="",
               title=titl, bathy.source="quick",
               plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
    
    tlvls<-lvls.cpue
    legend("bottomleft",c(paste("0-",tlvls[1],sep=""),
                          paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),
                          paste(tlvls[length(tlvls)],'+',sep='')),
           fill=bnk.polys[[years[i]]][[3]],title='CPUE (kg/hm)',inset=0.02,bg='NA',box.col=NA,cex=0.8)
    titl <- paste("Catch (BBn-",years[i],")",sep="")
    
    # Then get Catch
    ScallopMap("BBn",poly.lst=gp.catch[1:2],poly.border=gp.effort[[2]]$border,xlab="",ylab="",
               title=titl, bathy.source="quick",
               plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
    
    tlvls<-lvls.catch
    legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),
                          paste(tlvls[length(tlvls)],'+',sep='')),
           fill=gp.catch[[3]],title='Catch (tonnes)',inset=0.02,bg='NA',box.col=NA,cex=0.8)
    
    titl <- paste("Effort (BBn-",years[i],")",sep="")
    
    # Finally grab Effort
    ScallopMap("BBn",poly.lst=gp.effort[1:2],poly.border=gp.effort[[2]]$border,xlab="",ylab="",
               title=titl, bathy.source="quick",
               plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
    
    tlvls<-lvls.effort
    legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),
                          paste(tlvls[length(tlvls)],'+',sep='')),
           fill=gp.effort[[3]],title='Effort (h-m)',inset=0.02,bg='NA',box.col=NA,cex=0.8)
  } # end if(nrow(dat) > 0)
}# end (for i in 1:length(years))

dev.off()
##########
# Plot the spatial distribution of catch for each bank and save the image (or just plot to a window if you prefer)
















############# Now we can make the figures....################ Now we can make the figures....################ Now we can make the figures #################
png(paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/CPUE_C2_C3.png",sep=""),units="in",width = 16,
    height = 8,res=420,bg = "transparent")

lvls=seq(10,80,by=10)
cols <- c(rev(magma(length(lvls)-1,alpha=0.9,begin=0.2,end=1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
#windows(15,8)
par(mfrow=c(1,2))
ScallopMap(xlim = C2.X.range,ylim = C2.Y.range)
image(list(x = proj$x, y=proj$y, z = cpue.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)

#windows(11,11)
ScallopMap(xlim = C3.X.range,ylim = C3.Y.range)
image(list(x = proj$x, y=proj$y, z = cpue.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)
par(xpd=T)
legend(-65.944,42.88,leg.lvls,fill=cols,border="black",pch=c(rep(NA,length(lvls))),title = "CPUE: kg/(h-m)",title.adj = 0.1,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n",cex=0.8)
dev.off()




png(paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/Effort_C2_C3.png",sep=""),units="in",width = 16,
    height = 8,res=420,bg = "transparent")

lvls=seq(18,36,by=3)
cols <- c(rev(plasma(length(lvls)-1,alpha=0.9,begin=0.2,end=1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
#windows(15,8)
par(mfrow=c(1,2))
ScallopMap(xlim = C2.X.range,ylim = C2.Y.range)
image(list(x = proj$x, y=proj$y, z = effort.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)

#windows(11,11)
ScallopMap(xlim = C3.X.range,ylim = C3.Y.range)
image(list(x = proj$x, y=proj$y, z = effort.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)
par(xpd=T)
legend(-65.944,42.88,leg.lvls,fill=cols,border="black",pch=c(rep(NA,length(lvls))),title = "Effort: (h-m)/watch",title.adj = 0.1,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n",cex=0.8)
dev.off()


png(paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/Catch_C2_C3.png",sep=""),units="in",width = 16,
    height = 8,res=420,bg = "transparent")

lvls=seq(100,1225,by=125)
cols <- c(rev(inferno(length(lvls)-1,alpha=0.9,begin=0.2,end=1)))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
#windows(15,8)
par(mfrow=c(1,2))
ScallopMap(xlim = C2.X.range,ylim = C2.Y.range)
image(list(x = proj$x, y=proj$y, z = catch.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)

#windows(11,11)
ScallopMap(xlim = C3.X.range,ylim = C3.Y.range)
image(list(x = proj$x, y=proj$y, z = catch.proj), axes=F,las=1,add=T,breaks=lvls,col=cols)
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="WF"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="WF"],pch="W",cex=0.6,col="red")
points(bbn.fish.dat$lon[bbn.fish.dat$fleet =="FT"],bbn.fish.dat$lat[bbn.fish.dat$fleet =="FT"],pch="F",cex=0.6,col="blue")
addPolys(seedboxes[seedboxes$Bank == "BBn" & seedboxes$Open > 2017,],lwd=0.5,lty=2)
par(xpd=T)
legend(-65.944,42.88,leg.lvls,fill=cols,border="black",pch=c(rep(NA,length(lvls))),title = "Catch: (kg/watch)",title.adj = 0.1,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n",cex=0.8)
dev.off()


