yr <- 2016
direct <- "d:/r/"
library(INLA)
library(lattice)
require(maps)
require(maptools)
require(mapdata)
library(rgeos)
library(splancs)
library(boot)
library(fields)
library(PBSmapping)
library(viridis)
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  

yr <- 2016
direct <- "d:/r/"

source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))


i=1
banks <- "BBs"

# All of this happens just onces for each bank.
# So we make this for each bank
bound.poly.surv <- as.PolySet(survey.bound.polys[survey.bound.polys$label==banks[i],],projection="LL")
# Now convert this to an object for sp and set this up and the boundary polygon for the mesh.
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
#loc <- cbind(CF.current[[banks[i]]]$lon[CF.current[[banks[i]]]$year == yr],
#             CF.current[[banks[i]]]$lat[CF.current[[banks[i]]]$year == yr])
loc <- cbind(surv.Live[[banks[i]]]$slon[surv.Live[[banks[i]]]$year == yr],
             surv.Live[[banks[i]]]$slat[surv.Live[[banks[i]]]$year == yr])



### build the  bounded mesh 
mesh <- inla.mesh.2d(loc, 
                           max.edge=c(0.05), cutoff=0.001,
                           boundary=inla.sp2segment(bound.poly.surv.sp))
plot(mesh)
# Build the mesh, for our purposes I'm hopeful this should do the trick.
mesh <- inla.mesh.2d(loc, max.edge=0.5, cutoff=0.001)
plot(mesh)
A <- inla.spde.make.A(mesh, loc)
## All of our spatial plots are counts, so for our purposes a quasi-poisson should do the trick everytime
#family1 = "zeroinflated.poisson.0"
family1 <- "poisson"
family.clap <- "poisson"
#control.family1 <- list(control.link = list(model="log"))
# We can just make the one spde object for all of these as well.
spde <- inla.spde2.pcmatern(mesh,    
                            prior.sigma=c(1,0.5),
                            prior.range=c(1,0.5))
# As soon as you make a spatial model make your own intercept.  Here is
a0 <- 1 # intercept
s <- 1:spde$n.spde # Size of our SPDE model (I think this is our sparse matrix but I might be dumb)
# Mostly just using stock priors, again fine for our purposes for the moment.
pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))

#surv.Live[[banks[i]]]$i[surv.Live[[banks[i]]]$year == yr] <- 1:nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])
formula3 <- y ~ 0 + a0 + f(s, model=spde)

#############  This part of the code is going to have to be done for each type of plot we are looking for on a bank.
#stk = inla.stack(tag="est",
#                    data=list(y = CF.current[[banks[i]]]$CF[CF.current[[banks[i]]]$year == 2017], link=1L),
#                    effects=list(a0 = rep(1, nrow(CF.current[[banks[i]]][CF.current[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
#                    A = list(1, A))
stk = inla.stack(tag="est",
                 data=list(y = surv.Clap[[banks[i]]]$clap.prop[surv.Clap[[banks[i]]]$year == yr], link=1L),
                 effects=list(a0 = rep(1, nrow(surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                 A = list(1, A))
stk = inla.stack(tag="est",
                 data=list(y = surv.Live[[banks[i]]]$pre[surv.Live[[banks[i]]]$year == yr], link=1L),
                 effects=list(a0 = rep(1, nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                 A = list(1, A))

mod<- inla(formula3, family=family.clap, data = inla.stack.data(stk),#control.family = control.family1,
                           control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE),verbose=T)

mod.res <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
#mod.res <- (mod$summary.random$s$mean+mod$summary.fixed$mean)

proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
pre.proj = inla.mesh.project(proj, mod.res)

sort(mod.res)

# Now get the color correct...
lvls=c(0,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
cols <- c(rev(magma(length(lvls[lvls < 2000])-1,alpha=0.7,begin=0.6,end=1)),
          rev(magma(length(lvls[lvls > 1000]),alpha=0.8,begin=0.1,end=0.5)))

max.lvl <- lvls[which(lvls >= max(mod.res,na.rm=T))[1]]
# Get the levels correct
if(length(max.lvl) > 0) 
{
  lvls <- lvls[lvls <= max.lvl]
  cols <- cols[1:(length(lvls)-1)]
}

# For condition...
base.lvls <- c(0,5,8,10,12,14,16,18,50)
cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
# Get the levels correct            
min.lvl <- max(which(base.lvls <= min(mod.res)))
max.lvl <- min(which(base.lvls >= max(mod.res)))
lvls <- base.lvls[min.lvl:max.lvl]
cols <- cols[min.lvl:(max.lvl-1)]
ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                   paste(lvls[length(lvls)-1],'+',sep='')),
                                     leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                   paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))

# For clappers...
# The color ramp for Clapper proportion
base.lvls=c(0,5,10,15,20,50,100)
cols <- rev(plasma(length(base.lvls)-1,alpha=0.7))
# Get the levels correct            
min.lvl <- max(which(base.lvls <= min(pre.proj,na.rm=T)))
max.lvl <- min(which(base.lvls >= max(pre.proj,na.rm=T)))
lvls <- base.lvls[1:max.lvl]
cols <- cols[min.lvl:(max.lvl-1)]
ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                   paste(lvls[length(lvls)-1],'+',sep='')),
       leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                     paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))


# For some reason the colors are not matching up, I don't know what that reason is, but the legend and the figure
# aren't showing the same thing, figure this out tomorrow!!
pdf(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/tester.pdf",sep=""),width = 11,height = 8.5)
#windows(11,11)
ScallopMap("BBs",title="Pre-recs",direct=direct,plot.bathy = T,bathy.source = "quick")
image(list(x = proj$x, y=proj$y, z = pre.proj), axes=F,las=1,add=T,breaks = lvls,col=cols)
plot(bound.poly.surv.sp,add=T,lwd=2)
#points(CF.current[[banks[i]]]$lon[CF.current[[banks[i]]]$year == yr],CF.current[[banks[i]]]$lat[CF.current[[banks[i]]]$year == yr],cex=0.8,pch=19)
points(surv.Live[[banks[i]]]$slon[surv.Live[[banks[i]]]$year == yr],surv.Live[[banks[i]]]$slat[surv.Live[[banks[i]]]$year == yr],cex=0.8,pch=19)

legend("bottomleft",leg.lvls,fill=cols,
       title="N/tow", title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
dev.off()