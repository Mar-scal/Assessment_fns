#####################################  This sript is used to make a few plots that we don't normally show during survey summary


# Load your directory and the survey year
direct <- "d:/r/"
# direct <- "F:/Offshore scallop/Assessment/"
yr <- 2018

# Load in the spring survey results from this year.
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results.Rdata",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/shf.plt.r",sep=""))
library(PBSmapping)
library(INLA)
library(maptools)
banks <- "GB"

# This makes a 3 panel shf plot for the 3 seedboxes that are closed on GB this year.
# First 

par(mfrow=c(1,3))
boxy <- NULL
tmp <- NULL
tmp2 <- NULL
# Pick the box and pull out the data we want
for(j in 1:3)
{
  tmp[[j]] <- seedbox.obj$GB[[j]]$model.dat[seedbox.obj$GB[[j]]$model.dat$year %in% yr,]
  #
  tmp2[[j]]<- seedbox.obj$GB[[j]]$shf.dat$n.yst[nrow(seedbox.obj$GB[[j]]$shf.dat$n.yst),]
  #
  #if(j > 1) boxy$shf.dat$n.yst <- rbind(boxy$shf.dat$n.yst,seedbox.obj$GB[[j]]$shf.dat$n.yst[nrow(seedbox.obj$GB[[j]]$shf.dat$n.yst),])
}
boxy$model.dat <- do.call("rbind",tmp)
boxy$model.dat$year <- 2016:2018
boxy$shf.dat$n.yst <- do.call("rbind",tmp2)
# Get the number of survey tows.
s.size <- boxy[[1]]$n
SHF.title <- "C4-C6 SHF 2018"
# Make the plot.
windows(8.5,11)
png(filename=paste0(direct, "2018/Presentations/Survey_summary/test_figures/GB/seedbox SHF summary.png"), units="in",width = 8.5,height = 11,res=420,bg = "transparent")
shf.plt(boxy,from='surv',yr=2016:2018, col1='grey80',col2=1,rel=F,rows=3,adj = -5, ymax=c(400,400,800),
        recline=c(85,95),add.title = T,titl = SHF.title,cex.mn=3,sample.size = F)	
par(xpd=NA)
text(x = c(165,165,165),y = c(2500,1600,700),labels = paste("N =", s.size,sep=" "),cex=2)
text(x = c(25,25,25),y = c(2500,1600,700),labels = c("C4","C5","C6"),cex=2)
dev.off()

## For the spatial picture of the Starbox on Sable bank we need to run this since it is out side of the survey
## strata.  If the starbox becomes a regular on the tour this should be included 

# original presentations (maybe OSAC, but this isn't OSAC)....
sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))


bound.poly.surv <- subset(survey.bound.polys,label==banks[i]) 
attr(bound.poly.surv,"projection")<-"LL"
n.box <- length(seedbox.obj[[banks[i]]])
boxes <- as.PolySet(sb,projection = "LL")
box.dat <- data.frame(EID=1:nrow(surv.Live[[banks[i]]]),X=surv.Live[[banks[i]]]$lon,Y=surv.Live[[banks[i]]]$lat)
box.names <- unique(boxes$SCALLOP_Group_ID)
fig.box.name <- unique(boxes$Common_name)
j=1


# Get the data for the box of interest
key <-findPolys(box.dat, subset(boxes,SCALLOP_Group_ID == box.names[j]))  
this.box <- subset(boxes,SCALLOP_Group_ID == box.names[j])
boxy <- seedbox.obj[[banks[i]]][[j]]
surv.seed <- surv.Live[[banks[i]]][1:nrow(surv.Live[[banks[i]]]) %in% key$EID,]

# If any of the Strata_ID's are outside of the survey stratification scheme we will run the spatial plots as
# local fields using the box data only.
if(any(is.na(surv.seed$Strata_ID))==T)
{
  mod.res <- NULL
  pre.proj <- NULL
  # Now set up the INLA for this seedbox...
  bound.poly.surv.sp <- PolySet2SpatialPolygons(as.PolySet(this.box,projection="LL"))
  loc <- cbind(surv.seed$lon[surv.seed$year == yr],
               surv.seed$lat[surv.seed$year == yr])
  # Build the mesh, for our purposes I'm hopeful this should do the trick.
  mesh <- inla.mesh.2d(loc, max.edge=c(0.1), cutoff=0.001,
                       boundary = inla.sp2segment(bound.poly.surv.sp))
  # plot(mesh)
  #plot(mesh.cf)
  A <- inla.spde.make.A(mesh, loc)
  ## All of our spatial plots are counts, so for our simple purposes a poisson is o.k.
  family1 = "poisson"
  # We can just make the one spde object for all of these as well.
  spde <- inla.spde2.pcmatern(mesh,    
                              prior.sigma=c(1,0.5),
                              prior.range=c(1,0.5))
  # As soon as you make a spatial model make your own intercept.  Here is
  a0 <- 1 # intercept
  # Mostly just using stock priors, again fine for our purposes for the moment.
  pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
  # basically the random deviations for each piece of the mesh.
  formula3 <- y ~ 0 + a0 + f(s, model=spde)
  
    for(k in 1:length(seed.n.spatial.maps))
    {
      # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
      if(seed.n.spatial.maps[k] == "PR-spatial")    
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = surv.seed$pre[surv.seed$year == yr], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde$n.spde),
                          A = list(1, A))
        # This is the INLA model itself
        mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
        
        # This projects our mesh
        proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
      } # end if(seed.n.spatial.maps[k] == "PR-spatial")   
      
      if(seed.n.spatial.maps[k] == "Rec-spatial")        
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = surv.seed$rec[surv.seed$year == yr], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde$n.spde),
                          A = list(1, A))
        # This is the INLA model itself
        mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
        
        # This projects our mesh
        proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        
      } # end if(seed.n.spatial.maps[k] == "Rec-spatial") 
      
      if(seed.n.spatial.maps[k] == "FR-spatial") 
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = surv.seed$com[surv.seed$year == yr], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde$n.spde),
                          A = list(1, A))
        # This is the INLA model itself
        mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
        
        # This projects our mesh
        proj <- inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] <- inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
      } # end if(seed.n.spatial.maps[k] == "FR-spatial")
      
      
      if(seed.n.spatial.maps[k] == "CF-spatial")       
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = CF.current[[banks[i]]]$CF, link=1L),
                          effects=list(a0 = rep(1, nrow(CF.current[[banks[i]]])), s = 1:spde.cf$n.spde),
                          A = list(1, A.cf))
        # This is the INLA model itself
        mod <- inla(formula3.cf, family=family1.cf, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- mod$summary.random$s$mean+mod$summary.fixed$mean
        
        # This projects our mesh
        proj = inla.mesh.projector(mesh.cf,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
      }
      # THis seems to be making sense...
      if(seed.n.spatial.maps[k] == "MC-spatial")      
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = CF.current[[banks[i]]]$meat.count, link=1L),
                          effects=list(a0 = rep(1, nrow(CF.current[[banks[i]]])), s = 1:spde.cf$n.spde),
                          A = list(1, A.cf))
        # This is the INLA model itself
        mod <- inla(formula3.cf, family=family1.cf, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- mod$summary.random$s$mean+mod$summary.fixed$mean
        
        # This projects our mesh
        proj = inla.mesh.projector(mesh.cf,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
      } # end if(seed.n.spatial.maps[k] == "MC-spatial") 
      
      if(seed.n.spatial.maps[k] == "Clap-spatial")        
      {
        # This is the stack for the INLA model
        stk <- inla.stack(tag="est",data=list(y = surv.Clap[[banks[i]]]$clap.prop[surv.Clap[[banks[i]]]$year == yr], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                          A = list(1, A))
        # This is the INLA model itself
        mod <- inla(formula3, family=family.clap, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
        # This is a total hack for the moment, but b/c we're using a poisson model here it is possible to get values
        # > 100 which of course is BS.  Problem is I can't get any other likelihoods to give decent results.
        # For the rare case in which the poisson is predicting > 100 this seems to do the trick and give
        # resonalbe results for all banks.  So far I've only had this problem on BBs....
        mod.res[[seed.n.spatial.maps[k]]][mod.res[[seed.n.spatial.maps[k]]] > 100] <- 100
        # This projects our mesh
        proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
      } # end if(seed.n.spatial.maps[k] == "Clap-spatial")  
      
      # End the loop for getting all the data needed for a bank for the spatial maps.
    } # end for(k in 1:length(seed.n.spatial.maps))
  } # end if(length(seed.n.spatial.maps > 0))
  # Now we need to get the projections if we have specified the User.SH.bins plots to be produced.
  bin.names <- NULL # Name bin.names a NULL, if no user.SH.bins we still need this name to exist...
  if(any(plots == "user.SH.bins"))
  {
    # Get the number of these calculations we need to make..
    if(banks[i]  != "Ger") 
    {
      num.bins <- length(survey.obj[[banks[i]]]$bin.names)
      bin.names <- survey.obj[[banks[i]]]$bin.names
      user.bins <- survey.obj[[banks[i]]]$user.bins
    } # end if(banks[i]  != "Ger") 
    if(banks[i]  == "Ger")
    {
      num.bins <- length(spr.survey.obj$bin.names)
      bin.names <- spr.survey.obj$bin.names
      user.bins <- spr.survey.obj$user.bins
    } # end if(banks[i]  == "Ger")
    
    #ranges.bm <- data.frame(bin = rep(NA,num.bins/2),min = rep(NA,num.bins/2),max=rep(NA,num.bins/2)) # I need this to make nice figures for the biomasses...
    #count = 0
    for(k in 1:num.bins)
    {
      # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
      # This is the stack for the INLA model
      pick <- which(names(surv.seed) == bin.names[k])
      stk <- inla.stack(tag="est",data=list(y = surv.seed[surv.seed$year == yr,pick], link=1L),
                        effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde$n.spde),
                        A = list(1, A))
      # This is the INLA model itself
      mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                  control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
      # These are the results for each spatial mesh
      mod.res[[bin.names[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
      
      # This projects our mesh
      proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
      # this makes a big matrix with the values in the proper locations
      pre.proj[[bin.names[k]]] = inla.mesh.project(proj, mod.res[[bin.names[k]]])
      # This gets us the ranges of biomass values, I just did this once to get a good sense of good levels.
      #if(length(grep("bm",bin.names[k])) >0) 
      #{
      #  count <- count + 1
      #  ranges.bm[count,] <- c(bin.names[k],range(pre.proj[[bin.names[k]]],na.rm=T)) 
      #} # end if(length(grep("bm",bin.names[k])) >0) 
    } # End for(k in 1:num.bins)
  }# end if(any(plots %in% "user.SH.bins"))
  
  
}  # end if(any(is.na(surv.seed$Strata_ID))==T)



#######################  Here I look at German bank pre recruits but without the patch tow included.
tmp <-lined.dat[-which(lined.dat$random %in% c(2,4,5)),]
tmp <- tmp[-which(tmp$pre == max(tmp$pre)),]
# Can see that reduces the abundance by about 33 % from 295 to 188
tmp.spr.survey.obj <- sprSurv(tmp,2008:yr,ger.tows,chng=T)


###################### FK's seedbox SHF summary for BBn
# This makes a 3 panel shf plot for the 2 seedboxes that are closed on BBn this year.
# First 

par(mfrow=c(1,2))
boxy <- NULL
tmp <- NULL
tmp2 <- NULL
# Pick the box and pull out the data we want
for(j in 1:2)
{
  tmp[[j]] <- seedbox.obj$BBn[[j]]$model.dat[seedbox.obj$BBn[[j]]$model.dat$year %in% yr,]
  #
  tmp2[[j]]<- seedbox.obj$BBn[[j]]$shf.dat$n.yst[nrow(seedbox.obj$BBn[[j]]$shf.dat$n.yst),]
  #
  #if(j > 1) boxy$shf.dat$n.yst <- rbind(boxy$shf.dat$n.yst,seedbox.obj$GB[[j]]$shf.dat$n.yst[nrow(seedbox.obj$GB[[j]]$shf.dat$n.yst),])
}
boxy$model.dat <- do.call("rbind",tmp)
boxy$model.dat$year <- 2017:2018
boxy$shf.dat$n.yst <- do.call("rbind",tmp2)
# Get the number of survey tows.
s.size <- boxy[[1]]$n
SHF.title <- "C8 & C9 SHF 2018"
# Make the plot.
windows(8.5,11)
png(filename=paste0(direct, "2018/Presentations/Survey_summary/test_figures/BBn/seedbox SHF summary.png"), units="in",width = 8.5,height = 11,res=420,bg = "transparent")
shf.plt(boxy,from='surv',yr=2017:2018, col1='grey80',col2=1,rel=F,rows=2,adj = -5, ymax=160,
        recline=c(85,95),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
par(xpd=NA)
text(x = c(165,165),y = c(330,150),labels = paste("N =", s.size,sep=" "),cex=2)
text(x = c(25,25),y = c(330,150),labels = c("C8","C9"),cex=2)
dev.off()
